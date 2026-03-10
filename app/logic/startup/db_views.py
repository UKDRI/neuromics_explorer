"""
Shared helpers for DuckDB ATTACH and semantic view management.

Used by registry_parser.build_registry_index() and data_summaries.build_dataset_stats()
to avoid duplicating attach/view logic across modules.

"""

from datetime import datetime, timezone
import duckdb



def get_sql_col(col_name: str | None, fallback: str = "NULL") -> str:
    """
    Return a quoted SQL column from an original column name from column_mappings,
    or a literal fallback if the name is None/ unmapped to prevent None or unquoted identifiers.

    Used to safely embed original column names from column_mappings into
    SQL strings without risk of injecting None or unquoted identifiers.

    Examples:
        get_sql_col("Mouse_Gene")          →  '"Mouse_Gene"'
        get_sql_col("padj")                →  '"padj"'
        get_sql_col(None)                  →  'NULL'
        get_sql_col(None, "NULL::INTEGER") →  'NULL::INTEGER'
    """
    return f'"{col_name}"' if col_name else fallback


def attach_source_dbs(
    con: duckdb.DuckDBPyConnection,
    datasets: list[tuple],          # rows of (study_id, lab_source, ..., data_path, ...)
    lab_source_index: int,          # positional index of lab_source in each row
    data_path_index: int,           # positional index of data_path in each row
) -> tuple[dict[str, str], list[tuple]]:
    """
    Attach each unique DuckDB source file once per connection, returning
    a {alias : data_path} map for use in view creation and querying.

    Returns:
        attached_dbs:   dict mapping data_path → alias (e.g. "data/diaz.duckdb" → "src_diaz")
        failed:         list of (data_path, lab_source, error_str) tuples for any hard failures
    """
    attached_dbs: dict[str, str] = {}
    failed:   list[tuple]    = []

    for row in datasets:
        data_path    = row[data_path_index]
        lab_source   = row[lab_source_index]

        if not data_path or data_path in attached_dbs:
            continue    # already attached this session

        if not data_path.endswith(".duckdb"):   # skip RDS / parquet — cannot ATTACH
            continue

        # Attach each unique db_path once if not done already, record alias per path
        if data_path and data_path not in attached_dbs: # two guards against passing None to ATTACH & prevent re-attaching the same db under a second alias with multiple studies
            alias = f"src_{lab_source}"
            try:
                con.execute(f"ATTACH '{data_path}' AS {alias} (READ_ONLY)")
                attached_dbs[data_path] = alias
            except Exception as e:
                if "already attached" in str(e).lower() or "already exists" in str(e).lower():
                    attached_dbs[data_path] = alias     # record the alias it was given even if same file path used for different datasets, and continue
                else:
                    failed.append((data_path, lab_source, str(e)))
                    print(f"   ERROR attaching '{data_path}' as {alias}: {e}")
        # db_alias = attached_dbs[data_path]

    return attached_dbs, failed



def create_views(
    con: duckdb.DuckDBPyConnection,
    study_id: int,
    lab_source: str,
    dataset_name: str,
    db_alias: str,
    # actual_table: str,
    name_mappings: dict[str, str],  # {canonical_name: original_col}
    table_map: dict[str, str],  # {}
) -> tuple[list[str], list[tuple]]:
    """
    Create or replace the canonical semantic view for a dataset.
    - expression view (v_{lab}_{id}) for gene/feature identifiers + DE metrics only
    - metadata view (vm_{lab}_{id}) for sample/cell metadata (when obs_metadata table exists)
    
    The view translates heterogeneous source column names → canonical names
    so all downstream queries (gene index, stats, API endpoints) use consistent names.
    Reduces need to materialise data for zero-copy data streams/ transfers.

    Returns:
        created:    list of view names successfully created (e.g. "v_diaz_1")
        failed:     list of (logged_at, study_id, lab_source, dataset_name, issue) tuples if view creation fails
    """
    created: list[str]  = []
    failed: list[tuple] = []

    expr_view       = f"v_{lab_source}_{study_id}"
    obs_meta_view   = f"vm_{lab_source}_{study_id}"

    expr_table      = table_map.get("expression")
    obs_meta_table  = table_map.get("obs_metadata") or table_map.get("extra_metadata")
    # extra_meta_table  = table_map.get("extra_metadata")   #TODO: append into a single metadata_table?
    # metadata_table    = table_map.get("obs_metadata") or/and table_map.get("extra_metadata")
    gene_col        = name_mappings.get("gene_symbol")
    protein_col     = name_mappings.get("protein_id")
    human_col       = name_mappings.get("human_gene")      # Identify human gene column if available for cross-species mapping
    organism_col    = name_mappings.get("organism")
    organism        = f"'{organism_col}'" if organism_col else "'unknown'"

    # Build COALESCE expression for primary gene column so Mouse_Gene NULLs and it falls through to Human_Gene
    gene_candidates = [get_sql_col(c) for c in [gene_col, protein_col, human_col] if c]
    if not gene_candidates:
        err = f"No gene or protein column mapping for [{lab_source}] study_id={study_id}"
        print(f"   WARNING: {err} — skipping views")
        failed.append((datetime.now(timezone.utc), study_id, lab_source, dataset_name, err))
        return created, failed
    
    primary_gene_col = (
        f"COALESCE({', '.join(gene_candidates)})"
        if len(gene_candidates) > 1
        else gene_candidates[0]
    )

    print(f" [DEBUG] {lab_source} study_id={study_id} | alias={db_alias} | "
        f"expr_table={expr_table} | obs_table={obs_meta_table} | gene_col={gene_col} \n | primary_gene_col={primary_gene_col} \n | gene_candidates: {gene_candidates} \n | protein_col={protein_col} | human_col={human_col} | organism_col={organism_col} | expr_view={expr_view} | obs_meta_view={obs_meta_view}")
    # protein_col=Uniprot_id | protein_expr="Uniprot_id" 
    # primary_gene_col: COALESCE("Gene_Symbol", "Uniprot_ID")    gene_candidates: ['"Gene_Symbol"', '"Uniprot_ID"'] 

    # --- Expression view (gene identifiers + DE metrics only) ---
        #TODO: check "" colnames VS '' string e.g.      protein_expr = f'"{protein_col}"' if protein_col != "NULL" else "NULL"
    if expr_table:
        try:
            con.execute(f"""
                CREATE OR REPLACE VIEW {expr_view} AS
                SELECT
                    {study_id}                                              AS study_id,
                    -- Feature identifiers
                    {primary_gene_col}                                      AS gene_symbol,
                    {get_sql_col(name_mappings.get('human_gene'))}          AS human_gene,
                    {get_sql_col(name_mappings.get('protein_id'))}          AS protein_id,      -- {protein_col} AS protein_id,
                    {organism}                                              AS organism,
                    -- Expression metrics
                    {get_sql_col(name_mappings.get('log2fc'))}              AS log2fc,
                    {get_sql_col(name_mappings.get('pvalue'))}              AS pvalue,
                    {get_sql_col(name_mappings.get('padj'))}                AS padj,
                    {get_sql_col(name_mappings.get('abundance_a'))}         AS abundance_a,
                    {get_sql_col(name_mappings.get('abundance_b'))}         AS abundance_b,
                    {get_sql_col(name_mappings.get('pct_expressed_a'))}     AS pct_expressed_a,
                    {get_sql_col(name_mappings.get('pct_expressed_b'))}     AS pct_expressed_b,
                    {get_sql_col(name_mappings.get('expression_metric'))}   AS expression_metric,

                    -- Sample / cell metadata (when present in expression table)
                    {get_sql_col(name_mappings.get('sample_a'))}            AS sample_a,
                    {get_sql_col(name_mappings.get('sample_b'))}            AS sample_b,
                    {get_sql_col(name_mappings.get('condition_a'))}         AS condition_a,
                    {get_sql_col(name_mappings.get('condition_b'))}         AS condition_b,
                    {get_sql_col(name_mappings.get('cell_type'))}           AS cell_type,
                FROM {db_alias}.main.{expr_table}
                WHERE study_id = {study_id}
                AND {primary_gene_col} IS NOT NULL
            """)        # '{source_id}'.main.{expression_table} OR '{data_path}'.{actual_table} OR attach_alias.main.actual_table
            created.append(expr_view)
        except Exception as e:
            print(f"   ERROR creating expression view {expr_view} while building index: {e}")
            failed.append((
                datetime.now(timezone.utc), study_id, lab_source, dataset_name, 
                f"expression view creation failed: {e}"
            ))
    
    # --- Metadata view (sample/cell metadata from separate obs_metadata table) ---
    if obs_meta_table:
        try:
            con.execute(f"""
                CREATE OR REPLACE VIEW {obs_meta_view} AS
                SELECT
                    {study_id}                                              AS study_id,
                    -- Sample / cell metadata
                    {get_sql_col(name_mappings.get('sample_a'))}            AS sample_a,
                    {get_sql_col(name_mappings.get('sample_b'))}            AS sample_b,
                    {get_sql_col(name_mappings.get('condition_a'))}         AS condition_a,
                    {get_sql_col(name_mappings.get('condition_b'))}         AS condition_b,
                    {get_sql_col(name_mappings.get('cell_type'))}           AS cell_type,
                    {get_sql_col(name_mappings.get('cell_id'))}             AS cell_id,
                    {get_sql_col(name_mappings.get('cluster_id'))}          AS cluster_id,
                    {get_sql_col(name_mappings.get('tissue'))}              AS tissue,
                    {get_sql_col(name_mappings.get('age'), 'NULL::INTEGER')} AS age,
                    {get_sql_col(name_mappings.get('sex'))}                 AS sex
                FROM {db_alias}.main.{obs_meta_table}
                WHERE study_id = {study_id}
            """)
            created.append(obs_meta_view)
        except Exception as e:
            print(f"   ERROR creating obs metadata view {obs_meta_view} while building index: {e}")
            failed.append((
                datetime.now(timezone.utc), study_id, lab_source, dataset_name, 
                f"obs view creation failed: {e}"
            ))
    return created, failed



def view_exists(con: duckdb.DuckDBPyConnection, view_name: str) -> bool:
    """Check information_schema.views for DuckDB views."""
    row = con.execute("""
        SELECT 1
        FROM information_schema.views
        WHERE table_name = ?
        LIMIT 1
        """,
        [view_name]
    ).fetchone()
    return bool(row)
