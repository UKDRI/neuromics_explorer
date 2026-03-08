""""
Module parses YAML registry and loads it into neuromics_registry.duckdb.
.duckdb will contain canonical mappings and indexes of datasets, (columns and tables). 
It also creates DuckDB Views to implement a semantic layer for querying and aggregating data.
"""

from datetime import datetime, timezone
import yaml
import duckdb

# Every dataset gets mapped to these canonical names within the dictionary which also contains their categories.
CANONICAL_NAMES = {
    # Gene / feature identifiers
    "gene_symbol":          {"required": True,  "category": "feature"},
    "human_gene":           {"required": False, "category": "feature"},
    "protein_id":           {"required": False, "category": "feature"},
    "ensembl_id":           {"required": False, "category": "feature"},
    "entrez_id":            {"required": False, "category": "feature"},
    "protein_name":         {"required": False, "category": "feature"},
    "feature_description":  {"required": False, "category": "feature"},
    "location":             {"required": False, "category": "feature"},
    "biotype":              {"required": False, "category": "feature"}, #?

    # (Differential expression) Metrics
    "log2fc":               {"required": True,  "category": "metric"},
    "pvalue":               {"required": False, "category": "metric"},
    "padj":                 {"required": False, "category": "metric"},
    "abundance_a":          {"required": False, "category": "metric"},
    "abundance_b":          {"required": False, "category": "metric"},
    "pct_expressed_a":      {"required": False, "category": "metric"},
    "pct_expressed_b":      {"required": False, "category": "metric"},
    "expression_metric":    {"required": False, "category": "metric"},
    "normalisation_method": {"required": False, "category": "metric"},

    # Sample / cell metadata 
    # Any columns that aren't a feature or metric is considered metadata and can be used in filtering or plotting
    "sample_a":         {"required": False,"category": "metadata"},
    "sample_b":         {"required": False,"category": "metadata"},
    "condition_a":      {"required": False,"category": "metadata"},
    "condition_b":      {"required": False,"category": "metadata"},
    "cell_type":        {"required": False, "category": "metadata"},
    "age":              {"required": False, "category": "metadata"},
    "organism":         {"required": False, "category": "metadata"},
    "sex":              {"required": False, "category": "metadata"},
    "tissue":           {"required": False, "category": "metadata"},
    "cell_line":        {"required": False, "category": "metadata"},
    "cluster_id":       {"required": False, "category": "metadata"},
    "cell_id":          {"required": False, "category": "metadata"},
    "doublet_scores":   {"required": False, "category": "metadata"},
}

# Map original names to the canonical names. First match is used.
HEURISTIC_MAPPINGS = {
    "gene_symbol":          ["Mouse_Gene", "Gene_Symbol", "gene_symbol", "gene", "symbol"],
    "human_gene":           ["Human_Gene", "human_gene", "HGNC_Symbol"],
    "protein_id":           ["Uniprot_id", "Uniprot_ID", "uniprot", "protein_id"],
    "ensembl_id":           ["Ensembl_id", "ensembl_id", "ENSEMBL"],
    "entrez_id":            ["Entrez_id", "entrez_id", "ENTREZID"],
    "protein_name":         ["Protein_Name", "Protein_Description", "protein_name"],
    "feature_description":  ["Feature_Description", "feature_description", "gene_description", 
                                "Gene_Description", "protein_description", "Protein_Description"],
    "location":             ["Location", "location", "Subcellular_Location", "subcellular_location",
                                "localisation"],
    "biotype":              ["Biotype", "biotype", "gene_biotype", "Gene_Biotype"],
    "log2fc":               ["logFC", "logfc", "log2FC", "log2FoldChange", "lfc", "avg_log2FC"],
    "pvalue":               ["pvalue", "PValue", "p_value", "pval", "p.value"],
    "padj":                 ["padj", "FDR", "adj.P.Val", "p_adj", "adjusted_pvalue"],
    "abundance_a":          ["abundance_A", "mean_A", "avg_expr_A"],
    "abundance_b":          ["abundance_B", "mean_B", "avg_expr_B"],
    "pct_expressed_a":      ["pct_1", "pct.1", "pct_expressed_A"],
    "pct_expressed_b":      ["pct_2", "pct.2", "pct_expressed_B"],
    "expression_metric":    ["expression_metric", "avg_log2FC", "mean_expr"],
    "normalisation_method": ["normalisation_method", "normalization_method", "norm_method", 
                                "normalisation", "normalization", "normalisation_type", "normalization_type"],

    "sample_a":             ["Sample_or_condition_A", "sample_A", "Sample_A"],
    "sample_b":             ["Sample_or_condition_B", "sample_B", "Sample_B"],
    "condition_a":          ["condition_a", "condition_A", "group_A", "treatment", "Condition_A"],
    "condition_b":          ["condition_b", "condition_B", "group_B", "control", "Condition_B"],
    "cell_type":            ["cell_type", "Cell_Type", "celltype", "cell_label", "cluster_label", 
                                "annotation"],
    "age":                  ["age", "Age", "age_years", "age_group", "Age_group", "Age_Group"],
    "sex":                  ["gender", "sex", "Sex", "Gender"],
    "tissue":               ["Tissue", "tissue", "brain_region", "region"],
    "cell_line":            ["cell_line", "Cell_Line", "Cell_line", "cellLine"],
    "cluster_id":           ["cluster_id", "seurat_clusters", "seurat_cluster", "seurat_clust", "cluster", "clusters", 
                                "leiden", "louvain"],
    "cell_id":              ["Cell_ID", "cell_id", "cellID", "CellID", "barcode", "cell_barcode"],
    "doublet_scores":       ["scDblFinder_score", "score"],
}

# Map table name → logical role mapping i.e.:
# sqlite/duckdb sources: *_exp / _results → expression, 
#                        study_info / *_metadata → sample_/cell_/extra_metadata
# rds (SCE obj) sources: assay logcounts → (log)counts, assay counts → counts,
#                    colData → cell_metadata, metadata() → sample_metadata,
#                    rowData → feature_annotations
SQLITE_TABLE_MAPPINGS = {
    "proteomics_exp":       "expression",
    "bulk_exp":             "expression",
    "sc_deg_results":       "expression",
    "sc_normalised_expr":   "counts",
    "sc_gene_info":         "gene_annotations",
    "sc_metadata":          "cell_metadata",
    "proteomics_metadata":  "extra_metadata",
    "bulk_metadata":        "extra_metadata",
    "study_info":           "extra_metadata",
}
RDS_OBJ_MAPPINGS = {
    "assay(obj, 'logcounts')": "counts",
    "assay(obj, 'counts')":    "counts",
    "colData(obj)":            "cell_metadata",     #contains sample/cell info, sex, age, doublet_scores, project name for eac cell_id
    "metadata(obj)":           "sample_metadata",   #or 'extra_metadata' or 'expression' results
    "rowData(obj)":            "feature_annotations",
    "rownames(obj)":           "gene_annotations",
}


def resolve_column_mappings(feature_cols: list, metric_cols: list,
                             meta_cols: list = None) -> dict:   # None as default when not available
    """
    Given the columns declared in the YAML for each dataset,
    return {canonical_name: original_name}.
    """
    all_cols = (feature_cols or []) + (metric_cols or []) + (meta_cols or [])
    resolved = {}

    for role, candidates in HEURISTIC_MAPPINGS.items(): #.items() allows both key AND value, not just key
        for candidate in candidates:
            if candidate in all_cols:
                resolved[role] = candidate
                break  # use first match
    return resolved


def resolve_logical_table(yaml_key: str, actual_table: str, source_type: str) -> str:
    """
    Map a YAML table key + actual original table name to a canonical logical name.
    Logical tables will match those in Parquet files: 'expression', 'sample_metadata' &/or 'cell_metadata', 
        'counts', 'gene_annotations' &/or 'feature_annotations', 'extra_metadata' etc.
    Current yaml key in table dict: 'expression', 'metadata'
    Actual db table names (and how to access rds objects): 'proteomics_exp', 'proteomics_metadata',
        'study_info', 'assay(sce, 'logcounts')' etc.
    source_type: 'sqlite', 'duckdb', 'rds', 'parquet'
    """
    # RDS sources: actual_table is the R accessor expression
    if source_type == "rds":
        return RDS_OBJ_MAPPINGS.get(actual_table, yaml_key)

    # sqlite / duckdb: exact match first
    if actual_table in SQLITE_TABLE_MAPPINGS:
        return SQLITE_TABLE_MAPPINGS[actual_table]

    # Fallback: use the yaml_key as-is (e.g. 'features_metadata')
    return yaml_key

def _infer_organism(gene_col: str) -> str:
    if "Mouse_Gene" in gene_col:
        return "mouse"      #TODO: mus musculus
    if "Human_Gene" in gene_col:
        return "human"  #homo sapiens
    return "unknown"


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



def parse_and_load_registry(yaml_path: str, registry_db_path: str):
    """
    Parse Yaml and load into DuckDB. This is either run at startup, 
    when yaml is updated or whenever the db is missing.
    """
    with open(yaml_path) as f:
        registry = yaml.safe_load(f)

    con = duckdb.connect(registry_db_path)

    try:
        con.execute("""
            CREATE TABLE IF NOT EXISTS dataset_registry (
                study_id          INTEGER,           -- correspond to original sqlite db ids
                lab_source        VARCHAR NOT NULL,  -- 'diaz', 'hong' etc.
                lab_name          VARCHAR NOT NULL,  -- 'Díaz Castro'
                dataset_name      VARCHAR NOT NULL,  -- 'Astrocyte_TurboID_vs_tdT'
                study_description VARCHAR,           -- e.g. 'Proteomic and bulk RNA-seq data'
                omic_type         VARCHAR,
                source_type       VARCHAR,           -- 'sqlite', 'duckdb', 'rds', 'parquet'
                data_path         VARCHAR,
                registered_at     TIMESTAMPTZ DEFAULT current_timestamp,
                PRIMARY KEY (study_id, lab_source, omic_type)
            )
        """)

        # TODO remove `is_default_gene` as redundant due to primary_gene_col
        con.execute("""
            CREATE TABLE IF NOT EXISTS column_mappings (
                study_id        INTEGER,
                lab_source      VARCHAR NOT NULL,
                lab_name        VARCHAR NOT NULL,
                dataset_name    VARCHAR NOT NULL,
                canonical_name  VARCHAR NOT NULL,   -- gene_symbol, log2fc
                original_name   VARCHAR NOT NULL,   -- 'Mouse_Gene', 'logFC',
                col_category    VARCHAR,            -- 'feature', 'metric', 'metadata'
                is_default_gene BOOLEAN DEFAULT FALSE, -- whether this is the default gene identifier for the dataset
                notes           VARCHAR,            -- any notes about mapping, e.g. many mappings or manual curation
                PRIMARY KEY (study_id, lab_source, canonical_name)
            )
        """)

        # SQL table mappings for datasets where table names differ from logical names
        con.execute("""
            CREATE TABLE IF NOT EXISTS table_mappings (
                study_id        INTEGER,
                lab_source      VARCHAR NOT NULL,
                lab_name        VARCHAR NOT NULL,
                logical_table   VARCHAR,   -- 'expression', 'sample_metadata' &/or 'cell_metadata', 'counts', 'gene_annotations' &/or 'feature_annotations', 'extra_metadata' etc.
                actual_table    VARCHAR,   -- 'proteomics_exp', 'proteomics_metadata', etc.
                PRIMARY KEY (study_id, lab_source, logical_table)
            )
        """)

        con.execute( """
            CREATE TABLE IF NOT EXISTS registry_load_issues (
                logged_at     TIMESTAMPTZ,
                lab_source    VARCHAR,
                dataset_name  VARCHAR,
                study_id      INTEGER,
                issue         VARCHAR
            )
        """)

        issue_rows = []
        seen_keys  = set()  # for persistent across iterations to detect duplications

        # Walk through the YAML (from prototype) and populate
        for source_id, source in registry["sources"].items():   # source_id is diaz, hong etc, source is the dict of lab_name, label, datasets, defaults etc.
            for dataset_name, ds in source.get("datasets", {}).items(): # ds is the dict of display_name, omic_type, feature_cols, metric_cols, annotations etc.
                
                study_id = ds.get("study_id")
                if study_id is None: # NULL?
                    issue_rows.append((
                        datetime.now(timezone.utc), source_id, dataset_name, None, 
                        f"missing study_id"))
                    continue

                data_path = ds.get("db_path")
                source_type = ds.get("source_type")
                if not data_path and source_type == "rds":
                    data_path = ds.get("rds_path")

                # To detect vector/list of multiple omic modalities in YAML, convert to scalar
                all_modalities = ds.get("omic_type") or []
                if isinstance(all_modalities, str):          # guard against bare string in YAML
                    all_modalities = [all_modalities]
                omic_type = all_modalities[0] if all_modalities else None   #TODO: from [0] to multiple omics 
                print(f"[DEBUG] omic_type: {omic_type}")

                # Detect duplicates
                primary_key = (source_id, study_id, omic_type)
                if primary_key in seen_keys:
                    issue_rows.append((
                        datetime.now(timezone.utc), source_id, dataset_name, study_id, 
                        f"duplicate for (lab_source, study_id, omic_type) in YAML; skipped",))
                    continue
                seen_keys.add(primary_key)
            
                feature_cols = ds.get("feature_cols", [])
                metric_cols  = ds.get("metric_cols", [])
                meta_cols    = ds.get("annotations", [])
                col_map      = resolve_column_mappings(feature_cols, metric_cols, meta_cols)

                # For each dataset, upsert dataset_registry
                con.executemany("""
                    INSERT OR REPLACE INTO dataset_registry (study_id, lab_source, lab_name, dataset_name, study_description,
                        omic_type, source_type, data_path, registered_at)
                    VALUES (?,?,?,?,?,?,?,?,?)
                """, [(
                    study_id,
                    source_id,
                    source.get("lab_name"),
                    dataset_name,            #ds.get("display_name", dataset_id),
                    None,       # replace `source.get("label"),` with description from db Metadata_Text column later on
                    omic_type,  #omic_types[0] if omic_types else None,
                    source_type,            #ds.get("source_type", source.get("defaults", {}).get("source_type")),
                    data_path,      #ds.get("db_path",   source.get("defaults", {}).get("db_path")),
                    datetime.now(timezone.utc)
                )])   #executemany() requires tuples or list of tuples; or execute(); 

                # Upsert column_mappings rows
                for canonical_name, original_name in col_map.items():
                    canonical_info = CANONICAL_NAMES.get(canonical_name, {})
                    is_default_gene = (canonical_name == "gene_symbol")  # first feature col (i.e. Mouse_Gene > Human_Gene) is default gene identifier
                    con.executemany("""
                        INSERT OR REPLACE INTO column_mappings 
                        (study_id, lab_source, lab_name, dataset_name, canonical_name, original_name, col_category, is_default_gene, notes)
                        VALUES (?,?,?,?,?,?,?,?,?)
                    """, [
                        (ds.get("study_id"),
                        source_id,
                        source.get("lab_name"),
                        dataset_name,
                        canonical_name,
                        original_name,
                        canonical_info.get("category", "unknown"),
                        is_default_gene,
                        None    #? for notes
                    )])

                # Upsert table_mappings and issues
                for yaml_key, actual_table in ds.get("tables", {}).items():
                    con.executemany("""
                        INSERT OR REPLACE INTO table_mappings (study_id, lab_source, lab_name, logical_table, actual_table)
                        VALUES (?,?,?,?,?)
                    """, [(study_id, source_id, source.get("lab_name"),
                        resolve_logical_table(yaml_key, actual_table, source_type or ""), actual_table)])
                    print(f"  [DEBUG] resolve_logical_table(): {resolve_logical_table(yaml_key, actual_table, source_type or "")}")

        if issue_rows:
            con.executemany("""
                INSERT INTO registry_load_issues
                (logged_at, lab_source, dataset_name, study_id, issue)
                VALUES (?,?,?,?,?)
                """,
                issue_rows,
            )
            print(f"Registry load completed with {len(issue_rows)} skipped datasets.")
            for _, lab_source, dataset_name, study_id, issue in issue_rows:
                print(f"    Skipped [{lab_source}] {dataset_name} (study_id={study_id}): {issue}")

    finally:
        con.close()

    print("   Registry loaded successfully.")


# NB for dev, always rebuild; for prod, check timestamps
def build_registry_index(registry_db_path: str, force_rebuild: bool = False):
    """
    Create index table to enable fast gene → dataset lookups.
    Attaches source databases as read-only, creates semantic views per dataset, and populates the index.
    - checks if index exists and is fresher than registry mtime, otherwise rebuilds it at startup or manually
    - determine which col is the primary gene identifier (e.g. gene_symbol), and which is the human eq and organism source (if any)
    - query DISTINCT gene values and insert into gene_study_index and metadata
    """

    con = duckdb.connect(registry_db_path)
    try:
        con.execute("""
            CREATE TABLE IF NOT EXISTS index_build_log (
                index_name  VARCHAR,
                built_at    TIMESTAMPTZ DEFAULT current_timestamp,
                row_count   INTEGER
            )
        """)

        con.execute("""
            CREATE TABLE IF NOT EXISTS index_build_issues (
                logged_at     TIMESTAMPTZ,
                study_id      INTEGER,
                lab_source    VARCHAR,
                dataset_name  VARCHAR,
                issue         VARCHAR
            )
        """)

        # Rebuild check - for simplicity, always rebuild on startup (fast for <50 datasets)
        if not force_rebuild:
            last_index_build = con.execute("""
                SELECT MAX(built_at) FROM index_build_log 
                WHERE index_name = 'gene_study_index'
            """).fetchone()[0]

            db_mtime = con.execute("""
                SELECT MAX(registered_at) FROM dataset_registry
            """).fetchone()[0]
            print("   Last gene_study_index indexing built at:", last_index_build)

            # In production, compare index build to registry file mtime. Skip if fresh.
            if last_index_build and db_mtime and last_index_build >= db_mtime:
            # if last_index_build >= db_mtime:
                print("    gene_study_index is up to date. Skip rebuild.")
                return
            print("   Rebuilding index as gene_study_index is missing or outdated (as registry has been updated since last build)")

        con.execute("""
            CREATE TABLE IF NOT EXISTS gene_study_index (
                gene_symbol  VARCHAR NOT NULL,
                protein_id   VARCHAR,
                study_id     INTEGER,
                lab_source   VARCHAR NOT NULL,
                dataset_name VARCHAR NOT NULL,
                omic_type    VARCHAR,
                organism     VARCHAR,   -- 'mus musculus', 'homo sapiens', 'unknown'
                PRIMARY KEY (gene_symbol, study_id, lab_source)
            )
        """)
        con.execute("DELETE FROM gene_study_index")  # clear for rebuild

        # Extract datasets by joining table_mappings to dataset_registry
        # Then pull distinct gene_symbol/canonical or original_name and insert into index table along w metadata for filtering
        datasets = con.execute("""
            SELECT 
                dr.study_id, 
                dr.lab_source, 
                dr.dataset_name, 
                dr.omic_type, 
                dr.data_path, 
                tm.actual_table
            FROM dataset_registry dr
            LEFT JOIN table_mappings tm
                ON dr.study_id = tm.study_id
                AND dr.lab_source = tm.lab_source
                AND tm.logical_table = 'expression'
        """
        ).fetchall()
        print(f"     gene_study_index built with {len(datasets)} dataset entries.")

        issue_rows   = []
        attached_dbs = {}
        row_count    = 0

        for (study_id, lab_source, dataset_name, omic_type, data_path, actual_table) in datasets:
            
            if not data_path or not actual_table:
                issue_rows.append((
                    datetime.now(timezone.utc), study_id, lab_source,
                    dataset_name, "missing database path, expression table, or data object — skipped"
                ))
                continue

            # Attach each unique db_path once if not done already, record alias per path
            if data_path and data_path not in attached_dbs: # two gaurds against passing None to ATTACH & prevent re-attaching the same db under a second alias with multiple studies
                alias = f"src_{lab_source}"
                try:
                    con.execute(f"ATTACH '{data_path}' AS {alias} (READ_ONLY)")
                    attached_dbs[data_path] = alias
                except Exception as e:
                    if "already attached" not in str(e).lower():
                        attached_dbs[data_path] = alias     # record the alias it was given even if same file path used for different datasets
                    else:
                        issue_rows.append((
                            datetime.now(timezone.utc), study_id, lab_source, dataset_name, 
                            f"ATTACH failed: {e}"
                        ))
                        print(f"   ERROR attaching DB {data_path} as {alias}: {e}")
                        continue  # removed since it skips view creation if it exists, prevents it from properly failing and recieving errror/for debugging
            
            db_alias = attached_dbs[data_path] 

            # Identify primary column/ canonical gene as `gene_symbol` or, if unavailable, protein_id
            # gene_col = con.execute("""
            #     SELECT original_name FROM column_mappings
            #     WHERE study_id = ? AND lab_source = ? AND canonical_name = 'gene_symbol'
            # """, [study_id, lab_source]).fetchone() #OR for row in datasets: ..... [row[2], row[3]]).fetchone()
            name_mappings = dict(
                con.execute("""
                    SELECT canonical_name, original_name 
                    FROM column_mappings
                    WHERE study_id = ? AND lab_source = ?
                """, [study_id, lab_source]).fetchall()
            )

            gene_col     = name_mappings.get("gene_symbol")
            protein_col  = name_mappings.get("protein_id")
            organism_col = name_mappings.get("organism")
            human_col    = name_mappings.get("human_gene")    # Identify human gene column if available for cross-species mapping

            if not gene_col and not human_col and not protein_col:
                print(f" WARNING: No gene or protein mapping for study_id: {study_id}, lab: {lab_source}, skipping index.")
                issue_rows.append((
                    datetime.now(timezone.utc), study_id, lab_source, 
                    dataset_name, f"missing gene/ protein mapping for indexing"))
                continue

            # Primary gene column: COALESCE so Mouse_Gene NULLs it falls through to Human_Gene
            gene_candidates = [get_sql_col(c) for c in [gene_col, protein_col, human_col] if c]         # build as a list and join
            primary_gene_col = (f"COALESCE({', '.join(gene_candidates)})"
                if len(gene_candidates) > 1
                else gene_candidates[0])
            print(f"[DEBUG] primary_gene_col: {primary_gene_col}    gene_candidates: {gene_candidates}  ")
            # [DEBUG] primary_gene_col: COALESCE("Gene_Symbol", "Uniprot_ID")    gene_candidates: ['"Gene_Symbol"', '"Uniprot_ID"'] 
            # or
            # Use gene_col or human_col if available, else protein_col
            # gene_col     = gene_col if gene_col else protein_col #None # if statement to avoid error if fetchone returns None or NULL?? gene_col[0]
            # protein_col  = protein_col if protein_col else "NULL" #None
            # organism_col = organism_col if organism_col else "unknown" #None
            
            # primary_gene_col = (f'COALESCE("{gene_col}", "{human_col}")'  if gene_col and human_col
            #     else f'"{gene_col}"' if gene_col
            #     else f'"{human_col}"' if human_col
            #     else f'"{protein_col}"')
            # # coalesce_gene = []
            # # if gene_col:    coalesce_gene.append(f'"{gene_col}"')
            # # if human_col:   coalesce_gene.append(f'"{human_col}"')
            # # if protein_col: coalesce_gene.append(f'"{protein_col}"')
            # # primary_gene_col = f"COALESCE({', '.join(coalesce_gene)})" 
            # # protein_expr = f'"{protein_col}"' if protein_col != "NULL" else "NULL"

            # # print(f"[DEBUG] gene_col: {gene_col}    protein_col: {protein_col}  organism_col: {organism_col}")
            # # output: [DEBUG] gene_col: Mouse_Gene    protein_col: Uniprot_id  organism_col: unknown    (diaz)
            # # output: [DEBUG] gene_col: Gene_Symbol    protein_col: NULL  organism_col: unknown     (hong)


            # Look up alias and create semantic view for each dataset, and insert
            view_name = f"v_{lab_source}_{study_id}"
            print(f" [DEBUG] {lab_source} study_id={study_id} | alias={db_alias} | "
                f"table={actual_table} | gene_col={gene_col} | primary_gene_col={primary_gene_col} | protein_col={protein_col} | human_col={human_col} | organism_col={organism_col} | view={view_name}")
            # primary_gene_col="Gene_Symbol" | protein_col=Uniprot_id | protein_expr="Uniprot_id" 

            try:
                #TODO: check "" colnames VS '' string e.g.      protein_expr = f'"{protein_col}"' if protein_col != "NULL" else "NULL"
                # Use `Views` for semantic layer (e.g. translating heterogeneous column names into canonical names)
                # Reduce need to materialise data for zero-copy data streams/ transfers
                con.execute(f"""
                    CREATE OR REPLACE VIEW {view_name} AS
                    SELECT
                        {study_id}                  AS study_id,
                        -- Feature identifiers
                        {primary_gene_col}          AS gene_symbol,
                        {get_sql_col(human_col)}    AS human_gene,
                        {get_sql_col(protein_col)}  AS protein_id,      -- {protein_col} AS protein_id,
                        '{organism_col}'            AS organism,
                        -- Expression metrics
                        {get_sql_col(name_mappings.get('log2fc'))}             AS log2fc,
                        {get_sql_col(name_mappings.get('pvalue'))}             AS pvalue,
                        {get_sql_col(name_mappings.get('padj'))}               AS padj,
                        {get_sql_col(name_mappings.get('abundance_a'))}        AS abundance_a,
                        {get_sql_col(name_mappings.get('abundance_b'))}        AS abundance_b,
                        {get_sql_col(name_mappings.get('pct_expressed_a'))}    AS pct_expressed_a,
                        {get_sql_col(name_mappings.get('pct_expressed_b'))}    AS pct_expressed_b,
                        {get_sql_col(name_mappings.get('expression_metric'))}  AS expression_metric,
                        -- Sample / cell metadata
                        {get_sql_col(name_mappings.get('sample_a'))}           AS sample_a,
                        {get_sql_col(name_mappings.get('sample_b'))}           AS sample_b,
                        {get_sql_col(name_mappings.get('condition_a'))}        AS condition_a,
                        {get_sql_col(name_mappings.get('condition_b'))}        AS condition_b,
                        {get_sql_col(name_mappings.get('cell_type'))}          AS cell_type,
                        {get_sql_col(name_mappings.get('cell_id'))}            AS cell_id,
                        {get_sql_col(name_mappings.get('cluster_id'))}         AS cluster_id,
                        {get_sql_col(name_mappings.get('tissue'))}             AS tissue,
                        {get_sql_col(name_mappings.get('age'), 'NULL::INTEGER')} AS age,
                        {get_sql_col(name_mappings.get('sex'))}                AS sex
                    FROM {db_alias}.main.{actual_table}
                    WHERE study_id = {study_id} AND {primary_gene_col} IS NOT NULL
                """)    # '{source_id}'.main.{expression_table} OR '{data_path}'.{actual_table} OR attach_alias.main.actual_table
            except Exception as e:
                print(f"   ERROR creating view {view_name} while building index: {e}")
                issue_rows.append((
                    datetime.now(timezone.utc), study_id, lab_source, dataset_name, 
                    f"view creation failed: {e}"
                ))
                continue

            try:
                #TODO: check "" colnames VS '' string
                con.execute(f"""
                    INSERT INTO gene_study_index (gene_symbol, protein_id, study_id, lab_source, dataset_name, omic_type, organism)
                    SELECT DISTINCT 
                        gene_symbol,
                        TO_JSON(LIST(DISTINCT protein_id)) AS protein_id,       -- e.g. '["Q9Z223","Q9Z224"]' for gene MOCS2
                        study_id,
                        '{lab_source}',
                        '{dataset_name}',
                        '{omic_type}',
                        organism
                    FROM {view_name}
                    WHERE gene_symbol IS NOT NULL
                    AND gene_symbol != ''
                    GROUP BY gene_symbol, study_id, protein_id, organism
                """) #OR protein_id IS NOT NULL ;   WHERE NULLIF(TRIM(gene_symbol), '') IS NOT NULL

                inserted_rows = con.execute(f"""
                    SELECT COUNT(*) FROM gene_study_index
                    WHERE lab_source = '{lab_source}' AND study_id = {study_id}
                """).fetchone()[0]
                row_count += inserted_rows
                print(f"  [DEBUG] Indexed {inserted_rows} genes for [{lab_source}] {dataset_name} into gene_study_index from view: {view_name}")
                # Indexed 6790 genes for [hong] 3KL_vs_WT_ME-Macs_2 into gene_study_index from view: v_hong_2_proteomics_3KL_vs_WT_ME_Macs_2


            except Exception as e:
                print(f"ERROR inserting index from view: {view_name}: {e}")
                issue_rows.append((
                    datetime.now(timezone.utc), study_id, lab_source, dataset_name, 
                    f"index insert failed: {e}"))
                con.execute("""
                    INSERT INTO index_build_issues (logged_at, study_id, lab_source, dataset_name, issue)
                    VALUES (?,?,?,?,?)
                """, [datetime.now(timezone.utc), study_id, lab_source, dataset_name, issue_rows])
                continue

        # Create (composite) indexes for fast querying
        con.execute("""
            CREATE INDEX IF NOT EXISTS idx_gene_study ON gene_study_index (gene_symbol, study_id)      -- show me gene x in dataset y
            -- CREATE INDEX IF NOT EXISTS idx_gene ON gene_study_index (gene_symbol)              -- show me all datasets containing x gene, can end up too large
            -- CREATE INDEX IF NOT EXISTS idx_study ON study_index (study_id)                     -- show me dataset y, may as well just query dataset_registry directly
        """)
        con.execute("""
            CREATE INDEX IF NOT EXISTS idx_gene_omic  ON gene_study_index (gene_symbol, omic_type)
        """)

        # Log build time, total row count, and issues
        total_rows = con.execute("SELECT COUNT(*) FROM gene_study_index").fetchone()[0]
        con.executemany("""
            INSERT INTO index_build_log VALUES ('gene_study_index',?,?)
        """, [(datetime.now(timezone.utc), total_rows)]) #len(datasets)
        print(f"   Gene index built with {total_rows} total rows")
            
        if issue_rows:
            con.executemany("""
                INSERT INTO index_build_issues (logged_at, study_id, lab_source, dataset_name, issue)
                VALUES (?,?,?,?,?)
            """, issue_rows)
            print(f"   {len(issue_rows)} dataset(s) were skipped.")
            for _, study_id, lab_source, dataset_name, issue in issue_rows:
                print(f"   Skipped: [{lab_source}] {dataset_name} (study_id={study_id}): {issue}")
        print(f"Gene index built with {row_count} rows")
        
    finally:
        con.close()
    print("gene_study_index build's log updated.")

    







## 2. DuckDB Views as the Semantic Layer
# Yes — views are significantly better than building the mapping logic in R or Python query strings. Here's why and exactly how to implement them.

### Why Views Win
# With raw query building you embed mapping logic in application code, which means it runs on every request, is harder to test, and leaks into your UI layer. A view moves the translation to the database layer — application code always queries `v_diaz_Astrocyte_TurboID_vs_tdT` and gets back `gene_symbol`, `log2fc`, `padj` regardless of what the underlying table actually called those columns.

# Application code          DuckDB
# ─────────────────         ────────────────────────────────────
# SELECT gene_symbol,  →    v_diaz_Astrocyte:
#        log2fc,              SELECT Mouse_Gene AS gene_symbol,
#        padj                        logFC        AS log2fc,
# FROM v_diaz_Astro...                padj         AS padj,
# WHERE gene_symbol                   study_id, source_id, ...
#       IN (...)              FROM diaz.main.proteomics_exp
#                             WHERE study_id = 1