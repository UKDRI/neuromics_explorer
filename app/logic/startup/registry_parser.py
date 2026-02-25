""""
This startup module handles parsing the YAML registry and loading it into neuromics_registry.duckdb.
The .duckdb will contain canonical mappings of datasets, columns, and tables. It also creates DuckDB views to implement a semantic layer for querying.

Usage:
    python -m app.logic.registry_parser 
    OR called automatically from FastAPI lifespan if DB missing.
"""

import yaml
import duckdb
from datetime import datetime

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

    # Differential expression metrics
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
}


def resolve_column_mappings(feature_cols: list, metric_cols: list,
                             meta_cols: list = None) -> dict:
    """
    Given the columns declared in the YAML for each dataset,
    return {canonical_name: original_name}.
    """
    all_cols = (feature_cols or []) + (metric_cols or []) + (meta_cols or [])
    resolved = {}

    for role, candidates in HEURISTIC_MAPPINGS.items():
        for candidate in candidates:
            if candidate in all_cols:
                resolved[role] = candidate
                break  # use first match
    return resolved



def _infer_organism(gene_col: str) -> str:
    if "Mouse_Gene" in gene_col:
        return "mouse"
    if "Human_Gene" in gene_col:
        return "human"



def parse_and_load_registry(yaml_path: str, registry_db_path: str):
    """
    Parse Yaml and load into DuckDB. This is either run at startup, 
    when yaml is updated or whenever the db is missing.
    """
    with open(yaml_path) as f:
        registry = yaml.safe_load(f)

    con = duckdb.connect(registry_db_path)

    # Create registry tables
    con.execute("""
        CREATE TABLE IF NOT EXISTS dataset_registry (
            study_id         INTEGER,           -- correspond to original sqlite db ids
            lab_source       VARCHAR NOT NULL,  -- 'diaz', 'hong' etc.
            lab_name         VARCHAR NOT NULL,  -- 'Díaz Castro'
            display_name     VARCHAR NOT NULL,  -- 'Astrocyte_TurboID_vs_tdT'
            omic_type        VARCHAR,
            source_type      VARCHAR,           -- 'sqlite', 'duckdb', 'rds', 'parquet'
            db_path          VARCHAR,
            registered_at    TIMESTAMPTZ DEFAULT current_timestamp,
            PRIMARY KEY (study_id, lab_source)
        )
    """)

    con.execute("""
        CREATE TABLE IF NOT EXISTS column_mappings (
            study_id        INTEGER,
            lab_source      VARCHAR NOT NULL,
            lab_name        VARCHAR NOT NULL,
            display_name    VARCHAR NOT NULL,
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
            logical_table   VARCHAR,   -- 'expression', 'metadata', 'counts', etc.
            actual_table    VARCHAR,   -- 'proteomics_exp', 'proteomics_metadata', etc.
            PRIMARY KEY (study_id, lab_source, logical_table)
        )
    """)

    # Walk through the YAML and populate
    for source_id, source in registry["sources"].items():   # source_id is diaz, hong etc, source is the dict of lab_name, datasets, defaults etc.
        for dataset_name, ds in source.get("datasets", {}).items(): # ds is the dict of display_name, omic_type, feature_cols, metric_cols, annotations etc.

            omic_types = ds.get("omic_type", [])
            feature_cols  = ds.get("feature_cols", [])
            metric_cols   = ds.get("metric_cols", [])
            meta_cols = ds.get("annotations", [])

            # Resolve heuristic column mappings
            col_map = resolve_column_mappings(feature_cols, metric_cols, meta_cols)

            # For each dataset, upsert dataset_registry as row
            con.execute("""
                INSERT OR REPLACE INTO dataset_registry VALUES (?,?,?,?,?,?,?)
            """, [
                ds.get("study_id"),
                source_id,
                source.get("lab_name"),
                dataset_name,            #ds.get("display_name", dataset_id),
                omic_types[0] if omic_types else None,
                ds.get("source_type"),   #ds.get("source_type", source.get("defaults", {}).get("source_type")),
                ds.get("db_path")        #ds.get("db_path",   source.get("defaults", {}).get("db_path")),
            ])

            # Upsert column_mappings rows
            for canonical_name, original_name in col_map.items():
                canonical_info = CANONICAL_NAMES.get(canonical_name, {})
                is_default_gene = (canonical_name == "gene_symbol")  # first feature col (i.e. Mouse_Gene > Human_Gene) is default gene identifier
                con.execute("""
                    INSERT OR REPLACE INTO column_mappings VALUES (?,?,?,?,?,?,?,?)
                """, [
                    ds.get("study_id"),
                    source_id,
                    source.get("lab_name"),
                    dataset_name,
                    canonical_name,
                    original_name,
                    canonical_info.get("category", "unknown"),
                    is_default_gene
                ])

            # Upsert table_mappings rows
            for logical_table, actual_table in ds.get("tables", {}).items():
                con.execute("""
                    INSERT OR REPLACE INTO table_mappings VALUES (?,?,?,?,?)
                """, [ds.get("study_id"), source_id, source.get("lab_name"), logical_table, actual_table])

    con.close()
    print("Registry loaded successfully.")


# NB for dev, always rebuild; for prod, check timestamps
def build_registry_index(registry_db_path: str, force_rebuild: bool = False):
    """
    Create index table to enable fast lookup of which datasets contain a given gene.
    This joins column_mappings and dataset_registry with gene_symbol (and its values) for quick gene-centric querying.
    - checks if index exists and is fresher than registry mtime, otherwise rebuilds it at startup or manually with force_rebuild=TRUE
    - determine which col is the primary gene identifier (e.g. gene_symbol), which is the human eq and organism source (if any)
    - query DISTINCT gene values and insert into gene_study_index and metadata
    """

    con = duckdb.connect(registry_db_path)

    con.execute("""
        CREATE TABLE IF NOT EXISTS index_build_log (
            index_name  VARCHAR,
            built_at    TIMESTAMPTZ,
            row_count   INTEGER
        )
    """)

    # Rebuild check - for simplicity, always rebuild on startup (fast for <50 datasets)
    if not force_rebuild:
        last_build = con.execute("""
            SELECT MAX(built_at) FROM index_build_log
            WHERE index_name = 'gene_study_index'
        """).fetchone()
        
        print("Last gene_study_index build:", last_build)
        # In production, compare to registry file mtime. Skip if fresh.
        # db_mtime = con.execute("SELECT MAX(registered_at) FROM dataset_registry").fetchone()[0]
        # if last_build and db_mtime and last_build >= db_mtime:  # 
        #     print("gene_study_index is up to date. Skip rebuild.")
        #     con.close()
        #     return
        # print("Rebuilding gene_study_index - registry has been updated since last build...")


    # Create index table
    con.execute("""
        CREATE TABLE IF NOT EXISTS gene_study_index (
            gene_symbol  VARCHAR NOT NULL,
            protein_id   VARCHAR,
            study_id     INTEGER,
            lab_source   VARCHAR NOT NULL,
            display_name VARCHAR NOT NULL,
            omic_type    VARCHAR,
            organism     VARCHAR,   -- 'mouse', 'human', 'unknown'
            PRIMARY KEY (gene_symbol, lab_source, study_id)
        );
    """)
    con.execute("DELETE FROM gene_study_index;")  # clear for rebuild

    # Extract datasets by joining table_mappings to dataset_registry
    # Then pull distinct gene_symbol/canonical or original_name and insert into index table along w metadata for filtering
    datasets = con.execute("""
        SELECT 
            dr.study_id, 
            dr.lab_source, 
            dr.display_name, 
            dr.omic_type, 
            dr.db_path, 
            tm.actual_table
        FROM dataset_registry dr
        LEFT JOIN table_mappings tm ON dr.study_id = tm.study_id
            AND dr.lab_source = tm.lab_source
            AND tm.logical_table = 'expression'
    """
    #     """
    #     INSERT OR REPLACE INTO gene_study_index (gene_symbol, protein_id, study_id, lab_source, display_name, omic_type, db_path, actual_table, organism)
    #     SELECT DISTINCT
    #         cm.original_name AS gene_symbol,
    #         MAX(CASE WHEN cm.canonical_name = 'protein_id' THEN cm.original_name END) AS protein_id,    -- original_name where canonical_name is protein_id
    #         dr.study_id,
    #         dr.lab_source,
    #         dr.display_name,
    #         dr.omic_type,
    #         dr.db_path,
    #         tm.actual_table,
    #         MAX(CASE WHEN cm.canonical_name = 'organism' THEN cm.original_name END) AS organism
    #     FROM column_mappings cm
    #     JOIN dataset_registry dr ON cm.study_id = dr.study_id AND cm.lab_source = dr.lab_source
    #     LEFT JOIN table_mappings tm ON cm.study_id = tm.study_id AND cm.lab_source = tm.lab_source AND tm.logical_table = 'expression'
    #     WHERE cm.canonical_name IN ('gene_symbol', 'protein_id', 'organism', 'log2fc', 'padj')
    #         GROUP BY gene_symbol, dr.study_id          -- grouping genes by study for querying with a given gene
    #     -- is last two WHERE/GROUP commands needed???? tbc        
    # """
    ).fetchall()
    #     ....
    #     JOIN dataset_registry dr ON cm.study_id = dr.study_id AND cm.lab_source = dr.lab_source
    #     WHERE cm.canonical_name IN ('gene_symbol', 'protein_id', 'organism')
    #             GROUP BY gene_symbol, dr.study_id, dr.lab_source, dr.display_name, dr.omic_type
    print(f"gene_study_index built with {len(datasets)} entries.")

    for (study_id, lab_source, display_name, omic_type, db_path, actual_table) in datasets:
        # Identify primary column/ canonical gene as `gene_symbol` or, if unavailable, protein_id
        gene_col = con.execute("""
            SELECT original_name FROM column_mappings
            WHERE study_id = ? AND lab_source = ? AND canonical_name = 'gene_symbol'
        """, [study_id, lab_source]).fetchone() #OR for row in datasets: ..... [row[2], row[3]]).fetchone()
        protein_col = con.execute("""
            SELECT original_name FROM column_mappings
            WHERE study_id = ? AND lab_source = ? AND canonical_name = 'protein_id'
        """, [study_id, lab_source]).fetchone()
        organism_col = con.execute("""
            SELECT original_name FROM column_mappings
            WHERE study_id = ? AND lab_source = ? AND canonical_name = 'organism'
        """, [study_id, lab_source]).fetchone()
        # Identify human gene column if available for cross-species mapping
        human_col = con.execute("""
            SELECT original_name FROM column_mappings
            WHERE study_id = ? AND lab_source = ? AND canonical_name='human_gene'     
        """, [study_id, lab_source]).fetchone()

        if not gene_col and not protein_col:
                print(f"  WARNING: No gene_symbol mapping for study_id: {study_id}, lab: {lab_source}, skipping index.")
                continue
        # Use gene_col if available, else protein_col
        gene_col = gene_col[0] #if gene_col else None
        protein_col = protein_col[0] #if protein_col else None
        organism_col = organism_col[0] #if organism_col else None


        # Create semantic view for each dataset
        # Insert and query distinct genes from source db into index
        view_name = f"{lab_source}_{study_id}".replace(" ", "_").replace("-", "_")
        try:
            con.execute(f"""
                CREATE OR REPLACE VIEW {view_name} AS
                SELECT
                    -- study_id,
                    {gene_col} AS gene_symbol,
                    {protein_col} AS protein_id,
                    {organism_col} AS organism
                FROM '{db_path}'.{actual_table}
                -- WHERE study_id = {study_id}
            """)    #or FROM view_name
            
        except Exception as e:
            print(f"  ERROR creating view {view_name} while building index: {e}")
            continue

        try:
            rows = con.execute(f"""
                INSERT INTO gene_study_index (gene_symbol, protein_id, study_id, lab_source, display_name, omic_type, organism)
                SELECT DISTINCT gene_symbol, protein_id, {study_id}, {lab_source}, {display_name}, {omic_type}, organism
                FROM {view_name}
                WHERE gene_symbol IS NOT NULL OR protein_id IS NOT NULL
            """)
            print(f"  Inserted {rows.rowcount} rows into gene_study_index for view {view_name}")
        except Exception as e:
            print(f"ERROR inserting index for {view_name}: {e}")
            continue

    # Create (composite) indexes for fast querying
    con.execute("""
        -- CREATE INDEX IF NOT EXISTS idx_gene ON gene_study_index (gene_symbol);              -- show me all datasets containing x gene, can end up too large
        CREATE INDEX IF NOT EXISTS idx_gene_study ON gene_study_index (gene_symbol, study_id);      -- show me gene x in dataset y
        CREATE INDEX IF NOT EXISTS idx_gene_omic  ON gene_study_index (gene_symbol, omic_type);
        -- CREATE INDEX IF NOT EXISTS idx_study ON study_index (study_id);                     -- show me dataset y, may as well just query dataset_registry directly
    """)


    # Log build time and row count
    if datasets:
        row_count = con.execute("SELECT COUNT(*) FROM gene_study_index").fetchone()[0]
        con.execute("""
            INSERT INTO index_build_log VALUES ('gene_study_index',?,?)
            """, [datetime.now(datetime.timezone.utc), row_count])  #Or len(datasets)
        con.close()
        print(f"Gene index built: {row_count} rows")
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