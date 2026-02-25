"""
This file computes and stores aggregated dataset-level summaries/ stats at ingestion time for fast retrieval.
# cache??
"""

import duckdb
from tkinter.tix import INTEGER

def build_dataset_stats(registry_db_path: str, force: bool = False):
    con = duckdb.connect(registry_db_path)

    con.execute("""
        -- In neuromics_registry.duckdb
        CREATE TABLE IF NOT EXISTS dataset_stats (
            study_id            INTEGER,
            lab_source          VARCHAR NOT NULL,
            display_name        VARCHAR NOT NULL,
            omic_type           VARCHAR,
            total_features      INTEGER,   -- genes, proteins, transcripts
            n_sig_features      INTEGER,   -- padj < 0.05 count
            total_samples       INTEGER,   -- e.g. for bulk/proteomics
            n_conditions        INTEGER,   -- distinct conditions
            total_cells         INTEGER,   -- e.g. for scrna/ snrna (NULL for others)
            n_cell_types        INTEGER,   -- distinct cell_type_label values
            n_clusters          INTEGER,   -- distinct cluster_label values
            doublet_scores_json VARCHAR,   -- JSON array of doublet scores (if available for scrna/ snrna)
            cell_types_json     VARCHAR,   -- JSON array of distinct cell type labels
            conditions_json     VARCHAR,   -- JSON array of conditions
            tissues_json        VARCHAR,   -- JSON array of tissues/brain regions
            age_range_json      VARCHAR,   -- {"min": 3, "max": 24, "unit": "months"}
            sexes_json          VARCHAR,   -- ["M", "F", "unknown"]
            computed_at         TIMESTAMPTZ,
            PRIMARY KEY (study_id, lab_source)
        )
    """).fetchall()
    
    datasets = con.execute("""
        SELECT 
            study_id, 
            lab_source, 
            display_name, 
            omic_type, 
            db_path, 
        FROM dataset_registry
    """).fetchall()
    
    for (study_id, lab_source, display_name, omic_type, db_path) in datasets:
        # # Skip if fresh and not forced
        # if not force:
        #     last = con.execute("""
        #         SELECT computed_at FROM dataset_stats
        #         WHERE source_id=? AND dataset_key=?
        #     """, [source_id, dataset_key]).fetchone()
        #     if last:
        #         continue  # already computed
        
        # For queries, not actual view creation to avoid management, complexity, and overhead
        safe_key = lab_source.replace("-","_").replace(" ","_")
        expr_view = f"v_{study_id}_{safe_key}"  # for genes, features, and expression stats
        meta_view = f"vm_{study_id}_{safe_key}" # for sample/ cell metadata summaries

        # Feature summaries from expression view
        feature_stats_query = f"""
            SELECT
                COUNT(DISTINCT gene_symbol) AS total_features,
                SUM(CASE WHEN padj < 0.05 THEN 1 ELSE 0 END) AS n_sig_features      -- row-level summary
                -- COUNT(DISTINCT CASE WHEN padj < 0.05 THEN gene_symbol ELSE NULL END) AS n_sig_features       -- gene-level summaries due to unique rows only
            FROM {expr_view}    
        """ # or FROM '{db_path}'

        # Metadata summaries from metadata view (if exists)
        metadata_stats_query = f"""
            SELECT
                '{omic_type}' AS omic_type,
                COUNT(DISTINCT sample_id) AS total_samples
                COUNT(DISTINCT condition) AS n_conditions,
                COUNT(DISTINCT cell_id) AS total_cells,
                COUNT(DISTINCT cell_type) AS n_cell_types,
                COUNT(DISTINCT cluster_id) AS n_clusters,
                JSON_GROUP_ARRAY(DISTINCT doublet_scores) AS doublet_scores_json,
                JSON_GROUP_ARRAY(DISTINCT cell_type) AS cell_types_json,
                JSON_GROUP_ARRAY(DISTINCT condition) AS conditions_json,
                JSON_GROUP_ARRAY(DISTINCT tissue) AS tissues_json,
                JSON_OBJECT('min', MIN(age), 'max', MAX(age), 'unit', 'months') AS age_range_json,    -- JSON_GROUP_OBJECT gives key/value merging OR JSON_OBJECT for single structure
                JSON_GROUP_ARRAY(DISTINCT sex) AS sexes_json,
                NOW() AS computed_at
            FROM {meta_view}    
        """ # or FROM '{db_path}'

        # Execute queries and upsert into dataset_stats
        try:
            feature_stats = con.execute(feature_stats_query).fetchone()
            metadata_stats = con.execute(metadata_stats_query).fetchone()
            con.execute(f"""
                INSERT OR REPLACE INTO dataset_stats VALUES (
                    ?,?,?,?,?,?,?,?,?,?,?,?,?,?,?
                )
            """, [study_id, lab_source, display_name, omic_type] + list(feature_stats) + list(metadata_stats))
        except Exception:
            total_features = None; n_sig_features = None
            total_samples = None; n_conditions = None
            total_cells = None; n_cell_types = None
            n_clusters = None; doublet_scores_json = "[]"
            cell_types_json = "[]"; conditions_json = "[]"
            tissues_json = "[]"; age_range_json = "{}"; sexes_json = "[]"

    con.close()
    print("Aggregated stats computed.")

        # Compute tables or views for common queries for UI and filtering:
        # e.g. "top 10 most abundant x y z", "top 10 DE genes in cell type X" or "expression of gene Y across conditions" - can be expanded once usage patterns are clearer


