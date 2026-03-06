"""
Module computes and stores aggregated dataset-level stats at ingestion time for fast retrieval.
# TODO: Add cacheing for expensive queries on demand e.g. "top 10 most abundant x y z", "top 10 DE genes in cell type X" or "expression of gene Y across conditions" - can be expanded once usage patterns are clearer
"""

from datetime import datetime, timezone
import duckdb

def _view_exists(con, view_name: str) -> bool:
    row = con.execute(
        """
        SELECT 1
        FROM information_schema.tables
        WHERE table_name = ?
        LIMIT 1
        """,
        [view_name],
    ).fetchone()
    return bool(row)

def build_dataset_stats(registry_db_path: str, force: bool = False):
    con = duckdb.connect(registry_db_path)
    try:
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
            # Skip if stats is already computed and not forced
            if not force:
                computed = con.execute("""
                    SELECT computed_at FROM dataset_stats
                    WHERE study_id=? AND lab_source=?
                """, [study_id, lab_source]).fetchone()
                if computed:
                    continue
            
            # Generate view names for queries, to avoid management, complexity, and overhead
            # - views are created during registry parsing in build_registry_index
            expr_view = f"v_{lab_source}_{study_id}"  # for genes, features, and expression stats
            meta_view = f"vm_{lab_source}_{study_id}" # for sample/ cell metadata summaries

            defaults = {
                "total_features": None,
                "n_sig_features": None,
                "total_samples": None,
                "n_conditions": None,
                "total_cells": None,
                "n_cell_types": None,
                "n_clusters": None,
                "doublet_scores_json": "[]",
                "cell_types_json": "[]",
                "conditions_json": "[]",
                "tissues_json": "[]",
                "age_range_json": "{}",
                "sexes_json": "[]",
            }

            try:
                if _view_exists(con, expr_view):
                    # Feature summaries from expression view
                    feature_stats_query = f"""
                        SELECT
                            COUNT(DISTINCT gene_symbol) AS total_features,
                            SUM(CASE WHEN padj < 0.05 THEN 1 ELSE 0 END) AS n_sig_features      -- row-level summary
                            -- COUNT(DISTINCT CASE WHEN padj < 0.05 THEN gene_symbol ELSE NULL END) AS n_sig_features       -- gene-level summaries due to unique rows only
                        FROM {expr_view}    
                    """ # or FROM '{db_path}'

                if _view_exists(con, meta_view):
                    # Metadata summaries from metadata view (if exists)
                    metadata_stats_query = f"""
                        SELECT
                            COUNT(DISTINCT sample_id) AS total_samples,
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
                        FROM {meta_view}    
                    """ # or NOW() AS computed_at FROM '{db_path}'

                    # Execute queries, add defaults for any missing fields, and upsert into dataset_stats
                    feature_stats = con.execute(feature_stats_query).fetchone()
                    metadata_stats = con.execute(metadata_stats_query).fetchone()
                    # default_values = [defaults[k] for k in ["total_features", "n_sig_features", 
                    #             "total_samples", "n_conditions", "total_cells", "n_cell_types", "n_clusters", 
                    #             "doublet_scores_json", "cell_types_json", "conditions_json", "tissues_json", "age_range_json", "sexes_json"]]
                    # or
                    # defaults["total_features"]      = feature_stats[0]
                    # defaults["n_sig_features"]      = feature_stats[1]
                    # defaults["total_samples"]       = metadata_stats[0]
                    # defaults["n_conditions"]        = metadata_stats[1]
                    # defaults["total_cells"]         = metadata_stats[2]
                    # defaults["n_cell_types"]        = metadata_stats[3]
                    # defaults["n_clusters"]          = metadata_stats[4]
                    # defaults["doublet_scores_json"] = metadata_stats[5] or "[]"
                    # defaults["cell_types_json"]     = metadata_stats[6] or "[]"
                    # defaults["conditions_json"]     = metadata_stats[7] or "[]"
                    # defaults["tissues_json"]        = metadata_stats[8] or "[]"
                    # defaults["age_range_json"]      = metadata_stats[9] or "{}"
                    # defaults["sexes_json"]          = metadata_stats[10] or "[]"

            except Exception as e:
                print(f"  WARNING: stats computation failed for study_id: {study_id}, lab: {lab_source}: {e}")

            con.execute(f"""
                INSERT OR REPLACE INTO dataset_stats VALUES (
                    ?,?,?,?,?,?,?,?,?,?,?,?,?,?,?
                )
            """, [study_id, lab_source, display_name, omic_type] + list(feature_stats) + list(metadata_stats) + [datetime.now(timezone.utc)])

    finally:
        con.close()
        print("    Aggregated stats computed.")

        # Compute tables or views for common queries for UI and filtering:
        # e.g. "top 10 most abundant x y z", "top 10 DE genes in cell type X" or "expression of gene Y across conditions" - can be expanded once usage patterns are clearer


