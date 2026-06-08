"""
Module computes and stores aggregated dataset-level stats at ingestion time for fast retrieval.
Stats are written once (or on force=True).
# TODO: Add cacheing for expensive queries on-demand e.g. "top 10 most abundant x y z", "top 10 DE genes in cell type X" or "expression of gene Y across conditions" - can be expanded once usage patterns are clearer
"""

from datetime import datetime, timezone
from app.logic.startup.db_views import attach_source_dbs, view_exists, get_sql_col
import duckdb
import os

# Ordered key names for build_dataset_stats()
FEATURE_STAT_KEYS = ["total_features", "n_sig_features"]
META_STAT_KEYS = [
    "total_samples", "n_conditions", "total_cells", "n_cell_types", "n_clusters",
    # "avg_doublet_score",
    "cell_types_json", "conditions_json",
    "tissues_json", "age_range_json", "sexes_json",
]


def build_dataset_stats(registry_db_path: str, force: bool = False):
    """
    Aggregate datasets to create data summaries.
    """
    con = duckdb.connect(registry_db_path)
    try:
        con.execute("""
            CREATE TABLE IF NOT EXISTS dataset_stats (
                study_id            INTEGER,
                lab_source          VARCHAR NOT NULL,
                dataset_name        VARCHAR NOT NULL,
                omic_type           VARCHAR,
                total_features      INTEGER,   -- genes, proteins, transcripts
                n_sig_features      INTEGER,   -- padj < 0.05 count
                total_samples       INTEGER,   -- e.g. for bulk/proteomics
                n_conditions        INTEGER,   -- distinct conditions
                total_cells         INTEGER,   -- e.g. for scrna/ snrna (NULL for others)
                n_cell_types        INTEGER,   -- distinct cell_type_label values
                n_clusters          INTEGER,   -- distinct cluster_label values
                -- avg_doublet_score   VARCHAR,   -- avg. doublet scores (if available for scrna/ snrna)
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
                dataset_name, 
                omic_type,
                source_type,
                data_path
            FROM dataset_registry
            -- ORDER BY study_id, lab_source
        """).fetchall()
        
        attached_dbs = {}
        issue_rows   = []

        # Attach each unique db_path once if not done already, record alias per path
        attached_dbs, attach_failures = attach_source_dbs(con, datasets, 
            lab_source_index=1, data_path_index=5)
        for path, lab, err in attach_failures:
            issue_rows.append((datetime.now(timezone.utc), None, lab, None, f"ATTACH failed: {err}"))
        
        for (study_id, lab_source, dataset_name, omic_type, source_type, data_path) in datasets:
            
            if not data_path:
                issue_rows.append((
                    datetime.now(timezone.utc), study_id, lab_source,
                    dataset_name, "missing database path, expression table, or data object — skipped"
                ))
                continue

            # Skip unsupported source types
            if source_type not in ["duckdb", "parquet"]:
                print(f"   INFO: Skipping unsupported source type [{source_type}] for [{lab_source}] study_id={study_id} ({data_path})")
                continue
            if source_type == "parquet":
                db_alias = None
            else:
                db_alias = attached_dbs[os.path.realpath(data_path)]
                # db_alias = attached_dbs.get(os.path.realpath(data_path))
                if not db_alias:
                    issue_rows.append((
                        datetime.now(timezone.utc), study_id, lab_source,
                        dataset_name, "Issue giving db attachment an alias — skipped"
                    ))
                    continue

            # Mappings to extract data from any column that has an adjacent canonical name, from tables
            name_mappings = dict(
                con.execute("""
                    SELECT canonical_name, original_name 
                    FROM column_mappings
                    WHERE study_id = ? AND lab_source = ?
                """, [study_id, lab_source]).fetchall()
            )

            table_map = dict(con.execute("""
                SELECT logical_table, actual_table
                FROM table_mappings
                WHERE study_id = ? AND lab_source = ?
            """, [study_id, lab_source]).fetchall())

            # Skip if stats is already computed and not forced
            if not force:
                computed = con.execute("""
                    SELECT computed_at FROM dataset_stats
                    WHERE study_id=? AND lab_source=?
                """, [study_id, lab_source]).fetchone()
                if computed:
                    print(f"   Stats already computed for [{lab_source}] study_id={study_id} — skipping...")
                    continue
            
            # Generate view names for queries, to avoid management, complexity, and overhead
            # - views are created by build_registry_index() in registry_parser.py
            expr_view     = f"v_{lab_source}_{study_id}"
            obs_meta_view = f"vm_{lab_source}_{study_id}"

            # Initialise stat values to prevent `UnboundLocalError`
            defaults = {
                "total_features":       None,
                "n_sig_features":       None,
                "total_samples":        None,
                "n_conditions":         None,
                "total_cells":          None,
                "n_cell_types":         None,
                "n_clusters":           None,
                # "avg_doublet_scores":  "[]",
                "cell_types_json":      "[]",
                "conditions_json":      "[]",
                "tissues_json":         "[]",
                "age_range_json":       "{}",
                "sexes_json":           "[]",
            }

            try:
                feature_stats  = None
                obs_stats      = None
                metadata_stats = None

                # Create queries
                # Feature summaries from expression view
                feature_stats_query = f"""
                    SELECT
                        COUNT(DISTINCT gene_symbol)                         AS total_features,
                        SUM(CASE 
                            WHEN padj IS NOT NULL AND padj < 0.05 THEN 1
                            WHEN padj IS NULL AND pvalue IS NOT NULL AND pvalue < 0.05 THEN 1
                            ELSE 0 END)                                     AS n_sig_features      -- SUM(CASE WHEN padj < 0.05 THEN 1 ELSE 0 END)        AS n_sig_features      -- gene-level summaries due to unique rows only
                    FROM {expr_view}    
                """ # FROM {db_alias}.main.{expr_table} or FROM {db_alias}.main.{actual_table} or FROM '{data_path}'
                
                # Metadata summaries from metadata view (if exists)
                obs_stats_query = f"""
                    SELECT
                        COUNT(DISTINCT sample_a) + COUNT(DISTINCT sample_b)       AS total_samples,
                        COUNT(DISTINCT condition_a) + COUNT(DISTINCT condition_b) AS n_conditions,
                        COUNT(DISTINCT cell_id)                                   AS total_cells,
                        COUNT(DISTINCT cell_type)                                 AS n_cell_types,
                        COUNT(DISTINCT cluster_id)                                AS n_clusters,
                        -- DISTINCT AVG(doublet_scores)                           AS avg_doublet_score,            -- JSON_GROUP_ARRAY(DISTINCT doublet_scores) AS doublet_scores_json, -- OR consider TO_JSON(LIST(DISTINCT doublet_scores))
                        TO_JSON(LIST(DISTINCT cell_type))                         AS cell_types_json,                -- json_group_array(DISTINCT cell_type)
                        TO_JSON(LIST_DISTINCT(LIST_CAT(LIST(DISTINCT condition_a), LIST(DISTINCT condition_b)))) AS conditions_json,
                        -- TO_JSON(LIST(DISTINCT condition_a || ' vs ' || condition_b)) AS conditions_json,         -- TO_JSON(LIST(DISTINCT struct_pack(cond_a := condition_a, cond_b := condition_b))) AS conditions_json,   -- TO_JSON(LIST(DISTINCT (condition_a, condition_b)))     AS conditions_json,                -- json_group_array(DISTINCT (condition_a, condition_b))  -- json_group_array(DISTINCT condition_a) || json_group_array(DISTINCT condition_b);   JSON_GROUP_ARRAY()
                        TO_JSON(LIST(DISTINCT tissue))                            AS tissues_json,                   -- json_group_array(DISTINCT tissue)
                        JSON_OBJECT('min', MIN(age), 'max', MAX(age), 'unit', 'months') AS age_range_json,    -- JSON_GROUP_OBJECT gives key/value merging OR JSON_OBJECT for single structure
                        TO_JSON(LIST(DISTINCT sex))                               AS sexes_json                      -- json_group_array(DISTINCT sex)
                    FROM {obs_meta_view}    
                """ # or NOW() AS computed_at FROM '{data_path}'
                
                # Metadata (extra obs e.g. sample/ condition) summaries from expression view (if not present in obs view)
                metadata_stats_query = f"""
                    SELECT
                        COALESCE(array_length(LIST_DISTINCT(LIST_CAT(
                            LIST(DISTINCT sample_a), LIST(DISTINCT sample_b)))), 0)       AS total_samples,
                        COALESCE(array_length(LIST_DISTINCT(LIST_CAT(
                            LIST(DISTINCT condition_a), LIST(DISTINCT condition_b)))), 0) AS n_conditions,
                        COUNT(DISTINCT cell_id)                                           AS total_cells,
                        COUNT(DISTINCT cell_type)                                         AS n_cell_types,
                        COUNT(DISTINCT cluster_id)                                        AS n_clusters,
                        TO_JSON(LIST(DISTINCT cell_type))                                 AS cell_types_json,
                        TO_JSON(LIST_DISTINCT(LIST_CAT(
                            LIST(DISTINCT condition_a), LIST(DISTINCT condition_b))))     AS conditions_json,
                        TO_JSON(LIST(DISTINCT tissue))                                    AS tissues_json,
                        JSON_OBJECT('min', MIN(age), 'max', MAX(age), 'unit', 'months')  AS age_range_json,
                        TO_JSON(LIST(DISTINCT sex))                                       AS sexes_json
                    FROM {expr_view}
                """

                # Execute queries, add defaults for any missing fields, and upsert into dataset_stats
                if view_exists(con, expr_view):
                    feature_stats  = con.execute(feature_stats_query).fetchone() #or (None, None)
                    metadata_stats = con.execute(metadata_stats_query).fetchone()

                # Override empty fields with obs_meta_view when it has richer data (scrna cell_type, cell_id etc.)
                # Only replace individual fields that are actually better (non-zero/non-null)
                if view_exists(con, obs_meta_view):
                    obs_stats = con.execute(obs_stats_query).fetchone() #or (None,) * len(META_STAT_KEYS)               
                    if obs_stats and metadata_stats:
                        # Merge: take obs value if it's non-zero/non-null, else keep expr value
                        metadata_stats = tuple(
                            b if (b is not None and b not in (0, '[null]', '{}', '[]')) else a
                            for a, b in zip(metadata_stats, obs_stats)
                        )
                    elif obs_stats:
                        metadata_stats = obs_stats
                
                # Create combinations of iterations to map positional columns → key names to a dict, before merging with defaults
                if feature_stats:
                    defaults.update(dict(zip(FEATURE_STAT_KEYS, feature_stats)))
                print(f"   [DEBUG] mapped feature_stats [{lab_source}] study_id={study_id}: -> total_features & n_sig_genes {feature_stats}")
                if metadata_stats:
                    defaults.update(dict(zip(META_STAT_KEYS, metadata_stats)))
                print(f"   [DEBUG] mapped metadata_stats [{lab_source}] study_id={study_id}: -> total_samples & n_sig_features etc. {metadata_stats}")
                
            except Exception as e:
                print(f"  WARNING: stats failed for study_id: {study_id}, lab: {lab_source}: {e}")

            try:
                con.execute(f"""
                    INSERT OR REPLACE INTO dataset_stats (study_id, lab_source, dataset_name, omic_type, total_features,
                        n_sig_features, total_samples, n_conditions, total_cells, n_cell_types, n_clusters, 
                        cell_types_json, conditions_json, tissues_json, age_range_json, sexes_json, computed_at) 
                    VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
                """, [study_id, lab_source, dataset_name, omic_type,
                        *[defaults[k] for k in FEATURE_STAT_KEYS + META_STAT_KEYS], # *splat/ flatten values from merged dict, defaults
                        datetime.now(timezone.utc)]
                ) # TODO: avg_doublet_score,
                # [study_id, lab_source, dataset_name, omic_type] + list(feature_stats) + list(metadata_stats) + [datetime.now(timezone.utc)])
            
            except Exception as e:
                print(f"  ERROR: creating or inserting dataset_stats for study_id: {study_id}, lab: {lab_source}: {e}")

    finally:
        con.close()
        print("   Aggregated dataset stats computed.")

        # Compute tables or views for common queries for UI and filtering:
        # e.g. "top 10 most abundant x y z", "top 10 DE genes in cell type X" or "expression of gene Y across conditions" - can be expanded once usage patterns are clearer

