"""
Module computes and stores aggregated dataset-level stats at ingestion time for fast retrieval.
Stats are written once (or on force=True).
# TODO: Add cacheing for expensive queries on-demand e.g. "top 10 most abundant x y z", "top 10 DE genes in cell type X" or "expression of gene Y across conditions" - can be expanded once usage patterns are clearer
"""

from datetime import datetime, timezone
import duckdb

def view_exists(con, view_name: str) -> bool:  #con: duckdb.DuckDBPyConnection
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


# Ordered key names for build_dataset_stats()
FEATURE_STAT_KEYS = ["total_features", "n_sig_features"]
META_STAT_KEYS = [
    "total_samples", "n_conditions", "total_cells", "n_cell_types", "n_clusters",
    "avg_doublet_score", "cell_types_json", "conditions_json",
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
                avg_doublet_score   VARCHAR,   -- avg. doublet scores (if available for scrna/ snrna)
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
                data_path
            FROM dataset_registry
            -- ORDER BY study_id, lab_source
        """).fetchall()
        
        attached_dbs = {}
        
        for (study_id, lab_source, dataset_name, omic_type, data_path) in datasets:
            
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
                        continue
            db_alias = attached_dbs[data_path]

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

            expr_table        = table_map.get("expression")
            counts_table      = table_map.get("counts")
            sample_meta_table = table_map.get("sample_metadata")
            cell_meta_table   = table_map.get("cell_metadata")
            extra_meta_table  = table_map.get("extra_metadata")   #TODO: append into a single metadata_table?
            
            gene_col          = name_mappings.get("gene_symbol")
            protein_col       = name_mappings.get("protein_id")
            organism_col      = name_mappings.get("organism")
            human_col         = name_mappings.get("human_gene")
            padj_col          = name_mappings.get("padj")
            sample_a_col      = name_mappings.get("sample_a")
            sample_b_col      = name_mappings.get("sample_b")
            condition_a_col   = name_mappings.get("condition_a")
            condition_b_col   = name_mappings.get("condition_b")
            cell_id_col       = name_mappings.get("cell_id")
            cell_type_col     = name_mappings.get("cell_type")
            dbl_score_col     = name_mappings.get("doublet_scores")
            cluster_col       = name_mappings.get("cluster_id")
            tissue_col        = name_mappings.get("tissue")
            age_col           = name_mappings.get("age")
            sex_col           = name_mappings.get("sex")

            # Skip if stats is already computed and not forced
            if not force:
                computed = con.execute("""
                    SELECT computed_at FROM dataset_stats
                    WHERE study_id=? AND lab_source=?
                """, [study_id, lab_source]).fetchone()
                if computed:
                    print(f"   Stats already computed for [{lab_source}] study_id={study_id} — skipping.")
                    continue
            
            # Generate view names for queries, to avoid management, complexity, and overhead
            # - views are created by build_registry_index() in registry_parser.py
            view_name = f"v_{lab_source}_{study_id}"

            # Initialise stat values to prevent `UnboundLocalError`
            defaults = {
                "total_features":       None,
                "n_sig_features":       None,
                "total_samples":        None,
                "n_conditions":         None,
                "total_cells":          None,
                "n_cell_types":         None,
                "n_clusters":           None,
                "avg_doublet_scores":  "[]",
                "cell_types_json":      "[]",
                "conditions_json":      "[]",
                "tissues_json":         "[]",
                "age_range_json":       "{}",
                "sexes_json":           "[]",
            }

            try:
                feature_stats = None
                metadata_stats = None

                # Execute queries, add defaults for any missing fields, and upsert into dataset_stats
                if view_exists(con, view_name):
                    # Feature summaries from expression view
                    print("DEBUG VIEW CHECK!!!")
                    # feature_stats_query = f"""
                    #     SELECT
                    #         COUNT(DISTINCT {gene_col})                          AS total_features,
                    #         SUM(CASE WHEN {padj_col} < 0.05 THEN 1 ELSE 0 END)  AS n_sig_features      -- row-level summary
                    #         -- COUNT(DISTINCT CASE WHEN {padj_col} < 0.05 THEN {gene_col} ELSE NULL END) AS n_sig_features       -- gene-level summaries due to unique rows only
                    #     FROM {view_name}    
                    # """ # FROM {db_alias}.main.{expr_table} or FROM {db_alias}.main.{actual_table} or FROM '{data_path}'
                    feature_stats_query = f"""
                        SELECT
                            COUNT(DISTINCT gene_symbol)                         AS total_features,
                            SUM(CASE WHEN padj < 0.05 THEN 1 ELSE 0 END)        AS n_sig_features      -- row-level summary
                            -- COUNT(DISTINCT CASE WHEN padj < 0.05 THEN gene_symbol ELSE NULL END) AS n_sig_features       -- gene-level summaries due to unique rows only
                        FROM {view_name}    
                    """ # FROM {db_alias}.main.{expr_table} or FROM {db_alias}.main.{actual_table} or FROM '{data_path}'
                    feature_stats = con.execute(feature_stats_query).fetchone() #or (None, None)

                    # Metadata summaries from metadata view (if exists)
                    # metadata_stats_query = f"""
                    #     SELECT
                    #         COUNT(DISTINCT {sample_a_col}) + COUNT(DISTINCT {sample_b_col})       AS total_samples,           -- COUNT(DISTINCT sample_id) AS total_samples,
                    #         COUNT(DISTINCT {condition_a_col}) + COUNT(DISTINCT {condition_b_col}) AS n_conditions,            -- DISTINCT ({condition_a_col}, {condition_b_col})) OR COUNT(DISTINCT condition) AS n_conditions;    COUNT(DISTINCT {condition_a_col} AND {condition_b_col})
                    #         COUNT(DISTINCT {cell_id_col})                            AS total_cells,
                    #         COUNT(DISTINCT {cell_type_col})                          AS n_cell_types,
                    #         COUNT(DISTINCT {cluster_col})                            AS n_clusters,
                    #         json_group_array(DISTINCT {dbl_score_col})               AS doublet_scores_json,                  -- JSON_GROUP_ARRAY(DISTINCT doublet_scores) AS doublet_scores_json, -- OR consider TO_JSON(LIST(DISTINCT doublet_scores))
                    #         json_group_array(DISTINCT {cell_type_col})               AS cell_types_json,
                    #         json_group_array(DISTINCT ({condition_a_col}, {condition_b_col})) AS conditions_json,             -- json_group_array(DISTINCT condition_a) || json_group_array(DISTINCT condition_b);   JSON_GROUP_ARRAY()
                    #         json_group_array(DISTINCT {tissue_col})                  AS tissues_json,
                    #         JSON_OBJECT('min', MIN({age_col}), 'max', MAX({age_col}), 'unit', 'months') AS age_range_json,    -- JSON_GROUP_OBJECT gives key/value merging OR JSON_OBJECT for single structure
                    #         json_group_array(DISTINCT {sex_col})                     AS sexes_json
                    #     FROM {view_name}    
                    # """ # or NOW() AS computed_at FROM '{data_path}'
                    metadata_stats_query = f"""
                        SELECT
                            COUNT(DISTINCT sample_a) + COUNT(DISTINCT sample_b)       AS total_samples,           -- COUNT(DISTINCT sample_id) AS total_samples,
                            COUNT(DISTINCT condition_a) + COUNT(DISTINCT condition_b) AS n_conditions,            -- DISTINCT ({condition_a_col}, {condition_b_col})) OR COUNT(DISTINCT condition) AS n_conditions;    COUNT(DISTINCT {condition_a_col} AND {condition_b_col})
                            COUNT(DISTINCT cell_id)                            AS total_cells,
                            COUNT(DISTINCT cell_type)                          AS n_cell_types,
                            COUNT(DISTINCT cluster_id)                         AS n_clusters,
                            DISTINCT AVG(doublet_scores)                       AS avg_doublet_score,            -- JSON_GROUP_ARRAY(DISTINCT doublet_scores) AS doublet_scores_json, -- OR consider TO_JSON(LIST(DISTINCT doublet_scores))
                            TO_JSON(LIST(DISTINCT cell_type))                  AS cell_types_json,                -- json_group_array(DISTINCT cell_type)
                            TO_JSON(LIST(DISTINCT (condition_a, condition_b))) AS conditions_json,                -- json_group_array(DISTINCT (condition_a, condition_b))  -- json_group_array(DISTINCT condition_a) || json_group_array(DISTINCT condition_b);   JSON_GROUP_ARRAY()
                            TO_JSON(LIST(DISTINCT tissue))                     AS tissues_json,                   -- json_group_array(DISTINCT tissue)
                            JSON_OBJECT('min', MIN(age), 'max', MAX(age), 'unit', 'months') AS age_range_json,    -- JSON_GROUP_OBJECT gives key/value merging OR JSON_OBJECT for single structure
                            TO_JSON(LIST(DISTINCT sex))                        AS sexes_json                      -- json_group_array(DISTINCT sex)
                        FROM {view_name}    
                    """ # or NOW() AS computed_at FROM '{data_path}'
                    metadata_stats = con.execute(metadata_stats_query).fetchone() #or (None,) * len(META_STAT_KEYS)               
                    print(f"[DEBUG] metadata_stats  {metadata_stats}")
                
                # Create combinations of iterations to map positional columns → key names to a dict, before merging with defaults
                if feature_stats:
                    defaults.update(dict(zip(FEATURE_STAT_KEYS, feature_stats)))
                print(f"   [DEBUG] mapped feature_stats [{lab_source}] study_id={study_id}: {feature_stats}")
                if metadata_stats:
                    defaults.update(dict(zip(META_STAT_KEYS, metadata_stats)))
                print(f"   [DEBUG] mapped metadata_stats [{lab_source}] study_id={study_id}: {feature_stats}")

            except Exception as e:
                print(f"  WARNING: metadata stats failed for study_id: {study_id}, lab: {lab_source}: {e}")

            con.execute(f"""
                INSERT OR REPLACE INTO dataset_stats (study_id, lab_source, dataset_name, omic_type, total_features,
                    n_sig_features, total_samples, n_conditions, total_cells, n_cell_types, n_clusters, avg_doublet_score, 
                    cell_types_json, conditions_json, tissues_json, age_range_json, sexes_json, computed_at) 
                VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
            """, [study_id, lab_source, dataset_name, omic_type,
                    *[defaults[k] for k in FEATURE_STAT_KEYS + META_STAT_KEYS], # *splat/ flatten values from merged dict, defaults
                    datetime.now(timezone.utc)]
            )
            # [study_id, lab_source, dataset_name, omic_type] + list(feature_stats) + list(metadata_stats) + [datetime.now(timezone.utc)])
                # [study_id, lab_source, dataset_name, omic_type, defaults["total_features"],
                #     defaults["n_sig_features"],
                #     defaults["total_samples"],
                #     defaults["n_conditions"],
                #     defaults["total_cells"],
                #     defaults["n_cell_types"],
                #     defaults["n_clusters"],
                #     defaults["doublet_scores_json"],
                #     defaults["cell_types_json"],
                #     defaults["conditions_json"],
                #     defaults["tissues_json"],
                #     defaults["age_range_json"],
                #     defaults["sexes_json"], datetime.now(timezone.utc)])

    finally:
        con.close()
        print("    Aggregated dataset stats computed.")

        # Compute tables or views for common queries for UI and filtering:
        # e.g. "top 10 most abundant x y z", "top 10 DE genes in cell type X" or "expression of gene Y across conditions" - can be expanded once usage patterns are clearer


