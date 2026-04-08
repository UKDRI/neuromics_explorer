"""
FastAPI router for querying via DuckDB and returns Arrow IPC for tabular
data so the Shiny client stays a thin rendering layer.
"""

from __future__ import annotations

import io
import os
import re

import pyarrow.ipc as ipc
from fastapi import APIRouter, HTTPException, Query, Request, Response

from app.logic.startup.db_pool import get_conn

router = APIRouter(prefix="/api", tags=["query"])
ARROW_MEDIA_TYPE = "application/vnd.apache.arrow.stream"
SAFE_IDENTIFIER = re.compile(r"^[A-Za-z0-9_]+$")
SEMANTIC_GENE_EXPR = "COALESCE(NULLIF(gene_symbol, ''), NULLIF(human_gene, ''))"
DEFAULT_TABLE_LIMIT = 500
MAX_TABLE_LIMIT = 5000
DEFAULT_PLOT_LIMIT = 20000
MAX_PLOT_LIMIT = 100000
DEFAULT_EMBEDDING_MAX_POINTS = 50000
MAX_EMBEDDING_POINTS = 200000
TABLE_SORT_COLUMNS = {
    "gene_symbol": SEMANTIC_GENE_EXPR,
    "human_gene": "human_gene",
    "protein_id": "protein_id",
    "padj": "padj",
    "pvalue": "pvalue",
    "log2fc": "log2fc",
    "abundance_a": "abundance_a",
    "abundance_b": "abundance_b",
    "cell_type": "cell_type",
    "condition_a": "condition_a",
    "condition_b": "condition_b",
}
GROUPABLE_COLUMNS = {
    "cell_type": "cell_type",
    "condition_a": "condition_a",
    "condition_b": "condition_b",
    "sample_a": "sample_a",
    "sample_b": "sample_b",
    "organism": "organism",
}
METRIC_COLUMNS = {
    "log2fc": "log2fc",
    "pvalue": "pvalue",
    "padj": "padj",
    "abundance_a": "abundance_a",
    "abundance_b": "abundance_b",
    "pct_expressed_a": "pct_expressed_a",
    "pct_expressed_b": "pct_expressed_b",
}
GROUP_METRIC_COLUMNS = {
    "log2fc": "log2fc",
    "abundance_a": "abundance_a",
    "abundance_b": "abundance_b",
    "pct_expressed_a": "pct_expressed_a",
    "pct_expressed_b": "pct_expressed_b",
}


def _require_pool(request: Request):
    """Return the shared DuckDB pool or fail fast during partial startup."""
    pool = getattr(request.app.state, "db_pool", None)
    if pool is None:
        raise HTTPException(status_code=503, detail="DuckDB pool is not initialised.")
    return pool


def _arrow_response(table) -> Response:
    """Serialise a DuckDB/PyArrow result into the Arrow IPC stream the R client expects."""
    if hasattr(table, "read_all"):
        table = table.read_all()

    buf = io.BytesIO()
    with ipc.new_stream(buf, table.schema) as writer:
        writer.write_table(table)
    return Response(buf.getvalue(), media_type=ARROW_MEDIA_TYPE)


def _query_arrow(request: Request, sql: str, params: list | None = None) -> Response:
    """Run SQL through the pooled DuckDB connection and return Arrow bytes."""
    pool = _require_pool(request)
    with get_conn(pool) as con:
        table = con.execute(sql, params or []).arrow()
    return _arrow_response(table)


def _safe_view_name(prefix: str, lab: str, study_id: int) -> str:
    """Guard dynamic view names because lab ids are interpolated into SQL identifiers."""
    if not SAFE_IDENTIFIER.fullmatch(lab):
        raise HTTPException(status_code=400, detail=f"Invalid lab identifier: {lab!r}")
    return f"{prefix}_{lab}_{study_id}"


def _quote_identifier(identifier: str) -> str:
    return '"' + identifier.replace('"', '""') + '"'


def _clean_terms(values: list[str]) -> list[str]:
    """Trim, deduplicate, and validate search terms before they reach SQL."""
    cleaned = [value.strip() for value in values if value and value.strip()]
    if not cleaned:
        raise HTTPException(status_code=400, detail="At least one non-empty gene term is required.")
    return list(dict.fromkeys(cleaned))


def _clean_optional_terms(values: list[str] | None) -> list[str]:
    if not values:
        return []
    return _clean_terms(values)


def _first_existing(columns: set[str], *candidates: str | None) -> str | None:
    for candidate in candidates:
        if candidate and candidate in columns:
            return candidate
    return None


def _dataset_context(con, lab: str, study_id: int) -> dict:
    row = con.execute(
        """
        SELECT dataset_name, omic_type, source_type, data_path
        FROM dataset_registry
        WHERE lab_source = ? AND study_id = ?
        LIMIT 1
        """,
        [lab, study_id],
    ).fetchone()

    if row is None:
        raise HTTPException(status_code=404, detail=f"Dataset not found for {lab}:{study_id}")

    dataset_name, omic_type, source_type, data_path = row
    table_map = dict(con.execute(
        """
        SELECT logical_table, actual_table
        FROM table_mappings
        WHERE lab_source = ? AND study_id = ?
        """,
        [lab, study_id],
    ).fetchall())
    name_map = dict(con.execute(
        """
        SELECT canonical_name, original_name
        FROM column_mappings
        WHERE lab_source = ? AND study_id = ?
        """,
        [lab, study_id],
    ).fetchall())

    return {
        "dataset_name": dataset_name,
        "omic_type": omic_type,
        "source_type": source_type,
        "data_path": data_path,
        "table_map": table_map,
        "name_map": name_map,
        "lab": lab,
        "study_id": study_id,
    }


def _table_ref(ctx: dict, actual_table: str, alias: str | None = None) -> str:
    source_type = ctx["source_type"]
    if source_type == "parquet":
        path = os.path.join(ctx["data_path"], actual_table)
        ref = f"read_parquet('{path}')"
    elif source_type == "duckdb":
        ref = f"src_{ctx['lab']}.main.{_quote_identifier(actual_table)}"
    else:
        raise HTTPException(
            status_code=400,
            detail=f"Embeddings endpoint does not support source type {source_type!r}."
        )

    if alias:
        return f"{ref} AS {alias}"
    return ref


def _table_columns(con, ctx: dict, actual_table: str) -> set[str]:
    table_ref = _table_ref(ctx, actual_table)
    return {
        row[0] for row in con.execute(
            f"DESCRIBE SELECT * FROM {table_ref}"
        ).fetchall()
    }


def _term_predicates(genes: list[str], proteins: list[str]) -> tuple[list[str], list]:
    """Build the feature-level WHERE predicates shared by single and multi-dataset routes."""
    predicates: list[str] = []
    params: list = []

    if genes:
        gene_placeholders = ",".join(["?"] * len(genes))
        predicates.append(f"UPPER({SEMANTIC_GENE_EXPR}) IN ({gene_placeholders})")
        params.extend(term.upper() for term in genes)

    if proteins:
        protein_placeholders = ",".join(["?"] * len(proteins))
        predicates.append(f"UPPER(protein_id) IN ({protein_placeholders})")
        params.extend(term.upper() for term in proteins)

    return predicates, params


def _expression_filters(
    genes: list[str],
    proteins: list[str],
    cell_type: str | None = None,
) -> tuple[list[str], list]:
    """Build reusable WHERE clauses for expression-style routes."""
    clauses: list[str] = []
    params: list = []

    predicates, predicate_params = _term_predicates(genes, proteins)
    if predicates:
        clauses.append(f"({' OR '.join(predicates)})")
        params.extend(predicate_params)

    if cell_type:
        clauses.append("cell_type = ?")
        params.append(cell_type)

    return clauses, params


def _where_sql(clauses: list[str]) -> str:
    if not clauses:
        return ""
    return "WHERE " + " AND ".join(clauses)


def _validated_sort_sql(sort_by: str, sort_dir: str) -> str:
    sort_expr = TABLE_SORT_COLUMNS.get(sort_by)
    if sort_expr is None:
        raise HTTPException(status_code=400, detail=f"Unsupported sort column: {sort_by!r}")

    direction = sort_dir.upper()
    if direction not in {"ASC", "DESC"}:
        raise HTTPException(status_code=400, detail=f"Unsupported sort direction: {sort_dir!r}")

    return f"ORDER BY {sort_expr} {direction} NULLS LAST"


def _validated_group_expr(group_by: str | None) -> str:
    if not group_by:
        return "'All'"

    group_expr = GROUPABLE_COLUMNS.get(group_by)
    if group_expr is None:
        raise HTTPException(status_code=400, detail=f"Unsupported grouping column: {group_by!r}")
    return f"COALESCE(CAST({group_expr} AS VARCHAR), 'unlabelled')"


def _validated_metric_expr(metric: str, *, group_metric: bool = False) -> str:
    mapping = GROUP_METRIC_COLUMNS if group_metric else METRIC_COLUMNS
    metric_expr = mapping.get(metric)
    if metric_expr is None:
        raise HTTPException(status_code=400, detail=f"Unsupported metric: {metric!r}")
    return metric_expr


def _terms_cte(terms: list[str]) -> tuple[str, list[str]]:
    placeholders = ", ".join(["(?)"] * len(terms))
    return f"terms(term) AS (SELECT * FROM (VALUES {placeholders}) AS t(term))", terms


def _dataset_unions(
    dataset_refs: list[str],
    genes: list[str],
    proteins: list[str],
    padj: float,
    lfc: float,
    cell_type: str | None
):
    """
    Build one UNION ALL query across selected datasets for the Compare tab.

    The Shiny Compare tab sends the selected dataset keys and optional
    thresholds once; the API fans that out over the per-dataset semantic views.
    """
    selects: list[str] = []
    params: list = []

    for dataset_ref in dataset_refs:
        try:
            lab, study_str = dataset_ref.split(":", 1)
            study_id = int(study_str)
        except ValueError as exc:
            raise HTTPException(
                status_code=400,
                detail=f"Invalid dataset reference: {dataset_ref!r}. Expected lab:study_id."
            ) from exc

        view = _safe_view_name("v", lab, study_id)
        predicates, predicate_params = _term_predicates(genes, proteins)
        params.extend(predicate_params)
        cell_type_clause = "AND cell_type = ?" if cell_type else ""
        term_clause = f"AND ({' OR '.join(predicates)})" if predicates else ""

        selects.append(f"""
            SELECT
              '{lab}' AS lab_source,
              {study_id} AS study_id,
              {SEMANTIC_GENE_EXPR} AS gene_symbol,
              human_gene, protein_id, organism,
              log2fc, pvalue, padj,
              abundance_a, abundance_b,
              pct_expressed_a, pct_expressed_b, expression_metric,
              sample_a, sample_b, condition_a, condition_b, cell_type
            FROM {view}
            WHERE 1 = 1
              {term_clause}
              {cell_type_clause}
        """)

        if cell_type:
            params.append(cell_type)

    sql = "\nUNION ALL\n".join(selects) + "\nORDER BY lab_source, study_id, gene_symbol, padj ASC NULLS LAST"
    return sql, params


def _resolve_metadata_source_view(con, lab: str, study_id: int) -> str:
    """
    Prefer the metadata-only view when it exists, otherwise fall back to `v_*`.

    `vm_*` is created from a dedicated obs/metadata table and tends to hold
    richer sample or cell annotations. Some datasets only expose metadata inside
    the canonical expression view `v_*`, so the UI routes need this fallback.
    """
    vm_view = _safe_view_name("vm", lab, study_id)
    v_view = _safe_view_name("v", lab, study_id)

    try:
        con.execute(f"SELECT 1 FROM {vm_view} LIMIT 1")
        return vm_view
    except Exception:
        return v_view


@router.get("/health")
def health():
    """Lightweight liveness check used by local startup and deploy smoke tests."""
    return {"status": "ok"}


@router.get("/genes/index")
def gene_index(
    request: Request,
    q: str | None = None,
    limit: int = Query(5000, ge=1, le=50000),
):
    """
    Return the typeahead gene list for the search modal selectize input.

    UI connection:
    `gene_dataset_selector.R` calls this when the modal opens so users get an
    initial alphabetical slice before typing, then again as they narrow terms.
    """
    clauses = ["gene_symbol IS NOT NULL", "regexp_matches(gene_symbol, '^[A-Za-z]')"]
    params: list = []

    if q:
        clauses.append("gene_symbol ILIKE ?")
        params.append(f"{q.strip()}%")

    params.append(limit)
    sql = f"""
        SELECT DISTINCT gene_symbol
        FROM gene_study_index
        WHERE {" AND ".join(clauses)}
        ORDER BY gene_symbol
        LIMIT ?
    """
    return _query_arrow(request, sql, params)


@router.get("/proteins/index")
def protein_index(
    request: Request,
    q: str | None = None,
    limit: int = Query(5000, ge=1, le=50000),
):
    """Protein-id sibling of `/genes/index` for the modal's second selectize control."""
    clauses = ["protein_id IS NOT NULL"]
    params: list = []

    if q:
        clauses.append("protein_id ILIKE ?")
        params.append(f"{q.strip()}%")

    params.append(limit)
    sql = f"""
        SELECT DISTINCT protein_id
        FROM gene_study_index
        WHERE {" AND ".join(clauses)}
        ORDER BY protein_id
        LIMIT ?
    """
    return _query_arrow(request, sql, params)


@router.get("/datasets/all")
def all_datasets(request: Request):
    """
    Return the registry plus precomputed stats for every dataset.

    UI: the modal fetches this on open to populate lab choices and preview summary
    numbers without forcing a gene search first.
    """
    sql = """
        SELECT
          dr.study_id, dr.lab_source, dr.dataset_name, dr.omic_type,
          gi.organism, dr.source_type,
          ds.total_features, ds.n_sig_features,
          ds.total_samples, ds.total_cells,
          ds.n_conditions, ds.n_cell_types,
          ds.conditions_json, ds.cell_types_json,
          ds.computed_at
        FROM dataset_registry dr
        LEFT JOIN (
          SELECT study_id, lab_source, MIN(organism) AS organism
          FROM gene_study_index
          GROUP BY study_id, lab_source
        ) gi
          ON dr.study_id = gi.study_id AND dr.lab_source = gi.lab_source
        LEFT JOIN dataset_stats ds
          ON dr.study_id = ds.study_id AND dr.lab_source = ds.lab_source
        ORDER BY dr.lab_source, dr.study_id
    """
    return _query_arrow(request, sql)


@router.get("/datasets/search")
def search_datasets(
    request: Request,
    gene: list[str] | None = Query(None),
    protein: list[str] | None = Query(None),
    omic_type: str | None = None,
    lab_source: str | None = None,
):
    """
    Search `gene_study_index` for datasets containing one or more terms.

    UI connection:
    this powers the modal results table that appears after the user clicks the
    Search button in `gene_dataset_selector.R`.
    """
    genes = _clean_optional_terms(gene)
    proteins = _clean_optional_terms(protein)
    if not genes and not proteins:
        raise HTTPException(status_code=400, detail="At least one gene or protein search term is required.")

    params: list = []
    term_clauses: list[str] = []

    if genes:
        gene_predicates = " OR ".join(["gi.gene_symbol ILIKE ?"] * len(genes))
        term_clauses.append(f"({gene_predicates})")
        params.extend(f"%{term}%" for term in genes)

    if proteins:
        protein_predicates = " OR ".join(["gi.protein_id ILIKE ?"] * len(proteins))
        term_clauses.append(f"({protein_predicates})")
        params.extend(f"%{term}%" for term in proteins)

    clauses = [f"({' OR '.join(term_clauses)})"]

    if omic_type:
        clauses.append("gi.omic_type = ?")
        params.append(omic_type)

    if lab_source:
        clauses.append("gi.lab_source = ?")
        params.append(lab_source)

    sql = f"""
        SELECT
          gi.study_id,
          gi.lab_source,
          gi.dataset_name,
          gi.omic_type,
          gi.gene_symbol,
          gi.protein_id,
          ds.total_features,
          ds.n_sig_features,
          ds.total_samples,
          ds.total_cells,
          ds.n_cell_types,
          ds.n_conditions,
          ds.cell_types_json,
          ds.conditions_json
        FROM gene_study_index gi
        LEFT JOIN dataset_stats ds
          ON gi.study_id = ds.study_id AND gi.lab_source = ds.lab_source
        WHERE {" AND ".join(clauses)}
        ORDER BY gi.lab_source, gi.study_id, gi.gene_symbol
    """
    return _query_arrow(request, sql, params)


@router.get("/datasets/stats")
def dataset_stats(
    request: Request,
    lab_source: str | None = None,
    study_id: int | None = None,
    omic_type: str | None = None,
):
    """Return precomputed dataset-level summary rows for dashboard cards and overview tables."""
    clauses = []
    params: list = []

    if lab_source:
        clauses.append("lab_source = ?")
        params.append(lab_source)

    if study_id is not None:
        clauses.append("study_id = ?")
        params.append(study_id)

    if omic_type:
        clauses.append("omic_type = ?")
        params.append(omic_type)

    where_sql = f"WHERE {' AND '.join(clauses)}" if clauses else ""
    sql = f"""
        SELECT *
        FROM dataset_stats
        {where_sql}
        ORDER BY lab_source, study_id
    """
    return _query_arrow(request, sql, params)


@router.get("/datasets/{lab}/{study_id}/expression")
def expression(
    request: Request,
    lab: str,
    study_id: int,
    gene: list[str] | None = Query(None),
    protein: list[str] | None = Query(None),
    padj: float = 0.05,
    lfc: float = 0.0,
    cell_type: str | None = None,
):
    """
    Return canonical expression/DE rows for one dataset.

    When the caller omits `gene`/`protein`, this becomes the dataset-level route
    used by the Expression tab and single-dataset plots after a modal
    selection. When terms are supplied it also serves targeted lookups.
    """
    view = _safe_view_name("v", lab, study_id)
    genes = _clean_optional_terms(gene)
    proteins = _clean_optional_terms(protein)
    predicates, params = _term_predicates(genes, proteins)

    cell_type_clause = ""
    term_clause = ""
    if predicates:
        term_clause = f"AND ({' OR '.join(predicates)})"
    if cell_type:
        cell_type_clause = "AND cell_type = ?"
        params.append(cell_type)

    sql = f"""
        SELECT
          {SEMANTIC_GENE_EXPR} AS gene_symbol,
          human_gene, protein_id, organism,
          log2fc, pvalue, padj,
          abundance_a, abundance_b,
          pct_expressed_a, pct_expressed_b, expression_metric,
          sample_a, sample_b, condition_a, condition_b, cell_type,
          study_id
        FROM {view}
        WHERE 1 = 1
          {term_clause}
          {cell_type_clause}
        ORDER BY gene_symbol, padj ASC NULLS LAST, ABS(log2fc) DESC NULLS LAST
    """
    return _query_arrow(request, sql, params)


@router.get("/datasets/{lab}/{study_id}/expression/table")
def expression_table(
    request: Request,
    lab: str,
    study_id: int,
    gene: list[str] | None = Query(None),
    protein: list[str] | None = Query(None),
    cell_type: str | None = None,
    limit: int = Query(DEFAULT_TABLE_LIMIT, ge=1, le=MAX_TABLE_LIMIT),
    offset: int = Query(0, ge=0),
    sort_by: str = Query("padj"),
    sort_dir: str = Query("asc", pattern="^(?i:asc|desc)$"),
):
    """
    Return a paginated expression-table slice for the explorer table.

    UI connection:
    this is the default Expression-tab endpoint because it trims the payload to
    the columns the table actually renders and paginates at the API boundary.
    """
    view = _safe_view_name("v", lab, study_id)
    genes = _clean_optional_terms(gene)
    proteins = _clean_optional_terms(protein)
    clauses, params = _expression_filters(genes, proteins, cell_type)
    where_sql = _where_sql(clauses)
    sort_sql = _validated_sort_sql(sort_by, sort_dir)

    params.extend([limit, offset])
    sql = f"""
        SELECT
          {SEMANTIC_GENE_EXPR} AS gene_symbol,
          human_gene, protein_id, organism,
          log2fc, pvalue, padj,
          abundance_a, abundance_b,
          pct_expressed_a, pct_expressed_b, expression_metric,
          sample_a, sample_b, condition_a, condition_b, cell_type,
          study_id,
          COUNT(*) OVER() AS total_rows
        FROM {view}
        {where_sql}
        {sort_sql}
        LIMIT ?
        OFFSET ?
    """
    return _query_arrow(request, sql, params)


@router.get("/datasets/{lab}/{study_id}/expression/volcano")
def expression_volcano(
    request: Request,
    lab: str,
    study_id: int,
    cell_type: str | None = None,
    limit: int = Query(DEFAULT_PLOT_LIMIT, ge=100, le=MAX_PLOT_LIMIT),
    offset: int = Query(0, ge=0),
):
    """
    Return the lean volcano payload for one dataset.

    UI connection:
    the Plot-tab volcano only needs significance columns plus lightweight
    labels, so this route avoids sending abundance / percentage columns.
    """
    view = _safe_view_name("v", lab, study_id)
    clauses, params = _expression_filters([], [], cell_type)
    clauses.append("log2fc IS NOT NULL")
    clauses.append("pvalue IS NOT NULL")
    where_sql = _where_sql(clauses)

    params.extend([limit, offset])
    sql = f"""
        SELECT
          {SEMANTIC_GENE_EXPR} AS gene_symbol,
          human_gene, protein_id, organism,
          log2fc, pvalue, padj,
          cell_type, condition_a, condition_b,
          study_id
        FROM {view}
        {where_sql}
        ORDER BY padj ASC NULLS LAST, ABS(log2fc) DESC NULLS LAST
        LIMIT ?
        OFFSET ?
    """
    return _query_arrow(request, sql, params)


@router.get("/datasets/{lab}/{study_id}/expression/summary")
def expression_summary(
    request: Request,
    lab: str,
    study_id: int,
    cell_type: str | None = None,
    padj: float = 0.05,
    lfc: float = 0.0,
):
    """
    Return one-row summary stats for the active dataset.

    UI connection:
    this supports quick badges, overview cards, and future histogram defaults
    without paying for a full row-level expression fetch first.
    """
    view = _safe_view_name("v", lab, study_id)
    clauses, params = _expression_filters([], [], cell_type)
    where_sql = _where_sql(clauses)
    params = [padj, lfc] + params

    sql = f"""
        SELECT
          COUNT(*) AS total_rows,
          COUNT(DISTINCT {SEMANTIC_GENE_EXPR}) AS unique_genes,
          COUNT(DISTINCT protein_id) AS unique_proteins,
          SUM(
            CASE
              WHEN padj IS NOT NULL AND padj < ? AND log2fc IS NOT NULL AND ABS(log2fc) >= ?
              THEN 1 ELSE 0
            END
          ) AS significant_rows,
          AVG(log2fc) AS mean_log2fc,
          MIN(log2fc) AS min_log2fc,
          MAX(log2fc) AS max_log2fc,
          AVG(abundance_a) AS mean_abundance_a,
          AVG(abundance_b) AS mean_abundance_b,
          AVG(padj) AS mean_padj
        FROM {view}
        {where_sql}
    """
    return _query_arrow(request, sql, params)


@router.get("/datasets/{lab}/{study_id}/expression/histogram")
def expression_histogram(
    request: Request,
    lab: str,
    study_id: int,
    metric: str = Query("log2fc"),
    bins: int = Query(30, ge=5, le=100),
    group_by: str | None = Query(None),
    cell_type: str | None = None,
):
    """
    Return server-side binned counts for histogram-style plots.

    UI connection:
    the histogram plot can work from these binned counts directly instead of
    materialising every row in R just to calculate the same buckets again.
    """
    view = _safe_view_name("v", lab, study_id)
    metric_expr = _validated_metric_expr(metric)
    group_expr = _validated_group_expr(group_by)
    clauses, params = _expression_filters([], [], cell_type)
    clauses.append(f"{metric_expr} IS NOT NULL")
    where_sql = _where_sql(clauses)

    params.extend([bins, bins])
    sql = f"""
        WITH filtered AS (
          SELECT
            CAST({metric_expr} AS DOUBLE) AS metric_value,
            {group_expr} AS group_value
          FROM {view}
          {where_sql}
        ),
        bounds AS (
          SELECT MIN(metric_value) AS min_value, MAX(metric_value) AS max_value
          FROM filtered
        ),
        binned AS (
          SELECT
            group_value,
            metric_value,
            CASE
              WHEN bounds.min_value IS NULL OR bounds.max_value IS NULL THEN 0
              WHEN bounds.min_value = bounds.max_value THEN 0
              ELSE LEAST(
                ? - 1,
                CAST(
                  FLOOR(
                    (metric_value - bounds.min_value) /
                    NULLIF((bounds.max_value - bounds.min_value) / ?, 0)
                  ) AS INTEGER
                )
              )
            END AS bin_idx,
            bounds.min_value,
            bounds.max_value
          FROM filtered
          CROSS JOIN bounds
        )
        SELECT
          group_value,
          bin_idx,
          MIN(metric_value) AS bin_start,
          MAX(metric_value) AS bin_end,
          COUNT(*) AS row_count,
          AVG(metric_value) AS mean_value
        FROM binned
        GROUP BY group_value, bin_idx
        ORDER BY group_value, bin_idx
    """
    return _query_arrow(request, sql, params)


@router.get("/datasets/{lab}/{study_id}/expression/groups")
def expression_groups(
    request: Request,
    lab: str,
    study_id: int,
    group_by: str = Query("cell_type"),
    metric: str = Query("abundance_a"),
    top_n: int = Query(30, ge=1, le=200),
    gene: list[str] | None = Query(None),
    protein: list[str] | None = Query(None),
    cell_type: str | None = None,
):
    """
    Return grouped feature summaries for dot- and top-expression-style plots.

    UI connection:
    this route computes group-wise means and percentages inside DuckDB so R only
    receives the compact feature × group table it actually needs to render.
    """
    view = _safe_view_name("v", lab, study_id)
    genes = _clean_optional_terms(gene)
    proteins = _clean_optional_terms(protein)
    clauses, params = _expression_filters(genes, proteins, cell_type)
    group_expr = _validated_group_expr(group_by)
    metric_expr = _validated_metric_expr(metric, group_metric=True)
    feature_expr = f"COALESCE(NULLIF({SEMANTIC_GENE_EXPR}, ''), NULLIF(protein_id, ''))"
    where_sql = _where_sql(clauses)
    if genes or proteins:
        feature_rank_sql = """
        feature_rank AS (
          SELECT DISTINCT feature_label
          FROM base
          WHERE feature_label IS NOT NULL
        )
        """
        params_with_top = params
    else:
        feature_rank_sql = """
        feature_rank AS (
          SELECT feature_label
          FROM base
          WHERE feature_label IS NOT NULL AND metric_value IS NOT NULL
          GROUP BY feature_label
          ORDER BY AVG(metric_value) DESC NULLS LAST
          LIMIT ?
        )
        """
        params_with_top = params + [top_n]

    sql = f"""
        WITH base AS (
          SELECT
            {SEMANTIC_GENE_EXPR} AS gene_symbol,
            human_gene,
            protein_id,
            {feature_expr} AS feature_label,
            {group_expr} AS group_value,
            CAST({metric_expr} AS DOUBLE) AS metric_value,
            CAST(COALESCE(pct_expressed_a, pct_expressed_b) AS DOUBLE) AS pct_value,
            padj
          FROM {view}
          {where_sql}
        ),
        {feature_rank_sql}
        SELECT
          base.feature_label,
          MIN(base.gene_symbol) AS gene_symbol,
          MIN(base.human_gene) AS human_gene,
          MIN(base.protein_id) AS protein_id,
          base.group_value,
          AVG(base.metric_value) AS mean_value,
          AVG(base.pct_value) AS mean_pct_expressed,
          MIN(base.padj) AS min_padj,
          COUNT(*) AS row_count
        FROM base
        INNER JOIN feature_rank
          ON base.feature_label = feature_rank.feature_label
        GROUP BY base.feature_label, base.group_value
        ORDER BY mean_value DESC NULLS LAST, base.feature_label, base.group_value
    """
    return _query_arrow(request, sql, params_with_top)


@router.get("/datasets/{lab}/{study_id}/expression/goi")
def expression_goi(
    request: Request,
    lab: str,
    study_id: int,
    gene: list[str] | None = Query(None),
    protein: list[str] | None = Query(None),
    cell_type: str | None = None,
    limit: int = Query(DEFAULT_TABLE_LIMIT, ge=1, le=MAX_TABLE_LIMIT),
    offset: int = Query(0, ge=0),
):
    """
    Return rows for explicitly selected genes/proteins of interest only.

    UI connection:
    feature-level scatter or inspection plots should use this route rather than
    loading the entire dataset and filtering down to a few selected terms in R.
    """
    view = _safe_view_name("v", lab, study_id)
    genes = _clean_optional_terms(gene)
    proteins = _clean_optional_terms(protein)
    if not genes and not proteins:
        raise HTTPException(status_code=400, detail="At least one gene or protein is required for the GOI endpoint.")

    clauses, params = _expression_filters(genes, proteins, cell_type)
    where_sql = _where_sql(clauses)
    params.extend([limit, offset])
    sql = f"""
        SELECT
          {SEMANTIC_GENE_EXPR} AS gene_symbol,
          human_gene, protein_id, organism,
          log2fc, pvalue, padj,
          abundance_a, abundance_b,
          pct_expressed_a, pct_expressed_b, expression_metric,
          sample_a, sample_b, condition_a, condition_b, cell_type,
          study_id
        FROM {view}
        {where_sql}
        ORDER BY gene_symbol, protein_id, padj ASC NULLS LAST
        LIMIT ?
        OFFSET ?
    """
    return _query_arrow(request, sql, params)


@router.get("/datasets/{lab}/{study_id}/embeddings")
def embeddings(
    request: Request,
    lab: str,
    study_id: int,
    reduction: str = Query("umap", pattern="^(?i:umap|pca|tsne)$"),
    assay: str = Query("expression", pattern="^(?i:expression|counts)$"),
    gene: list[str] | None = Query(None),
    protein: list[str] | None = Query(None),
    max_points: int = Query(DEFAULT_EMBEDDING_MAX_POINTS, ge=1000, le=MAX_EMBEDDING_POINTS),
):
    """
    Return embedding coordinates plus optional expression overlays for selected terms.

    UI connection:
    powers the single-cell Plot-tab embedding explorer, where checked search
    genes are rendered as separate expression-coloured panels or combined
    overlay traces on top of UMAP/PCA/tSNE coordinates.
    """
    genes = _clean_optional_terms(gene)
    proteins = _clean_optional_terms(protein)
    selected_terms = genes + [term for term in proteins if term not in genes]

    pool = _require_pool(request)
    with get_conn(pool) as con:
        ctx = _dataset_context(con, lab, study_id)
        if ctx["omic_type"] not in {"scrna", "snrna"}:
            raise HTTPException(
                status_code=400,
                detail=f"Embeddings are only available for sc/snRNA datasets, not {ctx['omic_type']!r}."
            )

        reduction_key = reduction.lower()
        reduction_table = ctx["table_map"].get(reduction_key)
        if not reduction_table:
            raise HTTPException(
                status_code=404,
                detail=f"No {reduction_key.upper()} embedding table is registered for {lab}:{study_id}."
            )

        emb_cols = _table_columns(con, ctx, reduction_table)
        obs_col = _first_existing(emb_cols, "obs", "cell_id", "Cell_ID", "barcode")
        if obs_col is None:
            raise HTTPException(status_code=500, detail="Embedding table is missing an observation identifier column.")

        dim_cols = [col for col in emb_cols if col != obs_col]
        if len(dim_cols) < 2:
            raise HTTPException(status_code=500, detail="Embedding table does not expose two coordinate columns.")
        dim_cols = dim_cols[:2]

        obs_meta_table = ctx["table_map"].get("obs_metadata") or ctx["table_map"].get("extra_metadata")
        meta_select = ["emb.obs"]
        meta_join = ""
        if obs_meta_table:
            meta_cols = _table_columns(con, ctx, obs_meta_table)
            meta_obs_col = _first_existing(meta_cols, "obs", "cell_id", "Cell_ID", "barcode", obs_col)
            if meta_obs_col:
                for canonical_name in ("cell_type", "cluster_id", "condition_a", "condition_b", "tissue", "sex", "age", "cell_id"):
                    original_name = ctx["name_map"].get(canonical_name)
                    if original_name and original_name in meta_cols:
                        meta_select.append(f"meta.{_quote_identifier(original_name)} AS {canonical_name}")
                meta_join = (
                    f"LEFT JOIN {_table_ref(ctx, obs_meta_table, 'meta')} "
                    f"ON emb.obs = CAST(meta.{_quote_identifier(meta_obs_col)} AS VARCHAR)"
                )

        emb_sql = f"""
            emb AS (
              SELECT
                CAST({_quote_identifier(obs_col)} AS VARCHAR) AS obs,
                CAST({_quote_identifier(dim_cols[0])} AS DOUBLE) AS dim_1,
                CAST({_quote_identifier(dim_cols[1])} AS DOUBLE) AS dim_2
              FROM {_table_ref(ctx, reduction_table)}
              LIMIT {int(max_points)}
            )
        """

        if not selected_terms:
            sql = f"""
                WITH {emb_sql}
                SELECT
                  {", ".join(meta_select)},
                  emb.dim_1,
                  emb.dim_2,
                  NULL::VARCHAR AS term,
                  NULL::DOUBLE AS expression_value
                FROM emb
                {meta_join}
                ORDER BY emb.obs
            """
            return _query_arrow(request, sql)

        assay_key = assay.lower()
        assay_table = ctx["table_map"].get(assay_key)
        if not assay_table:
            raise HTTPException(
                status_code=404,
                detail=f"No assay table registered for {assay_key!r} in {lab}:{study_id}."
            )

        feature_table = ctx["table_map"].get("feature_annotations") or ctx["table_map"].get("gene_annotations")
        if not feature_table:
            raise HTTPException(
                status_code=404,
                detail=f"No feature annotation table is registered for {lab}:{study_id}."
            )

        assay_cols = _table_columns(con, ctx, assay_table)
        feature_cols = _table_columns(con, ctx, feature_table)

        assay_obs_col = _first_existing(assay_cols, "obs", "cell_id", "Cell_ID", "barcode")
        assay_feature_col = _first_existing(assay_cols, "feature_id", "ID", "gene_id")
        assay_value_col = next((col for col in assay_cols if col not in {assay_obs_col, assay_feature_col}), None)
        feature_id_col = _first_existing(feature_cols, "feature_id", "ID", "gene_id")

        if not assay_obs_col or not assay_feature_col or not assay_value_col or not feature_id_col:
            raise HTTPException(
                status_code=500,
                detail="Assay or feature annotation parquet schema is missing obs/feature/value columns needed for embedding overlays."
            )

        gene_label_col = _first_existing(
            feature_cols,
            ctx["name_map"].get("gene_symbol"),
            ctx["name_map"].get("human_gene"),
            "feature_name",
            "gene_name",
            "gene_symbol",
            "Human_Gene",
            "Mouse_Gene",
            "symbol",
        )
        protein_label_col = _first_existing(
            feature_cols,
            ctx["name_map"].get("protein_id"),
            "protein_id",
            "Uniprot_id",
            "Uniprot_ID",
        )

        term_predicates = []
        params: list[str] = []

        if genes and gene_label_col:
            placeholders = ",".join(["?"] * len(genes))
            term_predicates.append(
                f"UPPER(CAST(ann.{_quote_identifier(gene_label_col)} AS VARCHAR)) IN ({placeholders})"
            )
            params.extend(term.upper() for term in genes)

        if proteins and protein_label_col:
            placeholders = ",".join(["?"] * len(proteins))
            term_predicates.append(
                f"UPPER(CAST(ann.{_quote_identifier(protein_label_col)} AS VARCHAR)) IN ({placeholders})"
            )
            params.extend(term.upper() for term in proteins)

        if not term_predicates:
            raise HTTPException(
                status_code=400,
                detail="Selected terms cannot be resolved against the available feature annotation columns."
            )

        term_expr_candidates = []
        if gene_label_col:
            term_expr_candidates.append(f"NULLIF(CAST(ann.{_quote_identifier(gene_label_col)} AS VARCHAR), '')")
        if protein_label_col:
            term_expr_candidates.append(f"NULLIF(CAST(ann.{_quote_identifier(protein_label_col)} AS VARCHAR), '')")
        term_expr = "COALESCE(" + ", ".join(term_expr_candidates) + ")"

        terms_sql, term_params = _terms_cte(selected_terms)
        sql = f"""
            WITH
            {emb_sql},
            {terms_sql},
            expr_overlay AS (
              SELECT
                CAST(expr.{_quote_identifier(assay_obs_col)} AS VARCHAR) AS obs,
                {term_expr} AS term,
                MAX(CAST(expr.{_quote_identifier(assay_value_col)} AS DOUBLE)) AS expression_value
              FROM {_table_ref(ctx, assay_table, 'expr')}
              LEFT JOIN {_table_ref(ctx, feature_table, 'ann')}
                ON CAST(expr.{_quote_identifier(assay_feature_col)} AS VARCHAR) =
                   CAST(ann.{_quote_identifier(feature_id_col)} AS VARCHAR)
              WHERE {" OR ".join(term_predicates)}
              GROUP BY 1, 2
            )
            SELECT
              {", ".join(meta_select)},
              emb.dim_1,
              emb.dim_2,
              terms.term,
              COALESCE(expr_overlay.expression_value, 0) AS expression_value
            FROM emb
            CROSS JOIN terms
            {meta_join}
            LEFT JOIN expr_overlay
              ON emb.obs = expr_overlay.obs
             AND UPPER(terms.term) = UPPER(expr_overlay.term)
            ORDER BY emb.obs, terms.term
        """
        return _query_arrow(request, sql, term_params + params)


@router.get("/datasets/{lab}/{study_id}/top-de")
def top_de(
    request: Request,
    lab: str,
    study_id: int,
    n: int = Query(50, ge=1, le=500),
    padj: float = 0.05,
    lfc: float = 0.0,
    cell_type: str | None = None,
    direction: str = Query("both", pattern="^(both|up|down)$"),
):
    """
    Return the top-N DE rows for one dataset after threshold filtering.

    UI connection:
    the Compare heatmap path uses this to avoid fetching an entire dataset when
    it only needs a ranked subset for plotting.
    """
    view = _safe_view_name("v", lab, study_id)
    direction_clause = {"up": "AND log2fc > 0", "down": "AND log2fc < 0"}.get(direction, "")
    params: list = [padj, lfc]

    cell_type_clause = ""
    if cell_type:
        cell_type_clause = "AND cell_type = ?"
        params.append(cell_type)

    params.append(n)
    sql = f"""
        SELECT
          {SEMANTIC_GENE_EXPR} AS gene_symbol, human_gene, log2fc, padj, pvalue,
          abundance_a, abundance_b,
          cell_type, condition_a, condition_b
        FROM {view}
        WHERE padj < ?
          AND (log2fc IS NULL OR ABS(log2fc) >= ?)
          {direction_clause}
          {cell_type_clause}
        ORDER BY ABS(log2fc) DESC NULLS LAST
        LIMIT ?
    """
    return _query_arrow(request, sql, params)


@router.get("/datasets/{lab}/{study_id}/metadata/options")
def metadata_options(request: Request, lab: str, study_id: int):
    """
    Return distinct metadata values for filter controls.

    This is intentionally much lighter than `/metadata`: the UI only needs
    unique cell types / tissues / conditions to populate selectors, not every
    metadata row.
    """
    pool = _require_pool(request)

    with get_conn(pool) as con:
        src_view = _resolve_metadata_source_view(con, lab, study_id)

        table = con.execute(f"""
            SELECT
              LIST(DISTINCT sample_a ORDER BY sample_a) AS sample_a,
              LIST(DISTINCT sample_b ORDER BY sample_b) AS sample_b,
              LIST(DISTINCT condition_a ORDER BY condition_a) AS condition_a,
              LIST(DISTINCT condition_b ORDER BY condition_b) AS condition_b,
              LIST(DISTINCT cell_type ORDER BY cell_type) AS cell_types,
              LIST(DISTINCT tissue ORDER BY tissue) AS tissues
            FROM {src_view}
        """).arrow()

    return _arrow_response(table)


@router.get("/datasets/{lab}/{study_id}/metadata")
def metadata(request: Request, lab: str, study_id: int):
    """
    Return the full metadata table for one dataset.

    This route is for cases where the UI or downstream analysis needs complete
    metadata rows, for example table downloads or richer joins, not just filter
    option lists.
    """
    pool = _require_pool(request)

    with get_conn(pool) as con:
        src_view = _resolve_metadata_source_view(con, lab, study_id)

        table = con.execute(f"SELECT * FROM {src_view}").arrow()

    return _arrow_response(table)


@router.get("/compare/expression")
def compare_expression(
    request: Request,
    gene: list[str] | None = Query(None),
    protein: list[str] | None = Query(None),
    dataset: list[str] = Query(...),
    padj: float = 0.05,
    lfc: float = 0.0,
    cell_type: str | None = None,
):
    """
    Return stacked expression/DE rows across multiple datasets.

    UI connection:
    the Compare tab uses this to bind selected datasets together into one table
    before faceting plots and comparison summaries.
    """
    genes = _clean_optional_terms(gene)
    proteins = _clean_optional_terms(protein)
    datasets = _clean_terms(dataset)
    # Comparison fans out against the same semantic DE view contract used by the
    # single-dataset endpoint. When no gene/protein terms are supplied the
    # caller gets the full threshold-filtered dataset rows for each selection.
    sql, params = _dataset_unions(datasets, genes, proteins, padj, lfc, cell_type)
    return _query_arrow(request, sql, params)
