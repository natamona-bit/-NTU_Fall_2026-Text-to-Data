# ---------------------------------------------------------------------------
# gdelt_bigquery.R — query GDELT's Global Knowledge Graph (GKG) on BigQuery.
#
# One query per calendar year. Each year is checked with a free dry run first
# (estimated bytes -> cost), refused if above GDELT_CONFIG$max_gb_per_query,
# and the result is cached to data/gdelt/raw/gkg_<year>.rds so a year is only
# ever paid for once.
# ---------------------------------------------------------------------------

gdelt_path <- function(...) {
  p <- file.path(GDELT_CONFIG$dir, ...)
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  p
}

# Regex on LOWER(SourceCommonName) that matches the outlets and their subdomains.
gdelt_domain_regex <- function(domains = GDELT_OUTLETS$domain) {
  paste0("(^|\\.)(", paste(gsub(".", "\\.", domains, fixed = TRUE), collapse = "|"), ")$")
}

# Regex for the page title, built from the same keyword lists as the web
# scraper's title prefilter (Myanmar terms, specific terms, migrant words).
gdelt_title_regex <- function() {
  esc <- function(x) gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", x)
  th <- unique(c(KW_TH$myanmar, KW_TH$specific,
                 "ผู้ลี้ภัย", "ผู้อพยพ", "ผู้หนีภัย", "แรงงานต่างด้าว", "แรงงานข้ามชาติ"))
  en <- unique(c(KW_EN$myanmar, KW_EN$specific, "refugee", "migrant", "asylum", "stateless"))
  # word boundary at the start, and at the end for short words ("Mon" not "Monday"),
  # as in R/keywords.R
  en <- paste0("\\b", esc(en), ifelse(nchar(en) <= 4, "\\b", ""))
  paste0("(?i)(", paste(c(esc(th), en), collapse = "|"), ")")
}

# BigQuery string literal for a regex (raw string, so backslashes stay as is).
.bq_raw <- function(x) {
  stopifnot(!grepl("'", x, fixed = TRUE))
  paste0("r'", x, "'")
}

.partition_clause <- function(from, to_exclusive) {
  sprintf("_PARTITIONTIME >= TIMESTAMP('%s') AND _PARTITIONTIME < TIMESTAMP('%s')",
          format(from), format(to_exclusive))
}

# Main query: articles from the seven outlets that have a Myanmar location,
# a Myanmar-related theme (e.g. Rohingya), or a study keyword in the title.
# Migration themes alone are not enough to be pulled (that would bring in all
# refugee news worldwide); they are used later to flag relevance. Only the columns
# needed are read, because BigQuery bills by the columns it reads.
gdelt_gkg_sql <- function(from, to_exclusive) {
  my_themes <- paste(GDELT_THEMES$myanmar, collapse = "|")
  sprintf(
"SELECT
  GKGRECORDID AS gkg_record_id,
  CAST(DATE AS STRING) AS gdelt_date,
  LOWER(SourceCommonName) AS domain,
  DocumentIdentifier AS url,
  TranslationInfo AS translation_info,
  V2Themes AS themes_raw,
  V2Locations AS locations_raw,
  V2Tone AS tone_raw,
  REGEXP_EXTRACT(Extras, r'<PAGE_TITLE>(.*?)</PAGE_TITLE>') AS page_title,
  REGEXP_EXTRACT(Extras, r'<PAGE_AUTHORS>(.*?)</PAGE_AUTHORS>') AS page_authors,
  REGEXP_EXTRACT(Extras, r'<PAGE_PRECISEPUBTIMESTAMP>(.*?)</PAGE_PRECISEPUBTIMESTAMP>') AS page_pub_timestamp
FROM `%s`
WHERE %s
  AND REGEXP_CONTAINS(LOWER(SourceCommonName), %s)
  AND (
    REGEXP_CONTAINS(IFNULL(V2Locations, ''), r'#%s#')
    OR REGEXP_CONTAINS(IFNULL(V2Themes, ''), r'(%s)')
    OR REGEXP_CONTAINS(IFNULL(REGEXP_EXTRACT(Extras, r'<PAGE_TITLE>(.*?)</PAGE_TITLE>'), ''), %s)
  )",
    GDELT_CONFIG$gkg_table, .partition_clause(from, to_exclusive),
    .bq_raw(gdelt_domain_regex()), GDELT_MYANMAR_FIPS, my_themes,
    .bq_raw(gdelt_title_regex()))
}

# Coverage query: how many GKG records GDELT holds for each outlet and year.
# Reads only SourceCommonName, so it is cheap. Run this first.
gdelt_coverage_sql <- function(from = CONFIG$date_from, to_exclusive = CONFIG$date_to + 1) {
  sprintf(
"SELECT LOWER(SourceCommonName) AS domain,
       EXTRACT(YEAR FROM _PARTITIONTIME) AS year,
       COUNT(*) AS gkg_records
FROM `%s`
WHERE %s
  AND REGEXP_CONTAINS(LOWER(SourceCommonName), %s)
GROUP BY domain, year
ORDER BY domain, year",
    GDELT_CONFIG$gkg_table, .partition_clause(from, to_exclusive), .bq_raw(gdelt_domain_regex()))
}

# Year chunks covering the study period: list of (label, from, to_exclusive).
gdelt_year_chunks <- function(from = CONFIG$date_from, to = CONFIG$date_to, years = NULL) {
  ys <- as.integer(format(from, "%Y")):as.integer(format(to, "%Y"))
  if (!is.null(years)) ys <- intersect(ys, years)
  lapply(ys, function(y) {
    f <- max(from, as.Date(sprintf("%d-01-01", y)))
    # +1 day at the end: partitions are UTC days and Bangkok is UTC+7.
    t <- min(to + 1, as.Date(sprintf("%d-01-01", y + 1)))
    list(label = as.character(y), from = f, to_exclusive = t)
  })
}

# --- BigQuery calls (wrapped so tests can replace them) ------------------------

gdelt_billing <- function() {
  b <- GDELT_CONFIG$billing_project
  if (!nzchar(b)) stop("Set your Google Cloud project id: GDELT_CONFIG$billing_project, ",
                       "or the environment variable GDELT_BILLING_PROJECT (see GDELT.md).", call. = FALSE)
  if (!requireNamespace("bigrquery", quietly = TRUE))
    stop("install.packages('bigrquery') first.", call. = FALSE)
  b
}

gdelt_bq_dry_run <- function(sql) {
  as.numeric(bigrquery::bq_perform_query_dry_run(sql, billing = gdelt_billing()))
}

gdelt_bq_fetch <- function(sql) {
  tb <- bigrquery::bq_project_query(gdelt_billing(), sql)
  tibble::as_tibble(bigrquery::bq_table_download(tb, bigint = "character"))
}

fmt_gb <- function(bytes) sprintf("%.1f GB", bytes / 1e9)

# Estimate (free) what each yearly query would read.
gdelt_estimate <- function(years = NULL) {
  chunks <- gdelt_year_chunks(years = years)
  est <- vapply(chunks, function(ch) gdelt_bq_dry_run(gdelt_gkg_sql(ch$from, ch$to_exclusive)), numeric(1))
  res <- tibble::tibble(year = vapply(chunks, `[[`, "", "label"), gb = round(est / 1e9, 1),
                        cached = file.exists(gdelt_path("raw", paste0("gkg_", year, ".rds"))))
  message(sprintf("Total: %s, of which not yet downloaded: %s (BigQuery free tier: 1000 GB / month)",
                  fmt_gb(sum(est)), fmt_gb(sum(est[!res$cached]))))
  res
}

gdelt_coverage <- function() {
  sql <- gdelt_coverage_sql()
  message("Coverage query will read ", fmt_gb(gdelt_bq_dry_run(sql)))
  cov <- gdelt_bq_fetch(sql)
  cov$year <- as.integer(cov$year); cov$gkg_records <- as.numeric(cov$gkg_records)
  cov$outlet <- gdelt_match_outlet(cov$domain)$outlet
  out <- dplyr::summarise(cov, gkg_records = sum(gkg_records), .by = c(outlet, year)) |>
    dplyr::arrange(year) |>
    tidyr::pivot_wider(names_from = year, values_from = gkg_records, values_fill = 0)
  readr::write_excel_csv(out, gdelt_path("output", "gdelt_coverage_by_outlet_year.csv"))
  out
}

# Run the yearly queries (cached). Returns the combined raw GKG rows.
gdelt_fetch_gkg <- function(years = NULL, refresh = FALSE) {
  chunks <- gdelt_year_chunks(years = years)
  parts <- lapply(chunks, function(ch) {
    f <- gdelt_path("raw", paste0("gkg_", ch$label, ".rds"))
    if (!refresh && file.exists(f)) { message("== ", ch$label, ": cached"); return(readRDS(f)) }
    sql <- gdelt_gkg_sql(ch$from, ch$to_exclusive)
    bytes <- gdelt_bq_dry_run(sql)
    message("== ", ch$label, ": query reads ", fmt_gb(bytes))
    if (bytes / 1e9 > GDELT_CONFIG$max_gb_per_query) {
      message("   skipped: above GDELT_CONFIG$max_gb_per_query (", GDELT_CONFIG$max_gb_per_query, " GB)")
      return(NULL)
    }
    rows <- gdelt_bq_fetch(sql)
    message("   ", nrow(rows), " GKG records")
    saveRDS(rows, f)
    rows
  })
  dplyr::bind_rows(parts)
}
