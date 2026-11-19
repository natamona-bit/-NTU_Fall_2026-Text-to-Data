# ---------------------------------------------------------------------------
# gdelt_config.R — settings for collecting the same study sample from GDELT.
#
# The study scope (dates, outlets, keywords) is shared with the web scraper:
#   dates     -> CONFIG$date_from / CONFIG$date_to  in R/config.R
#   keywords  -> KW_TH / KW_EN                      in R/keywords.R
#   outlets   -> GDELT_OUTLETS below (same seven outlets as R/outlets.R)
# ---------------------------------------------------------------------------

GDELT_CONFIG <- list(
  # Google Cloud project that BigQuery bills the queries to. A free "BigQuery
  # sandbox" project works (no credit card; 1 TB of queries per month free).
  # Set it here or in the environment variable GDELT_BILLING_PROJECT.
  billing_project = Sys.getenv("GDELT_BILLING_PROJECT", unset = ""),

  # GDELT's public GKG 2.x table, one partition per day (UTC).
  gkg_table = "gdelt-bq.gdeltv2.gkg_partitioned",

  # Safety limit: a query whose dry-run estimate is above this is NOT run.
  # Row filters do not lower BigQuery cost; only the columns read and the
  # number of daily partitions do, so cost is roughly constant per year.
  max_gb_per_query = 1000,

  # Where GDELT results are kept (relative to the project root).
  dir = "data/gdelt",

  # DOC 2.0 API (only for the most recent ~3 months; see GDELT.md).
  doc_api_url    = "https://api.gdeltproject.org/api/v2/doc/doc",
  doc_delay_secs = 6,     # GDELT asks for at most one request every 5 seconds
  doc_maxrecords = 250    # API maximum per call
)

# The seven outlets of the study, by the domain GDELT records them under
# (GKG column SourceCommonName). `id` matches the names in R/outlets.R, so
# the optional full-text step can reuse that outlet's extraction settings.
GDELT_OUTLETS <- tibble::tribble(
  ~id,           ~outlet,        ~language, ~domain,
  "thairath",    "Thairath",     "th",      "thairath.co.th",
  "dailynews",   "Dailynews",    "th",      "dailynews.co.th",
  "matichon",    "Matichon",     "th",      "matichon.co.th",
  "khaosod",     "Khaosod",      "th",      "khaosod.co.th",
  "thaipbs",     "Thai PBS",     "th",      "thaipbs.or.th",
  "bangkokpost", "Bangkok Post", "en",      "bangkokpost.com",
  "nation",      "The Nation",   "en",      "nationthailand.com"
)

# GKG themes (column V2Themes) that signal the topic. Matched as substrings
# of theme names, so e.g. "REFUGEE" catches REFUGEES, TAX_FNCACT_REFUGEES ...
GDELT_THEMES <- list(
  myanmar   = c("ROHINGYA", "BURMESE", "MYANMAR", "KAREN"),
  migration = c("REFUGEE", "IMMIGRA", "MIGRA", "DISPLACED", "ASYLUM",
                "HUMAN_TRAFFICKING", "TRAFFICKING", "STATELESS", "DEPORT",
                "REPATRIAT", "ILLEGAL_ALIEN", "BORDER_CROSSING")
)

# GKG location country code (FIPS 10-4) for Myanmar.
GDELT_MYANMAR_FIPS <- "BM"
