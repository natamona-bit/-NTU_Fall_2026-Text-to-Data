# ---------------------------------------------------------------------------
# config.R — everything you are likely to want to change lives here.
# ---------------------------------------------------------------------------

CONFIG <- list(
  # Study period (inclusive). Articles outside it are dropped.
  date_from = as.Date("2017-01-01"),
  date_to   = as.Date("2026-01-01"),

  # Where cached pages and outputs go (relative to the project root).
  cache_dir  = "data/cache",
  output_dir = "data/output",

  # Politeness. One request every `delay_seconds` per website, retries with
  # back-off on 429 / 5xx, and robots.txt is checked before every request.
  delay_seconds = 2,
  max_retries   = 4,
  timeout_secs  = 40,
  respect_robots = TRUE,
  user_agent = paste(
    "Mozilla/5.0 (compatible; academic-research-scraper/1.0;",
    "Thai news on Myanmar migration 2017-2025; contact: set-your-email@example.org)"
  ),

  # How candidate article URLs are filtered *before* the full article is
  # downloaded, for outlets where URLs come from sitemaps:
  #   "title" – keep only sitemap entries whose title matches a keyword (fast,
  #             but misses articles that only mention the topic in the body)
  #   "none"  – download every article in the period and filter on full text
  #             (complete, but hundreds of thousands of requests per outlet)
  sitemap_prefilter = "title",

  # Stop after this many candidate articles per outlet (NULL = no limit).
  # Handy for a quick test run, e.g. 20.
  max_articles_per_outlet = NULL,

  # Max result pages to walk per keyword on a site search (each page ~10-20 hits).
  max_search_pages = 300
)
