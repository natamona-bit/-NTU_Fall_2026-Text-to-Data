# ---------------------------------------------------------------------------
# outlets.R — one entry per news outlet: how to find candidate articles and
# (optionally) CSS selectors that help extraction. The generic extractor in
# extract.R already understands JSON-LD and meta tags, so selectors are only
# hints and fall back gracefully if a site redesigns.
#
# Status as checked on 2026-11-09 (see README for details):
#   thairath   sitemap; site search is drawn by JavaScript (optional chromote)
#   dailynews  WordPress API (custom post types) -> ?s= search -> sitemaps
#   matichon   WordPress API -> ?s= search
#   khaosod    WordPress API -> ?s= search
#   thaipbs    monthly news sitemaps with titles (site search is robots-blocked)
#   bangkokpost search.bangkokpost.com; access is managed by TollBit (may block bots)
#   nation     monthly news sitemaps with titles (site search is robots-blocked)
# ---------------------------------------------------------------------------

# Try several discovery methods; each failure is reported, not fatal.
try_method <- function(label, expr) {
  message(" -> ", label)
  res <- tryCatch(expr, error = function(e) { message("    failed: ", conditionMessage(e)); NULL })
  if (is.null(res) || !nrow(res)) { message("    no candidates"); return(empty_candidates()) }
  message("    ", nrow(res), " candidates")
  res
}

months_in_period <- function() {
  seq(as.Date(format(CONFIG$date_from, "%Y-%m-01")), CONFIG$date_to, by = "month")
}

wordpress_discovery <- function(base, outlet, types = "posts", search_template, link_regex) {
  terms <- if (outlet$language == "en") SEARCH_TERMS_EN else SEARCH_TERMS_TH
  types_ok <- types[vapply(types, function(t) wp_api_available(base, outlet, t), logical(1))]
  if (length(types_ok)) {
    return(try_method(paste("WordPress API:", paste(types_ok, collapse = ", ")),
                      discover_wp_api(base, outlet, terms, types_ok)))
  }
  message("    WordPress API not available, using the site search pages")
  try_method("site search", discover_html_search(search_template, outlet, terms, link_regex))
}

OUTLETS <- list(

  thairath = list(
    id = "thairath", name = "Thairath", language = "th",
    selectors = list(body = c("[itemprop='articleBody']", ".article-content", "#article-content"),
                     author = c(".author-name", "[rel='author']")),
    discover = function(o) {
      sm <- try_method("sitemap", discover_sitemap(
        "https://www.thairath.co.th/sitemap.xml", o,
        url_filter = "thairath\\.co\\.th/(news|sport|entertain|lifestyle|money|scoop)/"))
      # JavaScript search only when chromote is installed.
      js <- if (requireNamespace("chromote", quietly = TRUE)) try_method("site search (headless Chrome)",
        discover_html_search("https://www.thairath.co.th/search?q=%s&page=%d", o, SEARCH_TERMS_TH,
                             "thairath\\.co\\.th/[a-z]+/[a-z-]+/\\d+$", js = TRUE)) else empty_candidates()
      dplyr::bind_rows(sm, js)
    }
  ),

  dailynews = list(
    id = "dailynews", name = "Dailynews", language = "th",
    selectors = list(body = c(".elementor-widget-theme-post-content", ".entry-content"),
                     author = c(".elementor-post-info__item--type-author", ".author-name")),
    discover = function(o) {
      res <- wordpress_discovery("https://www.dailynews.co.th", o, types = c("news", "articles", "posts"),
                                 search_template = "https://www.dailynews.co.th/page/%2$d/?s=%1$s",
                                 link_regex = "dailynews\\.co\\.th/(news|articles)/\\d+/?$")
      if (!nrow(res) && CONFIG$sitemap_prefilter == "none")
        res <- try_method("sitemaps (no titles: full crawl)", discover_sitemap(
          "https://www.dailynews.co.th/sitemap_index.xml", o, child_filter = "(news|article)-sitemap",
          url_filter = "/(news|articles)/\\d+", date_is_lastmod = TRUE))
      res
    }
  ),

  matichon = list(
    id = "matichon", name = "Matichon", language = "th",
    selectors = list(body = c(".td-post-content", ".entry-content"),
                     author = c(".td-post-author-name a", ".author-name")),
    discover = function(o) wordpress_discovery(
      "https://www.matichon.co.th", o,
      search_template = "https://www.matichon.co.th/page/%2$d?s=%1$s",
      link_regex = "matichon\\.co\\.th/.+/news_\\d+")
  ),

  khaosod = list(
    id = "khaosod", name = "Khaosod", language = "th",
    selectors = list(body = c(".td-post-content", ".entry-content", ".udsg__content"),
                     author = c(".td-post-author-name a", ".udsg__author", ".author-name")),
    discover = function(o) wordpress_discovery(
      "https://www.khaosod.co.th", o,
      search_template = "https://www.khaosod.co.th/page/%2$d?s=%1$s",
      link_regex = "khaosod\\.co\\.th/.+/news_\\d+")
  ),

  thaipbs = list(
    id = "thaipbs", name = "Thai PBS", language = "th",
    selectors = list(author = c(".author-name", "[class*='Author']")),
    discover = function(o) {
      idx <- try_method("monthly sitemaps", discover_sitemap(
        "https://www.thaipbs.or.th/sitemap/sitemap_news_monthly.xml", o,
        url_filter = "/news/content/\\d+"))
      # The monthly index stopped listing new months at 2024-12; the monthly
      # files themselves may still exist, so ask for 2025 months directly.
      later <- months_in_period()[months_in_period() > as.Date("2024-12-01")]
      extra <- lapply(format(later, "https://www.thaipbs.or.th/sitemap/sitemap-news/news-%Y-%m.xml"),
                      function(u) tryCatch(discover_sitemap(u, o, url_filter = "/news/content/\\d+"),
                                           error = function(e) empty_candidates()))
      dplyr::bind_rows(idx, extra)
    }
  ),

  bangkokpost = list(
    id = "bangkokpost", name = "Bangkok Post", language = "en",
    selectors = list(body = c(".article-content", ".articl-content", "#article-content"),
                     author = c(".article-info--col p a[href*='/author']", ".writer", ".author")),
    discover = function(o) {
      # Search sorts newest first; results outside the period are dropped later.
      try_method("search.bangkokpost.com", discover_html_search(
        "https://search.bangkokpost.com/search/result?q=%s&category=news&sort=newest&page=%d",
        o, SEARCH_TERMS_EN,
        "bangkokpost\\.com/(thailand|world|business|opinion|life|asia|investigation)/[^?]*\\d{5,}"))
    }
  ),

  nation = list(
    id = "nation", name = "The Nation", language = "en",
    selectors = list(author = c("a[href*='/bylines/']")),
    discover = function(o) try_method("monthly sitemaps", discover_sitemap(
      "https://api.nationthailand.com/sitemap", o,
      url_filter = "nationthailand\\.com/(?!category/)[a-z-]+/(?:[a-z-]+/)?\\d+$"))
  )
)
