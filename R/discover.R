# ---------------------------------------------------------------------------
# discover.R — ways of finding candidate article URLs.
#
# Every function returns a tibble with columns
#   url, title (may be NA), date (may be NA), via (how it was found)
# ---------------------------------------------------------------------------

empty_candidates <- function() tibble::tibble(url = character(), title = character(),
                                              date = as.Date(character()), via = character())

# Absolute, de-fragmented URL.
abs_url <- function(href, base) {
  u <- xml2::url_absolute(href, base)
  sub("#.*$", "", u)
}

# --- 1. Sitemaps -------------------------------------------------------------

# Does a child-sitemap URL look like it falls inside the study period?
# Recognises years/months in the URL (2017, 2017-03, 2017_03, 201703 ...).
# Sitemaps without a date in the name are always kept.
sitemap_in_period <- function(u, from = CONFIG$date_from, to = CONFIG$date_to) {
  m <- regmatches(u, regexec("(20[0-3][0-9])[-_/]?([01][0-9])?(?:[-_/.]|$)", u, perl = TRUE))[[1]]
  if (!length(m)) return(TRUE)
  y <- as.integer(m[2]); mo <- if (nzchar(m[3])) as.integer(m[3]) else NA
  if (!is.na(mo) && mo >= 1 && mo <= 12) {
    start <- as.Date(sprintf("%d-%02d-01", y, mo))
    end   <- seq(start, by = "month", length.out = 2)[2] - 1
  } else { start <- as.Date(sprintf("%d-01-01", y)); end <- as.Date(sprintf("%d-12-31", y)) }
  end >= from && start <= to
}

parse_sitemap <- function(txt) {
  doc <- tryCatch(xml2::read_xml(txt), error = function(e) NULL)
  if (is.null(doc)) return(list(children = character(), urls = empty_candidates()))
  xml2::xml_ns_strip(doc)
  children <- xml2::xml_text(xml2::xml_find_all(doc, "//sitemap/loc"))
  nodes <- xml2::xml_find_all(doc, "//url")
  get <- function(n, xp) { v <- xml2::xml_text(xml2::xml_find_first(n, xp)); ifelse(is.na(v), NA, trimws(v)) }
  urls <- if (length(nodes)) tibble::tibble(
    url   = vapply(nodes, get, "", xp = "./loc"),
    title = vapply(nodes, get, "", xp = ".//*[local-name()='title']"),
    date_raw = vapply(nodes, function(n) {
      d <- get(n, ".//*[local-name()='publication_date']")
      if (is.na(d)) get(n, "./lastmod") else d }, ""),
    via = "sitemap"
  ) else NULL
  if (!is.null(urls)) {
    urls$date <- as.Date(vapply(urls$date_raw, function(d) as.character(parse_pub_date(d)), ""))
    urls$date_raw <- NULL
  }
  list(children = trimws(children), urls = urls %||% empty_candidates())
}

# Walk a sitemap (index) recursively.
#   child_filter : regex a child sitemap URL must match (e.g. "news") — optional
#   url_filter   : regex an article URL must match (e.g. "/news/") — optional
#   date_is_lastmod : TRUE if the only date is <lastmod> (a *modified* date);
#                     then dates are only used to drop entries modified before
#                     the period starts, never to drop later ones.
discover_sitemap <- function(index_url, outlet, child_filter = NULL, url_filter = NULL,
                             language = outlet$language, date_is_lastmod = FALSE,
                             max_depth = 4) {
  seen <- character()
  out <- list()
  walk <- function(u, depth) {
    if (u %in% seen || depth > max_depth) return()
    seen <<- c(seen, u)
    message("  sitemap: ", u)
    txt <- fetch_text(u, outlet$id, "xml")
    if (is.null(txt)) return()
    # gzip sitemaps
    if (grepl("\\.gz$", u)) txt <- tryCatch(memDecompress(charToRaw(txt), "gzip", asChar = TRUE), error = function(e) txt)
    sm <- parse_sitemap(txt)
    kids <- sm$children
    if (!is.null(child_filter)) kids <- kids[grepl(child_filter, kids, perl = TRUE)]
    kids <- kids[vapply(kids, sitemap_in_period, logical(1))]
    for (k in kids) walk(k, depth + 1)
    if (nrow(sm$urls)) out[[length(out) + 1]] <<- sm$urls
  }
  walk(index_url, 0)
  res <- dplyr::bind_rows(empty_candidates(), out)
  if (!is.null(url_filter)) res <- res[grepl(url_filter, res$url, perl = TRUE), ]
  if (date_is_lastmod) {
    res <- res[is.na(res$date) | res$date >= CONFIG$date_from, ]
    res$date <- as.Date(NA)   # lastmod is not a publication date
  } else {
    res <- res[is.na(res$date) | (res$date >= CONFIG$date_from & res$date <= CONFIG$date_to), ]
  }
  res <- dplyr::distinct(res, url, .keep_all = TRUE)
  if (CONFIG$sitemap_prefilter == "title") {
    has_title <- !is.na(res$title) & nzchar(res$title)
    if (any(has_title)) {
      keep <- !has_title | title_is_candidate(res$title, language)
      # if the sitemap has titles for (nearly) everything, drop untitled rows too
      if (mean(has_title) > 0.9) keep <- has_title & keep
      res <- res[keep, ]
    }
  }
  res
}

# --- 2. WordPress REST API -----------------------------------------------------

# Many Thai news sites run on WordPress, which exposes
#   /wp-json/wp/v2/<type>?search=..&after=..&before=..&page=..
# Search is done year by year to keep result sets small.
discover_wp_api <- function(base, outlet, terms, types = "posts") {
  out <- list()
  years <- seq(as.integer(format(CONFIG$date_from, "%Y")), as.integer(format(CONFIG$date_to, "%Y")))
  for (type in types) for (term in terms) for (yr in years) {
    after  <- max(as.Date(sprintf("%d-01-01", yr)), CONFIG$date_from) - 1
    before <- min(as.Date(sprintf("%d-12-31", yr)), CONFIG$date_to) + 1
    if (after >= before) next
    for (page in seq_len(CONFIG$max_search_pages)) {
      u <- sprintf("%s/wp-json/wp/v2/%s?search=%s&after=%sT23:59:59&before=%sT00:00:00&per_page=100&page=%d&_fields=link,date,title&orderby=date&order=asc",
                   base, type, utils::URLencode(term, reserved = TRUE), after, before, page)
      j <- fetch_json(u, outlet$id)
      if (is.null(j) || !length(j) || !is.null(names(j))) break   # error objects are named lists
      out[[length(out) + 1]] <- tibble::tibble(
        url   = vapply(j, function(p) p$link %||% NA_character_, ""),
        title = vapply(j, function(p) html_to_text(p$title$rendered %||% "") %||% NA_character_, ""),
        date  = as.Date(substr(vapply(j, function(p) p$date %||% NA_character_, ""), 1, 10)),
        via   = paste0("wp_api:", term))
      message(sprintf("  wp-api %s '%s' %d p%d: %d hits", type, term, yr, page, length(j)))
      if (length(j) < 100) break
    }
  }
  dplyr::distinct(dplyr::bind_rows(empty_candidates(), out), url, .keep_all = TRUE)
}

# Quick probe: does the WP REST API answer for this post type?
wp_api_available <- function(base, outlet, type = "posts") {
  j <- fetch_json(sprintf("%s/wp-json/wp/v2/%s?per_page=1&_fields=link,date", base, type),
                  outlet$id, use_cache = FALSE)
  !is.null(j) && length(j) > 0 && is.null(names(j))
}

# --- 3. HTML search / listing pages -------------------------------------------

# Walk paginated HTML result pages.
#   url_template : sprintf template with %s = encoded term and %d = page
#   link_regex   : regex that article links must match
#   date_selector: optional CSS selector, relative to the link's result card,
#                  used to stop early once results are older than the period
discover_html_search <- function(url_template, outlet, terms, link_regex,
                                 js = FALSE, stop_when_empty = TRUE) {
  out <- list()
  for (term in terms) {
    prev <- character()
    for (page in seq_len(CONFIG$max_search_pages)) {
      u <- sprintf(url_template, utils::URLencode(term, reserved = TRUE), page)
      doc <- if (js) fetch_html_js(u, outlet$id) else fetch_html(u, outlet$id)
      if (is.null(doc)) break
      a <- xml2::xml_find_all(doc, "//a[@href]")
      links <- unique(abs_url(xml2::xml_attr(a, "href"), u))
      links <- links[grepl(link_regex, links, perl = TRUE)]
      new <- setdiff(links, prev)
      message(sprintf("  search '%s' p%d: %d links", term, page, length(new)))
      if (!length(new) && stop_when_empty) break
      titles <- vapply(new, function(l) {
        n <- a[abs_url(xml2::xml_attr(a, "href"), u) == l]
        t <- normalise_text(xml2::xml_text(n)); t <- t[nzchar(t)]
        if (length(t)) t[which.max(nchar(t))] else NA_character_ }, "")
      out[[length(out) + 1]] <- tibble::tibble(url = new, title = unname(titles),
                                               date = as.Date(NA), via = paste0("search:", term))
      prev <- c(prev, new)
    }
  }
  dplyr::distinct(dplyr::bind_rows(empty_candidates(), out), url, .keep_all = TRUE)
}
