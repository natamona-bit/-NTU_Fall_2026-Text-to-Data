# ---------------------------------------------------------------------------
# extract.R — turn an article page into one tidy row.
#
# Order of preference for every field:
#   1. JSON-LD (<script type="application/ld+json">, @type NewsArticle etc.)
#   2. Next.js page data (__NEXT_DATA__) for sites built on Next.js
#   3. <meta> tags (og:title, article:published_time, author ...)
#   4. outlet-specific CSS selectors (see R/outlets.R), then generic ones
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a)) ||
                             (is.character(a) && !any(nzchar(a)))) b else a

# --- JSON-LD -----------------------------------------------------------------

jsonld_objects <- function(doc) {
  nodes <- xml2::xml_find_all(doc, "//script[@type='application/ld+json']")
  out <- list()
  flatten <- function(x) {
    if (is.null(x)) return()
    if (!is.null(names(x))) {
      out[[length(out) + 1]] <<- x
      if (!is.null(x[["@graph"]])) for (g in x[["@graph"]]) flatten(g)
    } else for (y in x) if (is.list(y)) flatten(y)
  }
  for (n in nodes) {
    j <- tryCatch(jsonlite::fromJSON(xml2::xml_text(n), simplifyVector = FALSE),
                  error = function(e) NULL)
    flatten(j)
  }
  out
}

ARTICLE_TYPES <- c("NewsArticle", "Article", "ReportageNewsArticle", "AnalysisNewsArticle",
                   "BlogPosting", "OpinionNewsArticle", "WebPage")

pick_article_ld <- function(objs) {
  rank <- vapply(objs, function(o) {
    t <- unlist(o[["@type"]])
    m <- match(t, ARTICLE_TYPES); if (all(is.na(m))) 99L else min(m, na.rm = TRUE)
  }, integer(1))
  if (!length(objs) || min(rank) == 99L) return(NULL)
  objs[[which.min(rank)]]
}

ld_authors <- function(a) {
  if (is.null(a)) return(NULL)
  if (!is.null(names(a))) a <- list(a)
  nm <- vapply(a, function(x) if (is.list(x)) as.character(x[["name"]] %||% NA) else as.character(x),
               character(1))
  nm <- nm[!is.na(nm) & nzchar(nm)]
  if (length(nm)) nm else NULL
}

# --- Next.js -----------------------------------------------------------------

next_data <- function(doc) {
  n <- xml2::xml_find_first(doc, "//script[@id='__NEXT_DATA__']")
  if (is.na(n)) return(NULL)
  tryCatch(jsonlite::fromJSON(xml2::xml_text(n), simplifyVector = FALSE), error = function(e) NULL)
}

# Search a nested list for the first value stored under any of `keys`.
find_key <- function(x, keys, depth = 0) {
  if (depth > 12 || !is.list(x)) return(NULL)
  for (k in keys) if (!is.null(x[[k]]) && !is.list(x[[k]]) && nzchar(as.character(x[[k]])[1]))
    return(x[[k]])
  for (el in x) { r <- find_key(el, keys, depth + 1); if (!is.null(r)) return(r) }
  NULL
}

# --- helpers -----------------------------------------------------------------

meta <- function(doc, keys) {
  for (k in keys) {
    n <- xml2::xml_find_first(doc, sprintf("//meta[@property='%s' or @name='%s' or @itemprop='%s']", k, k, k))
    if (!is.na(n)) { v <- xml2::xml_attr(n, "content"); if (!is.na(v) && nzchar(trimws(v))) return(trimws(v)) }
  }
  NULL
}

html_to_text <- function(x) {
  if (is.null(x) || !nzchar(x)) return(x)
  if (!grepl("<[a-zA-Z/]", x)) return(x)
  d <- xml2::read_html(paste0("<div>", x, "</div>"), encoding = "UTF-8")
  paragraphs_text(xml2::xml_find_first(d, "//div"))
}

# Text of a body node, one paragraph per line, without scripts, ads, share
# buttons, "related news" boxes and the like.
JUNK_XPATH <- paste(
  "//script", "//style", "//noscript", "//iframe", "//figure", "//figcaption",
  "//aside", "//nav", "//form", "//button",
  paste0("//*[contains(@class,'related') or contains(@class,'share') or contains(@class,'social')",
         " or contains(@class,'advert') or contains(@class,'ads-') or contains(@class,'banner')",
         " or contains(@class,'tag-list') or contains(@class,'readmore')]"),
  sep = " | ")

paragraphs_text <- function(node) {
  if (is.null(node) || inherits(node, "xml_missing")) return(NULL)
  node <- xml2::read_html(as.character(node), encoding = "UTF-8")   # copy, so removing junk is safe
  xml2::xml_remove(xml2::xml_find_all(node, JUNK_XPATH))
  ps <- xml2::xml_find_all(node, "//p | //h2 | //h3 | //li | //blockquote")
  txt <- if (length(ps) >= 2) xml2::xml_text(ps) else
    strsplit(xml2::xml_text(xml2::xml_find_first(node, "//body")), "\n")[[1]]
  txt <- normalise_text(txt)
  txt <- txt[nzchar(txt)]
  paste(unique(txt), collapse = "\n")
}

first_node <- function(doc, selectors) {
  for (s in selectors) {
    n <- tryCatch(rvest::html_element(doc, s), error = function(e) NULL)
    if (!is.null(n) && !inherits(n, "xml_missing") && length(n) && !is.na(n)) return(n)
  }
  NULL
}

THAI_MONTHS <- c("ม.ค.", "ก.พ.", "มี.ค.", "เม.ย.", "พ.ค.", "มิ.ย.", "ก.ค.", "ส.ค.", "ก.ย.", "ต.ค.", "พ.ย.", "ธ.ค.")
THAI_MONTHS_FULL <- c("มกราคม", "กุมภาพันธ์", "มีนาคม", "เมษายน", "พฤษภาคม", "มิถุนายน",
                      "กรกฎาคม", "สิงหาคม", "กันยายน", "ตุลาคม", "พฤศจิกายน", "ธันวาคม")

# Parse ISO dates, and Thai dates such as "24 มิ.ย. 2564" or "5 มกราคม 2560"
# (Buddhist Era years are converted: BE - 543 = CE). Returns a Date in
# Bangkok time.
parse_pub_date <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(as.Date(NA))
  x <- trimws(as.character(x))
  # Thai digits -> Arabic
  x <- chartr("๐๑๒๓๔๕๖๗๘๙", "0123456789", x)
  if (grepl("^\\d{4}-\\d{2}-\\d{2}", x)) {
    dt <- suppressWarnings(lubridate::ymd_hms(x, quiet = TRUE))
    if (!is.na(dt)) return(as.Date(format(dt, "%Y-%m-%d", tz = "Asia/Bangkok")))
    return(as.Date(substr(x, 1, 10)))
  }
  if (grepl("^\\d{10,13}$", x)) {   # unix epoch (s or ms)
    s <- as.numeric(x); if (s > 1e11) s <- s / 1000
    return(as.Date(as.POSIXct(s, origin = "1970-01-01", tz = "Asia/Bangkok"), tz = "Asia/Bangkok"))
  }
  for (i in 12:1) {
    for (mn in c(THAI_MONTHS_FULL[i], THAI_MONTHS[i])) {
      m <- regmatches(x, regexec(paste0("(\\d{1,2})\\s*", gsub(".", "\\.", mn, fixed = TRUE), "\\s*(\\d{2,4})"), x))[[1]]
      if (length(m) == 3) {
        y <- as.integer(m[3]); if (y < 100) y <- y + 2500
        if (y > 2400) y <- y - 543
        return(as.Date(sprintf("%04d-%02d-%02d", y, i, as.integer(m[2]))))
      }
    }
  }
  d <- suppressWarnings(lubridate::parse_date_time(x, c("dmy", "mdy", "ymd", "dmy HM", "B d, Y", "d B Y"), quiet = TRUE))
  if (!is.na(d)) as.Date(d) else as.Date(NA)
}

clean_author <- function(a, outlet_name) {
  if (is.null(a)) return(NA_character_)
  a <- normalise_text(unlist(a))
  a <- a[nzchar(a) & !grepl("^https?://", a)]
  a <- sub("^(by|โดย|ผู้เขียน)\\s*:?\\s*", "", a, ignore.case = TRUE)
  a <- unique(a[nzchar(a)])
  if (!length(a)) return(NA_character_)
  paste(a, collapse = "; ")
}

# --- main extractor ------------------------------------------------------------

extract_article <- function(doc, url, outlet) {
  ld <- pick_article_ld(jsonld_objects(doc))
  nd <- next_data(doc)
  sel <- outlet$selectors %||% list()

  headline <- (ld$headline %||% ld$name) %||%
    find_key(nd$props$pageProps, c("title", "headline")) %||%
    meta(doc, c("og:title", "twitter:title")) %||%
    { n <- first_node(doc, c(sel$headline, "h1")); if (!is.null(n)) rvest::html_text2(n) }
  headline <- normalise_text(sub("\\s+\\|\\s+[^|]+$", "", headline %||% "", perl = TRUE))

  date_raw <- ld$datePublished %||%
    meta(doc, c("article:published_time", "og:article:published_time", "datePublished",
                "publishdate", "pubdate", "date")) %||%
    find_key(nd$props$pageProps, c("publishTime", "published_at", "publishedAt", "publish_date",
                                   "datePublished", "createdAt", "created_at")) %||%
    { n <- first_node(doc, c(sel$date, "time[datetime]"))
      if (!is.null(n)) (rvest::html_attr(n, "datetime") %||% rvest::html_text2(n)) }
  pub_date <- parse_pub_date(date_raw)

  authors <- ld_authors(ld$author) %||%
    { n <- first_node(doc, sel$author %||% character()); if (!is.null(n)) rvest::html_text2(n) } %||%
    meta(doc, c("author", "article:author", "byl")) %||%
    find_key(nd$props$pageProps, c("byline", "author", "writer", "authorName"))
  authors <- clean_author(authors, outlet$name)

  body <- NULL
  n <- first_node(doc, sel$body %||% character())
  if (!is.null(n)) body <- paragraphs_text(n)
  if (is.null(body) || nchar(body) < 200) {
    ld_body <- html_to_text(ld$articleBody %||% "")
    if (!is.null(ld_body) && nchar(ld_body) > nchar(body %||% "")) body <- ld_body
  }
  if (is.null(body) || nchar(body) < 200) {
    nd_body <- html_to_text(find_key(nd$props$pageProps, c("content", "body", "articleBody", "detail")) %||% "")
    if (!is.null(nd_body) && nchar(nd_body) > nchar(body %||% "")) body <- nd_body
  }
  if (is.null(body) || nchar(body) < 200) {
    n <- first_node(doc, c("article [itemprop='articleBody']", "[itemprop='articleBody']",
                            ".entry-content", ".article-content", ".article-body",
                            ".content-detail", ".news-content", "article", "main"))
    gb <- paragraphs_text(n)
    if (!is.null(gb) && nchar(gb) > nchar(body %||% "")) body <- gb
  }

  o_name <- outlet$name; o_lang <- outlet$language
  tibble::tibble(
    outlet         = o_name,
    language       = o_lang,
    headline       = headline %||% NA_character_,
    authors        = authors,
    published_date = pub_date,
    content        = body %||% NA_character_,
    url            = url,
    section        = meta(doc, "article:section") %||% NA_character_,
    tags           = paste(unique(c(unlist(ld$keywords), meta(doc, c("news_keywords", "keywords")))), collapse = "; "),
    scraped_at     = Sys.time()
  )
}
