# ===========================================================================
# thairath_scraper.R — a standalone scraper for Thairath (www.thairath.co.th)
# news on Myanmar migrants / refugees.
#
# Self-contained: it does not need anything from the R/ folder. Run it with
# run_thairath.R (see README.md in this folder).
#
# How articles are found (checked against the live site on 2026-11-19):
#   1. "sitemap" — Thairath publishes one sitemap per month
#      (sitemap-monthly-YYYY-MM.xml) from 2017-11 onwards. Each month lists
#      only ~80 articles with their headline, so this route is fast but sees a
#      small share of what Thairath published. There is no sitemap for
#      2017-01 to 2017-10.
#   2. "tags" — topic pages such as /tags/แรงงานต่างด้าว?page=2 are delivered
#      as plain HTML with article links and Thai dates. The scraper walks
#      ?page=1, 2, 3 ... for every tag until pages are older than the study
#      period or stop giving new links. Usually the richest route.
#   3. "search" — the site search (/search?q=...) is drawn by JavaScript, so it
#      needs the `chromote` package and Google Chrome. Optional.
#
# Every candidate article is then downloaded once (pages are cached on disk),
# parsed into one row, and scored against the keyword lists below.
# ===========================================================================

# ---------------------------------------------------------------------------
# SETTINGS — change these.
# ---------------------------------------------------------------------------
TR <- list(
  date_from = as.Date("2017-01-01"),
  date_to   = as.Date("2026-01-01"),

  # Which discovery routes to use (any of "sitemap", "tags", "search").
  methods = c("sitemap", "tags", "search"),

  # Tag pages to walk (Thairath tag names, in Thai).
  tags = c("แรงงานต่างด้าว", "แรงงานข้ามชาติ", "แรงงานพม่า", "แรงงานเมียนมา",
           "ผู้ลี้ภัย", "ผู้อพยพ", "ผู้หนีภัยการสู้รบ", "โรฮิงญา", "เมียนมา", "พม่า",
           "ค้ามนุษย์", "ศูนย์พักพิงชั่วคราว"),

  # Words typed into the site search (only used with chromote installed).
  search_terms = c("ผู้ลี้ภัย", "ผู้ลี้ภัยทางการเมือง", "ผู้อพยพชาวพม่า", "พม่า", "เมียนมา",
                   "โรฮิงญา", "แรงงานพม่า", "แรงงานเมียนมา", "ผู้หนีภัยการสู้รบ",
                   "แรงงานข้ามชาติ"),

  # Sitemap entries are kept only if their headline contains a keyword.
  # Set to FALSE to download every sitemap article (~80 per month).
  sitemap_title_filter = TRUE,

  max_tag_pages      = 400,   # per tag
  max_search_rounds  = 60,    # "load more" clicks per search term
  max_articles       = NULL,  # cap on articles downloaded (NULL = all); --test sets 20

  delay_seconds = 2,          # pause between requests to thairath.co.th
  max_retries   = 4,
  timeout_secs  = 40,
  user_agent = paste("Mozilla/5.0 (compatible; academic-research-scraper/1.0;",
                     "Thai news on Myanmar migration 2017-2025)"),

  # Folders (relative to the repository root).
  cache_dir  = "thairath/cache",
  output_dir = "thairath/output"
)

BASE <- "https://www.thairath.co.th"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a)) ||
                             (is.character(a) && !any(nzchar(a)))) b else a

# ---------------------------------------------------------------------------
# KEYWORDS — Kelvin's list plus common Thai terms.
# Thai has no spaces between words, so matching is plain substring matching.
#   kw_myanmar      mentions Myanmar / Burmese / Rohingya ...
#   kw_migration    mentions migrants, refugees, foreign workers, displacement ...
#   kw_specific     a term that is on-topic by itself (e.g. แรงงานเมียนมา)
#   relevant_strict kw_specific, OR (kw_myanmar AND kw_migration)
# ---------------------------------------------------------------------------
KW <- list(
  myanmar = c("พม่า", "เมียนมา", "เมียนมาร์", "ชาวพม่า", "ชาวเมียนมา", "โรฮิงญา", "โรฮีนจา",
              "กะเหรี่ยง", "ไทใหญ่", "ฉาน", "มอญ"),
  migration = c("ผู้ลี้ภัย", "ผู้ลี้ภัยทางการเมือง", "ผู้อพยพ", "อพยพ", "ผู้หนีภัย",
                "ผู้หนีภัยการสู้รบ", "หนีภัยสงคราม", "ค่ายผู้ลี้ภัย", "ศูนย์พักพิงชั่วคราว",
                "พื้นที่พักพิง", "แรงงานต่างด้าว", "แรงงานข้ามชาติ", "แรงงานข้ามแดน",
                "แรงงานเถื่อน", "หลบหนีเข้าเมือง", "ลักลอบเข้าเมือง", "เข้าเมืองผิดกฎหมาย",
                "ข้ามแดน", "ค้ามนุษย์", "ไร้สัญชาติ", "ไร้รัฐ", "บัตรสีชมพู", "ขอลี้ภัย",
                "ส่งกลับ", "ผลักดันกลับ", "ผู้พลัดถิ่น", "พลัดถิ่น", "MOU"),
  specific = c("ผู้อพยพชาวพม่า", "ผู้อพยพชาวเมียนมา", "ผู้ลี้ภัยชาวพม่า", "ผู้ลี้ภัยชาวเมียนมา",
               "ผู้ลี้ภัยเมียนมา", "ผู้ลี้ภัยพม่า", "แรงงานพม่า", "แรงงานเมียนมา",
               "แรงงานชาวพม่า", "แรงงานชาวเมียนมา", "ชาวโรฮิงญา", "โรฮิงญา", "โรฮีนจา",
               "ผู้หนีภัยการสู้รบ", "ผู้หนีภัยจากเมียนมา", "ผู้หนีภัยจากพม่า",
               "ค่ายแม่หละ", "ศูนย์พักพิงชั่วคราวบ้านแม่หละ", "ผู้ลี้ภัยกะเหรี่ยง")
)

normalise_text <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- stringi::stri_trans_nfc(x)
  x <- gsub("[​‌‍﻿­]", "", x, perl = TRUE)   # zero-width chars
  x <- gsub(" ", " ", x, fixed = TRUE)
  x <- gsub("[ \t]+", " ", x, perl = TRUE)
  trimws(x)
}

match_terms <- function(text, terms) terms[stringi::stri_detect_fixed(text, terms)]

score_article <- function(headline, content) {
  txt <- normalise_text(paste(headline, content))
  my <- match_terms(txt, KW$myanmar)
  mg <- match_terms(txt, KW$migration)
  sp <- match_terms(txt, KW$specific)
  all <- unique(c(sp, my, mg))
  tibble::tibble(
    matched_keywords = paste(all, collapse = "; "),
    n_keywords       = length(all),
    kw_myanmar       = length(my) > 0 || length(sp) > 0,
    kw_migration     = length(mg) > 0 || length(sp) > 0,
    kw_specific      = length(sp) > 0,
    relevant_strict  = length(sp) > 0 || (length(my) > 0 && length(mg) > 0)
  )
}

# Loose test used on headlines before downloading (sitemap route).
title_is_candidate <- function(title) {
  terms <- unique(c(KW$myanmar, KW$specific,
                    "ผู้ลี้ภัย", "ผู้อพยพ", "ผู้หนีภัย", "แรงงานต่างด้าว", "แรงงานข้ามชาติ"))
  t <- normalise_text(title)
  vapply(t, function(x) any(stringi::stri_detect_fixed(x, terms)), logical(1), USE.NAMES = FALSE)
}

# ---------------------------------------------------------------------------
# DATES — ISO strings, unix time, and Thai dates such as "15 พ.ย. 2569 13:42 น."
# (Buddhist Era years: BE - 543 = CE).
# ---------------------------------------------------------------------------
THAI_MONTHS <- c("ม.ค.", "ก.พ.", "มี.ค.", "เม.ย.", "พ.ค.", "มิ.ย.", "ก.ค.", "ส.ค.", "ก.ย.", "ต.ค.", "พ.ย.", "ธ.ค.")
THAI_MONTHS_FULL <- c("มกราคม", "กุมภาพันธ์", "มีนาคม", "เมษายน", "พฤษภาคม", "มิถุนายน",
                      "กรกฎาคม", "สิงหาคม", "กันยายน", "ตุลาคม", "พฤศจิกายน", "ธันวาคม")
THAI_MONTH_RX <- paste0("(\\d{1,2})\\s*(",
                        paste(gsub(".", "\\.", c(THAI_MONTHS_FULL, THAI_MONTHS), fixed = TRUE), collapse = "|"),
                        ")\\s*(\\d{2,4})")

thai_month_number <- function(m) {
  i <- match(m, THAI_MONTHS_FULL); ifelse(is.na(i), match(m, THAI_MONTHS), i)
}

be_to_ce <- function(y) { y <- as.integer(y); y <- ifelse(y < 100, y + 2500, y); ifelse(y > 2400, y - 543, y) }

parse_pub_date <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(x[1])) return(as.Date(NA))
  x <- trimws(chartr("๐๑๒๓๔๕๖๗๘๙", "0123456789", as.character(x[1])))
  if (grepl("^\\d{4}-\\d{2}-\\d{2}", x)) {
    dt <- suppressWarnings(lubridate::ymd_hms(x, quiet = TRUE))
    if (!is.na(dt)) return(as.Date(format(dt, "%Y-%m-%d", tz = "Asia/Bangkok")))
    return(as.Date(substr(x, 1, 10)))
  }
  if (grepl("^\\d{10,13}$", x)) {
    s <- as.numeric(x); if (s > 1e11) s <- s / 1000
    return(as.Date(as.POSIXct(s, origin = "1970-01-01", tz = "Asia/Bangkok"), tz = "Asia/Bangkok"))
  }
  m <- regmatches(x, regexec(THAI_MONTH_RX, x, perl = TRUE))[[1]]
  if (length(m) == 4) {
    return(as.Date(sprintf("%04d-%02d-%02d", be_to_ce(m[4]), thai_month_number(m[3]), as.integer(m[2]))))
  }
  as.Date(NA)
}

# All Thai dates written anywhere in a piece of text.
find_thai_dates <- function(txt) {
  txt <- chartr("๐๑๒๓๔๕๖๗๘๙", "0123456789", txt)
  m <- stringr::str_match_all(txt, THAI_MONTH_RX)[[1]]
  if (!nrow(m)) return(as.Date(character()))
  d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", be_to_ce(m[, 4]),
                                        thai_month_number(m[, 3]), as.integer(m[, 2]))))
  d[!is.na(d)]
}

# ---------------------------------------------------------------------------
# HTTP — polite fetching with a disk cache (so a re-run resumes, and nothing
# is downloaded twice).
# ---------------------------------------------------------------------------
cache_path <- function(url, kind, ext) {
  dir <- file.path(TR$cache_dir, kind)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.path(dir, paste0(digest::digest(url, algo = "md5"), ".", ext))
}

fetch_text <- function(url, kind = "pages", ext = "html", use_cache = TRUE) {
  cp <- cache_path(url, kind, ext)
  if (use_cache && file.exists(cp) && file.size(cp) > 0) return(readr::read_file(cp))
  req <- httr2::request(url) |>
    httr2::req_user_agent(TR$user_agent) |>
    httr2::req_headers(`Accept-Language` = "th,en;q=0.8") |>
    httr2::req_timeout(TR$timeout_secs) |>
    httr2::req_throttle(rate = 1 / TR$delay_seconds, realm = "thairath") |>
    httr2::req_retry(max_tries = TR$max_retries,
                     is_transient = function(r) httr2::resp_status(r) %in% c(429, 500, 502, 503, 504),
                     backoff = function(i) 5 * 2^i) |>
    httr2::req_error(is_error = function(r) FALSE)
  resp <- tryCatch(httr2::req_perform(req), error = function(e) {
    message("  [network error] ", url, " : ", conditionMessage(e)); NULL })
  if (is.null(resp)) return(NULL)
  if (httr2::resp_status(resp) != 200) {
    message("  [HTTP ", httr2::resp_status(resp), "] ", url); return(NULL)
  }
  txt <- tryCatch(httr2::resp_body_string(resp, encoding = "UTF-8"),
                  error = function(e) rawToChar(httr2::resp_body_raw(resp)))
  if (use_cache) readr::write_file(txt, cp)
  txt
}

read_html_safe <- function(txt) {
  if (is.null(txt)) return(NULL)
  tryCatch(xml2::read_html(txt, encoding = "UTF-8"), error = function(e) NULL)
}

# ---------------------------------------------------------------------------
# DISCOVERY — each returns a tibble: url, title, date, via
# ---------------------------------------------------------------------------
empty_candidates <- function() tibble::tibble(url = character(), title = character(),
                                              date = as.Date(character()), via = character())

# Article URLs end in a numeric id: /news/local/2961344,
# /newspaper/column/category/local/2966856, /entertain/news/1581525 ...
ARTICLE_RX <- "^https://www\\.thairath\\.co\\.th/(?!tags/|search|video/|clip/|tv/|horoscope/|lottery/latest)[a-z0-9_-]+(?:/[a-z0-9_-]+){0,4}/\\d{5,8}$"
ARTICLE_IN_TEXT_RX <- "(?:https://www\\.thairath\\.co\\.th)?(/(?:news|newspaper|scoop|money|sport|entertain|lifestyle|women|business|politics|foreign|local|social|crime|education|tech|auto|health|home|travel|food|content)(?:/[a-z0-9_-]+){0,4}/\\d{5,8})(?=[\"'?#<\\s\\\\]|$)"

clean_article_urls <- function(u) {
  u <- sub("[?#].*$", "", u)
  u <- ifelse(startsWith(u, "/"), paste0(BASE, u), u)
  u <- sub("^http://", "https://", u)
  u <- sub("^https://thairath\\.co\\.th", BASE, u)
  unique(u[grepl(ARTICLE_RX, u, perl = TRUE)])
}

# Article links in a page: <a href> plus any article paths inside embedded
# JSON (Thairath is a Next.js site, so lists are often in the page data too).
article_links_in_page <- function(html_txt) {
  doc <- read_html_safe(html_txt)
  hrefs <- if (!is.null(doc)) xml2::xml_attr(xml2::xml_find_all(doc, "//a[@href]"), "href") else character()
  unescaped <- gsub("\\/", "/", html_txt, fixed = TRUE)   # JSON writes / as \/
  in_text <- stringr::str_match_all(unescaped, ARTICLE_IN_TEXT_RX)[[1]][, 2]
  clean_article_urls(c(hrefs, in_text))
}

# Headline next to each link (longest link text), NA when not available.
link_titles <- function(html_txt, urls) {
  doc <- read_html_safe(html_txt)
  if (is.null(doc) || !length(urls)) return(rep(NA_character_, length(urls)))
  a <- xml2::xml_find_all(doc, "//a[@href]")
  href <- clean_article_urls_keep(xml2::xml_attr(a, "href"))
  txt <- normalise_text(xml2::xml_text(a))
  vapply(urls, function(u) {
    t <- txt[href == u & nzchar(txt)]
    if (length(t)) t[which.max(nchar(t))] else NA_character_
  }, character(1), USE.NAMES = FALSE)
}
clean_article_urls_keep <- function(u) {   # like clean_article_urls, but keeps positions
  u <- sub("[?#].*$", "", u)
  u <- ifelse(startsWith(u, "/"), paste0(BASE, u), u)
  sub("^http://", "https://", u)
}

# --- 1. monthly sitemaps -----------------------------------------------------
discover_sitemaps <- function() {
  message("-> sitemaps")
  idx <- fetch_text(paste0(BASE, "/sitemap.xml"), "sitemaps", "xml", use_cache = FALSE)
  if (is.null(idx)) { message("   sitemap index not reachable"); return(empty_candidates()) }
  doc <- xml2::read_xml(idx); xml2::xml_ns_strip(doc)
  kids <- xml2::xml_text(xml2::xml_find_all(doc, "//sitemap/loc"))
  m <- stringr::str_match(kids, "sitemap-monthly-(\\d{4})-(\\d{2})\\.xml")
  month_start <- as.Date(ifelse(is.na(m[, 1]), NA, sprintf("%s-%s-01", m[, 2], m[, 3])))
  keep <- !is.na(month_start) & month_start <= TR$date_to &
    month_start >= as.Date(format(TR$date_from, "%Y-%m-01"))
  kids <- kids[keep]; month_start <- month_start[keep]
  message(sprintf("   %d monthly sitemaps in the period (%s to %s)", length(kids),
                  format(min(month_start)), format(max(month_start))))
  if (min(month_start) > TR$date_from)
    message("   note: Thairath has no monthly sitemap before ", format(min(month_start), "%Y-%m"))
  out <- list()
  for (i in seq_along(kids)) {
    # the current month's file still changes; older months are cached
    txt <- fetch_text(kids[i], "sitemaps", "xml")
    if (is.null(txt)) next
    sm <- tryCatch(xml2::read_xml(txt), error = function(e) NULL)
    if (is.null(sm)) next
    xml2::xml_ns_strip(sm)
    nodes <- xml2::xml_find_all(sm, "//url")
    loc   <- trimws(xml2::xml_text(xml2::xml_find_first(nodes, "./loc")))
    title <- trimws(xml2::xml_text(xml2::xml_find_first(nodes, ".//*[local-name()='title']")))
    out[[i]] <- tibble::tibble(url = loc, title = title, date = month_start[i],
                               via = paste0("sitemap:", format(month_start[i], "%Y-%m")))
  }
  res <- dplyr::bind_rows(empty_candidates(), out)
  res <- res[nzchar(res$url), ]
  res$url <- clean_article_urls_keep(res$url)
  res <- res[grepl(ARTICLE_RX, res$url, perl = TRUE), ]
  n_all <- nrow(res)
  if (isTRUE(TR$sitemap_title_filter)) res <- res[title_is_candidate(res$title), ]
  message(sprintf("   %d sitemap articles, %d with a keyword in the headline", n_all, nrow(res)))
  # The date here is only the sitemap month; the real date comes from the article.
  res$date <- as.Date(NA)
  dplyr::distinct(res, url, .keep_all = TRUE)
}

# --- 2. tag pages ------------------------------------------------------------
tag_page_url <- function(tag, page) {
  u <- paste0(BASE, "/tags/", utils::URLencode(tag, reserved = TRUE))
  if (page > 1) paste0(u, "?page=", page) else u
}

discover_tags <- function(tags = TR$tags, max_pages = TR$max_tag_pages) {
  message("-> tag pages")
  out <- list()
  for (tag in tags) {
    seen <- character(); n_found <- 0; prev <- character()
    # Listing pages change as news is added, so their cache is kept per day.
    kind <- file.path("tags", format(Sys.Date()))
    for (page in seq_len(max_pages)) {
      u <- tag_page_url(tag, page)
      txt <- fetch_text(u, kind, "html")
      if (is.null(txt) && page == 1) txt <- fetch_text(paste0(u, "?page=1"), kind, "html")
      if (is.null(txt)) break
      links <- article_links_in_page(txt)
      new <- setdiff(links, seen)
      # Same list as the previous page: the site ignored ?page=, so stop.
      same_as_prev <- length(links) && mean(links %in% prev) > 0.8
      prev <- links
      doc <- read_html_safe(txt)
      dates <- find_thai_dates(if (!is.null(doc)) xml2::xml_text(doc) else txt)
      newest <- if (length(dates)) max(dates) else as.Date(NA)
      oldest <- if (length(dates)) min(dates) else as.Date(NA)
      message(sprintf("   tag '%s' page %d: %d new links (dates %s to %s)", tag, page,
                      length(new), format(oldest), format(newest)))
      if (!length(new) || same_as_prev) break
      out[[length(out) + 1]] <- tibble::tibble(url = new, title = link_titles(txt, new),
                                               date = as.Date(NA), via = paste0("tag:", tag))
      seen <- c(seen, new); n_found <- n_found + length(new)
      # Lists run newest first: once a page reaches back before the period, stop.
      if (!is.na(oldest) && oldest < TR$date_from) break
    }
    message(sprintf("   tag '%s': %d links in total", tag, n_found))
  }
  dplyr::distinct(dplyr::bind_rows(empty_candidates(), out), url, .keep_all = TRUE)
}

# --- 3. site search in headless Chrome (optional) ----------------------------
discover_search <- function(terms = TR$search_terms, max_rounds = TR$max_search_rounds) {
  message("-> site search (headless Chrome)")
  if (!requireNamespace("chromote", quietly = TRUE)) {
    message("   skipped: install.packages('chromote') and install Google Chrome to use the search")
    return(empty_candidates())
  }
  b <- tryCatch(chromote::ChromoteSession$new(), error = function(e) {
    message("   skipped: could not start Chrome (", conditionMessage(e), ")"); NULL })
  if (is.null(b)) return(empty_candidates())
  on.exit(try(b$close(), silent = TRUE), add = TRUE)
  b$Network$setUserAgentOverride(userAgent = TR$user_agent)
  # Log the data requests the search page makes, so a faster direct API
  # route can be added later if one exists.
  api_log <- file.path(TR$output_dir, "search_network_log.txt")
  dir.create(TR$output_dir, recursive = TRUE, showWarnings = FALSE)
  b$Network$enable()
  b$Network$responseReceived(callback_ = function(msg) {
    r <- msg$response
    if (grepl("json", r$mimeType %||% "") || grepl("api|search|graphql", r$url %||% ""))
      cat(format(Sys.time()), r$status %||% "", r$mimeType %||% "", r$url, "\n",
          file = api_log, append = TRUE)
  })
  js_links <- "Array.from(document.querySelectorAll('a[href]')).map(a => a.href).join('\\n')"
  js_more <- paste0(
    "(() => { window.scrollTo(0, document.body.scrollHeight);",
    " const b = Array.from(document.querySelectorAll('button, a')).find(e =>",
    " /ดูเพิ่ม|โหลดเพิ่ม|เพิ่มเติม|ข่าวเพิ่มเติม|load more|more/i.test(e.innerText || '') &&",
    " !(e.href && /\\/(news|tags)\\//.test(e.href)));",
    " if (b) { b.click(); return 'clicked'; } return 'scrolled'; })()")
  out <- list()
  for (term in terms) {
    u <- paste0(BASE, "/search?q=", utils::URLencode(term, reserved = TRUE))
    ok <- tryCatch({ b$Page$navigate(u); b$Page$loadEventFired(timeout_ = 60); TRUE },
                   error = function(e) { message("   could not load ", u); FALSE })
    if (!ok) next
    Sys.sleep(4)
    seen <- character(); stale <- 0
    for (round in seq_len(max_rounds)) {
      raw <- tryCatch(b$Runtime$evaluate(js_links)$result$value, error = function(e) "")
      links <- clean_article_urls(strsplit(raw %||% "", "\n")[[1]])
      new <- setdiff(links, seen)
      if (length(new)) {
        out[[length(out) + 1]] <- tibble::tibble(url = new, title = NA_character_,
                                                 date = as.Date(NA), via = paste0("search:", term))
        seen <- c(seen, new); stale <- 0
      } else stale <- stale + 1
      if (stale >= 3) break
      tryCatch(b$Runtime$evaluate(js_more), error = function(e) NULL)
      Sys.sleep(TR$delay_seconds + 1)
    }
    message(sprintf("   search '%s': %d links", term, length(seen)))
  }
  if (file.exists(api_log)) message("   data requests seen by the search page: ", api_log)
  dplyr::distinct(dplyr::bind_rows(empty_candidates(), out), url, .keep_all = TRUE)
}

# ---------------------------------------------------------------------------
# EXTRACTION — one article page -> one row.
# Order of preference: JSON-LD, Next.js page data, <meta> tags, page HTML.
# ---------------------------------------------------------------------------
jsonld_article <- function(doc) {
  nodes <- xml2::xml_find_all(doc, "//script[@type='application/ld+json']")
  objs <- list()
  flatten <- function(x) {
    if (is.null(x)) return()
    if (!is.null(names(x))) {
      objs[[length(objs) + 1]] <<- x
      if (!is.null(x[["@graph"]])) for (g in x[["@graph"]]) flatten(g)
    } else for (y in x) if (is.list(y)) flatten(y)
  }
  for (n in nodes) flatten(tryCatch(jsonlite::fromJSON(xml2::xml_text(n), simplifyVector = FALSE),
                                    error = function(e) NULL))
  types <- c("NewsArticle", "Article", "ReportageNewsArticle", "AnalysisNewsArticle",
             "OpinionNewsArticle", "BlogPosting")
  for (t in types) for (o in objs) if (t %in% unlist(o[["@type"]])) return(o)
  NULL
}

next_data <- function(doc) {
  n <- xml2::xml_find_first(doc, "//script[@id='__NEXT_DATA__']")
  if (is.na(n)) return(NULL)
  tryCatch(jsonlite::fromJSON(xml2::xml_text(n), simplifyVector = FALSE), error = function(e) NULL)
}

# First non-empty scalar stored under any of `keys`, searching a nested list.
find_key <- function(x, keys, depth = 0) {
  if (depth > 12 || !is.list(x)) return(NULL)
  for (k in keys) if (!is.null(x[[k]]) && !is.list(x[[k]]) && nzchar(as.character(x[[k]])[1]))
    return(x[[k]])
  for (el in x) { r <- find_key(el, keys, depth + 1); if (!is.null(r)) return(r) }
  NULL
}

meta <- function(doc, keys) {
  for (k in keys) {
    n <- xml2::xml_find_first(doc, sprintf("//meta[@property='%s' or @name='%s' or @itemprop='%s']", k, k, k))
    if (!is.na(n)) { v <- xml2::xml_attr(n, "content"); if (!is.na(v) && nzchar(trimws(v))) return(trimws(v)) }
  }
  NULL
}

JUNK_XPATH <- paste(
  "//script", "//style", "//noscript", "//iframe", "//figure", "//figcaption",
  "//aside", "//nav", "//form", "//button", "//header", "//footer",
  paste0("//*[contains(@class,'related') or contains(@class,'share') or contains(@class,'social')",
         " or contains(@class,'advert') or contains(@class,'ads-') or contains(@class,'banner')",
         " or contains(@class,'tag-list') or contains(@class,'readmore') or contains(@class,'breadcrumb')]"),
  sep = " | ")

# Paragraph text of a node, one paragraph per line, without ads and boxes.
paragraphs_text <- function(node) {
  if (is.null(node) || inherits(node, "xml_missing")) return(NULL)
  node <- xml2::read_html(as.character(node), encoding = "UTF-8")   # a copy, safe to edit
  xml2::xml_remove(xml2::xml_find_all(node, JUNK_XPATH))
  ps <- xml2::xml_find_all(node, "//p | //h2 | //h3 | //li | //blockquote")
  txt <- if (length(ps) >= 2) xml2::xml_text(ps) else
    strsplit(xml2::xml_text(xml2::xml_find_first(node, "//body")), "\n")[[1]]
  txt <- normalise_text(txt)
  paste(unique(txt[nzchar(txt)]), collapse = "\n")
}

html_to_text <- function(x) {
  if (is.null(x) || !nzchar(x)) return(NULL)
  if (!grepl("<[a-zA-Z/]", x)) return(normalise_text(x))
  paragraphs_text(xml2::xml_find_first(xml2::read_html(paste0("<div>", x, "</div>"), encoding = "UTF-8"), "//div"))
}

first_node <- function(doc, selectors) {
  for (s in selectors) {
    n <- tryCatch(rvest::html_element(doc, s), error = function(e) NULL)
    if (!is.null(n) && !inherits(n, "xml_missing") && length(n)) return(n)
  }
  NULL
}

clean_authors <- function(a) {
  if (is.null(a)) return(NA_character_)
  a <- normalise_text(unlist(a))
  a <- sub("^(by|โดย|ผู้เขียน)\\s*:?\\s*", "", a, ignore.case = TRUE)
  a <- unique(a[nzchar(a) & !grepl("^https?://", a)])
  if (length(a)) paste(a, collapse = "; ") else NA_character_
}

ld_author_names <- function(a) {
  if (is.null(a)) return(NULL)
  if (!is.null(names(a))) a <- list(a)
  nm <- vapply(a, function(x) if (is.list(x)) as.character(x[["name"]] %||% NA) else as.character(x), "")
  nm <- nm[!is.na(nm) & nzchar(nm)]
  if (length(nm)) nm else NULL
}

# "/news/local/northeast/2961344" -> "news/local/northeast"
section_from_url <- function(url) sub("^/", "", sub("/\\d+$", "", sub("^https://[^/]+", "", url)))

extract_article <- function(html_txt, url) {
  doc <- read_html_safe(html_txt)
  if (is.null(doc)) return(NULL)
  ld <- jsonld_article(doc)
  nd <- next_data(doc)
  pp <- nd$props$pageProps

  headline <- (ld$headline %||% ld$name) %||% meta(doc, c("og:title", "twitter:title")) %||%
    find_key(pp, c("title", "headline")) %||%
    { n <- first_node(doc, "h1"); if (!is.null(n)) rvest::html_text2(n) }
  headline <- normalise_text(sub("\\s+[|\\-]\\s+(ไทยรัฐ|Thairath)[^|]*$", "", headline %||% "", perl = TRUE))

  date_raw <- ld$datePublished %||%
    meta(doc, c("article:published_time", "og:article:published_time", "datePublished")) %||%
    find_key(pp, c("publishTime", "publishedAt", "published_at", "publishDate", "createdAt")) %||%
    { n <- first_node(doc, "time[datetime]"); if (!is.null(n)) rvest::html_attr(n, "datetime") }
  published_date <- parse_pub_date(date_raw)
  if (is.na(published_date)) {   # last resort: first Thai date written on the page
    n <- first_node(doc, c("article", "main", "body"))
    d <- find_thai_dates(if (!is.null(n)) xml2::xml_text(n) else "")
    if (length(d)) published_date <- d[1]
  }

  authors <- ld_author_names(ld$author) %||%
    meta(doc, c("author", "article:author")) %||%
    find_key(pp, c("byline", "authorName", "writer", "author")) %||%
    { n <- first_node(doc, c("[rel='author']", "[class*='author']", "[class*='Author']"))
      if (!is.null(n)) rvest::html_text2(n) }
  authors <- clean_authors(authors)

  # Body: take the longest of the candidates below.
  bodies <- list(
    html_to_text(ld$articleBody %||% ""),
    html_to_text(find_key(pp, c("content", "articleBody", "body", "detail")) %||% ""),
    paragraphs_text(first_node(doc, c("[itemprop='articleBody']", "#article-content",
                                      ".article-content", "[class*='article-body']",
                                      "[class*='ArticleContent']", "article")))
  )
  bodies <- unlist(Filter(Negate(is.null), bodies))
  content <- if (length(bodies)) bodies[which.max(nchar(bodies))] else NA_character_

  kws <- unique(c(unlist(ld$keywords), meta(doc, c("news_keywords", "keywords"))))
  tibble::tibble(
    outlet         = "Thairath",
    published_date = published_date,
    published_time = as.character(date_raw %||% NA_character_),
    headline       = headline %||% NA_character_,
    authors        = authors,
    section        = section_from_url(url),
    content        = content,
    n_chars        = nchar(content %||% ""),
    tags           = paste(kws[!is.na(kws) & nzchar(kws)], collapse = "; "),
    url            = url
  )
}

# ---------------------------------------------------------------------------
# PIPELINE
# ---------------------------------------------------------------------------
out_file <- function(name) {
  dir.create(TR$output_dir, recursive = TRUE, showWarnings = FALSE)
  file.path(TR$output_dir, name)
}

# Step 1: candidate article URLs from every chosen route (saved, so a re-run
# does not repeat this step unless refresh = TRUE).
get_candidates <- function(methods = TR$methods, refresh = FALSE) {
  f <- out_file("candidates.rds")
  if (!refresh && file.exists(f)) {
    cand <- readRDS(f)
    message("Using saved candidate list (", nrow(cand), " URLs). Run with --refresh to rebuild it.")
    return(cand)
  }
  parts <- list()
  if ("sitemap" %in% methods) parts$sitemap <- tryCatch(discover_sitemaps(), error = function(e) {
    message("   sitemap route failed: ", conditionMessage(e)); empty_candidates() })
  if ("tags" %in% methods) parts$tags <- tryCatch(discover_tags(), error = function(e) {
    message("   tag route failed: ", conditionMessage(e)); empty_candidates() })
  if ("search" %in% methods) parts$search <- tryCatch(discover_search(), error = function(e) {
    message("   search route failed: ", conditionMessage(e)); empty_candidates() })
  all <- dplyr::bind_rows(c(list(empty_candidates()), unname(parts)))
  # One row per URL; remember every route that found it.
  cand <- all |>
    dplyr::summarise(title = dplyr::first(stats::na.omit(title)) %||% NA_character_,
                     via = paste(unique(via), collapse = " | "), .by = url)
  message(sprintf("Candidates: %d unique article URLs", nrow(cand)))
  saveRDS(cand, f)
  readr::write_excel_csv(cand, out_file("candidates.csv"))
  cand
}

# Step 2: download and parse every candidate. Progress is saved every 25
# articles; finished URLs are skipped when you run again.
scrape_articles <- function(cand) {
  if (!is.null(TR$max_articles)) cand <- utils::head(cand, TR$max_articles)
  f <- out_file("articles_raw.rds")
  done <- if (file.exists(f)) readRDS(f) else NULL
  todo <- setdiff(cand$url, done$url)
  message(sprintf("Articles: %d candidates, %d already scraped, %d to download (~%d min)",
                  nrow(cand), nrow(done) %||% 0, length(todo),
                  ceiling(length(todo) * TR$delay_seconds / 60)))
  rows <- list(); fails <- character()
  save_progress <- function() { done <<- dplyr::bind_rows(done, rows); rows <<- list(); saveRDS(done, f) }
  for (i in seq_along(todo)) {
    u <- todo[i]
    txt <- fetch_text(u, "articles", "html")
    row <- if (!is.null(txt)) tryCatch(extract_article(txt, u), error = function(e) {
      message("  parse error: ", u, " : ", conditionMessage(e)); NULL })
    if (is.null(row)) { fails <- c(fails, u); next }
    row$found_via <- cand$via[match(u, cand$url)]
    row$scraped_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    rows[[length(rows) + 1]] <- dplyr::bind_cols(row, score_article(row$headline, row$content))
    if (i %% 25 == 0) { save_progress(); message(sprintf("  %d / %d", i, length(todo))) }
  }
  save_progress()
  if (length(fails)) {
    readr::write_lines(fails, out_file("failed_urls.txt"), append = TRUE)
    message(length(fails), " pages failed; listed in ", out_file("failed_urls.txt"))
  }
  done
}

# Step 3: tidy outputs (one row per article) and a short summary.
#   thairath_all_articles.csv   every article downloaded, in the study period
#   thairath_myanmar_migrants.csv  only those with at least one keyword
build_dataset <- function() {
  f <- out_file("articles_raw.rds")
  if (!file.exists(f)) { message("Nothing scraped yet."); return(invisible(NULL)) }
  raw <- readRDS(f)
  all <- raw |>
    dplyr::filter(!is.na(published_date), published_date >= TR$date_from,
                  published_date <= TR$date_to, !is.na(content), nchar(content) > 0) |>
    dplyr::distinct(url, .keep_all = TRUE) |>
    dplyr::arrange(published_date, url) |>
    dplyr::mutate(article_id = sprintf("thairath_%05d", dplyr::row_number()),
                  year = as.integer(format(published_date, "%Y"))) |>
    dplyr::relocate(article_id, outlet, published_date, year, headline, authors, section, content,
                    url, matched_keywords, n_keywords, kw_myanmar, kw_migration, kw_specific,
                    relevant_strict)
  hits <- dplyr::filter(all, n_keywords > 0)
  # write_excel_csv adds a byte-order mark so Excel shows Thai text correctly.
  readr::write_excel_csv(all, out_file("thairath_all_articles.csv"))
  readr::write_excel_csv(hits, out_file("thairath_myanmar_migrants.csv"))
  saveRDS(hits, out_file("thairath_myanmar_migrants.rds"))

  outside <- sum(is.na(raw$published_date) | raw$published_date < TR$date_from |
                   raw$published_date > TR$date_to)
  message("\n================ RESULTS ================")
  message(sprintf("Downloaded and parsed:          %d articles", nrow(raw)))
  message(sprintf("  outside the period / no date: %d", outside))
  message(sprintf("In period:                      %d  -> %s", nrow(all), out_file("thairath_all_articles.csv")))
  message(sprintf("  with any keyword:             %d  -> %s", nrow(hits), out_file("thairath_myanmar_migrants.csv")))
  message(sprintf("  relevant_strict:              %d", sum(hits$relevant_strict)))
  message(sprintf("  empty author field:           %d", sum(is.na(hits$authors))))
  if (nrow(hits)) {
    message("\nKeyword articles per year (strict in brackets):")
    yr <- dplyr::summarise(hits, n = dplyr::n(), strict = sum(relevant_strict), .by = year) |>
      dplyr::arrange(year)
    for (i in seq_len(nrow(yr))) message(sprintf("  %d: %4d (%d)", yr$year[i], yr$n[i], yr$strict[i]))
    message("\nFound via:")
    via <- table(unlist(lapply(strsplit(hits$found_via, " \\| "), function(v) unique(sub(":.*", "", v)))))
    for (k in names(via)) message(sprintf("  %-8s %d", k, via[[k]]))
  }
  invisible(hits)
}

# Quick check of every route: a couple of pages each plus 3 parsed articles.
probe <- function() {
  message("### Probing thairath.co.th (small sample, nothing is saved to the final outputs)")
  old <- TR; on.exit(TR <<- old)
  TR$max_tag_pages <<- 2; TR$max_search_rounds <<- 2
  TR$tags <<- TR$tags[1:2]; TR$search_terms <<- TR$search_terms[1]
  sm <- tryCatch(discover_sitemaps(), error = function(e) { message("   ", conditionMessage(e)); empty_candidates() })
  tg <- tryCatch(discover_tags(), error = function(e) { message("   ", conditionMessage(e)); empty_candidates() })
  se <- tryCatch(discover_search(), error = function(e) { message("   ", conditionMessage(e)); empty_candidates() })
  message(sprintf("\nFound: sitemap %d, tags %d, search %d", nrow(sm), nrow(tg), nrow(se)))
  sample_urls <- unique(c(utils::head(tg$url, 2), utils::head(sm$url, 1), utils::head(se$url, 1)))
  for (u in utils::head(sample_urls, 3)) {
    txt <- fetch_text(u, "articles", "html")
    r <- if (!is.null(txt)) extract_article(txt, u)
    if (is.null(r)) { message("\n", u, "\n  could not be parsed"); next }
    message("\n", u,
            "\n  headline: ", r$headline,
            "\n  date:     ", r$published_date, "  (", r$published_time, ")",
            "\n  authors:  ", r$authors,
            "\n  section:  ", r$section,
            "\n  body:     ", r$n_chars, " characters: ", substr(gsub("\n", " ", r$content %||% ""), 1, 120), "...")
  }
}
