# ---------------------------------------------------------------------------
# gdelt_doc.R — GDELT DOC 2.0 API, for the most recent ~3 months only.
#
# The DOC API's article lists only search the last 3 months, so it cannot
# reach the 2017-2025 study period (that comes from the GKG on BigQuery).
# It is kept here for following new coverage after the study period.
# Keywords are matched against GDELT's English machine translation of each
# article, so English terms are used for the Thai outlets too.
# ---------------------------------------------------------------------------

GDELT_DOC_TERMS <- c("Myanmar", "Burmese", "Burma", "Rohingya", "Karen")

gdelt_doc_query <- function(domain, terms = GDELT_DOC_TERMS) {
  sprintf("domainis:%s (%s)", domain, paste(terms, collapse = " OR "))
}

gdelt_doc_url <- function(query, start, end, maxrecords = GDELT_CONFIG$doc_maxrecords) {
  q <- list(query = query, mode = "artlist", format = "json", sort = "dateasc",
            maxrecords = maxrecords,
            startdatetime = format(start, "%Y%m%d%H%M%S", tz = "UTC"),
            enddatetime   = format(end, "%Y%m%d%H%M%S", tz = "UTC"))
  httr2::req_url_query(httr2::request(GDELT_CONFIG$doc_api_url), !!!q)$url
}

# One API call (cached on disk). Returns a tibble of articles.
gdelt_doc_call <- function(url) {
  txt <- {
    cp <- cache_path(url, "gdelt_doc", "json")
    if (file.exists(cp) && file.size(cp) > 0) readr::read_file(cp) else {
      resp <- httr2::request(url) |>
        httr2::req_user_agent(CONFIG$user_agent) |>
        httr2::req_timeout(CONFIG$timeout_secs) |>
        httr2::req_throttle(rate = 1 / GDELT_CONFIG$doc_delay_secs, realm = "gdelt-doc") |>
        httr2::req_retry(max_tries = CONFIG$max_retries,
                         is_transient = function(r) httr2::resp_status(r) %in% c(429, 500, 502, 503, 504),
                         backoff = function(i) 10 * i) |>
        httr2::req_perform()
      body <- httr2::resp_body_string(resp, encoding = "UTF-8")
      # Errors (bad query, rate limit) come back as plain text, not JSON.
      if (!grepl("^\\s*\\{", body)) stop("GDELT DOC API: ", substr(trimws(body), 1, 200), call. = FALSE)
      readr::write_file(body, cp)
      body
    }
  }
  gdelt_doc_parse(txt)
}

gdelt_doc_parse <- function(txt) {
  js <- jsonlite::fromJSON(txt, simplifyVector = TRUE)
  a <- js$articles
  if (is.null(a) || !length(a) || !NROW(a)) return(tibble::tibble())
  a <- tibble::as_tibble(a)
  a$gdelt_seen_at <- as.POSIXct(a$seendate, format = "%Y%m%dT%H%M%SZ", tz = "UTC")
  a
}

# All articles for one query between `start` and `end`. A window that returns
# the maximum number of records is split in two, so nothing is cut off.
gdelt_doc_search <- function(query, start, end, min_window_hours = 1) {
  res <- gdelt_doc_call(gdelt_doc_url(query, start, end))
  if (nrow(res) >= GDELT_CONFIG$doc_maxrecords &&
      difftime(end, start, units = "hours") > min_window_hours) {
    mid <- start + (end - start) / 2
    return(dplyr::bind_rows(gdelt_doc_search(query, start, mid, min_window_hours),
                            gdelt_doc_search(query, mid, end, min_window_hours)))
  }
  res
}

# Recent coverage for all seven outlets, tidied like the GKG table.
gdelt_doc_recent <- function(days = 90, window_days = 7) {
  end <- as.POSIXct(Sys.time(), tz = "UTC"); start <- end - days * 86400
  starts <- seq(start, end, by = window_days * 86400)
  out <- lapply(seq_len(nrow(GDELT_OUTLETS)), function(i) {
    o <- GDELT_OUTLETS[i, ]
    message("== DOC API: ", o$outlet)
    q <- gdelt_doc_query(o$domain)
    parts <- lapply(starts, function(s) tryCatch(
      gdelt_doc_search(q, s, min(s + window_days * 86400, end)),
      error = function(e) { message("   ", conditionMessage(e)); tibble::tibble() }))
    r <- dplyr::bind_rows(parts)
    if (nrow(r)) r$outlet_id <- o$id
    r
  })
  df <- dplyr::bind_rows(out)
  if (!nrow(df)) return(df)
  title_lang <- ifelse(grepl("[\\x{0E00}-\\x{0E7F}]", ifelse(is.na(df$title), "", df$title), perl = TRUE), "th", "en")
  kw <- dplyr::bind_rows(lapply(seq_len(nrow(df)), function(i) score_article(df$title[i], "", title_lang[i])))
  names(kw) <- paste0("title_", names(kw))
  dplyr::bind_cols(
    tibble::tibble(outlet = GDELT_OUTLETS$outlet[match(df$outlet_id, GDELT_OUTLETS$id)],
                   language = df$language,
                   published_date = as.Date(format(df$gdelt_seen_at, "%Y-%m-%d", tz = "Asia/Bangkok")),
                   headline = normalise_text(df$title), url = df$url, domain = df$domain,
                   gdelt_seen_at = df$gdelt_seen_at, outlet_id = df$outlet_id),
    kw) |>
    dplyr::mutate(.key = url_key(url)) |>
    dplyr::distinct(.key, .keep_all = TRUE) |>
    dplyr::select(-.key)
}
