# ---------------------------------------------------------------------------
# http.R — polite fetching: robots.txt, rate limiting, retries, disk cache.
# ---------------------------------------------------------------------------

.robots_cache <- new.env(parent = emptyenv())

host_of <- function(url) httr2::url_parse(url)$hostname

# Minimal robots.txt parser: rules of the `User-agent: *` group(s). Supports
# `*` wildcards and `$` anchors, longest-match wins, Allow beats Disallow on ties.
robots_rules <- function(host, scheme = "https") {
  if (!is.null(.robots_cache[[host]])) return(.robots_cache[[host]])
  rules <- data.frame(type = character(), path = character(), stringsAsFactors = FALSE)
  txt <- tryCatch({
    resp <- httr2::request(paste0(scheme, "://", host, "/robots.txt")) |>
      httr2::req_user_agent(CONFIG$user_agent) |>
      httr2::req_timeout(CONFIG$timeout_secs) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform()
    if (httr2::resp_status(resp) == 200) httr2::resp_body_string(resp) else ""
  }, error = function(e) "")
  lines <- trimws(sub("#.*$", "", strsplit(txt, "\r?\n")[[1]]))
  in_star <- FALSE; prev_was_ua <- FALSE
  for (ln in lines) {
    if (!nzchar(ln)) next
    kv <- regmatches(ln, regexec("^([A-Za-z-]+)\\s*:\\s*(.*)$", ln))[[1]]
    if (length(kv) < 3) next
    key <- tolower(kv[2]); val <- trimws(kv[3])
    if (key == "user-agent") {
      if (!prev_was_ua) in_star <- FALSE
      if (val == "*") in_star <- TRUE
      prev_was_ua <- TRUE
      next
    }
    prev_was_ua <- FALSE
    if (in_star && key %in% c("allow", "disallow") && nzchar(val))
      rules[nrow(rules) + 1, ] <- c(key, val)
  }
  .robots_cache[[host]] <- rules
  rules
}

robots_allowed <- function(url) {
  if (!isTRUE(CONFIG$respect_robots)) return(TRUE)
  u <- httr2::url_parse(url)
  path <- paste0(ifelse(is.null(u$path) || !nzchar(u$path), "/", u$path),
                 if (!is.null(u$query) && length(u$query))
                   paste0("?", paste(names(u$query), u$query, sep = "=", collapse = "&")) else "")
  path <- utils::URLdecode(path)
  rules <- robots_rules(paste0(u$hostname, if (!is.null(u$port)) paste0(":", u$port)), u$scheme)
  if (!nrow(rules)) return(TRUE)
  best_len <- -1; allowed <- TRUE
  for (i in seq_len(nrow(rules))) {
    p <- utils::URLdecode(rules$path[i])
    rx <- gsub("([.+?^(){}|\\[\\]\\\\])", "\\\\\\1", p)
    rx <- gsub("\\*", ".*", rx)
    rx <- paste0("^", sub("\\\\\\$$|\\$$", "$", rx))
    if (grepl(rx, path, perl = TRUE)) {
      len <- nchar(p)
      if (len > best_len || (len == best_len && rules$type[i] == "allow")) {
        best_len <- len; allowed <- rules$type[i] == "allow"
      }
    }
  }
  allowed
}

cache_path <- function(url, outlet = "misc", ext = "html") {
  dir <- file.path(CONFIG$cache_dir, outlet)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.path(dir, paste0(digest::digest(url, algo = "md5"), ".", ext))
}

# Fetch a URL as text. Returns NULL (with a message) on failure / robots block.
# Responses are cached on disk, so re-running the script resumes where it
# stopped and never downloads the same page twice.
fetch_text <- function(url, outlet = "misc", ext = "html", use_cache = TRUE,
                       headers = list()) {
  cp <- cache_path(url, outlet, ext)
  if (use_cache && file.exists(cp) && file.size(cp) > 0)
    return(readr::read_file(cp))
  if (!robots_allowed(url)) {
    message("  [robots.txt disallows] ", url)
    return(NULL)
  }
  req <- httr2::request(url) |>
    httr2::req_user_agent(CONFIG$user_agent) |>
    httr2::req_headers(`Accept-Language` = "th,en;q=0.8", !!!headers) |>
    httr2::req_timeout(CONFIG$timeout_secs) |>
    httr2::req_throttle(rate = 1 / CONFIG$delay_seconds, realm = host_of(url)) |>
    httr2::req_retry(max_tries = CONFIG$max_retries,
                     is_transient = function(r) httr2::resp_status(r) %in% c(429, 500, 502, 503, 504),
                     backoff = function(i) 5 * 2^i) |>
    httr2::req_error(is_error = function(r) FALSE)
  resp <- tryCatch(httr2::req_perform(req), error = function(e) {
    message("  [network error] ", url, " : ", conditionMessage(e)); NULL
  })
  if (is.null(resp)) return(NULL)
  st <- httr2::resp_status(resp)
  if (st != 200) {
    message("  [HTTP ", st, "] ", url)
    return(NULL)
  }
  txt <- tryCatch(httr2::resp_body_string(resp, encoding = "UTF-8"),
                  error = function(e) rawToChar(httr2::resp_body_raw(resp)))
  if (use_cache) readr::write_file(txt, cp)
  txt
}

fetch_html <- function(url, outlet = "misc", ...) {
  txt <- fetch_text(url, outlet, "html", ...)
  if (is.null(txt)) return(NULL)
  tryCatch(xml2::read_html(txt, encoding = "UTF-8"), error = function(e) NULL)
}

fetch_json <- function(url, outlet = "misc", ...) {
  txt <- fetch_text(url, outlet, "json", ...)
  if (is.null(txt)) return(NULL)
  tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
}

# Optional: render a JavaScript page in headless Chrome (needs the `chromote`
# package and Chrome/Chromium installed). Used only for sites whose search
# results are drawn by JavaScript.
fetch_html_js <- function(url, outlet = "misc", wait = 4) {
  cp <- cache_path(url, outlet, "js.html")
  if (file.exists(cp) && file.size(cp) > 0) return(xml2::read_html(readr::read_file(cp), encoding = "UTF-8"))
  if (!requireNamespace("chromote", quietly = TRUE)) {
    message("  [skip] install.packages('chromote') to render JavaScript pages: ", url)
    return(NULL)
  }
  if (!robots_allowed(url)) { message("  [robots.txt disallows] ", url); return(NULL) }
  Sys.sleep(CONFIG$delay_seconds)
  b <- chromote::ChromoteSession$new()
  on.exit(b$close(), add = TRUE)
  b$Network$setUserAgentOverride(userAgent = CONFIG$user_agent)
  b$Page$navigate(url); b$Page$loadEventFired(timeout_ = 60); Sys.sleep(wait)
  html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  readr::write_file(html, cp)
  xml2::read_html(html, encoding = "UTF-8")
}
