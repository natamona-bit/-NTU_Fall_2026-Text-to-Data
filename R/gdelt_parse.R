# ---------------------------------------------------------------------------
# gdelt_parse.R — turn raw GKG rows into one tidy row per article.
#
# GKG field formats (GDELT GKG 2.1 codebook):
#   DATE            20170101123000  (when GDELT processed the article, UTC)
#   TranslationInfo "srclc:tha;eng:GT-THA 1.0"  (empty for English originals)
#   V2Themes        "THEME,charoffset;THEME,charoffset;..."
#   V2Locations     "type#name#country#adm1#adm2#lat#lon#featureid#offset;..."
#   V2Tone          "tone,positive,negative,polarity,activity,selfgroup,wordcount"
#   Extras          XML-ish tags: <PAGE_TITLE>, <PAGE_AUTHORS>, <PAGE_PRECISEPUBTIMESTAMP>
#                   (the page's own publication time, YYYYMMDDHHMMSS UTC, when GDELT
#                   could find it). Not every article has every tag.
# ---------------------------------------------------------------------------

# Outlet for each domain (suffix match so "m.thairath.co.th" -> Thairath).
gdelt_match_outlet <- function(domains) {
  d <- tolower(domains)
  idx <- vapply(d, function(x) {
    hit <- which(x == GDELT_OUTLETS$domain | endsWith(x, paste0(".", GDELT_OUTLETS$domain)))
    if (length(hit)) hit[1] else NA_integer_
  }, integer(1), USE.NAMES = FALSE)
  tibble::tibble(domain = domains, outlet_id = GDELT_OUTLETS$id[idx],
                 outlet = GDELT_OUTLETS$outlet[idx], outlet_language = GDELT_OUTLETS$language[idx])
}

# Decode the HTML entities GDELT leaves in page titles (&amp; &quot; &#3614; ...).
decode_entities <- function(x) {
  x <- ifelse(is.na(x), NA_character_, x)
  num <- function(s, rx, base) {
    m <- gregexpr(rx, s, perl = TRUE)
    regmatches(s, m) <- lapply(regmatches(s, m), function(e) {
      if (!length(e)) return(e)
      code <- strtoi(gsub("[&#xX;]", "", e), base)
      ifelse(is.na(code), e, intToUtf8(code, multiple = TRUE))
    })
    s
  }
  x <- num(x, "&#[0-9]+;", 10L)
  x <- num(x, "&#[xX][0-9a-fA-F]+;", 16L)
  for (p in list(c("&quot;", "\""), c("&apos;", "'"), c("&lt;", "<"), c("&gt;", ">"),
                 c("&nbsp;", " "), c("&amp;", "&")))
    x <- gsub(p[1], p[2], x, fixed = TRUE)
  x
}

# "THEME,12;THEME,40;OTHER,3" -> "THEME; OTHER" (unique, in order of first use).
gdelt_theme_list <- function(x) {
  vapply(strsplit(ifelse(is.na(x), "", x), ";", fixed = TRUE), function(p) {
    t <- unique(sub(",.*$", "", p[nzchar(p)]))
    paste(t, collapse = "; ")
  }, character(1))
}

# V2Locations -> list of data frames (name, country) per article.
gdelt_locations <- function(x) {
  lapply(strsplit(ifelse(is.na(x), "", x), ";", fixed = TRUE), function(p) {
    p <- p[nzchar(p)]
    if (!length(p)) return(data.frame(name = character(), country = character()))
    f <- strsplit(p, "#", fixed = TRUE)
    data.frame(name = vapply(f, function(v) v[2] %||% NA_character_, ""),
               country = vapply(f, function(v) v[3] %||% NA_character_, ""))
  })
}

gdelt_tone <- function(x) {
  cols <- c("tone", "tone_positive", "tone_negative", "tone_polarity",
            "activity_ref_density", "selfgroup_ref_density", "word_count")
  m <- t(vapply(strsplit(ifelse(is.na(x), "", x), ",", fixed = TRUE), function(v) {
    v <- suppressWarnings(as.numeric(v)); length(v) <- 7; v
  }, numeric(7)))
  colnames(m) <- cols
  out <- tibble::as_tibble(m)
  out$word_count <- as.integer(out$word_count)
  out
}

# "srclc:tha;eng:GT-THA 1.0" -> "tha"; empty -> "eng".
gdelt_source_lang <- function(x) {
  l <- stringr::str_match(ifelse(is.na(x), "", x), "srclc:([a-z]{3})")[, 2]
  ifelse(is.na(l), "eng", l)
}

# Key used to find the same article under slightly different URLs.
url_key <- function(u) {
  k <- tolower(trimws(u))
  k <- sub("^https?://", "", k)
  k <- sub("^(www|m|amp)\\.", "", k)
  k <- sub("[?#].*$", "", k)
  k <- sub("/amp/?$", "", k)
  sub("/+$", "", k)
}

.has_any <- function(x, pats) grepl(paste(pats, collapse = "|"), ifelse(is.na(x), "", x))

# Raw GKG rows -> tidy article table (one row per article, study period only).
gdelt_tidy <- function(raw, from = CONFIG$date_from, to = CONFIG$date_to) {
  if (!nrow(raw)) stop("No GKG rows to process.", call. = FALSE)
  o <- gdelt_match_outlet(raw$domain)
  seen <- as.POSIXct(raw$gdelt_date, format = "%Y%m%d%H%M%S", tz = "UTC")
  pub  <- as.POSIXct(raw$page_pub_timestamp %||% rep(NA_character_, nrow(raw)),
                     format = "%Y%m%d%H%M%S", tz = "UTC")
  bkk_date <- function(x) as.Date(format(x, "%Y-%m-%d", tz = "Asia/Bangkok"))
  locs <- gdelt_locations(raw$locations_raw)
  df <- tibble::tibble(
    outlet_id   = o$outlet_id,
    outlet      = o$outlet,
    language    = dplyr::recode(gdelt_source_lang(raw$translation_info), tha = "th", eng = "en"),
    gdelt_seen_at = seen,
    seen_date   = bkk_date(seen),
    # Publication date from the page when GDELT recorded it, otherwise the
    # date GDELT first saw the article (usually the same day for news).
    published_date = dplyr::coalesce(bkk_date(pub), bkk_date(seen)),
    date_source = ifelse(is.na(pub), "gdelt_seen", "page_timestamp"),
    headline    = normalise_text(decode_entities(raw$page_title)),
    authors     = normalise_text(decode_entities(raw$page_authors %||% rep(NA_character_, nrow(raw)))),
    url         = raw$url,
    domain      = raw$domain,
    gkg_record_id = raw$gkg_record_id,
    themes      = gdelt_theme_list(raw$themes_raw),
    locations   = vapply(locs, function(l) paste(unique(l$name), collapse = "; "), ""),
    location_countries = vapply(locs, function(l) paste(unique(l$country), collapse = "; "), ""),
    myanmar_location   = vapply(locs, function(l) GDELT_MYANMAR_FIPS %in% l$country, logical(1))
  )
  df <- dplyr::bind_cols(df, gdelt_tone(raw$tone_raw))
  df$headline[!nzchar(df$headline)] <- NA_character_
  df$authors[!nzchar(df$authors)] <- NA_character_

  # GDELT's own signals.
  df$theme_myanmar   <- .has_any(df$themes, GDELT_THEMES$myanmar)
  df$theme_migration <- .has_any(df$themes, GDELT_THEMES$migration)

  # The study's keyword lists, applied to the headline (GDELT has no body text;
  # run gdelt_add_fulltext() to score the full article).
  title_lang <- ifelse(grepl("[\\x{0E00}-\\x{0E7F}]", ifelse(is.na(df$headline), "", df$headline), perl = TRUE), "th", "en")
  kw <- dplyr::bind_rows(lapply(seq_len(nrow(df)), function(i)
    score_article(df$headline[i], "", title_lang[i])))
  names(kw) <- paste0("title_", names(kw))
  df <- dplyr::bind_cols(df, kw)

  df <- df |>
    dplyr::mutate(
      myanmar_signal   = myanmar_location | theme_myanmar | title_kw_myanmar,
      migration_signal = theme_migration | title_kw_migration,
      relevant_strict  = title_kw_specific | (myanmar_signal & migration_signal)
    ) |>
    dplyr::filter(!is.na(outlet), published_date >= from, published_date <= to) |>
    dplyr::mutate(.key = url_key(url)) |>
    dplyr::arrange(gdelt_seen_at) |>
    dplyr::distinct(.key, .keep_all = TRUE) |>        # first time GDELT saw the article
    dplyr::select(-.key, -title_relevant_strict) |>
    dplyr::arrange(outlet, gdelt_seen_at) |>
    dplyr::mutate(article_id = sprintf("gdelt_%s_%05d", outlet_id, dplyr::row_number()),
                  .by = outlet) |>
    dplyr::relocate(article_id, outlet, language, published_date, headline, authors, url,
                    relevant_strict, myanmar_signal, migration_signal)
  df
}

# Save CSV (UTF-8 with BOM for Excel), RDS, and Parquet if a Parquet package is installed.
gdelt_save <- function(df, name = "gdelt_myanmar_migrants") {
  base <- gdelt_path("output", name)
  saveRDS(df, paste0(base, ".rds"))
  readr::write_excel_csv(df, paste0(base, ".csv"))
  pq <- paste0(base, ".parquet")
  if (requireNamespace("arrow", quietly = TRUE)) arrow::write_parquet(df, pq)
  else if (requireNamespace("nanoparquet", quietly = TRUE)) nanoparquet::write_parquet(df, pq)
  message(sprintf("Saved %d articles (%d strict) to %s.{csv,rds%s}", nrow(df), sum(df$relevant_strict),
                  base, if (file.exists(pq)) ",parquet" else ""))
  print(dplyr::count(df, outlet, year = format(published_date, "%Y")) |>
          tidyr::pivot_wider(names_from = year, values_from = n, values_fill = 0))
  invisible(df)
}
