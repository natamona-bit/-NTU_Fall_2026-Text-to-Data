# ---------------------------------------------------------------------------
# gdelt_fulltext.R — optional: download the article pages GDELT points to and
# add authors, publication date and full text, using the web scraper's polite
# fetcher (robots.txt, rate limit, cache) and extractor from R/http.R and
# R/extract.R. Must run on a computer that can reach the news sites.
# ---------------------------------------------------------------------------

gdelt_add_fulltext <- function(df, only_strict = TRUE, max_articles = NULL) {
  todo <- if (only_strict) df[df$relevant_strict, ] else df
  if (!is.null(max_articles)) todo <- utils::head(todo, max_articles)
  f <- gdelt_path("output", "fulltext_progress.rds")
  done <- if (file.exists(f)) readRDS(f) else NULL
  todo <- todo[!todo$url %in% done$url, ]
  message(sprintf("== Full text: %d articles to download (%d already done)", nrow(todo), nrow(done) %||% 0))

  rows <- list()
  for (i in seq_len(nrow(todo))) {
    u <- todo$url[i]
    outlet <- OUTLETS[[todo$outlet_id[i]]]
    doc <- fetch_html(u, outlet$id)
    r <- if (!is.null(doc)) tryCatch(extract_article(doc, u, outlet), error = function(e) NULL)
    rows[[length(rows) + 1]] <- if (is.null(r)) tibble::tibble(url = u, fulltext_ok = FALSE) else
      dplyr::bind_cols(
        tibble::tibble(url = u, fulltext_ok = !is.na(r$content) & nchar(r$content %||% "") > 0,
                       site_headline = r$headline, site_authors = r$authors,
                       site_published_date = r$published_date, content = r$content),
        score_article(r$headline, r$content, outlet$language) |>
          dplyr::rename_with(~ paste0("text_", .x)))
    if (i %% 25 == 0 || i == nrow(todo)) {
      done <- dplyr::bind_rows(done, rows); rows <- list(); saveRDS(done, f)
      message(sprintf("  %d / %d", i, nrow(todo)))
    }
  }
  if (is.null(done)) return(df)
  dplyr::left_join(df, dplyr::distinct(done, url, .keep_all = TRUE), by = "url")
}
