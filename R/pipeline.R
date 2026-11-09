# ---------------------------------------------------------------------------
# pipeline.R — discover -> download -> extract -> filter -> save.
# ---------------------------------------------------------------------------

load_scraper <- function(root = ".") {
  for (f in c("config.R", "keywords.R", "http.R", "extract.R", "discover.R", "outlets.R"))
    source(file.path(root, "R", f), encoding = "UTF-8")
}

out_file <- function(...) {
  dir.create(CONFIG$output_dir, recursive = TRUE, showWarnings = FALSE)
  file.path(CONFIG$output_dir, paste0(...))
}

# Step 1: candidate URLs for one outlet (saved, so a re-run skips this step).
get_candidates <- function(outlet, refresh = FALSE) {
  f <- out_file("candidates_", outlet$id, ".rds")
  if (!refresh && file.exists(f)) return(readRDS(f))
  message("== Discovering candidates: ", outlet$name)
  cand <- outlet$discover(outlet)
  cand <- dplyr::distinct(cand, url, .keep_all = TRUE)
  cand <- dplyr::filter(cand, is.na(date) | (date >= CONFIG$date_from & date <= CONFIG$date_to))
  saveRDS(cand, f)
  readr::write_excel_csv(cand, out_file("candidates_", outlet$id, ".csv"))
  cand
}

# Step 2: download + parse each candidate. Progress is saved every 25 articles,
# and finished URLs are skipped on a re-run.
scrape_outlet <- function(outlet, refresh_candidates = FALSE) {
  cand <- get_candidates(outlet, refresh_candidates)
  if (!is.null(CONFIG$max_articles_per_outlet))
    cand <- utils::head(cand, CONFIG$max_articles_per_outlet)
  f <- out_file("articles_", outlet$id, ".rds")
  done <- if (file.exists(f)) readRDS(f) else NULL
  todo <- setdiff(cand$url, done$url)
  message(sprintf("== %s: %d candidates, %d already scraped, %d to go",
                  outlet$name, nrow(cand), nrow(done) %||% 0, length(todo)))
  rows <- list(); fails <- character()
  save_progress <- function() {
    done <<- dplyr::bind_rows(done, rows); rows <<- list()
    saveRDS(done, f)
  }
  for (i in seq_along(todo)) {
    u <- todo[i]
    doc <- fetch_html(u, outlet$id)
    row <- if (!is.null(doc)) tryCatch(extract_article(doc, u, outlet),
                                       error = function(e) { message("  parse error: ", u); NULL })
    if (is.null(row)) { fails <- c(fails, u); next }
    row$found_via <- cand$via[match(u, cand$url)]
    rows[[length(rows) + 1]] <- dplyr::bind_cols(row, score_article(row$headline, row$content, outlet$language))
    if (i %% 25 == 0) { save_progress(); message(sprintf("  %d / %d", i, length(todo))) }
  }
  save_progress()
  if (length(fails)) readr::write_lines(fails, out_file("failed_", outlet$id, ".txt"), append = TRUE)
  done
}

# Step 3: combine, keep the study period and keyword matches, write outputs.
#   keep = "any"    : at least one keyword anywhere in headline/body (your list)
#          "strict" : a specific migrant term, or Myanmar term + migration term
build_dataset <- function(outlet_ids = names(OUTLETS), keep = "any") {
  files <- out_file("articles_", outlet_ids, ".rds")
  all <- dplyr::bind_rows(lapply(files[file.exists(files)], readRDS))
  if (!nrow(all)) { message("Nothing scraped yet."); return(invisible(all)) }
  df <- all |>
    dplyr::filter(!is.na(published_date),
                  published_date >= CONFIG$date_from, published_date <= CONFIG$date_to,
                  !is.na(content), nchar(content) > 0) |>
    dplyr::filter(if (keep == "strict") relevant_strict else n_keywords > 0) |>
    dplyr::distinct(url, .keep_all = TRUE) |>
    dplyr::arrange(outlet, published_date) |>
    dplyr::mutate(article_id = sprintf("%s_%05d", tolower(gsub("[^A-Za-z]", "", outlet)),
                                       dplyr::row_number()), .by = outlet) |>
    dplyr::relocate(article_id, outlet, language, published_date, headline, authors, content, url,
                    matched_keywords, relevant_strict)
  saveRDS(df, out_file("myanmar_migrants_news.rds"))
  # write_excel_csv adds a UTF-8 byte-order mark so Excel shows Thai correctly.
  readr::write_excel_csv(df, out_file("myanmar_migrants_news.csv"))
  message(sprintf("Saved %d articles to %s", nrow(df), out_file("myanmar_migrants_news.{csv,rds}")))
  print(dplyr::count(df, outlet, year = format(published_date, "%Y")) |>
          tidyr::pivot_wider(names_from = year, values_from = n, values_fill = 0))
  invisible(df)
}

# Quick health check: for each outlet, can we reach it and find a few candidates?
diagnose_sites <- function(outlet_ids = names(OUTLETS)) {
  old <- CONFIG$max_search_pages; CONFIG$max_search_pages <<- 1
  on.exit(CONFIG$max_search_pages <<- old)
  for (id in outlet_ids) {
    o <- OUTLETS[[id]]
    message("\n### ", o$name)
    cand <- tryCatch(o$discover(o), error = function(e) { message("error: ", conditionMessage(e)); empty_candidates() })
    message("candidates found: ", nrow(cand))
    if (nrow(cand)) {
      u <- cand$url[1]; doc <- fetch_html(u, o$id)
      if (!is.null(doc)) { r <- extract_article(doc, u, o)
        message("sample: ", r$headline, " | ", r$published_date, " | ", r$authors,
                " | body chars: ", nchar(r$content %||% "")) }
    }
  }
}
