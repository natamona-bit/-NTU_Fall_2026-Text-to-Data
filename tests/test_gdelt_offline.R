# Offline tests for the GDELT route: Rscript tests/test_gdelt_offline.R (from the project root)
library(testthat)
source("R/pipeline.R", encoding = "UTF-8"); load_gdelt(".")
fx <- function(f) file.path("tests", "fixtures", f)
raw <- readr::read_csv(fx("gkg_sample.csv"), col_types = readr::cols(.default = "c"))

test_that("SQL reads only the needed columns and filters outlets, period and topic", {
  sql <- gdelt_gkg_sql(as.Date("2017-01-01"), as.Date("2018-01-01"))
  expect_match(sql, "_PARTITIONTIME >= TIMESTAMP('2017-01-01')", fixed = TRUE)
  expect_match(sql, "_PARTITIONTIME < TIMESTAMP('2018-01-01')", fixed = TRUE)
  expect_match(sql, "thairath\\.co\\.th", fixed = TRUE)
  expect_match(sql, "#BM#", fixed = TRUE)
  expect_match(sql, "PAGE_PRECISEPUBTIMESTAMP", fixed = TRUE)
  expect_match(sql, "เมียนมา", fixed = TRUE)
  expect_false(grepl("SELECT *", sql, fixed = TRUE))
  expect_false(grepl("V2Persons|V2Organizations|GCAM", sql))
  # the domain regex picks outlets and subdomains only
  rx <- gdelt_domain_regex()
  expect_true(all(grepl(rx, c("thairath.co.th", "m.khaosod.co.th", "nationthailand.com"), perl = TRUE)))
  expect_false(any(grepl(rx, c("notthairath.co.th", "bangkokpost.com.evil.net"), perl = TRUE)))
  # the title regex is valid and catches Thai and English keywords
  trx <- sub("^\\(\\?i\\)", "", gdelt_title_regex())
  expect_true(all(grepl(trx, c("แรงงานเมียนมา", "Rohingya boat", "MIGRANT workers"), perl = TRUE, ignore.case = TRUE)))
  expect_false(grepl(trx, "Monday market report", perl = TRUE, ignore.case = TRUE))
})

test_that("year chunks cover 2017-01-01 to 2026-01-01", {
  ch <- gdelt_year_chunks()
  expect_equal(vapply(ch, `[[`, "", "label"), as.character(2017:2026))
  expect_equal(ch[[1]]$from, as.Date("2017-01-01"))
  expect_equal(ch[[10]]$to_exclusive, as.Date("2026-01-02"))
  expect_length(gdelt_year_chunks(years = 2019:2020), 2)
})

test_that("GKG rows become one tidy row per article", {
  df <- gdelt_tidy(raw)
  # duplicate URL (tracking parameter) collapsed; 2016 article, example.com dropped;
  # 2025-12-31 19:00 UTC is 2026-01-01 in Bangkok, so it stays in.
  expect_equal(nrow(df), 4)
  expect_setequal(df$outlet, c("Thairath", "Bangkok Post", "Khaosod", "Matichon"))
  tr <- df[df$outlet == "Thairath", ]
  expect_equal(tr$gkg_record_id, "20210315030000-T12")        # first sighting kept
  expect_equal(tr$language, "th")
  expect_equal(tr$seen_date, as.Date("2021-03-15"))
  expect_equal(tr$themes, "REFUGEES; TAX_FNCACT_REFUGEES; TAX_ETHNICITY_BURMESE")
  expect_equal(tr$locations, "Myanmar; Mae Sot, Tak, Thailand")
  expect_equal(tr$tone, -5.2); expect_equal(tr$word_count, 412L)
  expect_true(tr$myanmar_location); expect_true(tr$relevant_strict)
  bp <- df[df$outlet == "Bangkok Post", ]
  expect_equal(bp$headline, "Myanmar migrant workers rush to renew permits & visas")
  expect_equal(bp$language, "en")
  expect_equal(bp$authors, "Online Reporters")
  expect_equal(bp$published_date, as.Date("2019-07-01"))       # page timestamp, Bangkok time
  expect_equal(bp$date_source, "page_timestamp")
  expect_equal(tr$date_source, "gdelt_seen")
  expect_true(bp$title_kw_specific); expect_true(bp$relevant_strict)
  ks <- df[df$outlet == "Khaosod", ]
  expect_match(ks$headline, "^พม่า")                            # numeric entities decoded
  expect_true(ks$myanmar_signal); expect_false(ks$relevant_strict) # football: not migration
  mc <- df[df$outlet == "Matichon", ]
  expect_equal(mc$seen_date, as.Date("2026-01-01"))
  expect_true(mc$relevant_strict)                                # title: แรงงานเมียนมา
  expect_true(all(grepl("^gdelt_[a-z]+_\\d{5}$", df$article_id)))
})

test_that("yearly queries are cost-checked and cached", {
  GDELT_CONFIG$dir <<- file.path(tempdir(), "gdelt_test")
  calls <- 0
  gdelt_bq_dry_run <<- function(sql) if (grepl(">= TIMESTAMP('2018-01-01')", sql, fixed = TRUE)) 5e12 else 50e9
  gdelt_bq_fetch   <<- function(sql) { calls <<- calls + 1; raw[1:3, ] }
  out <- gdelt_fetch_gkg(years = 2017:2018)
  expect_equal(calls, 1)                                         # 2018 refused: 5000 GB > limit
  expect_true(file.exists(gdelt_path("raw", "gkg_2017.rds")))
  expect_false(file.exists(gdelt_path("raw", "gkg_2018.rds")))
  gdelt_fetch_gkg(years = 2017)
  expect_equal(calls, 1)                                         # served from cache
  df <- gdelt_tidy(out); gdelt_save(df)
  expect_true(file.exists(gdelt_path("output", "gdelt_myanmar_migrants.csv")))
})

test_that("DOC API helpers build queries and parse results", {
  q <- gdelt_doc_query("thairath.co.th")
  expect_equal(q, "domainis:thairath.co.th (Myanmar OR Burmese OR Burma OR Rohingya OR Karen)")
  u <- gdelt_doc_url(q, as.POSIXct("2026-10-01", tz = "UTC"), as.POSIXct("2026-10-08", tz = "UTC"))
  expect_match(u, "mode=artlist"); expect_match(u, "startdatetime=20261001000000")
  expect_match(u, "maxrecords=250")
  a <- gdelt_doc_parse(readr::read_file(fx("gdelt_doc_sample.json")))
  expect_equal(nrow(a), 2)
  expect_equal(format(a$gdelt_seen_at[1], "%Y-%m-%d %H:%M", tz = "UTC"), "2026-10-20 03:15")
  expect_equal(nrow(gdelt_doc_parse('{"articles": []}')), 0)
  expect_equal(nrow(gdelt_doc_parse("{}")), 0)
})

test_that("recent DOC API results are tidied and saved", {
  GDELT_CONFIG$dir <<- file.path(tempdir(), "gdelt_test_doc")
  sample <- gdelt_doc_parse(readr::read_file(fx("gdelt_doc_sample.json")))
  gdelt_doc_call <<- function(url) if (grepl("bangkokpost|thairath", url)) sample[grepl(sub(".*domainis%3A([a-z.]+).*", "\\1", url), sample$domain), ] else tibble::tibble()
  rec <- gdelt_doc_recent(days = 7, window_days = 7)
  expect_equal(nrow(rec), 2)
  expect_setequal(rec$outlet, c("Bangkok Post", "Thairath"))
  expect_true(all(rec$title_kw_specific))
  gdelt_save(dplyr::mutate(rec, relevant_strict = title_relevant_strict), "gdelt_doc_recent")
  expect_true(file.exists(gdelt_path("output", "gdelt_doc_recent.csv")))
})

test_that("full text is joined without clobbering GDELT columns", {
  GDELT_CONFIG$dir <<- file.path(tempdir(), "gdelt_test_ft")
  df <- gdelt_tidy(raw)
  fetch_html <<- function(url, outlet, ...) xml2::read_html(fx("article_jsonld.html"), encoding = "UTF-8")
  out <- gdelt_add_fulltext(df, only_strict = TRUE)
  expect_equal(nrow(out), nrow(df))
  expect_true(all(c("authors", "published_date", "site_authors", "content", "text_relevant_strict") %in% names(out)))
  expect_true(all(out$fulltext_ok[out$relevant_strict]))
  expect_true(all(is.na(out$content[!out$relevant_strict])))
})
