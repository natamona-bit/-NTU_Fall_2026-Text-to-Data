# Offline tests: Rscript tests/test_offline.R  (from the project root)
library(testthat)
source("R/pipeline.R", encoding = "UTF-8"); load_scraper(".")
fx <- function(f) file.path("tests", "fixtures", f)
o_th <- list(id = "t", name = "Test TH", language = "th", selectors = list())
o_en <- list(id = "t", name = "Test EN", language = "en", selectors = list(body = ".article-content", date = ".date"))

test_that("JSON-LD article is extracted", {
  doc <- xml2::read_html(fx("article_jsonld.html"), encoding = "UTF-8")
  r <- extract_article(doc, "https://ex.com/a", o_th)
  expect_equal(r$headline, "ผู้ลี้ภัยเมียนมาหนีการสู้รบข้ามแดนเข้าแม่สอด")
  expect_equal(r$published_date, as.Date("2021-12-21"))      # converted to Bangkok time
  expect_equal(r$authors, "สมชาย ใจดี; Jane Doe")
  expect_match(r$content, "แม่น้ำเมย")
  expect_match(r$content, "ผู้หญิงและเด็ก")
  expect_false(grepl("แชร์ Facebook|var ad", r$content))
  s <- score_article(r$headline, r$content, "th")
  expect_true(s$relevant_strict)
  expect_match(s$matched_keywords, "ผู้ลี้ภัย")
})

test_that("Thai Buddhist-era dates and English keywords", {
  expect_equal(parse_pub_date("24 มิ.ย. 2564"), as.Date("2021-06-24"))
  expect_equal(parse_pub_date("๕ มกราคม ๒๕๖๐"), as.Date("2017-01-05"))
  doc <- xml2::read_html(fx("article_thaidate.html"), encoding = "UTF-8")
  r <- extract_article(doc, "https://ex.com/b", o_en)
  expect_equal(r$published_date, as.Date("2021-06-24"))
  expect_equal(r$headline, "Myanmar migrant workers rush to renew permits")
  expect_equal(r$authors, "Online Reporters")
  s <- score_article(r$headline, r$content, "en")
  expect_true(s$kw_specific); expect_true(s$relevant_strict)
  expect_false(score_article("Monday market report", "Shares rose on Monday", "en")$kw_myanmar)
})

test_that("sitemap period filter and title prefilter", {
  expect_false(sitemap_in_period("https://ex.com/sitemap/news-2016-12.xml"))
  expect_true(sitemap_in_period("https://ex.com/sitemap/news-2017-01.xml"))
  expect_true(sitemap_in_period("https://ex.com/sitemap/xml/2025/sitemap_2025_12.xml"))
  expect_false(sitemap_in_period("https://ex.com/sitemap/news-2026-02.xml"))
  expect_true(sitemap_in_period("https://ex.com/post-sitemap12.xml"))
  sm <- parse_sitemap(readr::read_file(fx("sitemap_news.xml")))
  expect_equal(nrow(sm$urls), 3)
  expect_equal(title_is_candidate(sm$urls$title, "th"), c(TRUE, FALSE, TRUE))
})

test_that("robots.txt rules", {
  .robots_cache[["ex.com"]] <- data.frame(type = c("disallow", "allow", "disallow"),
                                          path = c("/search", "/search/ok", "/*.php$"))
  expect_false(robots_allowed("https://ex.com/search?q=x"))
  expect_true(robots_allowed("https://ex.com/search/ok"))
  expect_false(robots_allowed("https://ex.com/a/b.php"))
  expect_true(robots_allowed("https://ex.com/news/1"))
})
