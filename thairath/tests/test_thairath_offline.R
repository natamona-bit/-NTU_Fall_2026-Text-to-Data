# Offline tests for the Thairath scraper. No internet needed: page downloads
# are replaced by the files in tests/fixtures, which copy the structure of
# the live site (checked 2026-11-19).
#   Rscript thairath/tests/test_thairath_offline.R      (from the repo root)

source("thairath/thairath_scraper.R", encoding = "UTF-8")
fx <- function(f) readr::read_file(file.path("thairath/tests/fixtures", f))
ok <- function(cond, what) { if (!isTRUE(cond)) stop("FAILED: ", what, call. = FALSE); cat("ok -", what, "\n") }

tmp <- tempfile("thairath_test_"); dir.create(tmp)
TR$output_dir <- file.path(tmp, "output"); TR$cache_dir <- file.path(tmp, "cache")
TR$tags <- "แรงงานต่างด้าว"; TR$methods <- c("sitemap", "tags")

requested <- character()
fetch_text <- function(url, kind = "pages", ext = "html", use_cache = TRUE) {
  requested <<- c(requested, url)
  if (url == "https://www.thairath.co.th/sitemap.xml") return(fx("sitemap_index.xml"))
  if (grepl("sitemap-monthly", url)) return(fx("sitemap_monthly.xml"))
  if (grepl("/tags/", url) && !grepl("page=", url)) return(fx("tag_page.html"))
  if (grepl("/tags/.*page=2$", url)) return(fx("tag_page_old.html"))
  if (grepl("/tags/", url)) stop("walked past the end of the period: ", url)
  if (grepl("/2961344$", url)) return(fx("article_jsonld.html"))
  if (grepl("/2972621$", url)) return(gsub("2019-06-01", "2026-11-15", fx("article.html")))   # after the period
  if (grepl("/\\d+$", url)) return(fx("article.html"))
  NULL
}

# --- dates
ok(parse_pub_date("15 พ.ย. 2569 13:42 น.") == as.Date("2026-11-15"), "Thai short date, BE year")
ok(parse_pub_date("5 มกราคม 2560") == as.Date("2017-01-05"), "Thai full month name")
ok(parse_pub_date("2019-06-01T00:18:00+07:00") == as.Date("2019-06-01"), "ISO date in Bangkok time")
ok(parse_pub_date("2023-12-31T17:30:00Z") == as.Date("2024-01-01"), "UTC converted to Bangkok date")
ok(identical(find_thai_dates("a 3 ธ.ค. 2559 b 28 ธ.ค. 2559"), as.Date(c("2016-12-03", "2016-12-28"))), "all dates in text")

# --- URLs
u <- clean_article_urls(c("/news/local/2961344", "https://www.thairath.co.th/newspaper/column/category/local/2966856?x=1",
                          "/tags/abc", "/video/clip/123456", "https://www.thairath.co.th/horoscope/daily/2861761",
                          "/news/local/2961344#top"))
ok(identical(u, c("https://www.thairath.co.th/news/local/2961344",
                  "https://www.thairath.co.th/newspaper/column/category/local/2966856")), "article URL cleaning")
ok(section_from_url("https://www.thairath.co.th/news/local/northeast/2950001") == "news/local/northeast", "section from URL")

# --- sitemaps
sm <- discover_sitemaps()
ok(sum(grepl("sitemap-monthly", requested)) == 3, "only monthly sitemaps inside the period are read")
ok(!any(grepl("2026-11|hourly|women", requested)), "out-of-period and non-monthly sitemaps skipped")
ok(nrow(sm) == 1 && sm$url == "https://www.thairath.co.th/news/local/1581000", "headline filter keeps the migrant story only")

# --- tags
tg <- discover_tags()
ok(nrow(tg) == 6, "tag pages: links from HTML and from page data, both pages")
ok("https://www.thairath.co.th/news/local/northeast/2950001" %in% tg$url, "link found inside __NEXT_DATA__")
ok(tg$title[tg$url == "https://www.thairath.co.th/news/local/2961344"] == "ตรวจแรงงานเมียนมา", "link title kept")
ok(!any(grepl("page=3", requested)), "stops once a page is older than the period")

# --- extraction
a <- extract_article(fx("article.html"), "https://www.thairath.co.th/news/local/1581000")
ok(a$headline == "ตำรวจจับแรงงานชาวเมียนมาลักลอบเข้าเมือง 30 คน", "headline without site name")
ok(a$published_date == as.Date("2019-06-01"), "published date from meta")
ok(grepl("ผลักดันกลับ", a$content) && !grepl("ไม่ควรติดมา|Copyright", a$content), "body text without related/footer")
ok(a$tags == "แรงงานเมียนมา,ลักลอบเข้าเมือง", "tags from meta keywords")
b <- extract_article(fx("article_jsonld.html"), "https://www.thairath.co.th/news/local/2961344")
ok(b$authors == "สมชาย ใจดี; ไทยรัฐออนไลน์", "authors from JSON-LD")
ok(b$published_date == as.Date("2023-12-31") && grepl("พื้นที่พักพิงชั่วคราว", b$content), "JSON-LD date and body")

s <- score_article(a$headline, a$content)
ok(s$relevant_strict && s$kw_specific && grepl("แรงงานชาวเมียนมา", s$matched_keywords), "keyword scoring")
ok(!score_article("ฉุนพ่อเลี้ยง", "ไม่ให้เงิน")$relevant_strict, "unrelated article not relevant")

# --- whole pipeline
cand <- get_candidates(TR$methods, refresh = TRUE)
ok(nrow(cand) == 7, "candidates combined across routes")
raw <- scrape_articles(cand)
hits <- build_dataset()
out <- readr::read_csv(file.path(TR$output_dir, "thairath_myanmar_migrants.csv"), show_col_types = FALSE)
ok(nrow(out) == nrow(hits) && nrow(out) == 6, "CSV written, one row per article in the period")
ok(all(c("article_id", "published_date", "headline", "authors", "content", "url", "relevant_strict") %in% names(out)),
   "CSV has the analysis columns")
cat("\nAll offline tests passed.\n")
