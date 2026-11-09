# Thai news on Myanmar migrants, 2017–2025: R scraper

Collects news articles about Myanmar migrants and refugees from seven Thai
outlets and saves them as one tidy table (one row per article).

| Outlet | Language | How articles are found |
|---|---|---|
| Thairath | Thai | site sitemap; optional site search via headless Chrome |
| Dailynews | Thai | WordPress API search → site search (`?s=`) |
| Matichon | Thai | WordPress API search → site search (`?s=`) |
| Khaosod | Thai | WordPress API search → site search (`?s=`) |
| Thai PBS | Thai | monthly news sitemaps (they include headlines) |
| Bangkok Post | English | search.bangkokpost.com |
| The Nation | English | monthly news sitemaps (they include headlines) |

Bangkok Post and The Nation publish in English, so they are searched and
filtered with the English keyword list in `R/keywords.R`.

## Quick start

1. Install R (≥ 4.2) and, ideally, RStudio.
2. Open this folder in RStudio (or `cd` into it in a terminal).
3. Check that every site answers:

   ```r
   # in the R console
   source("R/pipeline.R", encoding = "UTF-8"); load_scraper("."); diagnose_sites()
   ```

   or in a terminal: `Rscript run_scrape.R --diagnose`.

   For a first try, use `Rscript run_scrape.R --test` (20 articles per outlet).
4. Full run: `Rscript run_scrape.R`, or a subset with `Rscript run_scrape.R thaipbs nation`.

A full run takes a long time (one request every 2 seconds per site, by design).
You can stop it at any point and start it again: downloaded pages are cached in
`data/cache/` and finished articles are skipped.

## Output

`data/output/myanmar_migrants_news.csv` (UTF-8 with BOM, so Excel shows Thai
correctly) and `.rds` (for R), one row per article:

| column | meaning |
|---|---|
| `article_id` | e.g. `thairath_00042` |
| `outlet`, `language` | news outlet; `th` or `en` |
| `published_date` | publication date (Bangkok time; Thai Buddhist-era dates converted) |
| `headline`, `authors`, `content` | headline, author(s) separated by `; `, full article text (one paragraph per line) |
| `url`, `section`, `tags` | article link, site section, the site's own tags |
| `matched_keywords`, `n_keywords` | which keywords occur in headline + body |
| `kw_myanmar`, `kw_migration`, `kw_specific`, `relevant_strict` | relevance flags (see below) |
| `found_via`, `scraped_at` | how the URL was found, when it was downloaded |

Intermediate files per outlet: `candidates_<outlet>.csv` (URLs found),
`articles_<outlet>.rds` (everything downloaded, before keyword filtering),
`failed_<outlet>.txt` (URLs that could not be downloaded).

### Relevance

Words such as พม่า / เมียนมา alone also match general Myanmar news (the coup,
earthquakes, football). The final file keeps every article containing **any**
keyword, and adds flags so you can tighten the sample at analysis time:

* `kw_specific`: a specific term such as แรงงานเมียนมา, ผู้ลี้ภัยชาวพม่า, โรฮิงญา, "Myanmar migrants"
* `relevant_strict`: a specific term, **or** a Myanmar term together with a migration term

```r
library(dplyr)
news <- readRDS("data/output/myanmar_migrants_news.rds")
core <- filter(news, relevant_strict)
```

To rebuild the final file with only strict matches: `build_dataset(keep = "strict")`.

## Settings

* `R/config.R`: dates (2017-01-01 to 2026-01-01), delay between requests, your
  contact e-mail in the user agent (please fill it in), and
  `sitemap_prefilter`.
* `R/keywords.R`: Thai and English keyword lists, and the shorter lists sent
  to site search boxes.
* `R/outlets.R`: per-outlet discovery and CSS selectors.

`sitemap_prefilter = "title"` (default) means that for sitemap-based outlets
(Thairath, Thai PBS, The Nation) only articles whose **headline** contains a
keyword are downloaded. Articles that mention migrants only in the body are
missed. Set it to `"none"` to download every article in the period and filter
on the full text; that is complete but means hundreds of thousands of requests.

## Respectful scraping, and site notes (checked 2026-11-09)

* robots.txt is read for every site and disallowed paths are skipped.
* One request every 2 seconds per site, with back-off on errors and 429s.
* Every page is cached, so nothing is downloaded twice.

Site-specific notes:

* **Thai PBS** and **The Nation**: robots.txt disallows `/search`, so the
  scraper uses their public news sitemaps instead. The Thai PBS sitemap index
  only lists months up to 2024-12; the script requests 2025 months directly
  and reports if they don't exist.
* **Thairath**: the search page is drawn by JavaScript. The scraper uses the
  sitemap; to also use site search, `install.packages("chromote")` (needs Chrome).
* **Dailynews**: robots.txt disallows `/search/*` and `/tags/`; the scraper uses
  the WordPress API and `?s=` search instead, which are allowed.
* **Matichon, Khaosod**: their WordPress API refused automated requests during
  checks, so the scraper falls back to the `?s=` search pages automatically.
* **Bangkok Post**: bot access is managed by TollBit (a paid-licensing gateway
  for AI/bot traffic). Requests may be refused with HTTP 402/403. If so, the
  log shows it; consider asking Bangkok Post for research access, or use a
  licensed archive (e.g. Factiva / ProQuest) for this outlet.

These checks were made from a cloud machine that could not reach the sites
directly, so the CSS selectors are best guesses backed by generic JSON-LD and
meta-tag extraction. Run `--diagnose` first: it prints a sample headline, date,
author and body length for each outlet.

## Tests

`Rscript tests/test_offline.R` checks extraction, Thai date parsing, keyword
matching, sitemap filtering and robots.txt handling on local sample pages.
