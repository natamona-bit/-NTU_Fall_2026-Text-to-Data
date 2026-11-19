# Myanmar migrants in Thai news, from GDELT

`run_gdelt.R` builds the same sample as the web scraper (`run_scrape.R`) from
the [GDELT Project](https://www.gdeltproject.org/) instead of from the news
sites themselves:

* **same outlets**: Thairath, Dailynews, Matichon, Khaosod, Thai PBS, Bangkok Post, The Nation
* **same period**: 2017-01-01 to 2026-01-01 (`R/config.R`)
* **same keywords**: the Thai and English lists in `R/keywords.R`

## Which part of GDELT, and why

| GDELT source | Reaches 2017–2025? | Used here |
|---|---|---|
| DOC 2.0 API (article search) | **No.** Article lists only search the most recent ~3 months | only `--recent`, to follow coverage after the study period |
| GKG raw files (15-minute CSV dumps) | Yes, from 2015 | no: about 300,000 files for 2017–2025, terabytes to download |
| **GKG on Google BigQuery** | Yes, from 2015 | **yes**: one SQL query per year returns only the matching articles |

GDELT's Global Knowledge Graph (GKG) has one record for every news article
GDELT processed, including Thai-language articles (GDELT machine-translates
them). Each record has the article URL, outlet, date, page title, author,
the places mentioned, topic "themes" and tone. The query keeps articles from
the seven outlets that

* mention a place in Myanmar (GKG location country code `BM`), or
* carry a Myanmar-related theme (Rohingya, Burmese, Karen ...), or
* have one of the study's keywords in the headline (Thai or English).

GKG has **no article text**. The optional `--fulltext` step downloads the
pages GDELT points to, with the web scraper's polite downloader.

## One-time setup

1. **R packages**: `run_gdelt.R` installs any that are missing (it adds
   `bigrquery` to the scraper's packages). For Parquet output also
   `install.packages("arrow")` (or `nanoparquet`).
2. **A Google Cloud project** for BigQuery to bill queries to. GDELT's data is
   public, but you pay for (or use your free allowance on) the queries.
   * Go to <https://console.cloud.google.com/bigquery>, sign in with a Google
     account and create a project. With no billing account it runs as a
     **BigQuery sandbox**: free, 1 TB of queries per month.
   * Note the **project ID** (e.g. `myanmar-news-123456`).
3. Tell R the project ID, either in `R/gdelt_config.R` (`billing_project`) or
   before running:
   ```r
   Sys.setenv(GDELT_BILLING_PROJECT = "myanmar-news-123456")
   ```
   In a Windows terminal: `set GDELT_BILLING_PROJECT=myanmar-news-123456`.
4. The first query opens a browser window to sign in to Google
   (`bigrquery` remembers it afterwards).

## Running it

Run these from this folder, in order:

```sh
Rscript run_gdelt.R --coverage    # how many GDELT records each outlet has, per year (cheap)
Rscript run_gdelt.R --estimate    # free dry run: GB each yearly query would read
Rscript run_gdelt.R               # query 2017-2025, build the dataset
Rscript run_gdelt.R --fulltext    # optional: add article text, authors, dates from the sites
```

Or in RStudio:

```r
source("R/pipeline.R", encoding = "UTF-8"); load_gdelt(".")
Sys.setenv(GDELT_BILLING_PROJECT = "myanmar-news-123456")
gdelt_coverage()                     # check GDELT actually covers each outlet
gdelt_estimate()                     # cost check, nothing is charged
raw  <- gdelt_fetch_gkg()            # or gdelt_fetch_gkg(years = 2017:2019)
news <- gdelt_tidy(raw)
gdelt_save(news)
```

**Start with `--coverage`.** GDELT does not monitor every outlet equally, and
some years may be thin. The result is saved to
`data/gdelt/output/gdelt_coverage_by_outlet_year.csv`.

### Cost

BigQuery charges by the columns a query reads across the dates it covers, not
by how many rows match, so each yearly query costs roughly the same whatever
the keywords. The script:

* runs a free **dry run** before every query and prints how many GB it reads,
* **refuses** any query above `max_gb_per_query` (1,000 GB by default, in
  `R/gdelt_config.R`),
* saves each finished year to `data/gdelt/raw/gkg_<year>.rds`, so a year is
  never queried (or paid for) twice.

If `--estimate` shows more than the 1 TB monthly free allowance in total, run
a few years per month with `--years`, e.g. `Rscript run_gdelt.R --years 2017:2019`.

## Output

`data/gdelt/output/gdelt_myanmar_migrants.csv` (UTF-8 with BOM for Excel),
`.rds`, and `.parquet` if `arrow` or `nanoparquet` is installed. One row per
article (the same article seen under several URLs is kept once):

| column | meaning |
|---|---|
| `article_id` | e.g. `gdelt_thairath_00042` |
| `outlet`, `language` | outlet; `th` or `en` (original language of the article) |
| `published_date` | publication date (Bangkok time) from the page when GDELT recorded it, otherwise the day GDELT first saw the article; `date_source` says which |
| `headline`, `authors`, `url` | page title, author(s) as GDELT recorded them, link |
| `relevant_strict` | a specific migrant term in the headline, **or** a Myanmar signal **and** a migration signal (see below) |
| `myanmar_signal` | Myanmar location, Myanmar-related theme, or Myanmar keyword in the headline |
| `migration_signal` | migration/refugee/trafficking theme, or migration keyword in the headline |
| `myanmar_location`, `theme_myanmar`, `theme_migration` | the GDELT signals separately |
| `title_matched_keywords`, `title_kw_*` | the study's keyword lists applied to the headline |
| `themes`, `locations`, `location_countries` | GDELT themes and places mentioned (`; `-separated) |
| `tone` … `word_count` | GDELT tone scores (`tone` < 0 is negative coverage) and article length |
| `gdelt_seen_at`, `seen_date`, `gkg_record_id`, `domain` | GDELT bookkeeping |

With `--fulltext`, these are added for `relevant_strict` articles:
`fulltext_ok`, `site_headline`, `site_authors`, `site_published_date`,
`content` (full text), and `text_*` keyword flags computed on the full text
with the same rules as the web scraper.

```r
library(dplyr)
news <- readRDS("data/gdelt/output/gdelt_myanmar_migrants.rds")
core <- filter(news, relevant_strict)
count(core, outlet, year = format(published_date, "%Y"))
```

## Limitations to keep in mind

* **Coverage depends on GDELT.** It only has articles its crawlers found. Use
  `--coverage` and compare with the scraper where both work.
* **Relevance is judged on metadata, not text.** Without `--fulltext`,
  keywords are matched on the headline only, plus GDELT's places and themes
  (which GDELT computes from the machine-translated text for Thai outlets).
  An article that mentions Myanmar migrants only in passing may be missed,
  and "Myanmar location" alone also catches general Myanmar news, which is
  why `relevant_strict` needs both signals.
* **Dates**: when GDELT did not record the page's publication time, the date
  is when GDELT first saw the article (usually within hours of publication).
* **Full text** comes from the news sites, so the site notes in `README.md`
  apply (e.g. Bangkok Post may refuse automated requests). It must run on a
  computer that can reach the sites.

## Recent coverage (after the study period)

`Rscript run_gdelt.R --recent` asks the DOC 2.0 API for the last 90 days,
one outlet at a time, with English terms (the API searches GDELT's English
translation of Thai articles). It waits 6 seconds between calls, as GDELT
asks, and saves `data/gdelt/output/gdelt_doc_recent.{csv,rds}`.

## Tests

`Rscript tests/test_gdelt_offline.R` checks the SQL, the parsing of GKG
records, de-duplication, the date window, the cost limit and caching, and the
DOC API helpers, on sample records (no internet or Google account needed).
