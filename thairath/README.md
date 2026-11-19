# Thairath only: news on Myanmar migrants, 2017–2025

A standalone R scraper for **Thairath** (www.thairath.co.th). It does not use
the multi-outlet code in `R/`, so it can be run and checked on its own.

## Run it

Needs R 4.2 or newer. Open a terminal in the repository folder (the folder
that contains `thairath/`), then:

```
Rscript thairath/run_thairath.R --probe     # 2–3 min: checks each route, prints 3 parsed articles
Rscript thairath/run_thairath.R --test      # ~10 min: 5 pages per tag, 20 articles -> thairath/output_test/
Rscript thairath/run_thairath.R             # full run -> thairath/output/
```

In RStudio or Positron: set the working directory to the repository folder
and `source("thairath/run_thairath.R")` (that does the full run). Missing
packages are installed automatically the first time.

Options: `--methods=sitemap,tags` (choose routes), `--max=200` (cap articles),
`--refresh` (rebuild the list of article links instead of reusing it).

You can stop the full run at any point and start it again: every page is
cached in `thairath/cache/` and finished articles are skipped.

## How articles are found

| Route | What it is | Notes |
|---|---|---|
| `sitemap` | Thairath's monthly sitemaps (`sitemap-monthly-YYYY-MM.xml`) | Only about 80 articles per month, so this sees a small share of Thairath's output. No sitemap exists for Jan–Oct 2017. Headlines are checked for keywords before downloading. |
| `tags` | Topic pages such as `/tags/แรงงานต่างด้าว?page=2` | Plain HTML with links and dates, newest first. Each tag is walked page by page until it reaches 2016 or stops giving new links. Likely the main source. |
| `search` | The site search `/search?q=…` | Drawn by JavaScript, so it only runs if the `chromote` package and Google Chrome are installed. It also logs the data requests the search page makes (`output/search_network_log.txt`), which could become a faster route. |

Tags and search words are in the `TR` settings at the top of
`thairath/thairath_scraper.R`, together with the dates and the keyword lists.

## Output (one row per article)

* `thairath/output/thairath_myanmar_migrants.csv` — articles in the period with at least one keyword
* `thairath/output/thairath_all_articles.csv` — every article downloaded in the period
* `candidates.csv` — every link found and which route found it; `failed_urls.txt` — pages that did not load

| Column | Meaning |
|---|---|
| `article_id` | `thairath_00001` …, ordered by date |
| `published_date`, `year`, `published_time` | date in Bangkok time, and the raw timestamp |
| `headline`, `authors`, `section`, `content` | `content` is the full Thai text, one paragraph per line. Thairath often credits only "ไทยรัฐออนไลน์", and some pages name no author (left empty). |
| `url`, `tags`, `found_via` | `tags` are Thairath's own keywords for the article |
| `matched_keywords`, `n_keywords` | the keywords found in headline + body |
| `kw_myanmar`, `kw_migration`, `kw_specific`, `relevant_strict` | `relevant_strict` = a specific term (e.g. แรงงานเมียนมา, โรฮิงญา) or a Myanmar term together with a migration term. Use it to drop general Myanmar news (coup, earthquake, football …). |

The CSV files carry a UTF-8 byte-order mark so Excel shows Thai text
correctly. In R: `readr::read_csv("thairath/output/thairath_myanmar_migrants.csv")`.

## Tests

`Rscript thairath/tests/test_thairath_offline.R` runs the whole pipeline on
saved sample pages (no internet needed).
