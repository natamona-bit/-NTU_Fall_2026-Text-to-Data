# ---------------------------------------------------------------------------
# run_gdelt.R — build the Myanmar-migrants news dataset from GDELT instead of
# scraping the sites. Same outlets, dates and keywords as run_scrape.R.
#
#   Rscript run_gdelt.R --coverage          # cheap: GDELT records per outlet and year
#   Rscript run_gdelt.R --estimate          # free dry run: GB each yearly query reads
#   Rscript run_gdelt.R                     # query 2017-2025 year by year, build dataset
#   Rscript run_gdelt.R --years 2017:2019   # only some years (e.g. to spread cost)
#   Rscript run_gdelt.R --fulltext          # also download article text (strict matches)
#   Rscript run_gdelt.R --recent            # DOC API, last 3 months (after the study period)
#
# First set your Google Cloud project, e.g. in R:
#   Sys.setenv(GDELT_BILLING_PROJECT = "my-project-id")
# or in a terminal before running:  set GDELT_BILLING_PROJECT=my-project-id   (Windows)
# See GDELT.md for setup.
# ---------------------------------------------------------------------------

pkgs <- c("httr2", "rvest", "xml2", "jsonlite", "dplyr", "purrr", "stringr", "stringi",
          "lubridate", "readr", "tibble", "tidyr", "digest", "bigrquery")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)

source("R/pipeline.R", encoding = "UTF-8")
load_gdelt(".")

args <- commandArgs(trailingOnly = TRUE)
years <- NULL
if ("--years" %in% args) years <- eval(parse(text = args[which(args == "--years") + 1]))

if ("--coverage" %in% args) {
  print(gdelt_coverage(), n = Inf, width = Inf)
  message("Saved to ", gdelt_path("output", "gdelt_coverage_by_outlet_year.csv"))
  quit(save = "no")
}
if ("--estimate" %in% args) { print(gdelt_estimate(years), n = Inf); quit(save = "no") }
if ("--recent" %in% args) {
  rec <- gdelt_doc_recent()
  gdelt_save(dplyr::mutate(rec, relevant_strict = title_relevant_strict), "gdelt_doc_recent")
  quit(save = "no")
}

raw <- gdelt_fetch_gkg(years)
news <- gdelt_tidy(raw)
if ("--fulltext" %in% args) news <- gdelt_add_fulltext(news, only_strict = TRUE)
gdelt_save(news)
