# ---------------------------------------------------------------------------
# run_scrape.R — entry point.
#
#   Rscript run_scrape.R                    # all outlets
#   Rscript run_scrape.R thaipbs nation     # selected outlets
#   Rscript run_scrape.R --test             # 20 articles per outlet, quick check
#   Rscript run_scrape.R --diagnose         # check every site responds
#
# Or open in RStudio, set the working directory to this folder and source().
# Safe to stop and re-run: pages are cached and finished articles are skipped.
# ---------------------------------------------------------------------------

pkgs <- c("httr2", "rvest", "xml2", "jsonlite", "dplyr", "purrr", "stringr", "stringi",
          "lubridate", "readr", "tibble", "tidyr", "digest")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)

source("R/pipeline.R", encoding = "UTF-8")
load_scraper(".")

args <- commandArgs(trailingOnly = TRUE)
if ("--test" %in% args) CONFIG$max_articles_per_outlet <- 20
if ("--diagnose" %in% args) { diagnose_sites(); quit(save = "no") }

ids <- setdiff(args, c("--test", "--diagnose"))
if (!length(ids)) ids <- names(OUTLETS)

for (id in ids) {
  tryCatch(scrape_outlet(OUTLETS[[id]]),
           error = function(e) message("!! ", id, " stopped: ", conditionMessage(e)))
}

build_dataset(ids, keep = "any")
