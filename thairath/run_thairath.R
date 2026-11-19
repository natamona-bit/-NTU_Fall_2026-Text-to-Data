# ===========================================================================
# run_thairath.R — scrape Thairath articles on Myanmar migrants.
#
# Run from the repository root (the folder that contains thairath/):
#
#   Rscript thairath/run_thairath.R --probe    # 2-minute check that each route works
#   Rscript thairath/run_thairath.R --test     # 5 pages per tag, 20 articles (~10 min)
#   Rscript thairath/run_thairath.R            # everything
#
# Options (combine freely):
#   --methods=sitemap,tags     only these discovery routes (default: all three)
#   --refresh                  rebuild the candidate list instead of reusing it
#   --max=200                  download at most 200 articles
#
# In RStudio / Positron: open this file, set the working directory to the
# repository root (Session > Set Working Directory), then Source.
# Safe to stop and run again: pages are cached and finished articles skipped.
# ===========================================================================

pkgs <- c("httr2", "rvest", "xml2", "jsonlite", "dplyr", "stringr", "stringi",
          "lubridate", "readr", "tibble", "digest")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)

if (!file.exists("thairath/thairath_scraper.R"))
  stop("Run this from the repository root (the folder that contains thairath/).")
source("thairath/thairath_scraper.R", encoding = "UTF-8")

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name) { a <- grep(paste0("^--", name, "="), args, value = TRUE); if (length(a)) sub("^[^=]*=", "", a[1]) }

if ("--probe" %in% args) { probe(); quit(save = "no") }
if ("--test" %in% args) {
  # Small and fast: 5 pages per tag, 20 articles, separate output folder so
  # the full run later starts clean.
  TR$max_tag_pages <- 5; TR$max_search_rounds <- 5; TR$max_articles <- 20
  TR$output_dir <- "thairath/output_test"
}
if (!is.null(opt("max"))) TR$max_articles <- as.integer(opt("max"))
if (!is.null(opt("methods"))) TR$methods <- strsplit(opt("methods"), ",")[[1]]

cand <- get_candidates(TR$methods, refresh = "--refresh" %in% args)
scrape_articles(cand)
build_dataset()
