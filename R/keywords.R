# ---------------------------------------------------------------------------
# keywords.R — search terms and relevance rules.
#
# Two kinds of terms:
#   * search_terms_*  : sent to site search engines / matched against sitemap
#                       titles to find CANDIDATE articles.
#   * the regex groups: applied to the full headline + body of each downloaded
#                       article to decide relevance and fill matched_keywords.
#
# Thai has no spaces between words, so matching is substring matching on
# normalised text. "พม่า" and "เมียนมา" on their own also catch all general
# Myanmar news (coups, earthquakes, football...), so the output carries
# separate flags so you can choose how strict to be at analysis time:
#   kw_myanmar   — mentions Myanmar / Burmese / Rohingya
#   kw_migration — mentions migrants, refugees, workers, displacement ...
#   relevant_strict = specific migrant term, OR (Myanmar term AND migration term)
# ---------------------------------------------------------------------------

# Keywords Kelvin supplied, plus additional Thai terms commonly used in coverage.
KW_TH <- list(
  myanmar = c(
    "พม่า", "เมียนมา", "เมียนมาร์", "ชาวพม่า", "ชาวเมียนมา", "โรฮิงญา", "โรฮีนจา",
    "กะเหรี่ยง", "ไทใหญ่", "ฉาน", "มอญ"
  ),
  migration = c(
    "ผู้ลี้ภัย", "ผู้ลี้ภัยทางการเมือง", "ผู้อพยพ", "อพยพ", "ผู้หนีภัย", "ผู้หนีภัยการสู้รบ",
    "หนีภัยสงคราม", "ค่ายผู้ลี้ภัย", "ศูนย์พักพิงชั่วคราว", "พื้นที่พักพิง",
    "แรงงานต่างด้าว", "แรงงานข้ามชาติ", "แรงงานข้ามแดน", "แรงงานเถื่อน",
    "หลบหนีเข้าเมือง", "ลักลอบเข้าเมือง", "เข้าเมืองผิดกฎหมาย", "ข้ามแดน",
    "ค้ามนุษย์", "ไร้สัญชาติ", "ไร้รัฐ", "บัตรสีชมพู", "ขอลี้ภัย", "ส่งกลับ",
    "ผลักดันกลับ", "ผู้พลัดถิ่น", "พลัดถิ่น", "MOU"
  ),
  # Terms that are on-topic by themselves.
  specific = c(
    "ผู้อพยพชาวพม่า", "ผู้อพยพชาวเมียนมา", "ผู้ลี้ภัยชาวพม่า", "ผู้ลี้ภัยชาวเมียนมา",
    "ผู้ลี้ภัยเมียนมา", "ผู้ลี้ภัยพม่า", "แรงงานพม่า", "แรงงานเมียนมา",
    "แรงงานชาวพม่า", "แรงงานชาวเมียนมา", "ชาวโรฮิงญา", "โรฮิงญา", "โรฮีนจา",
    "ผู้หนีภัยการสู้รบ", "ผู้หนีภัยจากเมียนมา", "ผู้หนีภัยจากพม่า",
    "ค่ายแม่หละ", "ศูนย์พักพิงชั่วคราวบ้านแม่หละ", "ผู้ลี้ภัยกะเหรี่ยง"
  )
)

# English terms for Bangkok Post and The Nation (both publish in English).
KW_EN <- list(
  myanmar = c(
    "Myanmar", "Burma", "Burmese", "Rohingya", "Karen", "Karenni", "Shan", "Mon",
    "Tatmadaw", "junta"
  ),
  migration = c(
    "migrant", "migrants", "migration", "refugee", "refugees", "asylum",
    "asylum seeker", "displaced", "displacement", "fled", "fleeing",
    "foreign worker", "foreign workers", "migrant worker", "migrant workers",
    "illegal immigrant", "illegal immigrants", "illegal entry", "undocumented",
    "human trafficking", "trafficking", "stateless", "deport", "deported",
    "deportation", "repatriation", "pink card", "work permit", "border camp",
    "temporary shelter", "influx", "boat people"
  ),
  specific = c(
    "Myanmar migrant", "Myanmar migrants", "Burmese migrant", "Burmese migrants",
    "Myanmar refugee", "Myanmar refugees", "Burmese refugee", "Burmese refugees",
    "Myanmar workers", "Myanmar worker", "Myanmar nationals", "Rohingya",
    "Karen refugees", "Mae La", "Mae Sot refugees", "fleeing Myanmar",
    "fled Myanmar", "displaced persons from Myanmar"
  )
)

# Terms actually sent to site search boxes (kept short: every term = many
# requests). Kelvin's original list first.
SEARCH_TERMS_TH <- c(
  "ผู้ลี้ภัย", "ผู้ลี้ภัยทางการเมือง", "ผู้อพยพชาวพม่า", "พม่า", "เมียนมา", "โรฮิงญา",
  "แรงงานพม่า", "แรงงานเมียนมา", "ผู้หนีภัยการสู้รบ", "แรงงานข้ามชาติ"
)
SEARCH_TERMS_EN <- c(
  "Myanmar migrants", "Myanmar refugees", "Rohingya", "Burmese migrants",
  "Myanmar workers", "refugees Myanmar border", "migrant workers Myanmar",
  "asylum seekers Myanmar", "Mae Sot", "Karen refugees"
)

# --- matching helpers --------------------------------------------------------

normalise_text <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- stringi::stri_trans_nfc(x)
  x <- gsub("[​‌‍﻿­]", "", x, perl = TRUE)  # zero-width chars
  x <- gsub(" ", " ", x, fixed = TRUE)
  x <- gsub("[ \t]+", " ", x, perl = TRUE)
  trimws(x)
}

# Thai: plain substring. English: case-insensitive, word-boundary at the start
# so "Mon" does not match "Monday" ... and "migrant" matches "migrants".
.kw_regex <- function(term, language) {
  esc <- stringr::str_replace_all(term, "([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1")
  if (language == "en") paste0("(?i)\\b", esc, ifelse(nchar(term) <= 4, "\\b", "")) else esc
}

match_terms <- function(text, terms, language) {
  if (!nzchar(text)) return(character())
  hits <- vapply(terms, function(t) stringr::str_detect(text, .kw_regex(t, language)), logical(1))
  terms[hits]
}

keyword_sets <- function(language) if (language == "en") KW_EN else KW_TH

# Returns a one-row tibble of keyword columns for one article.
score_article <- function(headline, content, language) {
  txt <- normalise_text(paste(headline, content))
  kw  <- keyword_sets(language)
  my  <- match_terms(txt, kw$myanmar, language)
  mg  <- match_terms(txt, kw$migration, language)
  sp  <- match_terms(txt, kw$specific, language)
  all <- unique(c(sp, my, mg))
  tibble::tibble(
    matched_keywords = paste(all, collapse = "; "),
    n_keywords       = length(all),
    kw_myanmar       = length(my) > 0 || length(sp) > 0,
    kw_migration     = length(mg) > 0 || length(sp) > 0,
    kw_specific      = length(sp) > 0,
    relevant_strict  = length(sp) > 0 || (length(my) > 0 && length(mg) > 0)
  )
}

# Quick check used on sitemap titles before downloading. Deliberately loose:
# any Myanmar term, any specific term, or a refugee/migrant term.
title_is_candidate <- function(title, language) {
  kw <- keyword_sets(language)
  t  <- normalise_text(title)
  terms <- unique(c(kw$myanmar, kw$specific,
                    if (language == "en") c("refugee", "migrant", "asylum", "stateless")
                    else c("ผู้ลี้ภัย", "ผู้อพยพ", "ผู้หนีภัย", "แรงงานต่างด้าว", "แรงงานข้ามชาติ")))
  vapply(t, function(x) length(match_terms(x, terms, language)) > 0, logical(1), USE.NAMES = FALSE)
}
