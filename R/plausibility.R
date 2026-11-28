# ============================================================
# Final check on a whole life (November 2026)
#
# Each dimension is drawn from an SSB table that conditions on one or two
# others. Combinations that no single table rules out can still not
# belong in one life: a 27-year-old with grunnskole earning 1 080 000 in
# a bank, a title the person is not qualified for, a 22-year-old owning a
# home no lender would finance. Fixing these one at a time, after a
# reader had seen them, did not converge. counterfact_me() therefore
# checks the finished life against the rules below and redraws it when
# one is broken (see the rejection loop there).
#
# The rules are assumptions, not SSB data; each says what it guards
# against. They are deliberately loose: they should remove lives that
# read as wrong, not trim the tails of every distribution. The redraw
# rate is reported by dev/sandkasse/sweep.R.
# ============================================================

# Education code (NUS level) of a drawn life.
.edu_code_of <- function(x) {
  e <- suppressWarnings(as.integer(x$edu_code %||% NA))
  if (!is.na(e)) return(e)
  et <- .cfm_env$education
  lbl <- x$education %||% NA
  if (is.null(et) || is.na(lbl)) return(NA_integer_)
  for (col in c("level_no", "level", "label_no", "label")) {
    if (col %in% names(et)) {
      ix <- which(et[[col]] == lbl)
      if (length(ix)) return(as.integer(et$code[ix[1]]))
    }
  }
  NA_integer_
}

# Annual median wage (all schedules) for a four-digit STYRK code.
.styrk_annual_median <- function(code) {
  os <- .cfm_env$occupations_salary
  if (is.null(os) || is.null(code) || is.na(code)) return(NA_real_)
  m <- os$median_nok[os$code == code & os$schedule == "all"]
  if (!length(m) || is.na(m[1]) || m[1] <= 0) return(NA_real_)
  12 * m[1]
}

.find_implausibilities <- function(x) {
  v <- character(0)
  if (is.null(x)) return(v)
  age <- x$age %||% NA
  edu <- .edu_code_of(x)
  occ <- x$occupation %||% NA
  code <- attr(x, "occ_styrk")
  if (is.null(code)) code <- NA_character_
  benefit <- !is.na(occ) && grepl("^(AAP|Dagpenger|Sosialhjelp|Uf\u00f8re)", occ)
  worker <- !is.na(code) && !benefit && !isTRUE(x$neet) && !is.na(age) && age >= 16 && age < 72

  # 1. The drawn title fits age, education and sex (R/title_rules.R).
  # .draw_detail_yrke() already filters, but can leak when no title in
  # any of eight codes fits.
  row <- attr(x, "occ_title_row")
  if (!is.null(row) && !is.na(row) &&
      !isTRUE(.title_fits(row, age = age, edu_code = edu, gender = x$gender)))
    v <- c(v, "tittel_passer_ikke")

  # 2. Regulated codes need their education (also enforced in the
  # weights; this catches the background shift and forced codes).
  if (worker && !is.na(edu)) {
    if (code %in% .LICENSED_STYRK && edu < 7L) v <- c(v, "autorisert_uten_lang_utdanning")
    if (code %in% .AUTHORISED_BACHELOR_STYRK && edu < 6L) v <- c(v, "helseprofesjon_uten_bachelor")
    if (code %in% .FAGBREV_STYRK && edu < 4L) v <- c(v, "fagbrev_mangler")
    if (edu <= 2L && substr(code, 1, 2) %in% c("11", "12", "13")) v <- c(v, "toppleder_med_grunnskole")
  }

  # 3. Wage against the occupation's own median, allowing for part-time.
  # The noise is capped at +2 sd in .cond_income(), so a full-time ratio
  # above 1.9 means several upward draws stacked; under 30 the bar is
  # 1.45, because the age multiplier already puts the young below the
  # median.
  inc <- x$income_nok %||% NA
  if (worker && !is.na(inc) && inc > 0) {
    med <- .styrk_annual_median(code)
    share <- x$stillingsandel %||% 1
    if (!is.na(med) && share > 0) {
      ratio <- inc / (med * share)
      if (ratio > 1.9 || (age < 30 && ratio > 1.45)) v <- c(v, "lonn_langt_over_yrket")
    }
  }

  # 4. Young owners who could not have financed the home: price above
  # 5.5 times income plus a tenth of the parents' capital and 300 000.
  if (!is.na(age) && age < 25 && identical(x$housing_tenure, "Eier")) {
    price <- x$housing_purchase_price_nok %||% x$housing_value_nok %||% NA
    pc <- max(0, x$parents_capital %||% 0)
    if (!is.na(price) && price > 5.5 * max(0, inc %||% 0) + 0.1 * pc + 300000)
      v <- c(v, "ung_eier_uten_betalingsevne")
  }

  # 4b. Recent immigrants in homes beyond their means. A man who came
  # from Somalia at 42, on 370 000, owned a 10.9 million house. The
  # housing draw now applies the affordability test to everyone who came
  # within the last 30 years (see .cond_housing); this is the same test
  # on the finished life, on the purchase price.
  if (identical(x$background, "first_gen") && identical(x$housing_tenure, "Eier")) {
    yin <- x$years_in_norway %||% NA
    price <- x$housing_purchase_price_nok %||% NA
    pc <- max(0, x$parents_capital %||% 0)
    if (!is.na(yin) && yin <= 30 && !is.na(price) &&
        price > 5.5 * max(0, inc %||% 0) + 0.1 * pc + 300000)
      v <- c(v, "innvandrer_bolig_over_evne")
  }

  # 5. Children under 18 with children of their own.
  if (!is.na(age) && age < 18 && isTRUE((x$n_children %||% 0) > 0)) v <- c(v, "barn_med_barn")

  # 6. An immigrant child's parents: both abroad or both here.
  if (identical(x$background, "first_gen")) {
    ab <- c(grepl("^(Bodde|Lived) i", x$mother$occupation %||% ""),
            grepl("^(Bodde|Lived) i", x$father$occupation %||% ""))
    if (sum(ab) == 1L) v <- c(v, "foreldre_delt_utland_norge")
  }

  v
}
