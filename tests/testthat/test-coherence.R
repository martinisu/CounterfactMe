# Koherens i trukne liv og fortellinger.
#
# Fram til november 2026 ble de fleste logiske feilene i fortellingene
# oppdaget av forfatteren, ett og ett skjermbilde om gangen: leietakere
# beskrevet som eiere ("Leier" inneholder "eier"), "Han bor hjemme hos
# foreldrene. Han eier en bolig.", en 17-aaring med "mer gjeld enn formue",
# "jobber som kassa-veteran", "tilhoerer doept i Den norske kirke", en
# bachelor i medisin som overlege. Denne fila trekker mange liv og sjekker
# egenskapene samlet, slik at slike feil fanges foer de publiseres.

draw_many <- function(n, seed) {
  set.seed(seed)
  bands <- list(c(0L, 12L), c(13L, 19L), c(20L, 29L), c(30L, 49L), c(50L, 66L), c(67L, 95L))
  lapply(seq_len(n), function(i) {
    b <- bands[[(i - 1L) %% length(bands) + 1L]]
    x <- counterfact_me(min_age = b[1], max_age = b[2])
    list(x = x, txt = as.character(narrate_life(x, style = "biography")))
  })
}

test_that("narratives do not contradict the life they describe", {
  lives <- draw_many(240L, 701)
  expect_gte(length(lives), 240L)
  bad <- character(0)
  flag <- function(why, l) bad <<- c(bad, paste0("[", why, "] ", substr(l$txt, 1, 160)))

  for (l in lives) {
    x <- l$x; t <- l$txt; tl <- tolower(t)
    age <- x$age
    hh <- tolower(x$household %||% "")
    ten <- x$housing_tenure %||% ""

    if (grepl("^Leier", ten) && grepl("\\beier (en|et|bolig)", tl)) flag("leier beskrevet som eier", l)
    if (grepl("foreldre", hh) && grepl("\\b(eier|leier) (en|et|bolig)", tl)) flag("bor hos foreldre men eier/leier", l)
    # The narration's wealth sentences, not the word: a toddler's pocket-
    # money joke "Tror en tikrone er en formue" is fine.
    if (!is.na(age) && age < 18 && grepl("mer gjeld enn formue|har (betydelig|stor) formue", tl))
      flag("formue/gjeld for mindrearig", l)
    if (!is.na(age) && age <= 18 &&
        grepl(paste0("(jobber som|i bagasjen er \\w+) (",
                     paste(tolower(CounterfactMe:::.TEEN_OCCUPATIONS), collapse = "|"), ")"), tl))
      flag("ungdomsetikett som jobb", l)
    if (grepl("jobber som student|som er student \\(", tl)) flag("student som jobbtittel", l)
    if (grepl("tilhører (døpt|foreldrenes|navnefest)", tl)) flag("barnereligion som tilhoerighet", l)
    if (grepl("\\([^()]*\\([^()]*\\)[^()]*\\)", t)) flag("nestede parenteser", l)
    if (grepl("jobbet som hjemmev|jobber som hjemmev|uoppgitt / yrker", tl)) flag("ikke-yrke som yrke", l)
    if (grepl("leser lokalavis|fra lokalavis\\.", tl) && grepl("Lokalavis", t, fixed = TRUE)) flag("Lokalavis", l)
    if (grepl("etter et samlivsbrudd har", tl)) flag("samlivsbrudd-formulering", l)
    if (lengths(regmatches(tl, gregexpr("\\bstemmer\\b", tl))) > 1L) flag("parti nevnt to ganger", l)
    if (grepl("klatret lenger", tl) && grepl("der foreldrene stoppet|lengre utdanning enn noen", tl))
      flag("mobilitet nevnt to ganger", l)
    if (grepl("var (\\w+), og faren \\w+ var \\1\\b", tl, perl = TRUE)) flag("samme ledd to ganger", l)

    # Round 2 (audit-fil 2026-11-09)
    if (grepl("jobber som ikke i utdanning", tl)) flag("NEET som jobb", l)
    if (grepl("\\ber 0 \u00e5r", tl)) flag("0 aar", l)
    if (grepl("stoppet ved (\\w+), gikk \\w+ videre til \\1\\b", tl, perl = TRUE)) flag("mobilitet til samme nivaa", l)
    if (grepl("ansatte\\)", tl)) flag("tittelrest om antall ansatte", l)
    if (grepl("bor i kollektiv", tl) && grepl("leier bolig", tl)) flag("kollektiv og leier", l)
    if (grepl("ikke som troende", tl) && grepl("religi\u00f8s aktivitet", tl)) flag("religioes hobby uten tro", l)
    if (grepl("^(AAP|Dagpenger)", x$occupation %||% "") && isTRUE(x$income_nok > 0.66 * 711720 * 1.2))
      flag("stoenad over taket", l)
    if (!is.na(age) && age < 18 && isTRUE(x$income_nok > 150000)) flag("hoey inntekt for mindrearig", l)
    if (!is.na(age) && age < 18 && grepl("avsluttende|universitets", tolower(x$education %||% "")))
      flag("utdanning for hoey for alderen", l)

    code <- attr(x, "occ_styrk")
    edu_cd <- suppressWarnings(as.integer(x$edu_code %||% NA))
    if (is.na(edu_cd)) {
      et <- CounterfactMe:::.cfm_env$education
      ix <- which(et$level_no == x$education)
      if (length(ix)) edu_cd <- as.integer(et$code[ix[1]])
    }
    if (!is.null(code) && !is.na(code) && code %in% CounterfactMe:::.LICENSED_STYRK &&
        !is.na(edu_cd) && edu_cd < 7L)
      flag("autorisert yrke uten lang utdanning", l)
    # Round 4: 4-digit group labels are plural and should never be a title.
    os_lab <- tolower(CounterfactMe:::.cfm_env$occupations_salary$label)
    if (tolower(x$occupation %||% "") %in% os_lab && !grepl("^(student|ufor)", tolower(x$occupation %||% "")))
      flag("gruppenavn (flertall) som yrkestittel", l)
    if (grepl(", (annet|andre|uoppgitt)\\b", tl)) flag("fagfelt-rest", l)
    # Round 3 (audit-fil 2026-11-09, andre lesning)
    if (!is.null(code) && !is.na(code) && code %in% CounterfactMe:::.AUTHORISED_BACHELOR_STYRK &&
        !is.na(edu_cd) && edu_cd < 6L)
      flag("autorisert helseprofesjon uten bachelor", l)
    if (!is.na(edu_cd) && edu_cd < 7L &&
        grepl(CounterfactMe:::.LICENSED_TITLE_RE, toupper(x$occupation %||% ""), perl = TRUE))
      flag("lege/prest-tittel uten lang utdanning", l)
    if (!is.na(age) && age <= 12 && grepl("ungdomsskole|videreg", tolower(x$education %||% "")))
      flag("barn med for hoey utdanning", l)
    if (!is.na(age) && age >= 67 && age < 72 && grepl("pensjonen ligger", tl) &&
        !(x$occupation %||% "") %in% CounterfactMe:::.PENSJONIST_LABELS &&
        !grepl("^(AAP|Dagpenger|Sosialhjelp|Uf)", x$occupation %||% ""))
      flag("jobber, men omtalt som pensjonist", l)
    if (!is.na(age) && age < 25 && identical(x$housing_tenure, "Eier") &&
        isTRUE(x$housing_value_nok > 5.5 * max(0, x$income_nok %||% 0) + 0.1 * max(0, x$parents_capital %||% 0) + 300000) &&
        isTRUE(x$housing_purchase_price_nok > 5.5 * max(0, x$income_nok %||% 0) + 0.1 * max(0, x$parents_capital %||% 0) + 300000))
      flag("ung eier uten betalingsevne", l)
  }
  expect_equal(bad, character(0))
})

test_that("occupation follows SSB education and age shares", {
  # 12849: grunnskole holders are 2.2 pct ingenioerer/teknikere (31)
  # against about 5 pct of all employed; 09793: 15-24-aaringer are far
  # less often in 31 than the employed as a whole. The product must put
  # a 19-year-old with grunnskole well below the population share.
  CounterfactMe:::.load_data()
  ow_young <- CounterfactMe:::.occupation_weights(19L, 2L)
  ow_mid <- CounterfactMe:::.occupation_weights(45L, 4L)
  share31 <- function(ow) sum(ow$w[substr(ow$tbl$code, 1, 2) == "31"]) / sum(ow$w)
  expect_lt(share31(ow_young), 0.5 * share31(ow_mid))

  # Licensed professions need a long degree.
  ow_bach <- CounterfactMe:::.occupation_weights(45L, 6L)
  ow_mast <- CounterfactMe:::.occupation_weights(45L, 7L)
  lic <- CounterfactMe:::.LICENSED_STYRK
  expect_equal(sum(ow_bach$w[ow_bach$tbl$code %in% lic]), 0)
  expect_gt(sum(ow_mast$w[ow_mast$tbl$code %in% lic]), 0)
})
