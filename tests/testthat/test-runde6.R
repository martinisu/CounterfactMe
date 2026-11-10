# Funn fra lesing av 600 liv og 26 liv paa den publiserte siden, 10. november 2026.

test_that("an immigrant child's parents are either both abroad or both here", {
  # "Den ene forelderen bodde i India, den andre var bonde
  # (kombinasjonsbruk)" for a man who came at 17.
  set.seed(6101)
  seen <- 0L
  for (i in 1:120) {
    x <- counterfact_me(min_age = 20, max_age = 70)
    if (!identical(x$background, "first_gen")) next
    seen <- seen + 1L
    txt <- as.character(narrate_life(x))
    expect_false(grepl("Den ene forelderen bodde i", txt, fixed = TRUE), label = txt)
  }
  expect_gte(seen, 10L)
})

test_that("second-generation parents are not narrated as living abroad", {
  # "Foreldrene kom fra Ungarn. / Foreldrene bodde i Ungarn mens de var i
  # arbeidsfoer alder" -- the person was born in Norway.
  # Drawn through the background step directly: second_gen is 4 pct of
  # adults, so 200 whole lives held only a handful.
  set.seed(6102)
  seen <- 0L
  for (i in 1:400) {
    x <- counterfact_me(min_age = 20, max_age = 50)
    if (!identical(x$background, "second_gen")) next
    seen <- seen + 1L
    txt <- as.character(narrate_life(x))
    expect_false(grepl("levde arbeidslivet sitt i|Foreldrene bodde i [A-Z]", txt, perl = TRUE),
                 label = txt)
    if (seen >= 15L) break
  }
  expect_gte(seen, 5L)
})

test_that("old second-generation lives come only from old migration flows", {
  # An 88-year-old was second generation from Nepal (born 1938): countries
  # missing from immigration_start_year.csv defaulted to 1900.
  CounterfactMe:::.load_data()
  set.seed(6103)
  for (i in 1:300) {
    bg <- CounterfactMe:::.cond_immigrant_background(85)
    if (identical(bg$background, "second_gen"))
      expect_true(bg$name_region %in% c("norden", "vesteuropa"), label = bg$country_label)
  }
})

test_that("no 'Med ingen fullfoert utdanning tok ...'", {
  set.seed(6104)
  for (i in 1:80) {
    x <- counterfact_me(min_age = 25, max_age = 60)
    txt <- as.character(narrate_life(x))
    expect_false(grepl("Med ingen fullf", txt, fixed = TRUE), label = txt)
  }
})

test_that("men do not get -vertinne, -pike or -dame titles", {
  # "Vidar, som er solvertinne". The old exact list caught FOSTERMOR but
  # not SOLVERTINNE, FLYVERTINNE or KONTORDAME.
  female_re <- "^[^/]*(vertinne|pike|dame|jomfru|oldfrue)( \\(|$)"
  set.seed(6105)
  for (i in 1:100) {
    x <- counterfact_me(gender = "M", min_age = 20, max_age = 66)
    occ <- x$occupation
    if (is.null(occ) || is.na(occ)) occ <- ""
    expect_false(grepl(female_re, tolower(occ), perl = TRUE), label = occ)
  }
  CounterfactMe:::.load_data()
  for (i in 1:200) {
    t <- CounterfactMe:::.draw_detail_yrke("5113", NA, gender = "M")
    expect_false(grepl("VERTINNE", toupper(t)), label = t)
  }
})

test_that("title cosmetics", {
  tc <- CounterfactMe:::.titlecase_yrke
  expect_equal(tc("LEDER - GJESTERELASJONER (HOTELL)"), "Leder, Gjesterelasjoner (Hotell)")
  expect_equal(tc("DP-OPERATØR"), "DP-operatør")
  expect_equal(tc("KJØKKENINTENIØRSELGER"), "Kjøkkeninteriørselger")
  expect_false(grepl("  ", tc("STORKUNDEANSVARLIG  (ENGROSALG)")))
  expect_false(grepl(",\\S", tc("SPESIALKONSULENT (TEKNISK,NATURVITENSKAPELIG UTREDNING)")))
  expect_false(grepl("Og ", tc("AKTIVITETSLEDER (BARNE- OG UNGDOMSARBEID)"), fixed = TRUE))
})

test_that("pensions stay within an education-dependent ceiling", {
  # "Pensjonen ligger paa rundt 1 310 000" for videregaaende; a 79-year-old
  # ex-sjekkflyger drew 1 580 000 as a wage.
  set.seed(6107)
  for (i in 1:80) {
    x <- counterfact_me(min_age = 72, max_age = 95)
    lvl <- CounterfactMe:::.edu_code_from_label(x$education)
    cap <- if (is.na(lvl)) 1e6 else if (lvl <= 2) 450000 else if (lvl <= 5) 650000 else 1e6
    inc <- x$income_nok
    if (is.null(inc) || is.na(inc)) inc <- 0
    # x 1.06: .background_income_factor() (vesteuropa 1.05) applies after.
    expect_lte(inc, cap * 1.06)
  }
})

test_that("majority lives do not get unmistakably immigrant first names", {
  # "Przemyslaw" and "Abdul" as children of Toril and Svein.
  set.seed(6108)
  nm <- c(sample_first_name(400, gender = "M", birth_year = 1988),
          sample_first_name(400, gender = "M", birth_year = 1960))
  expect_false(any(grepl("sz|cz|rz|^Abd|^Moha|^Muha|^Arturas$", nm)),
               label = paste(unique(nm), collapse = " "))
})

test_that("the upward-mobility lead is kept for real jumps", {
  # Videregaaende -> bachelor is the modal path; the lead fired on 3 of 14
  # live draws.
  set.seed(6109)
  for (i in 1:100) {
    x <- counterfact_me(min_age = 30, max_age = 70)
    txt <- as.character(narrate_life(x))
    expect_false(grepl("fra videregående i barndomshjemmet til bachelorgrad", txt, fixed = TRUE))
  }
})

test_that("university-only fields are not attached to videregaaende", {
  set.seed(6110)
  for (i in 1:100) {
    x <- counterfact_me(min_age = 22, max_age = 80)
    txt <- tolower(as.character(narrate_life(x)))
    expect_false(grepl("videregående med fordypning i (sykepleie|psykologi|arkitektur|fysikk)", txt),
                 label = txt)
  }
})

test_that("the narrative keeps acronyms in job titles", {
  # "driftsutvikler (it)", "assistent (pu)", "sjaafoer klasse b".
  lc <- CounterfactMe:::.lc_job
  expect_equal(lc("Driftsutvikler (IT)"), "driftsutvikler (IT)")
  expect_equal(lc("DP-operatør"), "DP-operatør")
  expect_equal(CounterfactMe:::.titlecase_yrke("SJÅFØR KLASSE B"), "Sjåfør klasse B")
  expect_equal(CounterfactMe:::.titlecase_yrke("BUTIKKMEDARABEIDER (BENSINSTASJON)"),
               "Butikkmedarbeider (Bensinstasjon)")
})

test_that("retirees are not said to make a living from a pastime", {
  # "Det er ikke mange igjen som livnaerer seg slik Ingjerd gjoer -- som
  # fjordfisker" for a 76-year-old pensioner.
  set.seed(6112)
  for (i in 1:80) {
    x <- counterfact_me(min_age = 67, max_age = 95)
    txt <- as.character(narrate_life(x))
    expect_false(grepl("livnærer seg", txt, fixed = TRUE), label = txt)
  }
})

test_that("young workers with grunnskole rarely land in office and professional jobs", {
  CounterfactMe:::.load_data()
  set.seed(6113)
  codes <- vapply(1:400, function(i) {
    o <- CounterfactMe:::.cond_occupation(age = 27, edu_code = 2L, mun_pop = 60000, gender = "F")
    if (is.null(o$styrk_code)) NA_character_ else as.character(o$styrk_code)
  }, character(1))
  codes <- codes[!is.na(codes)]
  hi <- substr(codes, 1, 1) %in% c("1", "2", "3") & substr(codes, 1, 2) != "14"
  expect_lt(mean(hi), 0.15)
})

test_that("more title cosmetics: missing space, source typos, B&B", {
  tc <- CounterfactMe:::.titlecase_yrke
  expect_equal(tc("FAGARBEIDER(MURER)"), "Fagarbeider (Murer)")
  expect_equal(tc("PEDRIATISK SYKEPLEIER"), "Pediatrisk sykepleier")
  expect_equal(tc("B&B-VERT/VERTINNE"), "B&B-vert/vertinne")
})

test_that("parents who worked before 1990 do not get modern titles", {
  # A mother born in the 1920s "jobbet som logistiker"; others were
  # "key account manager" and "booker".
  CounterfactMe:::.load_data()
  set.seed(6114)
  hits <- vapply(1:300, function(i) {
    o <- CounterfactMe:::.cond_occupation(age = 45, edu_code = 4L, mun_pop = 60000,
                                          gender = if (i %% 2) "F" else "M",
                                          work_year = 1975L)
    grepl(CounterfactMe:::.MODERN_TITLE_RE, toupper(o$label), perl = TRUE)
  }, logical(1))
  # A few STYRK groups have only modern titles (4222 kundesenter); the
  # filter then falls back to the full list rather than to nothing.
  expect_lt(mean(hits), 0.02)
})
