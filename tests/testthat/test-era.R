test_that("era-modus returnerer en kohort per aarstall", {
  res <- counterfact_era_lives(eras = c(1950L, 1965L, 1980L), lang = "no")
  expect_s3_class(res, "counterfact_era")
  expect_length(res, 3L)
  expect_equal(vapply(res, function(e) e$birth_year, integer(1)),
               c(1950L, 1965L, 1980L))
})

test_that("hver kohort observeres ved samme alder, ikke samme aar", {
  res <- counterfact_era_lives(eras = c(1950L, 1980L), at_age = 40L)
  expect_true(all(vapply(res, function(e) e$observed_at, integer(1)) == 40L))
  # Kalenderaaret maa variere -- det er hele poenget.
  yrs <- vapply(res, function(e) e$calendar_year, numeric(1))
  expect_equal(yrs, c(1990, 2020))
})

test_that("kohorter eldre enn registeret droppes med advarsel", {
  expect_warning(
    res <- counterfact_era_lives(eras = c(1890L, 1980L)),
    "utelates"
  )
  expect_length(res, 1L)
  expect_equal(res[[1]]$birth_year, 1980L)
})

test_that("utdanningsekspansjonen slaar ut: senere kohorter faar mer skole", {
  # Maalt med 300 trekk per kohort (dev/diag-era.R) er den sanne differansen
  # 1.217 NUS-nivaaer, dekomponert som 0.553 fra alderskondisjonering i
  # registerbaandet (91 mot 46 aar) og 0.663 fra kohortnedjusteringen av 1935.
  #
  # Terskelen er 0.8. Dimensjoneringen av n foelger av det, og forrige versjon
  # hadde den for lav: med n = 40 er SE paa differansen 0.364, marginen bare
  # 1.15 SE, og testen feiler i 12.6 pst av kjoeringene UANSETT kode. Den
  # gamle kommentaren her paastod "over 3x margin", som var oeyemaal.
  #
  #   n     SE      margin    feilrate
  #   40    0.364   1.15 SE   12.6 %
  #   150   0.188   2.22 SE    1.3 %
  #   200   0.163   2.56 SE    0.5 %
  n <- 150L
  mean_code <- function(birth_year) {
    codes <- vapply(seq_len(n), function(i) {
      e <- counterfact_era_lives(eras = birth_year)[[1]]
      cd <- e$education_code
      if (is.null(cd) || is.na(cd)) NA_real_ else as.numeric(cd)
    }, numeric(1))
    mean(codes, na.rm = TRUE)
  }
  old <- mean_code(1935L)
  young <- mean_code(1980L)
  expect_gt(young - old, 0.8)
})

test_that("boligprisindeksen er reell og stigende over etterkrigstida", {
  i1975 <- .era_housing_index(1975L)
  i1992 <- .era_housing_index(1992L)
  i2015 <- .era_housing_index(2015L)
  expect_false(is.na(i1975))
  # Serien er forankret i 1992 = 100 per konstruksjon.
  expect_equal(i1992, 100, tolerance = 1e-6)
  # Realprisene steg kraftig etter 1992; 1975 laa langt under.
  expect_lt(i1975, i1992)
  expect_gt(i2015, i1992)
})

test_that("ikke-forankrede felt er merket som anslag", {
  res <- counterfact_era_lives(eras = 1975L)
  assumed <- attr(res, "assumed")
  expect_true(is.character(assumed))
  # Utdanning har ekte aera-data og skal IKKE staa der.
  expect_false("education" %in% assumed)
  # Yrke overlever IKKE overforingen (91-aaringens yrke er "pensjonist"),
  # saa det SKAL staa der.
  expect_true("occupation" %in% assumed)
  # Noe skal staa der; ellers har flagget mistet betydning.
  expect_gt(length(assumed), 5L)
})

test_that("bakgrunn holdes fast paa tvers av kohorter", {
  res <- counterfact_era_lives(eras = c(1950L, 1980L),
                               givens = list(gender = "F"))
  gs <- vapply(res, function(e) toupper(substr(e$life$gender, 1, 1)), character(1))
  expect_true(all(gs == "F"))
})

test_that("kohorter som ikke har naadd at_age droppes", {
  # Regresjonstest: forste versjon merket en 31-aaring som "sett ved 40 ar".
  ref <- CounterfactMe:::.cfm_env$ref_year
  ung <- ref - 25L   # bare 25 aar i dag
  expect_warning(
    res <- counterfact_era_lives(eras = c(1960L, ung), at_age = 40L),
    "har ikke fylt"
  )
  expect_length(res, 1L)
  expect_equal(res[[1]]$birth_year, 1960L)
  # Og observasjonsalderen som rapporteres skal faktisk vaere naadd.
  expect_lte(res[[1]]$observed_at, res[[1]]$age_in_register)
})

test_that("alle kohorter rapporterer en observasjonsalder de har naadd", {
  res <- counterfact_era_lives()
  for (e in res) expect_lte(e$observed_at, e$age_in_register)
})

test_that("kohorter INNENFOR samme registerbaand skilles fra hverandre", {
  # Kjernen i opploesningsfiksen. Foedt 1935 og 1950 er begge 67+ i dag og
  # trekker fra IDENTISK registerbaand ("67 aar eller eldre", som dekker
  # foedselsaarene 1926-1959). Uten kohortjusteringen er forskjellen mellom
  # dem ren stoy. Multiplikatoren er 0.36 for 1935 og 0.65 for 1950, altsaa
  # naer en dobling av tertiaersannsynligheten.
  #
  # Terskelen er 6:8, ikke >= 6: kode 9 er "Uoppgitt", ikke en universitetsgrad.
  # Ingen aldersbaand baerer 09a-vekt i dag, men .ssb_to_pkg_edu() har en
  # mapping for den, saa en ny datahenting kunne ellers stilltiende gjort
  # uoppgitte til akademikere.
  #
  # Baandet 67+ har 20.2 pst UH kort og 7.3 pst UH lang, og demoteringen
  # ganger m med 0.6 for lange grader. Ventet andel er derfor
  # 0.202*m + 0.073*m*0.6, altsaa 0.089 for 1935 og 0.160 for 1950 --
  # en ventet differanse paa 0.071, ikke 0.08 som en naiv 27.5*m ville gitt.
  # Med 120 trekk per kohort er SE paa differansen ca 0.040, saa terskelen
  # 0.02 ligger godt under ventet differanse, og klart over 0, som er det
  # uten fiksen ville gitt.
  n <- 120L
  share_tertiary <- function(birth_year) {
    cds <- vapply(seq_len(n), function(i) {
      e <- counterfact_era_lives(eras = birth_year)[[1]]
      cd <- e$education_code
      if (is.null(cd) || is.na(cd)) NA_real_ else as.numeric(cd)
    }, numeric(1))
    mean(cds %in% 6:8, na.rm = TRUE)
  }
  expect_gt(share_tertiary(1950L) - share_tertiary(1935L), 0.02)
})

test_that("registerbaandet beholdes ved siden av den kohortjusterte verdien", {
  res <- counterfact_era_lives(eras = c(1935L, 1980L))
  for (e in res) {
    expect_false(is.null(e$education_band))
    expect_false(is.null(e$education_band_code))
    # Justeringen kan bare senke nivaaet, aldri heve det.
    if (!is.na(e$education_code) && !is.na(e$education_band_code)) {
      expect_lte(e$education_code, e$education_band_code)
    }
  }
})

test_that("kohortjusteringen roerer ikke unge kohorter", {
  # Multiplikatoren er 1 fra 1965, saa baand og justert verdi maa vaere like.
  for (i in seq_len(25)) {
    e <- counterfact_era_lives(eras = 1980L)[[1]]
    expect_equal(e$education_code, e$education_band_code)
  }
})
