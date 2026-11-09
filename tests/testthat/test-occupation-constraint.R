# Yrke som betingelse i counterfact_me_constrained() / parallel lives.
#
# Foer november 2026 endte HVERT kall med vary_dim = "occupation" i direkte
# overstyring: maalyrket kom fra 4-sifrede STYRK-titler, livet fikk en
# detaljert 7-sifret tittel, og tekstsammenlikningen mellom de to traff
# nesten aldri. Tittelen ble saa limt paa et liv trukket for et annet yrke.
# Maalt (dev/diag-override-yrke.R, n = 120): Spearman mellom yrkets
# loennsnivaa og livets inntekt var 0.005 [-0.18, 0.17] for de overstyrte.
# Etter fiksen: 0.664 [0.534, 0.757] for oppgitte yrker, 0.795
# [0.728, 0.845] for vanlige trekk.

wage_of_code <- function(code) {
  os <- CounterfactMe:::.cfm_env$occupations_salary
  as.numeric(os$median_nok[os$code == code & os$schedule == "all"][1])
}

test_that("et oppgitt yrke settes inne i trekket, uten overstyring", {
  set.seed(401)
  codes <- CounterfactMe:::.resolve_occupation_codes("Sykepleiere")
  expect_gte(length(codes), 1L)
  for (i in 1:5) {
    expect_no_warning(
      x <- counterfact_me_constrained(
        list(age = 40, gender = "F", occupation = "Sykepleiere"),
        max_attempts = 15L)
    )
    expect_true(attr(x, "occ_styrk") %in% codes)
  }
})

test_that("ukjent yrke gir en tydelig feil, ikke et paaklistret liv", {
  expect_error(
    counterfact_me_constrained(list(age = 40, occupation = "Dragetemmer")),
    "No occupation matches"
  )
})

test_that("inntekten foelger det oppgitte yrket", {
  # Dette er egenskapen som manglet. Med n = 60 er SE paa Spearman ca
  # 1/sqrt(59) = 0.13. Oppgitte yrker ga rho 0.664 (n = 120); terskelen
  # 0.2 ligger 3.6 SE under det, mens en loesrevet etikett (rho ~ 0)
  # passerer den bare i ca 6 pst av kjoeringene.
  set.seed(402)
  CounterfactMe:::.load_data()
  os <- CounterfactMe:::.cfm_env$occupations_salary
  cands <- unique(os$label[os$schedule == "all" & nzchar(os$code) &
                             os$code != "0000" & os$headcount > 500])
  n <- 60L
  wage <- numeric(n); inc <- numeric(n)
  for (i in seq_len(n)) {
    x <- suppressWarnings(counterfact_me_constrained(
      list(age = sample(30:60, 1), gender = sample(c("M", "F"), 1),
           occupation = sample(cands, 1)),
      max_attempts = 15L))
    wage[i] <- wage_of_code(attr(x, "occ_styrk"))
    inc[i] <- suppressWarnings(as.numeric(x$income_nok))
  }
  ok <- is.finite(wage) & is.finite(inc) & inc > 0
  expect_gte(sum(ok), 45L)
  rho <- stats::cor(wage[ok], inc[ok], method = "spearman")
  expect_gt(rho, 0.2)
})
