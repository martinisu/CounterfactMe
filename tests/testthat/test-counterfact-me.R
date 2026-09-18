test_that("counterfact_me() returns a counterfactme object with expected fields", {
  set.seed(1)
  x <- counterfact_me(lang = "no")
  expect_s3_class(x, "counterfactme")
  expect_true(!is.null(x$age))
  expect_true(x$age >= 0 && x$age <= 99)
  expect_true(x$gender %in% c("M", "F"))
  expect_true(!is.null(x$lang))
})

test_that("counterfact_me works with subset of dimensions", {
  # gender is a parameter, not a dimension -- it is always drawn
  x <- counterfact_me(dimensions = c("name", "age"))
  expect_true(!is.null(x$name))
  expect_true(!is.null(x$age))
  expect_true(!is.null(x$gender))
  expect_true(is.null(x$housing_value_nok))
})

test_that("unknown dimensions are rejected", {
  expect_error(counterfact_me(dimensions = c("name", "not_a_dimension")),
               "Unknown dimensions")
})

test_that("counterfact_me works in both languages", {
  set.seed(2)
  x_no <- counterfact_me(lang = "no")
  set.seed(2)
  x_en <- counterfact_me(lang = "en")
  expect_equal(x_no$age, x_en$age)
  expect_equal(x_no$gender, x_en$gender)
})

test_that("min_age and max_age are respected", {
  for (i in 1:20) {
    x <- counterfact_me(min_age = 25, max_age = 35)
    expect_true(x$age >= 25 && x$age <= 35)
  }
})

test_that("forced gender is respected", {
  for (i in 1:10) {
    x <- counterfact_me(gender = "F")
    expect_equal(x$gender, "F")
  }
})

test_that("conditional = FALSE works", {
  x <- counterfact_me(conditional = FALSE)
  expect_s3_class(x, "counterfactme")
})

test_that("print method runs without error", {
  x <- counterfact_me(lang = "no")
  expect_output(print(x), "Ditt kontrafaktiske liv")
})

test_that("default language is Norwegian", {
  x <- counterfact_me()
  expect_equal(x$lang, "no")
  expect_output(print(x), "Ditt kontrafaktiske liv")
})

test_that("gendered occupation titles match ego's gender", {
  # FOSTERMOR/FOSTERFAR and similar pairs live in the same STYRK-08 group,
  # so the detail draw used to be blind to gender.
  # Utledes fra kilden, ikke skrevet av. Hardkodede kopier drifter: da
  # DEKKSGUTT og HOTELLGAARDSGUTT kom inn i .gendered_yrke_titles ble disse
  # listene stille utdaterte, og testen sluttet aa dekke de nye titlene.
  # Output gaar gjennom .titlecase_yrke(), saa samme funksjon brukes her.
  tc <- function(v) vapply(v, CounterfactMe:::.titlecase_yrke, character(1),
                           USE.NAMES = FALSE)
  male_only   <- tc(CounterfactMe:::.gendered_yrke_titles$male)
  female_only <- tc(CounterfactMe:::.gendered_yrke_titles$female)
  expect_gt(length(male_only), 5L)
  expect_gt(length(female_only), 5L)
  set.seed(11)
  for (i in 1:150) {
    x <- counterfact_me(gender = "F", min_age = 25, max_age = 60)
    expect_false(isTRUE(x$occupation %in% male_only))
  }
  set.seed(12)
  for (i in 1:150) {
    x <- counterfact_me(gender = "M", min_age = 25, max_age = 60)
    expect_false(isTRUE(x$occupation %in% female_only))
  }
})

test_that("customer-gendered titles are NOT filtered", {
  # DAMEFRISOR/HERREFRISOR and DAMESKREDDER/HERRESKREDDER describe the
  # CUSTOMER's gender, not the worker's -- a damefrisor can be a man.
  # This guards against a future over-eager MOR/FAR sweep of the register.
  tbl <- CounterfactMe:::.gendered_yrke_titles
  alle <- c(tbl$female, tbl$male)

  # Listene hadde hardkodet lengde 6 her. Det var en lat proxy for egenskapen
  # testen faktisk handler om, og den feilet to ganger paa en dag da
  # legitime titler ble lagt til (DEKKSGUTT, HOTELLGAARDSGUTT, deretter
  # STUEPIKE, HUSMORVIKAR). Nivaaet er ikke poenget -- fravaeret er.
  expect_gt(length(tbl$female), 0L)
  expect_gt(length(tbl$male), 0L)

  # De fire kundekjoennede titlene i registeret, navngitt eksplisitt. En
  # prefiksregel paa "HERRE" ville tatt feil: HERREDSRETTSDOMMER,
  # HERREDSKASSERER og tre andre HERREDS-titler kommer av "herred",
  # gammelt ord for landkommune, og er ikke kundekjoennede.
  kundekjoennet <- c("DAMEFRIS\u{00d8}R", "HERREFRIS\u{00d8}R",
                     "DAMESKREDDER", "HERRESKREDDER")
  for (k in kundekjoennet) {
    expect_false(k %in% toupper(alle),
                 info = paste(k, "beskriver kundens kjoenn, ikke arbeiderens,",
                              "og skal ikke filtreres"))
  }
  expect_false(any(grepl("FRIS", toupper(alle))))
  expect_false(any(grepl("SKREDDER", toupper(alle))))
})
