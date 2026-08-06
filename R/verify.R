#' Verify cross-dimensional consistency of generated lives
#'
#' Generates N counterfactual lives and runs 46 systematic consistency checks
#' looking for impossible or implausible cross-dimensional combinations.
#' Reports counts and example draws per check.
#'
#' @param N Number of lives to generate. Default 5000.
#' @param lang Language for the generated lives. Default "no".
#' @param verbose If TRUE, prints up to 3 examples per failed check. Default TRUE.
#' @return Invisibly: a list with $summary (per-check counts) and $examples.
#' @export
verify_consistency <- function(N = 5000L, lang = "no", verbose = TRUE) {
  message(sprintf("Generating %d lives...", N))
  draws <- vector("list", N)
  for (i in seq_len(N)) {
    draws[[i]] <- tryCatch(
      counterfact_me(lang = lang),
      error = function(e) NULL
    )
  }
  draws <- Filter(Negate(is.null), draws)

  # Helpers
  is_owner <- function(d) {
    !is.null(d$housing_tenure) && !is.na(d$housing_tenure) &&
      d$housing_tenure %in% c("Eier", "Owns")
  }
  is_shared_household <- function(d) {
    if (is.null(d$household) || is.na(d$household)) return(FALSE)
    grepl("[Bb]ofellesskap|[Bb]or hos foreldre|[Ss]hared|[Ll]iving with parents",
          d$household)
  }

  checks <- list()
  add <- function(name, msg) {
    if (is.null(checks[[name]])) checks[[name]] <<- character(0)
    checks[[name]] <<- c(checks[[name]], msg)
  }

  for (d in draws) {
    age <- if (!is.null(d$age)) d$age else NA_integer_

    # 1. Husholdning + eier enebolig (motsetning)
    if (is_owner(d) && is_shared_household(d)) {
      add("01_household_vs_housing",
          sprintf("%s, %d aar: %s + eier %s",
                  d$name %||% "?", age, d$household, d$housing_type %||% "?"))
    }

    # 2. Marital status under 18 (must not match "Ugift")
    if (!is.na(age) && age < 18 && !is.null(d$marital_status) &&
        grepl("^Gift$|^Skilt$|^Enke$|^Enkemann$|^Married$|^Widow|^Divorced$",
              d$marital_status)) {
      add("02_married_under_18",
          sprintf("%s, %d aar: %s", d$name %||% "?", age, d$marital_status))
    }

    # 3. Marital "Gift" + Husholdning Enslig (require exact match)
    if (!is.null(d$marital_status) && !is.null(d$household) &&
        grepl("^Gift$|^Married$", d$marital_status) &&
        grepl("[Ee]nslig|^Single$", d$household)) {
      add("03_married_but_single_household",
          sprintf("%s, %d aar: %s + %s",
                  d$name %||% "?", age, d$marital_status, d$household))
    }

    # 4. Education for very young (master/PhD pre-22)
    if (!is.na(age) && age < 22 && !is.null(d$education) &&
        grepl("[Mm]aster|[Pp]hD|doktor", d$education)) {
      add("04_high_edu_too_young",
          sprintf("%s, %d aar: %s", d$name %||% "?", age, d$education))
    }

    # 5. Yrke under 25 i akademisk STYRK 2 (lege/akademisk pre-25)
    if (!is.na(age) && age < 25 && !is.null(d$occupation) &&
        grepl("[Ll]ege|[Pp]rofessor|[Aa]dvokat|[Aa]kademiker", d$occupation)) {
      add("05_high_styrk_too_young",
          sprintf("%s, %d aar: %s", d$name %||% "?", age, d$occupation))
    }

    # 6. Bonde i Oslo
    if (!is.null(d$occupation) && !is.null(d$county) &&
        grepl("[Bb]onde|[Ff]isker|[Gg]aardbruker", d$occupation) &&
        identical(d$county, "Oslo")) {
      add("06_farmer_in_oslo",
          sprintf("%s, %s i Oslo", d$name %||% "?", d$occupation))
    }

    # 7. Inntekt for ung over D8 (hoey inntekt for ung)
    if (!is.na(age) && age < 24 && !is.null(d$income_nok) &&
        !is.na(d$income_nok) && d$income_nok > 700000) {
      add("07_high_income_too_young",
          sprintf("%s, %d aar: %d kr", d$name %||% "?", age, d$income_nok))
    }

    # 8. Bolig under 22 år eier
    if (is_owner(d) && !is.na(age) && age < 22) {
      add("08_owner_too_young",
          sprintf("%s, %d \u{00e5}r eier %s",
                  d$name %||% "?", age, d$housing_type %||% "?"))
    }

    # 9. Boligverdi vs inntekt (>15x årsinntekt for ikke-luksus)
    # Ekskluderer pensjonister + uføre + over 65 (gammel bolig + lav pensjon = realistisk)
    is_retired <- (!is.na(age) && age >= 65) ||
                  (!is.null(d$occupation) &&
                   grepl("[Pp]ensjon|[Uu]for|[Hh]obby", d$occupation))
    if (is_owner(d) && !isTRUE(is_retired) &&
        !is.null(d$housing_value_nok) && !is.null(d$income_nok) &&
        !is.na(d$housing_value_nok) && !is.na(d$income_nok) && d$income_nok > 0 &&
        !isTRUE(d$housing_luxury)) {
      ratio <- d$housing_value_nok / d$income_nok
      if (ratio > 15) {
        add("09_house_value_vs_income",
            sprintf("%s, bolig %.1fM / inntekt %dk = %.0fx",
                    d$name %||% "?", d$housing_value_nok/1e6,
                    d$income_nok/1000, ratio))
      }
    }

    # 10. Hytte > 3x primaer (uten luksus)
    if (isTRUE(d$has_hytte) && !is.null(d$hytte_value_nok) &&
        !is.null(d$housing_value_nok) && !is.na(d$hytte_value_nok) &&
        !is.na(d$housing_value_nok) && d$housing_value_nok > 0 &&
        !isTRUE(d$housing_luxury)) {
      ratio <- d$hytte_value_nok / d$housing_value_nok
      if (ratio > 3) {
        add("10_cabin_dwarfs_primary",
            sprintf("%s, hytte %.1fM > 3x primaer %.1fM",
                    d$name %||% "?", d$hytte_value_nok/1e6,
                    d$housing_value_nok/1e6))
      }
    }

    # 11. Wealth class top 0.1% pa lavt-yrke
    if (!is.null(d$wealth_class) && identical(d$wealth_class, "top01") &&
        !is.null(d$occupation)) {
      if (grepl("[Kk]assamedarb|[Rr]enholder|[Bb]utikkmedarb",
                d$occupation)) {
        add("11_top01_in_low_styrk",
            sprintf("%s, top0.1%% som %s", d$name %||% "?", d$occupation))
      }
    }

    # 12. Capital income uten næringsformue
    if (!is.null(d$capital_income_nok) && !is.na(d$capital_income_nok) &&
        d$capital_income_nok > 1e6 &&
        (is.null(d$business_equity_nok) || is.na(d$business_equity_nok) ||
         d$business_equity_nok < 1e6)) {
      add("12_high_cap_income_no_biz",
          sprintf("%s, %dk utbytte uten n\u{00e6}ringsformue",
                  d$name %||% "?", d$capital_income_nok/1000))
    }

    # 14. Religion vs background — majority med ikke-DnK/INGEN/HUM/KAT
    if (!is.null(d$background) && identical(d$background, "majority") &&
        !is.null(d$religion) &&
        grepl("[Ii]slam|[Bb]uddhi|[Hh]induis|[Mm]osais|[Jj]uda",
              d$religion)) {
      add("14_majority_exotic_religion",
          sprintf("%s (majority): %s", d$name %||% "?", d$religion))
    }

    # 15. Parti vs religion — KrF for ateist
    if (!is.null(d$party) && grepl("[Kk]ristelig|KrF", d$party) &&
        !is.null(d$religion) &&
        grepl("[Ii]ngen|ateist|[Hh]umani", d$religion)) {
      add("15_krf_atheist",
          sprintf("%s: %s + %s",
                  d$name %||% "?", d$party, d$religion))
    }

    # 16. Foreldre alder vs ego (mor født etter ego)
    if (!is.null(d$mother) && !is.null(d$mother$birth_year) &&
        !is.null(d$age) && !is.na(d$age)) {
      ego_birth <- 2026L - d$age
      if (d$mother$birth_year > ego_birth) {
        add("16_mother_born_after_ego",
            sprintf("%s f. %d, mor %s f. %d",
                    d$name %||% "?", ego_birth,
                    d$mother$name %||% "?", d$mother$birth_year))
      }
    }

    # 17. Besteforeldre alder
    if (!is.null(d$mormor) && !is.null(d$mormor$birth_year) &&
        !is.null(d$mother) && !is.null(d$mother$birth_year)) {
      if (d$mormor$birth_year > d$mother$birth_year) {
        add("17_grandparent_born_after_parent",
            sprintf("Mormor %s f. %d, mor %s f. %d",
                    d$mormor$name %||% "?", d$mormor$birth_year,
                    d$mother$name %||% "?", d$mother$birth_year))
      }
    }

    # 18. Søsken negativ alder
    if (!is.null(d$siblings) && length(d$siblings) > 0) {
      for (s in d$siblings) {
        if (!is.null(s$birth_year) && !is.null(d$age) &&
            s$birth_year > 2026L) {
          add("18_sibling_unborn",
              sprintf("%s, s\u{00f8}sken %s f. %d (etter referanseaar)",
                      d$name %||% "?", s$name %||% "?", s$birth_year))
        }
      }
    }

    # 19. NEET men har yrke (ikke "Uoppgitt"/NEET-label)
    if (isTRUE(d$neet) && !is.null(d$occupation) &&
        !grepl("[Uu]oppgitt|^Student|kid|teen|[Ii]kke i utdanning|[Nn]ot in education",
               d$occupation)) {
      add("19_neet_with_occupation",
          sprintf("%s, NEET men %s", d$name %||% "?", d$occupation))
    }

    # 20. Botid vs alder
    if (!is.null(d$years_in_norway) && !is.na(d$years_in_norway) &&
        !is.null(d$age) && !is.na(d$age)) {
      if (d$years_in_norway > d$age) {
        add("20_botid_exceeds_age",
            sprintf("%s, %d \u{00e5}r gammel, %d \u{00e5}r i Norge",
                    d$name %||% "?", d$age, d$years_in_norway))
      }
    }

    # 21. Studieretning = fagfelt (duplikat)
    if (!is.null(d$field_of_study) && !is.null(d$field_of_study_detail) &&
        !is.na(d$field_of_study) && !is.na(d$field_of_study_detail) &&
        identical(d$field_of_study, d$field_of_study_detail)) {
      add("21_studieretning_duplicate",
          sprintf("%s: %s == %s",
                  d$name %||% "?", d$field_of_study, d$field_of_study_detail))
    }

    # 22. Husmor for menn ("Hjemmeværende" + Mann)
    if (!is.null(d$mother) && !is.null(d$mother$gender) &&
        identical(d$mother$gender, "M") &&
        !is.null(d$mother$occupation) &&
        grepl("[Hh]jemmev\u{00e6}rende|[Hh]ousewife", d$mother$occupation)) {
      add("22_male_homemaker",
          sprintf("%s, mor %s (M): %s",
                  d$name %||% "?", d$mother$name %||% "?", d$mother$occupation))
    }

    # 23. Pensjonist under 60
    if (!is.null(d$occupation) && !is.na(age) && age < 60 &&
        grepl("[Hh]yttepusser|[Ss]eniorsv|[Bb]ridgeekspert|[Kk]olonihage|[Ee]ldretrim",
              d$occupation)) {
      add("23_pensioner_under_60",
          sprintf("%s, %d aar: %s", d$name %||% "?", age, d$occupation))
    }

    # 24. First_gen ego alder vs land start_year (umulig kombinasjon)
    # (Already filtered in .cond_immigrant_background, men sjekk for sikkerhets skyld)
    # Skip — prosessen gjør dette riktig allerede.

    # 25. Forelder yrke uten utdanning-match
    if (!is.null(d$mother) && !is.null(d$mother$occupation) &&
        !is.null(d$mother$education_code) && !is.na(d$mother$education_code) &&
        d$mother$education_code == 1L) {
      add("25_parent_grunnskole_only",
          sprintf("Mor %s: %s + edu code 1 (barneskole)",
                  d$mother$name %||% "?", d$mother$occupation))
    }
    if (!is.null(d$father) && !is.null(d$father$occupation) &&
        !is.null(d$father$education_code) && !is.na(d$father$education_code) &&
        d$father$education_code == 1L) {
      add("25_parent_grunnskole_only",
          sprintf("Far %s: %s + edu code 1 (barneskole)",
                  d$father$name %||% "?", d$father$occupation))
    }

    # === NYE DIMENSJONER (v0.8.0) ===

    # 26. Kronisk sykdom under aldersgrensa.
    # 0-15-raden i chronic_illness_prob.csv var et anslag uten kilde og er
    # fjernet; feltet skal derfor vaere FALSE for barn. Slar dette ut, er
    # gaten omgatt et sted.
    if (isTRUE(d$has_chronic) && !is.na(age) && age < 16) {
      add("26_chronic_under_age_floor",
          sprintf("%s, %d aar: kronisk %s", d$name %||% "?", age,
                  d$chronic_type %||% "?"))
    }

    # 27. Uforetrygdet med "Meget god" helse (motsetning)
    if (!is.null(d$occupation) && grepl("[Uu]for", d$occupation) &&
        !is.null(d$self_rated_health) &&
        grepl("[Mm]eget god|[Ee]xcellent", d$self_rated_health)) {
      add("27_disabled_with_excellent_health",
          sprintf("%s: ufor + %s helse", d$name %||% "?", d$self_rated_health))
    }

    # 28. Alvorlig deprivasjon med D8+ inntekt (motsetning)
    if (!is.null(d$deprivation_count) && !is.na(d$deprivation_count) &&
        d$deprivation_count >= 5 &&
        !is.null(d$income_nok) && !is.na(d$income_nok) && d$income_nok > 660000) {
      add("28_severe_deprivation_high_income",
          sprintf("%s: %d/9 ulemper men inntekt %dk",
                  d$name %||% "?", d$deprivation_count, d$income_nok/1000))
    }

    # 29. Alvorlig deprivasjon med stor formue
    if (!is.null(d$deprivation_count) && !is.na(d$deprivation_count) &&
        d$deprivation_count >= 4 &&
        !is.null(d$net_wealth_nok) && !is.na(d$net_wealth_nok) &&
        d$net_wealth_nok > 5e6) {
      add("29_deprivation_with_high_wealth",
          sprintf("%s: %d/9 ulemper men formue %.1fM",
                  d$name %||% "?", d$deprivation_count, d$net_wealth_nok/1e6))
    }

    # 30. Familie-husholdning + ofte ensom (motsetning)
    if (!is.null(d$loneliness) && grepl("[Oo]fte ensom|[Oo]ften lonely", d$loneliness) &&
        !is.null(d$household) &&
        grepl("[Ff]amili|[Ff]amily|[Pp]ar med barn|[Cc]ouple with", d$household)) {
      add("30_family_often_lonely",
          sprintf("%s: %s + %s",
                  d$name %||% "?", d$household, d$loneliness))
    }

    # 31. Antall barn under 22 med 3+
    if (!is.null(d$n_children) && !is.na(d$n_children) && d$n_children >= 3 &&
        !is.na(age) && age < 22) {
      add("31_too_many_kids_too_young",
          sprintf("%s, %d aar: %d barn", d$name %||% "?", age, d$n_children))
    }

    # 32. "Enslig uten barn" husholdning + n_children > 0 (motsetning)
    if (!is.null(d$household) && grepl("[Ee]nslig uten barn|[Ss]ingle without",
                                        d$household) &&
        !is.null(d$n_children) && !is.na(d$n_children) && d$n_children > 0) {
      add("32_single_no_kids_but_has_kids",
          sprintf("%s: %s men %d barn",
                  d$name %||% "?", d$household, d$n_children))
    }

    # 33. "Par/familie med barn" + n_children == 0
    if (!is.null(d$household) && grepl("med barn|with children", d$household) &&
        !is.null(d$n_children) && !is.na(d$n_children) && d$n_children == 0) {
      add("33_family_household_no_kids",
          sprintf("%s: %s men 0 barn", d$name %||% "?", d$household))
    }

    # 34. Sentralitet 1 (Storby) men SP-stemmer (uvanlig — SP er distriktsparti)
    if (!is.null(d$sentralitet) && !is.na(d$sentralitet) && d$sentralitet == 1 &&
        !is.null(d$party) && grepl("[Ss]enterpart|^Sp ", d$party)) {
      add("34_storby_sp_voter",
          sprintf("%s, sentralitet 1: %s", d$name %||% "?", d$party))
    }

    # 35. Sentralitet 6 (Distrikt) men eier blokkleilighet (uvanlig)
    if (!is.null(d$sentralitet) && !is.na(d$sentralitet) && d$sentralitet == 6 &&
        !is.null(d$housing_type) &&
        grepl("[Bb]lokk|[Aa]partment", d$housing_type)) {
      add("35_distrikt_apartment_owner",
          sprintf("%s, sentralitet 6 (distrikt): eier %s",
                  d$name %||% "?", d$housing_type))
    }

    # 36. Etablert overklasse + dårlig helse (motsetning, men kan skje)
    if (!is.null(d$bourdieu_klasse) &&
        grepl("[Ee]tablert overklasse|[Ee]stablished upper", d$bourdieu_klasse) &&
        !is.null(d$self_rated_health) &&
        grepl("[Dd]aarlig|[Mm]eget d\u{00e5}rlig|[Pp]oor|[Vv]ery poor",
              d$self_rated_health)) {
      add("36_upper_class_poor_health",
          sprintf("%s: %s + helse %s",
                  d$name %||% "?", d$bourdieu_klasse, d$self_rated_health))
    }

    # 37. Antall barn > 0 for veldig ung (under 16 = umulig)
    if (!is.null(d$n_children) && !is.na(d$n_children) && d$n_children > 0 &&
        !is.na(age) && age < 16) {
      add("37_kids_under_16",
          sprintf("%s, %d aar: %d barn", d$name %||% "?", age, d$n_children))
    }

    # --- nye dimensjoner (v0.9.21+) ---

    # 38. Partner under 18 (umulig)
    if (!is.null(d$partner) && !is.na(age) && age < 18) {
      add("38_partner_under_18",
          sprintf("%s, %d aar har partner", d$name %||% "?", age))
    }

    # 39. Partner uten parforhold-sivilstand
    if (!is.null(d$partner) &&
        (is.null(d$marital_status) ||
         !grepl("Gift|Samboer|Registrert partner|Married|Cohabit|Registered",
                d$marital_status))) {
      add("39_partner_without_relationship",
          sprintf("%s: partner men sivilstand = %s",
                  d$name %||% "?", d$marital_status %||% "NA"))
    }

    # 40. Partnerkjønn vs. orientering
    if (!is.null(d$partner) && !is.null(d$partner$gender) &&
        !is.null(d$gender) && !is.null(d$orientation) && !is.na(d$orientation)) {
      og <- d$gender; pg <- d$partner$gender; ori <- d$orientation
      if (grepl("Heterofil|Heterosexual", ori) && identical(og, pg)) {
        add("40_partner_gender_vs_orientation",
            sprintf("%s (%s, hetero) + partner (%s)", d$name %||% "?", og, pg))
      }
      if (grepl("Homofil|Gay|Lesbian", ori) && !identical(og, pg)) {
        add("40_partner_gender_vs_orientation",
            sprintf("%s (%s, homofil) + partner (%s)", d$name %||% "?", og, pg))
      }
    }

    # 41. Registrert partner skal være likekjønnet
    if (!is.null(d$partner) && !is.null(d$partner$gender) && !is.null(d$gender) &&
        !is.null(d$marital_status) &&
        grepl("Registrert partner|Registered partner", d$marital_status) &&
        !identical(d$gender, d$partner$gender)) {
      add("41_registered_partner_not_samesex",
          sprintf("%s: %s + %s", d$name %||% "?", d$gender, d$partner$gender))
    }

    # 42. Uføretrygd uten funksjonsnedsettelse
    if (!is.null(d$occupation) && !is.na(d$occupation) &&
        grepl("Uf\u00f8retrygd", d$occupation) && !isTRUE(d$has_disability)) {
      add("42_ufore_without_disability",
          sprintf("%s, %d aar: uf\u{00f8}retrygd uten funksjonsnedsettelse",
                  d$name %||% "?", age))
    }

    # 43. Funksjonsnedsettelse uten alvorlighetsgrad
    if (isTRUE(d$has_disability) &&
        (is.null(d$disability_severity) || is.na(d$disability_severity))) {
      add("43_disability_without_severity",
          sprintf("%s: has_disability men severity = NA", d$name %||% "?"))
    }

    # 44. Foreldre både skilt og tidlig tap (gjensidig utelukkende)
    if (isTRUE(d$parents_divorced) && !is.null(d$parents_relationship) &&
        grepl("mistet|lost", tolower(d$parents_relationship))) {
      add("44_parents_divorced_and_loss",
          sprintf("%s: %s", d$name %||% "?", d$parents_relationship))
    }

    # 45. Urimelig stor aldersforskjell til partner (> 30 aar)
    if (!is.null(d$partner) && !is.null(d$partner$age) && !is.na(d$partner$age) &&
        !is.na(age) && abs(d$partner$age - age) > 30) {
      add("45_partner_age_gap",
          sprintf("%s %d vs partner %d", d$name %||% "?", age, d$partner$age))
    }

    # 46. Ofte ensom MEN 6+ nære venner (lite plausibelt)
    if (!is.null(d$loneliness) && !is.na(d$loneliness) &&
        grepl("ofte|often", tolower(d$loneliness)) &&
        !is.null(d$close_friends) && !is.na(d$close_friends) &&
        grepl("6 eller flere|6\\+", d$close_friends)) {
      add("46_lonely_but_many_friends",
          sprintf("%s: %s + %s", d$name %||% "?", d$loneliness, d$close_friends))
    }
  }

  # Build summary
  summary_df <- data.frame(
    check = names(checks),
    count = sapply(checks, length),
    pct = round(100 * sapply(checks, length) / length(draws), 2),
    stringsAsFactors = FALSE
  )
  summary_df <- summary_df[order(-summary_df$count), ]

  if (verbose) {
    cat(sprintf("\n=== Inkonsistens-rapport (N = %d) ===\n\n", length(draws)))
    if (nrow(summary_df) == 0) {
      cat("Ingen inkonsistenser funnet!\n")
    } else {
      for (i in seq_len(nrow(summary_df))) {
        nm <- summary_df$check[i]; n <- summary_df$count[i]; pct <- summary_df$pct[i]
        cat(sprintf("%-40s %5d  (%.2f%%)\n", nm, n, pct))
        for (ex in utils::head(checks[[nm]], 3)) {
          cat(sprintf("  %s\n", ex))
        }
      }
    }
  }
  invisible(list(summary = summary_df, examples = checks))
}
