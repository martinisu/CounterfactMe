#' Print a counterfactual life
#'
#' Pretty-prints the result of \code{\link{counterfact_me}} as a readable
#' life summary.
#'
#' @param x A \code{counterfactme} object.
#' @param ... Ignored.
#' @return Invisibly returns \code{x}.
#' @export
print.counterfactme <- function(x, ...) {
  is_no <- identical(x$lang, "no")

  header <- if (is_no) {
    "\n--- Ditt kontrafaktiske liv ---\n"
  } else {
    "\n--- Your counterfactual life ---\n"
  }
  cat(header)

  if (!is.null(x$name)) {
    lbl <- if (is_no) "Navn" else "Name"
    cat(sprintf("  %s: %s\n", lbl, x$name))
  }
  if (!is.null(x$gender)) {
    lbl <- if (is_no) "Kj\u{00f8}nn" else "Gender"
    g <- if (x$gender == "M") {
      if (is_no) "Mann" else "Male"
    } else {
      if (is_no) "Kvinne" else "Female"
    }
    cat(sprintf("  %s: %s\n", lbl, g))
  }
  if (!is.null(x$background) && !is.na(x$background) &&
      !identical(x$background, "majority")) {
    lbl <- if (is_no) "Bakgrunn" else "Background"
    cb <- if (!is.null(x$country_background) && !is.na(x$country_background))
            x$country_background else ""
    desc <- if (identical(x$background, "first_gen")) {
      yrs <- if (!is.null(x$years_in_norway) && !is.na(x$years_in_norway))
               sprintf(", %d ar i Norge", x$years_in_norway) else ""
      if (is_no) sprintf("Innvandret fra %s%s", cb, yrs)
      else sprintf("Immigrated from %s%s", cb,
                   if (!is.null(x$years_in_norway) && !is.na(x$years_in_norway))
                     sprintf(", %d years in Norway", x$years_in_norway) else "")
    } else if (identical(x$background, "second_gen")) {
      if (is_no) sprintf("Norskf\u{00f8}dt med foreldre fra %s", cb)
      else sprintf("Norwegian-born to parents from %s", cb)
    } else x$background
    cat(sprintf("  %s: %s\n", lbl, desc))
  }
  if (!is.null(x$age)) {
    lbl <- if (is_no) "Alder" else "Age"
    cat(sprintf("  %s: %d\n", lbl, x$age))
  }
  if (!is.null(x$municipality)) {
    lbl <- if (is_no) "Kommune" else "Municipality"
    sentr_part <- if (!is.null(x$sentralitet_label) && !is.na(x$sentralitet_label))
                    sprintf(", %s", x$sentralitet_label) else ""
    cat(sprintf("  %s: %s (%s%s)\n", lbl, x$municipality,
                x$county %||% "", sentr_part))
  }
  if (!is.null(x$occupation)) {
    lbl <- if (is_no) "Yrke" else "Occupation"
    cat(sprintf("  %s: %s\n", lbl, x$occupation))
  }
  if (!is.null(x$education)) {
    lbl <- if (is_no) "Utdanning" else "Education"
    cat(sprintf("  %s: %s\n", lbl, x$education))
  }
  if (!is.null(x$field_of_study) && !is.na(x$field_of_study)) {
    lbl <- if (is_no) "Fagfelt" else "Broad field of study"
    cat(sprintf("  %s: %s\n", lbl, x$field_of_study))
  }
  if (!is.null(x$field_of_study_detail) && !is.na(x$field_of_study_detail) &&
      !identical(x$field_of_study_detail, x$field_of_study)) {
    lbl <- if (is_no) "Studieretning" else "Field of study"
    cat(sprintf("  %s: %s\n", lbl, x$field_of_study_detail))
  }
  if (!is.null(x$income_bracket)) {
    lbl <- if (is_no) "Inntekt" else "Income"
    if (!is.null(x$ukepenger)) {
      cat(sprintf("  %s: %s\n", lbl, x$ukepenger))
    } else {
      # NEET / ytelse: vis bracket-label direkte istedenfor D-posisjon
      is_ytelse <- !is.null(x$income_bracket) &&
                   grepl("^AAP$|^Sosialhjelp$|^Ingen inntekt$|^Ikke yrkesaktiv$|^Not employed$|Work Assessment|Social assistance|No income",
                         x$income_bracket)
      if (is_ytelse) {
        if (!is.null(x$income_nok) && !is.na(x$income_nok) && x$income_nok > 0) {
          cat(sprintf("  %s: %s kr  (%s)\n", lbl,
                      format(x$income_nok, big.mark = " "), x$income_bracket))
        } else {
          cat(sprintf("  %s: %s\n", lbl, x$income_bracket))
        }
      } else {
        pos <- .income_position(x$income_nok, lang = x$lang)
        cat(sprintf("  %s: %s kr  (%s)\n", lbl,
                    format(x$income_nok, big.mark = " "), pos))
      }
    }
  }
  if (!is.null(x$household)) {
    lbl <- if (is_no) "Husholdning" else "Household"
    cat(sprintf("  %s: %s\n", lbl, x$household))
  }
  if (!is.null(x$marital_status) &&
      (is.null(x$age) || is.na(x$age) || x$age >= 18)) {
    lbl <- if (is_no) "Sivilstand" else "Marital status"
    cat(sprintf("  %s: %s\n", lbl, x$marital_status))
  }
  if (!is.null(x$partner)) {
    lbl <- if (is_no) "Partner" else "Partner"
    pinfo <- paste(c(x$partner$name,
                     if (!is.null(x$partner$occupation) && !is.na(x$partner$occupation))
                       tolower(x$partner$occupation) else NULL,
                     if (!is.null(x$partner$age) && !is.na(x$partner$age))
                       sprintf("%d \u00e5r", x$partner$age) else NULL),
                   collapse = ", ")
    cat(sprintf("  %s: %s\n", lbl, pinfo))
  }
  if (!is.null(x$parents_relationship) && !is.na(x$parents_relationship)) {
    lbl <- if (is_no) "Foreldrenes samliv" else "Parents"
    cat(sprintf("  %s: %s\n", lbl, x$parents_relationship))
  }
  if (!is.null(x$orientation) && !is.na(x$orientation)) {
    lbl <- if (is_no) "Orientering" else "Orientation"
    cat(sprintf("  %s: %s\n", lbl, x$orientation))
  }
  if (!is.null(x$religion) && !is.na(x$religion)) {
    lbl <- if (is_no) "Trosamfunn" else "Religion"
    cat(sprintf("  %s: %s\n", lbl, x$religion))
  }
  if (!is.null(x$party) && !is.na(x$party)) {
    lbl <- if (is_no) "Partipreferanse" else "Party preference"
    cat(sprintf("  %s: %s\n", lbl, x$party))
  }
  if (isTRUE(x$neet)) {
    lbl <- if (is_no) "NEET" else "NEET"
    msg <- if (is_no) "Ikke i utdanning, jobb eller oppl\u{00e6}ring" else "Not in education, employment, or training"
    cat(sprintf("  %s: %s\n", lbl, msg))
  }


  if (!is.null(x$housing_tenure) && !is.na(x$housing_tenure)) {
    lbl <- if (is_no) "Bolig" else "Housing"
    cat(sprintf("  %s: %s", lbl, x$housing_tenure))
    if (!is.null(x$housing_type) && !is.na(x$housing_type)) {
      cat(sprintf(", %s", x$housing_type))
    }
    if (!is.null(x$housing_area_m2) && !is.na(x$housing_area_m2)) {
      cat(sprintf(", %d m\u00b2", x$housing_area_m2))
    }
    cat("\n")
    if (!is.null(x$housing_value_nok) && !is.na(x$housing_value_nok)) {
      val_lbl  <- if (is_no) "    Verdi"   else "    Value"
      debt_lbl <- if (is_no) "    Gjeld"   else "    Debt"
      eq_lbl   <- if (is_no) "    Egenkapital" else "    Equity"
      cat(sprintf("%s: %s kr", val_lbl,
                  format(x$housing_value_nok, big.mark = " ")))
      if (isTRUE(x$housing_luxury)) {
        cat(if (is_no) " (luksusbolig)" else " (luxury)")
      }
      cat("\n")
      if (!is.null(x$housing_purchase_year) && !is.na(x$housing_purchase_year) &&
          !is.null(x$housing_purchase_price_nok) && !is.na(x$housing_purchase_price_nok)) {
        py_lbl <- if (is_no) "    Kjopt" else "    Bought"
        cat(sprintf("%s: %d (%s kr)\n", py_lbl,
                    x$housing_purchase_year,
                    format(x$housing_purchase_price_nok, big.mark = " ")))
      }
      if (!is.null(x$housing_debt_nok) && !is.na(x$housing_debt_nok)) {
        cat(sprintf("%s: %s kr\n", debt_lbl,
                    format(x$housing_debt_nok, big.mark = " ")))
      }
      if (!is.null(x$housing_equity_nok) && !is.na(x$housing_equity_nok)) {
        cat(sprintf("%s: %s kr\n", eq_lbl,
                    format(x$housing_equity_nok, big.mark = " ")))
      }
    }
  }

  if (isTRUE(x$has_hytte)) {
    lbl <- if (is_no) "Hytte" else "Cabin"
    typ <- if (!is.null(x$hytte_type) && !is.na(x$hytte_type)) x$hytte_type else ""
    val <- if (!is.null(x$hytte_value_nok) && !is.na(x$hytte_value_nok))
             sprintf(", %s kr", format(x$hytte_value_nok, big.mark = " ")) else ""
    cat(sprintf("  %s: %s%s\n", lbl, typ, val))
  }

  if (!is.null(x$net_wealth_nok) && !is.na(x$net_wealth_nok)) {
    lbl <- if (is_no) "Nettoformue" else "Net wealth"
    cls <- if (!is.null(x$wealth_class) && !is.na(x$wealth_class)) {
      tag <- switch(x$wealth_class,
        "top01" = if (is_no) " [topp 0,1 %]" else " [top 0.1%]",
        "top1"  = if (is_no) " [topp 1 %]"   else " [top 1%]",
        "top5"  = if (is_no) " [topp 5 %]"   else " [top 5%]",
        "")
      tag
    } else ""
    cat(sprintf("  %s: %s kr%s\n", lbl,
                format(x$net_wealth_nok, big.mark = " "), cls))
    if (!is.null(x$financial_assets_nok) && !is.na(x$financial_assets_nok) &&
        x$financial_assets_nok > 0) {
      fl <- if (is_no) "    Finansformue" else "    Financial"
      cat(sprintf("%s: %s kr\n", fl,
                  format(x$financial_assets_nok, big.mark = " ")))
    }
    if (!is.null(x$business_equity_nok) && !is.na(x$business_equity_nok) &&
        x$business_equity_nok > 0) {
      bl <- if (is_no) "    N\u{00e6}ringsformue" else "    Business equity"
      cat(sprintf("%s: %s kr\n", bl,
                  format(x$business_equity_nok, big.mark = " ")))
    }
    if (!is.null(x$capital_income_nok) && !is.na(x$capital_income_nok) &&
        x$capital_income_nok > 0) {
      cl <- if (is_no) "    Utbytte/kapitalavkastning (anslag, brutto)" else "    Dividends/returns (est., gross)"
      cat(sprintf("%s: %s kr/\u{00e5}r\n", cl,
                  format(x$capital_income_nok, big.mark = " ")))
    }
    if (!is.null(x$inheritance_nok) && !is.na(x$inheritance_nok) &&
        x$inheritance_nok > 0) {
      il <- if (is_no) "    Hvorav arv (anslag)" else "    Of which inheritance (est.)"
      cat(sprintf("%s: %s kr\n", il,
                  format(x$inheritance_nok, big.mark = " ")))
    }
  }

  if (!is.null(x$n_children) && !is.na(x$n_children) &&
      (x$n_children > 0 || is.null(x$age) || is.na(x$age) || x$age >= 18)) {
    lbl <- if (is_no) "Antall barn" else "Children"
    cat(sprintf("  %s: %d\n", lbl, x$n_children))
  }
  if (!is.null(x$self_rated_health) && !is.na(x$self_rated_health)) {
    lbl <- if (is_no) "Selvrapportert helse" else "Self-rated health"
    cat(sprintf("  %s: %s\n", lbl, x$self_rated_health))
  }
  if (isTRUE(x$has_chronic) && !is.null(x$chronic_type) && !is.na(x$chronic_type)) {
    lbl <- if (is_no) "Kronisk sykdom" else "Chronic condition"
    cat(sprintf("  %s: %s\n", lbl, x$chronic_type))
  }
  if (!is.null(x$deprivation_label) && !is.na(x$deprivation_label) &&
      !is.null(x$deprivation_count) && x$deprivation_count > 0) {
    lbl <- if (is_no) "Materielle ulemper" else "Material deprivation"
    cat(sprintf("  %s: %s\n", lbl, x$deprivation_label))
  }
  if (isTRUE(x$has_disability) && !is.null(x$disability) && !is.na(x$disability)) {
    lbl <- if (is_no) "Funksjonsnedsettelse" else "Disability"
    cat(sprintf("  %s: %s\n", lbl, x$disability))
  }
  if (!is.null(x$close_friends) && !is.na(x$close_friends)) {
    lbl <- if (is_no) "N\u{00e6}re venner" else "Close friends"
    cat(sprintf("  %s: %s\n", lbl, x$close_friends))
  }
  if (!is.null(x$has_confidant) && !is.na(x$has_confidant)) {
    lbl <- if (is_no) "Fortrolig venn" else "Confidant"
    val <- if (isTRUE(x$has_confidant)) (if (is_no) "ja" else "yes") else (if (is_no) "nei" else "no")
    cat(sprintf("  %s: %s\n", lbl, val))
  }
  if (!is.null(x$loneliness) && !is.na(x$loneliness)) {
    lbl <- if (is_no) "Ensomhet" else "Loneliness"
    cat(sprintf("  %s: %s\n", lbl, x$loneliness))
  }
  if (!is.null(x$trust) && !is.na(x$trust)) {
    lbl <- if (is_no) "Generell tillit (0-10)" else "Generalized trust (0-10)"
    cat(sprintf("  %s: %d\n", lbl, x$trust))
  }

  if (!is.null(x$hobbies) && length(x$hobbies) > 0) {
    lbl <- if (is_no) "Hobbier" else "Hobbies"
    hobby_list <- paste(unlist(x$hobbies), collapse = ", ")
    cat(sprintf("  %s: %s\n", lbl, hobby_list))
  }

  if (!is.null(x$media_paper) && !is.na(x$media_paper)) {
    lbl <- if (is_no) "Avis-favoritt" else "Newspaper preference"
    cat(sprintf("  %s: %s\n", lbl, x$media_paper))
  }
  if (!is.null(x$media_tv_hours) && !is.na(x$media_tv_hours)) {
    lbl <- if (is_no) "TV-tid (t/dag)" else "TV time (h/day)"
    cat(sprintf("  %s: %.1f\n", lbl, x$media_tv_hours))
  }
  if (!is.null(x$media_podcast) && !is.na(x$media_podcast)) {
    lbl <- if (is_no) "Podcast-lytting" else "Podcast listening"
    cat(sprintf("  %s: %s\n", lbl, x$media_podcast))
  }
  if (!is.null(x$media_social) && !is.na(x$media_social)) {
    lbl <- if (is_no) "Sosiale medier" else "Social media"
    cat(sprintf("  %s: %s\n", lbl, x$media_social))
  }
  if (!is.null(x$sleep_hours) && !is.na(x$sleep_hours)) {
    lbl <- if (is_no) "S\u{00f8}vn (t/natt)" else "Sleep (h/night)"
    cat(sprintf("  %s: %.1f\n", lbl, x$sleep_hours))
  }
  if (!is.null(x$diet) && !is.na(x$diet)) {
    lbl <- if (is_no) "Kosthold" else "Diet"
    cat(sprintf("  %s: %s\n", lbl, x$diet))
  }
  if (!is.null(x$alcohol_pattern) && !is.na(x$alcohol_pattern)) {
    lbl <- if (is_no) "Alkohol" else "Alcohol"
    cat(sprintf("  %s: %s\n", lbl, x$alcohol_pattern))
  }

  if (!is.null(x$crime_safety) && !is.na(x$crime_safety)) {
    lbl <- if (is_no) "Opplevd trygghet" else "Perceived safety"
    cat(sprintf("  %s: %s\n", lbl, x$crime_safety))
  }
  if (!is.null(x$crime_victimizations) && length(x$crime_victimizations) > 0) {
    lbl <- if (is_no) "Utsatt for (siste 12 mnd)" else "Victimized (last 12 mo)"
    cat(sprintf("  %s: %s\n", lbl,
                paste(x$crime_victimizations, collapse = ", ")))
  }
  if (!is.null(x$minor_offence) && !is.na(x$minor_offence) &&
      !grepl("^Ingen$|^None$", x$minor_offence)) {
    lbl <- if (is_no) "Lovbrudd-historie" else "Minor offence"
    cat(sprintf("  %s: %s\n", lbl, x$minor_offence))
  }

  if (!is.null(x$bourdieu_klasse) && !is.na(x$bourdieu_klasse)) {
    lbl <- if (isTRUE(x$bourdieu_origin)) {
      if (is_no) "Klassebakgrunn (fra foreldrene)" else "Class background (from parents)"
    } else if (is_no) "Bourdieu-profil" else "Bourdieu profile"
    cat(sprintf("  %s: %s\n", lbl, x$bourdieu_klasse))
    cat(sprintf("    \u{00d8}konomisk: %.0f / Kulturell: %.0f / Sosial: %.0f\n",
                x$bourdieu_okonomisk, x$bourdieu_kulturell, x$bourdieu_sosial))
  }


  cat("\n")
  if (!is.null(x$mother) || !is.null(x$father)) {
    lbl <- if (is_no) "Foreldre" else "Parents"
    cat(sprintf("  %s:\n", lbl))
    .fmt_parent <- function(p, role_no, role_en) {
      if (is.null(p)) return(invisible(NULL))
      role <- if (is_no) role_no else role_en
      name <- if (!is.null(p$name) && !is.na(p$name)) p$name else "?"
      occ <- if (!is.null(p$occupation) && !is.na(p$occupation)) p$occupation else ""
      edu <- if (!is.null(p$education) && !is.na(p$education)) p$education else ""
      by  <- if (!is.null(p$birth_year) && !is.na(p$birth_year)) p$birth_year else NA
      dy  <- if (!is.null(p$death_year) && !is.na(p$death_year)) p$death_year else NA
      year_str <- if (!is.na(by) && !is.na(dy)) {
        sprintf("(f. %d, d. %d)", by, dy)
      } else if (!is.na(by)) {
        sprintf("(f. %d)", by)
      } else ""
      pieces <- c(
        if (nzchar(occ)) occ else NULL,
        if (nzchar(edu)) edu else NULL
      )
      detail <- if (length(pieces) > 0) paste0(" -- ", paste(pieces, collapse = ", ")) else ""
      cat(sprintf("    %s: %s %s%s\n", role, name, year_str, detail))
    }
    cs <- if (!is.null(x$couple_sex)) x$couple_sex else "FM"
    if (identical(cs, "FF")) {
      .fmt_parent(x$mother, "Mor", "Mother")
      .fmt_parent(x$father, "Medmor", "Co-mother")
    } else if (identical(cs, "MM")) {
      .fmt_parent(x$mother, "Far", "Father")
      .fmt_parent(x$father, "Medfar", "Co-father")
    } else {
      .fmt_parent(x$mother, "Mor", "Mother")
      .fmt_parent(x$father, "Far", "Father")
    }
    if (!is.null(x$parents_capital) && !is.na(x$parents_capital)) {
      both_dead <- !is.null(x$mother$death_year) && !is.na(x$mother$death_year) &&
                   !is.null(x$father$death_year) && !is.na(x$father$death_year)
      lbl <- if (both_dead) {
        if (is_no) "Foreldrekapital (f\u{00f8}r arveoppgj\u{00f8}r)" else "Parental capital (pre-inheritance)"
      } else {
        if (is_no) "Anslatt foreldrekapital" else "Est. parental capital"
      }
      mnok <- x$parents_capital / 1e6
      cat(sprintf("    %s: %.1f MNOK\n", lbl, mnok))
    }
  }

  # Siblings
  if (!is.null(x$n_siblings) && !is.na(x$n_siblings) && x$n_siblings > 0) {
    lbl <- if (is_no) "S\u{00f8}sken" else "Siblings"
    cat(sprintf("  %s: %d\n", lbl, x$n_siblings))
    if (!is.null(x$siblings) && length(x$siblings) > 0) {
      for (s in x$siblings) {
        relation_no <- if (s$age_delta < 0) sprintf("(%d \u{00e5}r eldre)", -s$age_delta)
                       else if (s$age_delta > 0) sprintf("(%d \u{00e5}r yngre)", s$age_delta)
                       else "(jevnaldring)"
        relation_en <- if (s$age_delta < 0) sprintf("(%d yrs older)", -s$age_delta)
                       else if (s$age_delta > 0) sprintf("(%d yrs younger)", s$age_delta)
                       else "(same age)"
        rel <- if (is_no) relation_no else relation_en
        cat(sprintf("    %s %s\n", s$name, rel))
      }
    }
  }

  # Grandparents (besteforeldre)
  if (!is.null(x$mormor) || !is.null(x$morfar) ||
      !is.null(x$farmor) || !is.null(x$farfar)) {
    lbl <- if (is_no) "Besteforeldre" else "Grandparents"
    cat(sprintf("  %s:\n", lbl))
    .fmt_gp <- function(p, role_no, role_en) {
      if (is.null(p)) return(invisible(NULL))
      role <- if (is_no) role_no else role_en
      year_str <- if (!is.null(p$death_year) && !is.na(p$death_year)) {
        sprintf("(f. %d, d. %d)", p$birth_year, p$death_year)
      } else {
        sprintf("(f. %d)", p$birth_year)
      }
      cat(sprintf("    %s: %s %s\n", role, p$name, year_str))
    }
    .fmt_gp(x$mormor, "Mormor", "Maternal grandmother")
    .fmt_gp(x$morfar, "Morfar", "Maternal grandfather")
    .fmt_gp(x$farmor, "Farmor", "Paternal grandmother")
    .fmt_gp(x$farfar, "Farfar", "Paternal grandfather")
  }

  invisible(x)
}

#' Null-coalescing operator
#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Income position label — rough Norwegian individual income deciles (2024 after-tax).
.income_position <- function(nok, lang = "en") {
  no <- identical(lang, "no")
  if (is.null(nok) || is.na(nok)) return(if (no) "ukjent" else "unknown")
  if (nok < 200000) return(if (no) "Desil 1 - lavt, under fattigdomsgrensen" else "Decile 1 - low, below poverty line")
  if (nok < 280000) return(if (no) "Desil 2 - lavt" else "Decile 2 - low")
  if (nok < 340000) return(if (no) "Desil 3 - under medianen" else "Decile 3 - below median")
  if (nok < 410000) return(if (no) "Desil 4 - rett under medianen" else "Decile 4 - just below median")
  if (nok < 480000) return(if (no) "Desil 5 - midt p\u{00e5} treet" else "Decile 5 - middle of the pack")
  if (nok < 560000) return(if (no) "Desil 6 - over medianen" else "Decile 6 - above median")
  if (nok < 660000) return(if (no) "Desil 7 - over snittet" else "Decile 7 - above average")
  if (nok < 810000) return(if (no) "Desil 8 - godt over snittet" else "Decile 8 - well above average")
  if (nok < 1100000) return(if (no) "Desil 9 - blant de h\u{00f8}yest betalte" else "Decile 9 - among top earners")
  if (nok < 2000000) return(if (no) "Topp 10 %" else "Top 10%")
  if (nok < 5000000) return(if (no) "Topp 1 %" else "Top 1%")
  return(if (no) "Topp 0,1 %" else "Top 0.1%")
}
