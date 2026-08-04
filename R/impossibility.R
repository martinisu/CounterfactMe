# ---------------------------------------------------------------
# Impossibility filter — blocks HARD impossible combinations.
# Improbable but possible combinations pass through.
# Used by counterfact_me() as a safety net rejection layer.
# ---------------------------------------------------------------

# Return character vector of impossibility codes found in life x.
# Empty character() = no impossibilities.
.find_impossibilities <- function(x) {
  v <- character(0)
  if (is.null(x)) return("null_life")
  age <- x$age
  if (is.null(age) || is.na(age)) age <- NA_integer_

  # --- 1. Married under 18 ---
  if (!is.na(age) && age < 18 && !is.null(x$marital_status) &&
      grepl("^Gift$|^Skilt$|^Enke$|^Enkemann$|^Separert$|^Reg",
            x$marital_status)) {
    v <- c(v, "married_under_18")
  }

  # --- 2. Master/PhD before age allows ---
  # Master normally requires age >= 23, PhD >= 26.
  if (!is.na(age) && !is.null(x$education)) {
    if (age < 23 && grepl("[Mm]aster|^[Mm]astergrad", x$education)) {
      v <- c(v, "master_too_young")
    }
    if (age < 26 && grepl("[Pp]hD|[Dd]oktor", x$education)) {
      v <- c(v, "phd_too_young")
    }
  }

  # --- 3. Mor født etter ego ---
  if (!is.null(x$mother) && !is.null(x$mother$birth_year) && !is.na(age)) {
    ego_birth <- 2026L - age
    # Mor må være minst 14 år gammel ved fødsel
    if (x$mother$birth_year > ego_birth - 14L) {
      v <- c(v, "mother_too_young_or_after_ego")
    }
  }
  if (!is.null(x$father) && !is.null(x$father$birth_year) && !is.na(age)) {
    ego_birth <- 2026L - age
    if (x$father$birth_year > ego_birth - 14L) {
      v <- c(v, "father_too_young_or_after_ego")
    }
  }

  # --- 4. Besteforeldre født etter forelder ---
  for (gp_name in c("mormor", "morfar")) {
    if (!is.null(x[[gp_name]]) && !is.null(x[[gp_name]]$birth_year) &&
        !is.null(x$mother) && !is.null(x$mother$birth_year)) {
      if (x[[gp_name]]$birth_year > x$mother$birth_year - 14L) {
        v <- c(v, paste0(gp_name, "_too_young_or_after_mor"))
      }
    }
  }
  for (gp_name in c("farmor", "farfar")) {
    if (!is.null(x[[gp_name]]) && !is.null(x[[gp_name]]$birth_year) &&
        !is.null(x$father) && !is.null(x$father$birth_year)) {
      if (x[[gp_name]]$birth_year > x$father$birth_year - 14L) {
        v <- c(v, paste0(gp_name, "_too_young_or_after_far"))
      }
    }
  }

  # --- 5. Sibling born after reference year (i.e., unborn) ---
  if (!is.null(x$siblings) && length(x$siblings) > 0) {
    for (s in x$siblings) {
      if (!is.null(s$birth_year) && s$birth_year > 2026L) {
        v <- c(v, "sibling_unborn"); break
      }
    }
  }

  # --- 6. NEET med ekte yrke ---
  if (isTRUE(x$neet) && !is.null(x$occupation) &&
      !grepl("[Uu]oppgitt|^Student|kid|teen|[Ii]kke i utdanning|[Nn]ot in education|hjelp|trygd|stoenad|stønad|sosial",
             x$occupation)) {
    v <- c(v, "neet_with_real_occupation")
  }

  # --- 7. Botid > alder ---
  if (!is.null(x$years_in_norway) && !is.na(x$years_in_norway) &&
      !is.na(age) && x$years_in_norway > age) {
    v <- c(v, "botid_exceeds_age")
  }

  # --- 8. Studieretning duplikat ---
  if (!is.null(x$field_of_study) && !is.null(x$field_of_study_detail) &&
      !is.na(x$field_of_study) && !is.na(x$field_of_study_detail) &&
      identical(as.character(x$field_of_study),
                as.character(x$field_of_study_detail))) {
    v <- c(v, "studieretning_duplicate")
  }

  # --- 9. Mann som "husmor"/"hjemmeværende husmor" ---
  for (p in c("mother", "father")) {
    if (!is.null(x[[p]]) && !is.null(x[[p]]$gender) &&
        identical(x[[p]]$gender, "M") && !is.null(x[[p]]$occupation) &&
        grepl("[Hh]usmor", x[[p]]$occupation)) {
      v <- c(v, paste0(p, "_male_husmor"))
    }
  }

  # --- 10. Husholdning vs n_children kontradiksjon ---
  if (!is.null(x$household) && !is.null(x$n_children) &&
      !is.na(x$n_children)) {
    hh <- x$household
    if (grepl("[Ee]nslig uten barn|^Single without|^Living alone|^Enslig$",
              hh) && x$n_children > 0) {
      # n_children counts ALL children (incl. moved out), so "lives alone"
      # is compatible with adult children. Not impossible. Skip.
    }
    if (grepl("med barn 0-5|with children 0-5", hh) && x$n_children == 0L) {
      v <- c(v, "household_with_kids_but_zero")
    }
    if (grepl("med barn 6-17|with children 6-17", hh) && x$n_children == 0L) {
      v <- c(v, "household_with_kids_but_zero")
    }
  }

  # --- 11. Kids under 16 (already hard-ruled in cond, men safety) ---
  if (!is.null(x$n_children) && !is.na(x$n_children) && x$n_children > 0L &&
      !is.na(age) && age < 16L) {
    v <- c(v, "kids_under_16")
  }

  # --- 12. Foreldre med barneskole-only (edu code 1) ---
  for (p in c("mother", "father")) {
    if (!is.null(x[[p]]) && !is.null(x[[p]]$education_code) &&
        !is.na(x[[p]]$education_code) && x[[p]]$education_code == 1L) {
      v <- c(v, paste0(p, "_only_barneskole"))
    }
  }

  # --- 13. Foreldre med samme navn (umulig for samkjønnede par også —
  #         eller hvis par består av to identiske mennesker) ---
  if (!is.null(x$mother) && !is.null(x$father) &&
      !is.null(x$mother$name) && !is.null(x$father$name) &&
      identical(x$mother$name, x$father$name)) {
    v <- c(v, "parents_identical_name")
  }

  v
}

# Post-hoc fixer for impossibilities — best-effort.
# Only used as last resort after rejection sampling fails.
.fix_impossibilities <- function(x, problems) {
  if (length(problems) == 0) return(x)
  for (p in problems) {
    if (identical(p, "married_under_18")) {
      x$marital_status <- if (identical(x$lang, "no")) "Ugift" else "Unmarried"
      x$marital_code <- 1L
    } else if (identical(p, "neet_with_real_occupation")) {
      x$neet <- FALSE
    } else if (identical(p, "studieretning_duplicate")) {
      x$field_of_study_detail <- x$field_of_study
      # Just drop detail to avoid duplicate
      x$field_of_study_detail <- NA_character_
    } else if (identical(p, "botid_exceeds_age")) {
      x$years_in_norway <- as.integer(min(x$years_in_norway, x$age))
    } else if (grepl("^household_with_kids_but_zero", p)) {
      # Increment n_children to at least 1 if household says kids
      x$n_children <- max(1L, as.integer(x$n_children))
    } else if (identical(p, "kids_under_16")) {
      x$n_children <- 0L
    } else if (grepl("_male_husmor$", p)) {
      who <- sub("_male_husmor$", "", p)
      if (!is.null(x[[who]])) {
        x[[who]]$occupation <- "Hjemmeværende"
      }
    } else if (identical(p, "parents_identical_name")) {
      # Append "*" so they're distinguishable; downstream rerunning is hard.
      x$father$name <- paste0(x$father$name, "*")
    }
    # mother/father/grandparent age inconsistencies + sibling_unborn are
    # hard to fix post-hoc; we leave them and just log.
  }
  x
}

#' Check whether a generated life has impossible cross-dimensional combinations
#'
#' Returns a character vector of impossibility codes. Empty if clean.
#' This is the function used internally as the rejection-sampling guard.
#'
#' @param x A counterfactme object (or list with the same fields).
#' @return Character vector of impossibility codes.
#' @export
find_impossibilities <- function(x) {
  .find_impossibilities(x)
}
