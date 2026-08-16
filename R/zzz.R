# Internal environment to cache loaded data
.cfm_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  .cfm_env$data_loaded <- FALSE
}

#' Load all package data from inst/extdata CSVs
#' @noRd
.load_data <- function() {
  if (isTRUE(.cfm_env$data_loaded)) return(invisible(NULL))

  extdata <- system.file("extdata", package = "CounterfactMe")

  .cfm_env$occupations <- read.csv(
    file.path(extdata, "occupations.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8",
    colClasses = c(code = "character", name = "character", styrk08 = "character")
  )

  .cfm_env$municipalities <- read.csv(
    file.path(extdata, "municipalities.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )

  .cfm_env$first_names <- read.csv(
    file.path(extdata, "first_names_cohort.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )
  .cfm_env$ref_year <- as.integer(format(Sys.Date(), "%Y"))

  .cfm_env$education <- read.csv(
    file.path(extdata, "education_levels.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )

  .cfm_env$income <- read.csv(
    file.path(extdata, "income_deciles.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )

  .cfm_env$households <- read.csv(
    file.path(extdata, "household_types.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )

  .cfm_env$marital <- read.csv(
    file.path(extdata, "marital_status.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )

  .cfm_env$age_dist <- read.csv(
    file.path(extdata, "age_distribution.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )

  .cfm_env$education_by_age <- read.csv(
    file.path(extdata, "education_by_age.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )

  .cfm_env$counties <- read.csv(
    file.path(extdata, "counties.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )


  .cfm_env$occupations_salary <- read.csv(
    file.path(extdata, "occupations_salary.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )

  .cfm_env$disability_by_age <- read.csv(
    file.path(extdata, "disability_by_age.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )


  .cfm_env$occupations_gender <- read.csv(
    file.path(extdata, "occupations_gender.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )

  .cfm_env$nus_fields <- read.csv(
    file.path(extdata, "nus_fields.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )

  .cfm_env$nus_by_styrk <- read.csv(
    file.path(extdata, "nus_by_styrk.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )

  .cfm_env$nus_detailed <- read.csv(
    file.path(extdata, "nus_detailed.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8",
    colClasses = c(code = "character", broad = "character", label_en = "character")
  )

  .cfm_env$nus_detail_by_styrk <- read.csv(
    file.path(extdata, "nus_detail_by_styrk.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8",
    colClasses = c(styrk2 = "character", nus_code = "character", weight = "numeric")
  )


  .cfm_env$housing_prices <- read.csv(
    file.path(extdata, "housing_prices.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8",
    colClasses = c(fylke_code = "character", boligtype_code = "character")
  )

  .cfm_env$housing_index <- read.csv(
    file.path(extdata, "housing_price_index.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8",
    colClasses = c(region_code = "character", boligtype_code = "character")
  )

  .cfm_env$fylke_index_region <- read.csv(
    file.path(extdata, "fylke_index_region.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8",
    colClasses = c(fylke_code = "character", index_region_code = "character")
  )


  .cfm_env$wealth_deciles <- read.csv(
    file.path(extdata, "wealth_deciles.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )

  .cfm_env$wealth_by_age <- read.csv(
    file.path(extdata, "wealth_by_age.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8",
    colClasses = c(age_band = "character", bracket_code = "character")
  )


  .cfm_env$immigrant_country_dist <- read.csv(
    file.path(extdata, "immigrant_country_dist.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8",
    colClasses = c(code = "character", name_region = "character")
  )

  .cfm_env$names_by_region <- read.csv(
    file.path(extdata, "names_by_region.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )


  .cfm_env$religion_baseline <- read.csv(
    file.path(extdata, "religion_baseline.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )

  .cfm_env$religion_by_region <- read.csv(
    file.path(extdata, "religion_by_region.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )

  .cfm_env$religion_by_country <- read.csv(
    file.path(extdata, "religion_by_country.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8",
    colClasses = c(code = "character")
  )

  .cfm_env$party_baseline <- read.csv(
    file.path(extdata, "party_baseline.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )


  .cfm_env$kommune_price_multiplier <- read.csv(
    file.path(extdata, "kommune_price_multiplier.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8",
    colClasses = c(kommune_code = "character", boligtype_code = "character")
  )


  .cfm_env$immigration_start_year <- read.csv(
    file.path(extdata, "immigration_start_year.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8",
    colClasses = c(code = "character")
  )


  .cfm_env$religion_humor <- read.csv(
    file.path(extdata, "religion_humor.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )

  .cfm_env$party_humor <- read.csv(
    file.path(extdata, "party_humor.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )


  .cfm_env$kommune_sentralitet <- read.csv(
    file.path(extdata, "kommune_sentralitet.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8",
    colClasses = c(kommune_code = "character")
  )

  .cfm_env$n_children_by_cohort <- read.csv(
    file.path(extdata, "n_children_by_cohort.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )

  .cfm_env$material_deprivation <- read.csv(
    file.path(extdata, "material_deprivation.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )

  .cfm_env$self_rated_health <- read.csv(
    file.path(extdata, "self_rated_health.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )

  .cfm_env$chronic_illness_prob <- read.csv(
    file.path(extdata, "chronic_illness_prob.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )

  .cfm_env$social_isolation <- read.csv(
    file.path(extdata, "social_isolation.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )


  .cfm_env$hobbies <- read.csv(
    file.path(extdata, "hobbies.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )


  .cfm_env$media_papers <- read.csv(
    file.path(extdata, "media_papers.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )
  .cfm_env$tv_hours <- read.csv(
    file.path(extdata, "tv_hours.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )
  .cfm_env$sleep_hours <- read.csv(
    file.path(extdata, "sleep_hours.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )
  .cfm_env$diet_patterns <- read.csv(
    file.path(extdata, "diet_patterns.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )
  .cfm_env$alcohol_patterns <- read.csv(
    file.path(extdata, "alcohol_patterns.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )
  .cfm_env$podcast_activity <- read.csv(
    file.path(extdata, "podcast_activity.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )
  .cfm_env$social_media_use <- read.csv(
    file.path(extdata, "social_media_use.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )


  .cfm_env$crime_victimization <- read.csv(
    file.path(extdata, "crime_victimization.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )
  .cfm_env$crime_safety_feeling <- read.csv(
    file.path(extdata, "crime_safety_feeling.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )
  .cfm_env$minor_offences <- read.csv(
    file.path(extdata, "minor_offences.csv"),
    stringsAsFactors = FALSE, encoding = "UTF-8"
  )

  .cfm_env$data_loaded <- TRUE
  invisible(NULL)
}
