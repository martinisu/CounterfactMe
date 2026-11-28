# ============================================================
# Field of study for occupations whose training defines the field
# (November 2026)
#
# The field is drawn from SSB 12849, which gives field by education level
# for each TWO-digit occupation group. Group 23 holds barnehagelaerere,
# grunnskolelaerere and lektorer alike, so a barnehagelaerer could get a
# master's in vokal og instrumentalmusikk, and the detailed field inside
# the drawn two-digit field was then drawn without regard to the job: an
# electrician with "fordypning i mekaniske fag".
#
# For four-digit codes where the job is defined by its training, the
# rules below restrict the two-digit field to the ones that training
# gives, and name the detailed field inside it. They apply only from the
# education level where that training sits (min_edu). Within the allowed
# fields, 12849's counts still decide. The mappings follow the NUS2000
# field codes and are assumptions, not data.
# ============================================================

.OCC_FIELD_RULES <- list(
  # code     fields (NUS 2-digit)            detail inside them              min_edu
  "2342" = list(f2 = "21",                    detail = c("21" = "211"),      min_edu = 6L),
  "2341" = list(f2 = c("22", "23", "25", "24"), detail = c("22" = "221"),    min_edu = 6L),
  "2352" = list(f2 = c("24", "25", "22"),     detail = c("24" = "242"),      min_edu = 6L),
  "2622" = list(f2 = c("12"),                 detail = c("12" = "122"),      min_edu = 6L),
  "2411" = list(f2 = c("41"),                 detail = c("41" = "411"),      min_edu = 6L),
  "3313" = list(f2 = c("41", "43"),           detail = c("41" = "411"),      min_edu = 3L),
  "2142" = list(f2 = c("57"),                 detail = c("57" = "571"),      min_edu = 6L),
  "2144" = list(f2 = c("55"),                 detail = c("55" = "552"),      min_edu = 6L),
  "3355" = list(f2 = c("82"),                 detail = c("82" = "823"),      min_edu = 5L),
  "3251" = list(f2 = c("64"),                 detail = c("64" = "642"),      min_edu = 5L),
  "5321" = list(f2 = c("61"),                 detail = c("61" = "612"),      min_edu = 3L),
  "7411" = list(f2 = c("55"),                 detail = c("55" = "551"),      min_edu = 3L),
  "7412" = list(f2 = c("55"),                 detail = c("55" = "551"),      min_edu = 3L),
  "7413" = list(f2 = c("55"),                 detail = c("55" = "551"),      min_edu = 3L),
  "7126" = list(f2 = c("57"),                 detail = c("57" = "571"),      min_edu = 3L),
  "7115" = list(f2 = c("57"),                 detail = c("57" = "571"),      min_edu = 3L),
  "7112" = list(f2 = c("57"),                 detail = c("57" = "571"),      min_edu = 3L),
  "7231" = list(f2 = c("55"),                 detail = c("55" = "552"),      min_edu = 3L),
  "5141" = list(f2 = c("83"),                 detail = c("83" = "831"),      min_edu = 3L),
  "5120" = list(f2 = c("83", "44"),           detail = c("83" = "832", "44" = "442"), min_edu = 3L)
)

.occ_field_rule <- function(styrk_code, edu_code) {
  if (is.null(styrk_code) || is.na(styrk_code)) return(NULL)
  r <- .OCC_FIELD_RULES[[as.character(styrk_code)]]
  if (is.null(r)) return(NULL)
  e <- suppressWarnings(as.integer(edu_code))
  if (!length(e) || is.na(e) || e < r$min_edu || e == 9L) return(NULL)
  r
}
