.res <- character(0)
test_that <- function(desc, code) {
  .cur <<- desc
  tryCatch(eval(substitute(code), envir = new.env(parent = globalenv())),
           error = function(e) .res <<- c(.res, paste0("FEIL [", desc, "]: ", conditionMessage(e))))
}
expect_equal <- function(object, expected, ...) {
  if (!isTRUE(all.equal(object, expected))) .res <<- c(.res, paste0("BRUDD [", .cur, "]:"), paste("  ", head(object, 40)))
}
expect_gte <- function(o, e, ...) if (!(o >= e)) .res <<- c(.res, sprintf("BRUDD [%s]: %s < %s", .cur, o, e))
expect_gt <- function(o, e, ...) if (!(o > e)) .res <<- c(.res, sprintf("BRUDD [%s]: %s <= %s", .cur, o, e))
expect_lt <- function(o, e, ...) if (!(o < e)) .res <<- c(.res, sprintf("BRUDD [%s]: %s >= %s", .cur, o, e))
CounterfactMe <- NULL
src <- readLines("/cfm/test-coherence.R")
src <- gsub("CounterfactMe:::", "", src, fixed = TRUE)
eval(parse(text = src))
writeLines(c(if (length(.res)) .res else "ALLE SJEKKER OK"), "/out.txt")
