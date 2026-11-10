set.seed(777)
bands <- list(c(0,12), c(13,19), c(20,29), c(30,49), c(50,66), c(67,95))
out <- character(0)
N <- 300
for (i in seq_len(N)) {
  b <- bands[[(i - 1) %% 6 + 1]]
  x <- tryCatch(counterfact_me(min_age = b[1], max_age = b[2]), error = function(e) e)
  if (inherits(x, "error")) { out <- c(out, paste0("#", i, " FEIL i trekk: ", conditionMessage(x))); next }
  txt <- tryCatch(as.character(narrate_life(x)), error = function(e) paste("FEIL i narrate:", conditionMessage(e)))
  cmp <- tryCatch(as.character(narrate_life(x, style = "compact")), error = function(e) paste("FEIL compact:", conditionMessage(e)))
  head <- sprintf("#%03d %s aar | %s | %s | %s kr | %s | %s | styrk=%s | bg=%s", i, x$age,
    x$education %||% "-", x$occupation %||% "-", format(x$income_nok %||% NA, big.mark = " "),
    x$household %||% "-", x$housing_tenure %||% "-", attr(x, "occ_styrk") %||% "-", x$background %||% "-")
  out <- c(out, head, gsub("\n\n", " / ", txt), paste("KORT:", cmp), "")
}
writeLines(out, "/out.txt")
