set.seed(99)
N <- 700
rows <- vector("list", N)
for (i in seq_len(N)) {
  x <- counterfact_me(min_age = 16, max_age = 85)
  os <- .cfm_env$occupations_salary
  cd <- attr(x, "occ_styrk")
  med <- if (!is.null(cd) && !is.na(cd)) os$median_nok[os$code == cd & os$schedule == "all"][1] else NA
  rows[[i]] <- data.frame(age = x$age, g = x$gender, kids = x$n_children %||% NA,
                          inc = x$income_nok %||% NA, occ = x$occupation %||% NA,
                          med = med, own = identical(x$housing_tenure, "Eier"),
                          hval = x$housing_value_nok %||% NA, edu = x$education %||% NA)
}
d <- do.call(rbind, rows)
out <- character(0)
d$band <- cut(d$age, c(15, 19, 24, 29, 34, 39, 49, 70))
for (gg in c("F", "M")) {
  s <- tapply(d$kids[d$g == gg] > 0, d$band[d$g == gg], mean)
  out <- c(out, paste(gg, "andel med barn:", paste(names(s), round(s, 3), collapse = "  ")))
}
w <- d[!is.na(d$med) & !grepl("deltid|AAP|Dagpenger|Student", d$occ) & d$age >= 25, ]
r <- w$inc / (12 * w$med)
out <- c(out, sprintf("heltid 25+: n=%d  inntekt/(12*median) p10=%.2f p50=%.2f p90=%.2f p99=%.2f  andel>1.6=%.3f",
                      nrow(w), quantile(r, .1), median(r), quantile(r, .9), quantile(r, .99), mean(r > 1.6)))
o <- d[d$own & !is.na(d$hval) & d$inc > 0, ]
hr <- o$hval / o$inc
out <- c(out, sprintf("eiere: boligverdi/inntekt p50=%.1f p90=%.1f p99=%.1f maks=%.0f; verdi > 20 MNOK: %d av %d",
                      median(hr), quantile(hr, .9), quantile(hr, .99), max(hr), sum(o$hval > 20e6), nrow(o)))
big <- o[o$hval > 20e6, c("age","inc","hval","occ")]
out <- c(out, capture.output(print(head(big, 10))))
out <- c(out, sprintf("inntekt > 1.5 MNOK: %d av %d (%.3f)", sum(d$inc > 1.5e6, na.rm=TRUE), N, mean(d$inc > 1.5e6, na.rm=TRUE)))
pen <- d[d$age >= 72 & !is.na(d$inc) & d$inc > 0, ]
pen$lvl <- ifelse(grepl("Barneskole|Ungdomsskole|Ingen", pen$edu), "1 grunnsk", ifelse(grepl("Videreg|Påbygging", pen$edu), "2 vgs", ifelse(grepl("kort", pen$edu), "3 bachelor", "4 master+")))
tp <- tapply(pen$inc, pen$lvl, median)
out <- c(out, paste("pensjon 72+, median etter utdanning:", paste(names(tp), round(tp/1000), collapse = "  ")))
writeLines(out, "/out.txt")
