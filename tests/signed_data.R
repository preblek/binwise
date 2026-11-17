library(binwise)
data(binwise_negative)
data(binwise_mixed)

# The paired scales differ only in origin. All observations retain their bin
# assignments under either automatic method, including the repeated value.
stopifnot(nrow(binwise_negative) == 1000, all(binwise_negative$score < 0),
          sum(binwise_negative$score == -10) == 400,
          all(binwise_negative$shifted_score > 0),
          identical(binwise_negative$shifted_score, binwise_negative$score + 11))
for (method in c("quantile", "equal_width")) {
  neg <- bin_diagnose(binwise_negative, "score", method = method, warn = FALSE)
  pos <- bin_diagnose(binwise_negative, "shifted_score", method = method, warn = FALSE)
  stopifnot(identical(neg$assignments, pos$assignments),
            identical(neg$bins$n, pos$bins$n),
            isTRUE(all.equal(neg$breaks + 11, pos$breaks)))
  applied <- bin_apply(binwise_negative, neg)
  stopifnot(identical(applied$score_bin, neg$assignments))
}
neg <- bin_diagnose(binwise_negative, "score", warn = FALSE)
stopifnot(neg$actual_interval_bins == 3,
          identical(neg$bins$n, c(500L, 250L, 250L)))

# Four balanced groups mix signs; a custom zero boundary preserves direction
# without needing any observed zeros. All rows and levels survive application.
stopifnot(nrow(binwise_mixed) == 1000, sum(binwise_mixed$change < 0) == 700,
          sum(binwise_mixed$change > 0) == 300, !any(binwise_mixed$change == 0))
comparison <- bin_compare(binwise_mixed, "change", methods = "quantile",
  custom_breaks = list(direction = c(min(binwise_mixed$change), 0, max(binwise_mixed$change))))
q <- comparison$diagnoses$quantile_4
stopifnot(identical(q$bins$n, rep(250L, 4)),
          sum(binwise_mixed$change < 0 & q$assignments == "B3") == 200,
          sum(binwise_mixed$change > 0 & q$assignments == "B3") == 50)
d <- comparison$diagnoses$direction
a <- bin_apply(binwise_mixed, d)
stopifnot(identical(d$bins$n, c(700L, 300L)), !anyNA(a$change_bin),
          all(as.character(a$change_bin[binwise_mixed$change < 0]) == "B1"),
          all(as.character(a$change_bin[binwise_mixed$change > 0]) == "B2"),
          identical(a$change_bin, d$assignments))
future <- bin_apply(data.frame(change = 0), d)
stopifnot(as.character(future$change_bin) == "B1",
          !"Zero" %in% levels(future$change_bin))
cat("Signed-data teaching examples verified.\n")
