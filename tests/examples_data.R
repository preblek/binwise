library(binwise)
data(binwise_zero)
data(binwise_tail)
data(binwise_smooth)
stopifnot(nrow(binwise_zero) == 1000, sum(binwise_zero$score == 0) == 400,
          all(binwise_zero$score >= 0 & binwise_zero$score < 1),
          nrow(binwise_tail) == 1000, all(binwise_tail$share > 0 & binwise_tail$share < 1),
          nrow(binwise_smooth) == 1000,
          identical(binwise_smooth$expected_outcome, 2 + .5 * binwise_smooth$exposure))
scheme <- bin_diagnose(binwise_zero, "score", bins = 3, separate_zero = TRUE, warn = FALSE)
binned <- bin_apply(binwise_zero, scheme)
stopifnot(identical(as.integer(table(binned$score_bin)), c(400L, 200L, 200L, 200L)))
smooth <- bin_diagnose(binwise_smooth, "exposure", bins = 4, warn = FALSE)
means <- tapply(binwise_smooth$expected_outcome, smooth$assignments, mean)
within_error <- binwise_smooth$expected_outcome - means[as.character(smooth$assignments)]
stopifnot(mean(within_error^2) > 0)
cat("Simulated teaching datasets verified.\n")
