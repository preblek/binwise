library(binwise)
fails <- function(expr, pattern) {
  err <- tryCatch({ force(expr); NULL }, error = identity)
  stopifnot(inherits(err, "error"), grepl(pattern, conditionMessage(err)))
}
data(binwise_interaction)
d <- binwise_interaction
before <- serialize(d, NULL)
x4 <- bin_diagnose(d, "x", warn = FALSE)
z4 <- bin_diagnose(d, "z", warn = FALSE)
x2 <- bin_diagnose(d, "x", method = "custom", breaks = c(min(d$x), 0, max(d$x)), warn = FALSE)
schemes <- list(x_quartiles = x4, both_quartiles = list(x = x4, z = z4), x_threshold = x2)

# Continuous and factor regressions agree with independently specified base R
# models. Both predictors can be binned, or just one; all keep the same sample.
r <- bin_interact(d, "outcome", c("x", "z"), schemes, controls = "control")
direct <- lm(outcome ~ x * z + control, data = d)
category_data <- d
category_data$x <- x4$assignments
category_data$z <- z4$assignments
direct_bins <- lm(outcome ~ x * z + control, data = category_data)
stopifnot(isTRUE(all.equal(unname(coef(r$models$continuous)), unname(coef(direct)))),
          isTRUE(all.equal(unname(coef(r$models$both_quartiles)), unname(coef(direct_bins)))),
          abs(r$comparison$AIC[1] - AIC(direct)) < 1e-10,
          r$comparison$parameters[1] == attr(logLik(direct), "df"),
          all(r$comparison$n == nrow(d)), all(r$comparison$status == "ok"),
          identical(before, serialize(d, NULL)),
          sum(r$support$both_quartiles$n) == nrow(d),
          nrow(r$support$both_quartiles) == 16)

# No interaction means an additive model with the same controls. One-variable
# comparisons accept a bin_comparison and do not require any moderator.
additive <- bin_interact(d, "outcome", c("x", "z"), list(quartiles = x4),
                         interaction = FALSE, controls = "control")
stopifnot(isTRUE(all.equal(unname(coef(additive$models$continuous)),
                          unname(coef(lm(outcome ~ x + z + control, data = d))))))
single <- bin_interact(d, "outcome", "x", bin_compare(d, "x", bins = 3))
stopifnot(!single$interaction, all(single$comparison$status == "ok"))

# Models and prediction grids use the same covariate values. Differences are
# matched by row, not by category coefficient or reference-category label.
new <- data.frame(x = c(-1, 0, 1), z = c(-.5, 0, .5), control = c(0, 1, 2))
p <- predict(r, new)
baseline <- as.numeric(predict(direct, new))
stopifnot(isTRUE(all.equal(p$estimate[p$model == "continuous"], baseline)),
          all(p$status == "ok"), !any(p$extrapolated))
for (name in unique(p$model)) {
  part <- p[p$model == name, ]
  stopifnot(isTRUE(all.equal(part$difference, part$estimate - baseline[part$row])))
}
stopifnot(all(r$grid$data$control == median(d$control)),
          identical(r$grid$moderator_values, as.numeric(quantile(d$z, c(.25, .5, .75), names = FALSE))))

# Missingness is resolved once across all outcomes, predictors, and controls.
missing <- d
missing$outcome[1] <- NA
missing$x[2] <- Inf
missing$control[3] <- NA
same <- bin_interact(missing, "outcome", c("x", "z"), schemes,
                     controls = "control", warn = FALSE)
stopifnot(identical(same$sample$rows, 4:nrow(d)),
          identical(same$sample$excluded$row, 1:3),
          all(same$comparison$n == nrow(d) - 3L))
uncovered <- d
uncovered$x[1] <- max(d$x) + 1
fails(bin_interact(uncovered, "outcome", "x", list(quartiles = x4)), "no category")

# Logit uses probabilities and the Brier score on identical rows; verify against
# glm directly and check event/non-event support counts.
logit <- bin_interact(d, "binary_outcome", c("x", "z"), schemes,
                      model = "logit", controls = "control")
direct_logit <- glm(binary_outcome ~ x * z + control, data = d, family = binomial())
lp <- predict(logit, new)
stopifnot(isTRUE(all.equal(unname(coef(logit$models$continuous)), unname(coef(direct_logit)))),
          isTRUE(all.equal(lp$estimate[lp$model == "continuous"],
                           as.numeric(predict(direct_logit, new, type = "response")))),
          all(lp$estimate >= 0 & lp$estimate <= 1),
          abs(logit$comparison$training_score[1] - mean((d$binary_outcome - fitted(direct_logit))^2)) < 1e-12,
          sum(logit$support$both_quartiles$events) == sum(d$binary_outcome),
          all(logit$support$both_quartiles$events + logit$support$both_quartiles$non_events == logit$support$both_quartiles$n))
for (j in seq_len(nrow(logit$support$both_quartiles))) {
  cell <- logit$support$both_quartiles[j, ]
  members <- x4$assignments == cell$bin_1 & z4$assignments == cell$bin_2
  stopifnot(cell$n == sum(members), cell$events == sum(d$binary_outcome[members]))
}

# Empty interaction cells do not produce apparently valid categorical effects.
g <- expand.grid(x = c(-2, -1, 1, 2), z = c(-2, -1, 1, 2), replicate = 1:5)
g <- g[!(g$x > 0 & g$z > 0), ]
g$y <- 1 + g$x + .3 * g$z + .2 * g$x * g$z + sin(seq_len(nrow(g))) * .1
gx <- bin_diagnose(g, "x", method = "custom", breaks = c(-2, 0, 2), warn = FALSE)
gz <- bin_diagnose(g, "z", method = "custom", breaks = c(-2, 0, 2), warn = FALSE)
empty <- bin_interact(g, "y", c("x", "z"), list(cells = list(x = gx, z = gz)), warn = FALSE)
stopifnot(identical(empty$comparison$status, c("ok", "failed")),
          is.na(empty$comparison$AIC[2]), any(empty$support$cells$n == 0),
          all(empty$predictions$status[empty$predictions$model == "cells"] == "model_unavailable"))

# New-data gaps, unseen factor levels, and missing rows remain explicit, and
# extrapolated continuous predictions are flagged even if numerically available.
with_factor <- d
with_factor$group <- factor(rep(c("A", "B"), length.out = nrow(d)))
factor_model <- bin_interact(with_factor, "outcome", "x", list(quartiles = x4), controls = "group")
np <- predict(factor_model, data.frame(x = c(0, NA, 3), group = c("C", "A", "B")), warn = FALSE)
stopifnot(np$status[np$model == "continuous" & np$row == 1] == "unobserved_category",
          np$status[np$model == "continuous" & np$row == 2] == "missing_or_nonfinite_predictor",
          np$extrapolated[np$model == "continuous" & np$row == 3],
          np$status[np$model == "quartiles" & np$row == 3] == "uncovered_by_scheme")
stopifnot(nrow(predict(r, new[FALSE, ])) == 0)
flat <- d
flat$outcome <- 0
flat_result <- bin_interact(flat, "outcome", "x", list(quartiles = x4))
stopifnot(all(is.na(flat_result$comparison$AIC)), all(flat_result$predictions$estimate == 0))

# Complete separation can appear to converge. Withhold the resulting logit
# comparisons rather than treating extreme probabilities as reliable results.
sep <- data.frame(x = seq(-2, 2, length.out = 100))
sep$y <- as.integer(sep$x > 0)
sd <- bin_diagnose(sep, "x", method = "custom", breaks = c(-2, 0, 2), warn = FALSE)
separated <- bin_interact(sep, "y", "x", list(sign = sd), model = "logit", warn = FALSE)
stopifnot(all(separated$comparison$status != "ok"),
          all(is.na(separated$comparison$AIC)),
          all(is.na(separated$predictions$estimate)),
          any(grepl("separation", unlist(separated$issues))))

# Non-syntactic names are treated as column identifiers, never parsed as code.
named <- d[c("x", "outcome")]
names(named) <- c("exposure score", "response value")
ns <- bin_diagnose(named, "exposure score", warn = FALSE)
nr <- bin_interact(named, "response value", "exposure score", list(quartiles = ns))
stopifnot(all(nr$predictions$status == "ok"))

fails(bin_interact(d, "outcome", "x", schemes), "focal variables")
fails(bin_interact(d, "outcome", "x", list(a = x4), interaction = TRUE), "two focal")
fails(bin_interact(d, "outcome", "x", list(continuous = x4)), "reserved")
fails(bin_interact(d, "outcome", "x", list(a = x4), controls = "x"), "distinct")
fails(bin_interact(d, "outcome", "x", list(a = x4), model = "logit"), "both outcome")
fails(predict(r, data.frame(x = 1)), "every focal")
fails(predict(r, new, se.fit = TRUE), "not supported")

# Known smooth versus threshold-generating rules illustrate both directions.
threshold <- bin_interact(d, "threshold_outcome", c("x", "z"), schemes, controls = "control")
stopifnot(r$comparison$AIC[1] < r$comparison$AIC[4],
          threshold$comparison$AIC[4] < threshold$comparison$AIC[1])
pdf_file <- tempfile(fileext = ".pdf")
grDevices::pdf(pdf_file, width = 13, height = 6)
layout_before <- graphics::par("mfrow")
for (result in list(r, single, logit, empty)) {
  plot(result)
  if (any(result$comparison$status[-1] == "ok")) plot(result, type = "differences")
}
stopifnot(identical(layout_before, graphics::par("mfrow")),
          any(grepl("same 1200 observations", capture.output(print(r)))))
grDevices::dev.off()
unlink(pdf_file)
cat("Common-sample model comparisons and prediction checks passed.\n")
