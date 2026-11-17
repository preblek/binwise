library(binwise)
data(binwise_interaction)
d <- binwise_interaction
scheme <- bin_diagnose(d, "x", method = "custom",
  breaks = c(min(d$x), 0, max(d$x)), warn = FALSE)
r <- bin_interact(d, "outcome", c("x", "z"), list(threshold = scheme),
                  controls = "control")
new <- data.frame(x = c(-1.8, -.5, .5, 1.8), z = 0, control = 0)
p <- predict(r, new)
near <- function(a, b) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = 1e-10))
fails <- function(expr, pattern) {
  err <- tryCatch({ force(expr); NULL }, error = identity)
  stopifnot(inherits(err, "error"), grepl(pattern, conditionMessage(err)))
}

# Compare both forms against independently fitted base R models on the same data.
fit <- lm(outcome ~ x * z + control, data = d)
expected <- predict(fit, new, interval = "confidence")
pc <- p[p$model == "continuous", ]
stopifnot(near(pc$estimate, expected[, "fit"]), near(pc$lower, expected[, "lwr"]),
          near(pc$upper, expected[, "upr"]), all(p$interval_status == "ok"),
          identical(attr(p, "level"), .95))
bd <- d
bd$x <- factor(ifelse(d$x <= 0, "low", "high"), levels = c("low", "high"))
bn <- new
bn$x <- factor(ifelse(new$x <= 0, "low", "high"), levels = levels(bd$x))
expected_bins <- predict(lm(outcome ~ x * z + control, data = bd), bn,
                          interval = "confidence")
pb <- p[p$model == "threshold", ]
stopifnot(near(pb$lower, expected_bins[, "lwr"]), near(pb$upper, expected_bins[, "upr"]),
          pb$lower[1] == pb$lower[2], pb$upper[3] == pb$upper[4])
# These are mean intervals, narrower than individual prediction intervals.
individual <- predict(fit, new, interval = "prediction")
stopifnot(all(pc$upper - pc$lower < individual[, "upr"] - individual[, "lwr"]))

# Logistic bounds use the link-scale SE, then inverse-logit transformation.
g <- bin_interact(d, "binary_outcome", c("x", "z"), list(threshold = scheme),
                  controls = "control", model = "logit")
gp <- predict(g, new)
for (name in c("continuous", "threshold")) {
  gd <- if (name == "continuous") d else bd
  gn <- if (name == "continuous") new else bn
  direct <- glm(binary_outcome ~ x * z + control, data = gd, family = binomial())
  link <- predict(direct, gn, type = "link", se.fit = TRUE)
  part <- gp[gp$model == name, ]
  stopifnot(near(part$estimate, predict(direct, gn, type = "response")),
            near(part$lower, plogis(link$fit - qnorm(.975) * link$se.fit)),
            near(part$upper, plogis(link$fit + qnorm(.975) * link$se.fit)))
}
stopifnot(all(gp$lower >= 0), all(gp$upper <= 1),
          all(gp$lower <= gp$estimate), all(gp$estimate <= gp$upper))

# Changing levels or suppressing intervals preserves means, fits, and sample.
for (object in list(r, g)) {
  saved <- serialize(object, NULL)
  narrow <- predict(object, new, level = .9)
  wide <- predict(object, new, level = .99)
  none <- predict(object, new, interval = "none")
  stopifnot(all(wide$lower < narrow$lower), all(wide$upper > narrow$upper),
            near(none$estimate, wide$estimate), near(none$difference, wide$difference),
            all(is.na(none$lower)), all(is.na(none$upper)),
            all(none$interval_status == "not_requested"),
            identical(saved, serialize(object, NULL)), nrow(predict(object, new[FALSE, ])) == 0)
}
r90 <- bin_interact(d, "outcome", c("x", "z"), list(threshold = scheme),
                    controls = "control", level = .9)
stopifnot(identical(attr(r90$predictions, "level"), .9),
          identical(attr(predict(r90), "level"), .9))
for (bad in list(0, 1, NA_real_, Inf, c(.9, .95), "95%")) {
  fails(predict(r, new, level = bad), "strictly between")
  fails(bin_interact(d, "outcome", "x", list(threshold = scheme), level = bad), "strictly between")
}
fails(predict(r, new, interval = "prediction"), "arg")

# Missing and uncovered rows retain their positions without manufactured limits.
bad_new <- new
bad_new$x[1] <- NA
bad_new$x[2] <- max(d$x) + 1
bad <- predict(r, bad_new, warn = FALSE)
stopifnot(all(is.na(bad$lower[bad$status != "ok"])),
          all(is.na(bad$upper[bad$status != "ok"])),
          all(bad$interval_status[bad$status != "ok"] == "prediction_unavailable"),
          any(bad$extrapolated & bad$interval_status == "ok"))
factor_data <- d
factor_data$group <- factor(rep(c("a", "b"), length.out = nrow(d)))
factor_result <- bin_interact(factor_data, "outcome", "x", list(threshold = scheme), controls = "group")
unknown <- predict(factor_result, data.frame(x = 0, group = "c"), warn = FALSE)
stopifnot(all(unknown$status == "unobserved_category"), all(is.na(unknown$lower)))
separated <- d
separated$binary_outcome <- as.numeric(d$x > 0)
sep <- bin_interact(separated, "binary_outcome", "x", list(threshold = scheme),
                    model = "logit", warn = FALSE)
stopifnot(all(sep$comparison$status != "ok"), all(is.na(sep$predictions$lower)))
flat <- d
flat$outcome <- 0
flat_result <- bin_interact(flat, "outcome", "x", list(threshold = scheme), warn = FALSE)
stopifnot(all(flat_result$predictions$lower == 0), all(flat_result$predictions$upper == 0))

# Plot paths, including old saved objects and a singleton separate-zero category.
device <- tempfile(fileext = ".pdf")
pdf(device, width = 12, height = 5)
before <- par(no.readonly = TRUE)
plot(r)
plot(g, level = .9)
plot(r, show_confidence = FALSE)
plot(r, type = "differences")
fails(plot(r, type = "differences", show_confidence = TRUE), "not implemented")
fails(plot(r, show_confidence = NA), "TRUE or FALSE")
fails(plot(r, level = 1), "strictly between")
old <- r
old$level <- NULL
old$predictions <- old$predictions[setdiff(names(old$predictions), c("lower", "upper", "interval_status"))]
plot(old)
data(binwise_zero)
zero <- binwise_zero
zero$outcome <- 2 + zero$score + sin(seq_len(nrow(zero)))
zero_scheme <- bin_diagnose(zero, "score", separate_zero = TRUE, warn = FALSE)
zero_result <- bin_interact(zero, "outcome", "score", list(zero_separate = zero_scheme))
plot(zero_result)
stopifnot(identical(before, par(no.readonly = TRUE)))
dev.off()
unlink(device)
cat("Mean confidence intervals, invalid predictions, and plot options passed.\n")
