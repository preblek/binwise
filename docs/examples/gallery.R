# binwise example gallery
# Run from top to bottom in RStudio after installing binwise.
library(binwise)

# meaningful-zero
data("binwise_zero", package = "binwise")
zero_groups <- bin_diagnose(
  binwise_zero, "score", bins = 3, separate_zero = TRUE, warn = FALSE
)
zero_groups$bins[, c("bin", "lower", "upper", "n")]

# long-tail
data("binwise_tail", package = "binwise")
tail_choices <- bin_compare(
  binwise_tail, "share", methods = "quantile",
  custom_breaks = list(more_detail = as.numeric(quantile(
    binwise_tail$share, c(0, .5, .75, .9, .95, .99, 1)
  )))
)
plot(
  tail_choices, type = "boundaries", show_counts = TRUE,
  value_format = "percent", xlab = "Share",
  main = "Similar counts can hide very different values",
  scheme_labels = c(quantile_4 = "Four groups", more_detail = "More detail at the top")
)

# smooth-change
data("binwise_smooth", package = "binwise")
four_groups <- bin_diagnose(binwise_smooth, "exposure", bins = 4)
smooth_fit <- bin_interact(
  binwise_smooth, outcome = "outcome", variables = "exposure",
  schemes = list(four_groups = four_groups)
)
plot(smooth_fit, xlab = "Exposure", main = "Grouping hides change within each interval")

# curved-change
curved <- binwise_smooth
noise <- curved$outcome - curved$expected_outcome
curved$outcome <- 2 + .008 * (curved$exposure - 50)^2 + noise
curve_groups <- bin_diagnose(curved, "exposure", bins = 4)
curve_fit <- bin_interact(
  curved, outcome = "outcome", variables = "exposure",
  schemes = list(four_groups = curve_groups), spline_df = 3
)
plot(curve_fit, xlab = "Exposure", main = "Give the numeric model room to bend")

# known-jump
data("binwise_interaction", package = "binwise")
jump_groups <- bin_diagnose(
  binwise_interaction, "x", method = "custom",
  breaks = c(min(binwise_interaction$x), 0, max(binwise_interaction$x))
)
jump_fit <- bin_interact(
  binwise_interaction, outcome = "threshold_outcome",
  variables = c("x", "z"), controls = "control",
  schemes = list(two_groups = jump_groups)
)
plot(jump_fit, xlab = "Distance from the boundary",
     main = "Two groups can represent a known jump")

# test-the-jump
jump_check <- bin_validate(
  binwise_interaction, outcome = "threshold_outcome",
  variables = c("x", "z"), controls = "control",
  schemes = list(two_groups = list(x = list(breaks = c(-Inf, 0, Inf)))),
  spline_df = 3, reference = "spline", folds = 5, seed = 12, reps = 199
)
jump_check$scores

# direction
data("binwise_mixed", package = "binwise")
direction_choices <- bin_compare(
  binwise_mixed, "change", methods = "quantile",
  custom_breaks = list(direction = c(min(binwise_mixed$change), 0,
                                     max(binwise_mixed$change)))
)
plot(
  direction_choices, type = "boundaries", show_counts = TRUE,
  xlab = "Change", main = "Keep declines and increases separate",
  scheme_labels = c(quantile_4 = "Four groups", direction = "Boundary at zero")
)
