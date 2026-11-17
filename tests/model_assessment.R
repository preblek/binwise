library(binwise)
data(binwise_interaction)
d <- binwise_interaction
b <- bin_diagnose(d, "x", method = "custom", breaks = c(min(d$x), 0, max(d$x)), warn = FALSE)
r <- bin_interact(d, "threshold_outcome", c("x", "z"), list(threshold = b), controls = "control")
text <- paste(r$assessment, collapse = "\n")
stopifnot(is.character(r$assessment), length(r$assessment) > 5,
          grepl("include an interaction", text), grepl("outcome units", text),
          grepl("AIC is 273.6 lower", text), grepl("569 to 631", text),
          grepl("No scheme is selected automatically", text),
          grepl("not per-unit effects or causal", text),
          any(grepl("Assessment", capture.output(print(r)))))

# Same cutpoints with a different outcome change the fit assessment.
smooth <- bin_interact(d, "outcome", c("x", "z"), list(threshold = b), controls = "control")
stopifnot(r$comparison$delta_AIC[2] < 0, smooth$comparison$delta_AIC[2] > 0,
          grepl("AIC is [0-9.]+ higher", paste(smooth$assessment, collapse = "\n")))

# Known truth and dataset names do not enter the assessment. User-named columns
# and an explicitly disabled interaction retain their actual meaning.
ordinary <- d[c("threshold_outcome", "x", "z", "control")]
same <- bin_interact(ordinary, "threshold_outcome", c("x", "z"), list(threshold = b), controls = "control")
stopifnot(identical(same$assessment, r$assessment))
names(ordinary) <- c("measurement", "signed score", "context", "adjustment")
own <- bin_diagnose(ordinary, "signed score", bins = 3, warn = FALSE)
custom <- bin_interact(ordinary, "measurement", c("signed score", "context"),
  list(my_groups = own), controls = "adjustment", interaction = FALSE)
stopifnot(any(grepl("have no interaction", custom$assessment)),
          any(grepl("signed score", custom$assessment)),
          any(grepl("my_groups", custom$assessment)))

# Logit changes use percentage points, not probability units or percent changes.
logit <- bin_interact(d, "binary_outcome", c("x", "z"), list(threshold = b), controls = "control", model = "logit")
maximum <- max(abs(logit$predictions$difference[logit$predictions$model == "threshold"])) * 100
stopifnot(any(grepl(paste0(format(signif(maximum, 4), trim = TRUE), " percentage points"),
                    logit$assessment, fixed = TRUE)),
          any(grepl("smallest event and non-event counts", logit$assessment)))

# Excluded rows and the specified support screen are reported from the model
# sample, not from the data used to choose cutpoints.
missing <- d
missing$threshold_outcome[1:20] <- NA
screen <- bin_interact(missing, "threshold_outcome", "x", list(threshold = b), min_count = 1000, warn = FALSE)
stopifnot(any(grepl("1180 observations; 20 rows", screen$assessment)),
          any(grepl("2 fall below your count screen (n < 1000)", screen$assessment, fixed = TRUE)))

# An unavailable baseline must not give an apparent ranking of its candidates.
collinear <- d
collinear$duplicate <- collinear$x
bad_base <- bin_interact(collinear, "outcome", "x", list(threshold = b), controls = "duplicate", warn = FALSE)
stopifnot(bad_base$comparison$status[1] == "failed", bad_base$comparison$status[2] == "ok",
          any(grepl("Relative fit and prediction comparisons are unavailable", bad_base$assessment)),
          !any(grepl("Compared with continuous, AIC", bad_base$assessment)))

# Empty joint cells are reported even when the user disables the sparse screen.
g <- expand.grid(x = c(-2, -1, 1, 2), z = c(-2, -1, 1, 2), replicate = 1:5)
g <- g[!(g$x > 0 & g$z > 0), ]
g$y <- 1 + g$x + .3 * g$z + .2 * g$x * g$z + sin(seq_len(nrow(g))) * .1
gx <- bin_diagnose(g, "x", method = "custom", breaks = c(-2, 0, 2), warn = FALSE)
gz <- bin_diagnose(g, "z", method = "custom", breaks = c(-2, 0, 2), warn = FALSE)
empty <- bin_interact(g, "y", c("x", "z"), list(cells = list(x = gx, z = gz)), min_count = 0, warn = FALSE)
stopifnot(any(grepl("1 groups are empty and 0 fall below", empty$assessment)),
          any(grepl("This fit is flagged or failed", empty$assessment)))

# Exact fits can have usable predictions but unavailable information criteria.
flat <- d
flat$outcome <- 0
zero <- bin_interact(flat, "outcome", "x", list(threshold = b), warn = FALSE)
stopifnot(any(grepl("AIC comparison is unavailable", zero$assessment)),
          any(grepl("training RMSE is effectively unchanged", zero$assessment)))

# Exercise a criteria disagreement without tying the test to random searches
# for a particular sample. The assessment must report rather than resolve it.
discordant <- r
discordant$comparison$AIC[2] <- discordant$comparison$AIC[1] - 1
discordant$comparison$BIC[2] <- discordant$comparison$BIC[1] + 1
assess <- getFromNamespace("assess_model_comparison", "binwise")
stopifnot(any(grepl("AIC and BIC favor different", assess(discordant))))
# Older objects still print without an assessment field.
old <- r
old$assessment <- NULL
stopifnot(length(capture.output(print(old))) > 0)
cat("Plain-language model assessments and failure cases passed.\n")
