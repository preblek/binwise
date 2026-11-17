library(binwise)
near <- function(a, b) stopifnot(isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = 1e-9)))
fails <- function(x) stopifnot(inherits(tryCatch({force(x); NULL}, error = identity), "error"))
data(binwise_interaction)
d <- binwise_interaction
b <- bin_diagnose(d, "x", bins = 4, warn = FALSE)

# Independent fits verify the full two-variable spline interaction and intervals.
r <- bin_interact(d, "outcome", c("x", "z"), list(quartiles = b),
                  controls = "control", spline_df = 3)
manual <- lm(outcome ~ splines::ns(x, df = 3) * splines::ns(z, df = 3) + control, d)
new <- data.frame(x = c(-3, -.4, .4, 3), z = c(-1, 0, 1, 0), control = 0)
p <- predict(r, new, warn = FALSE)
sp <- p[p$model == "spline", ]
expected <- predict(manual, new, interval = "confidence")
near(sp$estimate, expected[, "fit"]); near(sp$lower, expected[, "lwr"]); near(sp$upper, expected[, "upr"])
stopifnot(all(r$comparison$status == "ok"), is.null(r$support$spline),
  all(sp$extrapolated[c(1, 4)]), all(sp$status == "ok"),
  any(grepl("compared with spline", r$assessment)),
  !any(grepl("Inf observations", r$assessment)), identical(r$spline_df, 3))
# Predicting a single row or an expanded batch must not change its spline basis.
near(predict(r, new[2, , drop = FALSE])$estimate[2], sp$estimate[2])

# Binary outcomes, additive splines, and categorical controls use the same API.
d$group <- factor(rep(c("a", "b"), length.out = nrow(d)))
g <- bin_interact(d, "binary_outcome", c("x", "z"), list(quartiles = b),
                 interaction = FALSE, controls = "group", spline_df = 3, model = "logit")
gm <- glm(binary_outcome ~ splines::ns(x, 3) + splines::ns(z, 3) + group, d, family = binomial())
new$group <- factor("a", levels = levels(d$group))
gp <- predict(g, new, warn = FALSE); gp <- gp[gp$model == "spline", ]
link <- predict(gm, new, type = "link", se.fit = TRUE)
near(gp$estimate, plogis(link$fit))
near(gp$lower, plogis(link$fit - qnorm(.975) * link$se.fit))
near(gp$upper, plogis(link$fit + qnorm(.975) * link$se.fit))

# Out-of-fold predictions agree with independent training-only fits. Extreme
# test values and outcome changes cannot influence training knots or fitted y.
d$x[1] <- 20
d$outcome[2] <- NA
ids <- rep(1:5, length.out = nrow(d))
recipes <- list(quartiles = list(x = list(probs = c(0, .25, .5, .75, 1))))
set.seed(8); before <- .Random.seed; saved <- d
v <- bin_validate(d, "outcome", "x", schemes = recipes, controls = "control",
                  fold_id = ids, spline_df = 3, reference = "spline", reps = 99)
stopifnot(identical(before, .Random.seed), identical(saved, d),
          all(v$predictions$scored), identical(v$excluded_rows, 2L),
          all(v$differences$reference == "spline"))
for (k in 1:5) {
  label <- unique(ids[!is.na(d$outcome)])[k]
  train <- d[ids != label & !is.na(d$outcome), ]
  rows <- v$predictions$row[v$predictions$fold == k]
  fit <- lm(outcome ~ splines::ns(x, 3) + control, train)
  near(v$predictions$spline[v$predictions$fold == k], predict(fit, d[rows, ]))
  def <- v$fold_details[[k]]$spline$x
  near(def$boundary_knots, range(train$x))
  near(def$knots, quantile(train$x, c(1/3, 2/3)))
}
stopifnot(v$fold_details[[1]]$spline$x$boundary_knots[2] < 20)
# Check the sign and numeric value of the direct bins-versus-spline difference.
rmse <- function(p) sqrt(mean((v$predictions$outcome - p)^2))
near(v$differences$difference[v$differences$model == "quartiles" & v$differences$metric == "RMSE"],
     rmse(v$predictions$quartiles) - rmse(v$predictions$spline))
# The no-spline call keeps its original columns and fold predictions.
old <- bin_validate(d, "outcome", "x", schemes = recipes, controls = "control", fold_id = ids, reps = 0)
near(old$predictions$continuous, v$predictions$continuous)
near(old$predictions$quartiles, v$predictions$quartiles)
stopifnot(!"spline" %in% names(old$predictions))

# Logistic held-out probabilities also match an independent fit.
gv <- bin_validate(d, "binary_outcome", c("x", "z"), schemes = recipes,
                   controls = "group", interaction = FALSE, model = "logit",
                   fold_id = ids, spline_df = 3, reps = 0)
fit <- glm(binary_outcome ~ splines::ns(x, 3) + splines::ns(z, 3) + group,
           d[ids != 1, ], family = binomial())
near(gv$predictions$spline[ids == 1], predict(fit, d[ids == 1, ], type = "response"))

# Arbitrary column names, no binned candidate, and meaningful failures.
n <- d[c("outcome", "x")]; names(n) <- c("response value", "signed score")
only <- bin_interact(n, "response value", "signed score", list(), spline_df = 3, warn = FALSE)
stopifnot(identical(only$comparison$model, c("continuous", "spline")))
only_cv <- bin_validate(n, "response value", "signed score", spline_df = 3, reps = 0)
stopifnot(all(only_cv$predictions$scored))
for (bad in list(1, 0, -2, NA_real_, Inf, 2.5, c(3, 4), "3")) {
  fails(bin_interact(d, "outcome", "x", list(quartiles = b), spline_df = bad))
  fails(bin_validate(d, "outcome", "x", schemes = recipes, spline_df = bad))
}
fails(bin_interact(d, "outcome", "x", list(spline = b), spline_df = 3))
fails(bin_validate(d, "outcome", "x", schemes = list(spline = recipes[[1]]), spline_df = 3))
fails(bin_validate(d, "outcome", "x", schemes = recipes, reference = "absent", reps = 0))
fails(bin_validate(d, "outcome", fit_predict = function(a, b) NULL, spline_df = 3))
sparse <- data.frame(x = rep(c(-1, 1), each = 10), y = sin(1:20))
unavailable <- bin_interact(sparse, "y", "x", list(), spline_df = 3, warn = FALSE)
stopifnot(unavailable$comparison$status[2] != "ok", all(is.na(unavailable$predictions$estimate[unavailable$predictions$model == "spline"])))

# Plot both mean intervals and score differences using the spline reference.
path <- tempfile(fileext = ".pdf")
grDevices::pdf(path, width = 10, height = 6)
plot(r); plot(g); plot(v)
grDevices::dev.off(); unlink(path)
cat("Spline predictions, training-only bases, paired scores, and failure cases passed.\n")
