library(binwise)
fails <- function(x) stopifnot(inherits(tryCatch({force(x); NULL}, error = identity), "error"))
set.seed(9388)
d <- data.frame(x = runif(500, -2, 2), unit = rep(1:100, each = 5))
d$y <- ifelse(d$x <= 0, -2, 2) + rnorm(500, sd = .3)
before <- d; rng <- .Random.seed
v <- bin_validate(d, "y", "x", schemes = list(step = list(x = list(breaks = c(-Inf, 0, Inf)))),
  group = "unit", reps = 99, seed = 42)
stopifnot(identical(rng, .Random.seed), identical(d, before), all(v$predictions$scored),
  all(v$differences$difference < 0), all(v$differences$upper < 0),
  all(vapply(split(v$predictions$fold, d$unit), function(z) length(unique(z)) == 1L, logical(1))))
stopifnot(identical(v$predictions, bin_validate(d, "y", "x",
  schemes = list(step = list(x = list(breaks = c(-Inf, 0, Inf)))), group = "unit", reps = 0, seed = 42)$predictions))

# Training-only quantiles, including held-out extremes and tied boundaries.
d$x[1] <- 1000
ids <- rep(1:5, length.out = nrow(d))
q <- bin_validate(d, "y", "x", schemes = list(quantiles = list(x = list(probs = c(0, .5, 1)))), fold_id = ids, reps = 0)
stopifnot(q$fold_details[[1]]$quantiles$x$breaks[2] == median(d$x[ids != 1]),
  all(q$predictions$scored), is.infinite(q$fold_details[[1]]$quantiles$x$breaks[1]))
fails(bin_validate(d, "y", "x", schemes = list(bad = bin_diagnose(d, "x", warn = FALSE)), reps = 0))
fails(bin_validate(d, "y", "x", schemes = list(q = list(x = list(probs = c(.1, .9)))), reps = 0))
fails(bin_validate(d, "y", "x", schemes = list(q = list(x = list(probs = c(0, 1)))), group = "unit", fold_id = ids, reps = 0))

# Custom probabilities, paired comparisons, common coverage and crossed clusters.
z <- data.frame(y = rep(0:1, 120), a = rep(1:20, each = 12), b = rep(1:12, 20))
callback <- function(train, test) cbind(reference = rep(.5, nrow(test)), same = rep(.5, nrow(test)))
cv <- bin_validate(z, "y", model = "logit", group = "a", cluster = c("a", "b"), fit_predict = callback, reps = 99)
stopifnot(all(cv$differences$difference == 0), all(cv$differences$lower == 0),
  all(cv$differences$upper == 0), all(cv$scores$score[cv$scores$metric == "Brier"] == .25),
  all(abs(cv$scores$score[cv$scores$metric == "log_loss"] - log(2)) < 1e-12))
bad <- function(train, test) {p <- callback(train, test); p[1, 2] <- 2; p}
partial <- bin_validate(z, "y", model = "logit", fit_predict = bad, reps = 0)
stopifnot(sum(!partial$predictions$scored) == 5, sum(partial$diagnostics$unavailable) == 5)
fails(bin_validate(z, "y", model = "logit", fit_predict = function(a, b) matrix(1, nrow(b) - 1, 2), reps = 0))

# A smooth linear truth should not mechanically favor categories.
set.seed(2)
s <- data.frame(x = runif(1000, -3, 3))
s$y <- 4 * s$x + rnorm(1000, sd = .2)
smooth <- bin_validate(s, "y", "x", schemes = list(q4 = list(x = list(probs = seq(0, 1, .25)))), reps = 0)
stopifnot(all(smooth$differences$difference > 0))
