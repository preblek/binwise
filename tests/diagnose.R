library(binwise)

diagnose <- function(x, ...) bin_diagnose(data.frame(x = x), "x", warn = FALSE, ...)
fails <- function(expr, pattern) {
  err <- tryCatch({ force(expr); NULL }, error = identity)
  stopifnot(inherits(err, "error"), grepl(pattern, conditionMessage(err)))
}

# Equal-count quartiles on a known ordered sequence.
d <- diagnose(1:8, bins = 4, min_count = 0, min_prop = 0)
stopifnot(identical(d$bins$n, rep(2L, 4)),
          identical(as.integer(d$assignments), rep(1:4, each = 2)),
          d$summary$median == 4.5, abs(d$summary$sd - sd(1:8)) < 1e-12,
          abs(d$summary$skewness) < 1e-12, !any(d$bins$sparse))

# NA, NaN and infinity are excluded, with original row alignment preserved.
d <- diagnose(c(0, NA, 1, NaN, Inf, -Inf, 2), bins = 2)
stopifnot(d$summary$n_missing == 2, d$summary$n_infinite == 2,
          d$summary$n_finite == 3, d$summary$prop_zero == 1/3,
          identical(is.na(d$assignments), c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)),
          sum(d$bins$n) == 3, abs(sum(d$bins$proportion) - 1) < 1e-12)

# Ties collapse boundaries, and no tied group is split.
d <- diagnose(c(rep(0, 80), 1:20), bins = 4)
stopifnot(d$actual_interval_bins == 1L, length(unique(d$assignments[1:80])) == 1L,
          any(grepl("Repeated boundaries", d$notes)))
z <- diagnose(c(rep(0, 80), 1:20), bins = 4, separate_zero = TRUE)
stopifnot(identical(z$bins$n, c(80L, 5L, 5L, 5L, 5L)),
          z$bins$proportion[1] == .8, all(as.character(z$assignments[1:80]) == "Zero"),
          sum(z$bins$sparse) == 4L)

# Boundary convention, empty custom bins, and sparse rule equality.
d <- diagnose(c(0, 1, 2, 3), method = "custom", breaks = c(0, 1, 2, 3, 4),
              min_count = 1, min_prop = .25)
stopifnot(identical(d$bins$n, c(2L, 1L, 1L, 0L)),
          identical(d$bins$sparse, c(FALSE, FALSE, FALSE, TRUE)),
          is.na(d$bins$mean[4]), d$bins$left_closed[1], !d$bins$left_closed[2])
stopifnot(identical(diagnose(0:4, bins = 2, method = "equal_width")$bins$n, c(3L, 2L)))
stopifnot(identical(diagnose(c(0, 1), bins = 4)$bins$n, c(1L, 0L, 0L, 1L)))

# Constant, singleton, all-zero, and no-zero samples.
stopifnot(diagnose(rep(7, 10))$actual_interval_bins == 1L,
          is.na(diagnose(7)$summary$sd), is.na(diagnose(7)$summary$skewness),
          diagnose(rep(0, 4), separate_zero = TRUE)$actual_interval_bins == 0L,
          diagnose(rep(0, 4), separate_zero = TRUE)$bins$n == 4L,
          nrow(diagnose(1:8, bins = 2, separate_zero = TRUE)$bins) == 2L)
stopifnot(diagnose(rep(7, 3), method = "custom", breaks = c(0, 5, 10))$bins$n[1] == 0)

# Invalid inputs fail clearly rather than silently changing the sample.
fails(diagnose(c(NA_real_, Inf)), "no finite")
fails(diagnose(numeric()), "no finite")
fails(diagnose(c(-1, 0, 1), separate_zero = TRUE), "nonnegative")
fails(diagnose(1:4, bins = 2.5), "whole number")
fails(diagnose(1:4, min_prop = 2), "min_prop")
fails(diagnose(1:4, method = "custom", breaks = c(1, 2)), "do not cover")
fails(diagnose(1:4, method = "custom", breaks = c(1, 1, 4)), "strictly increasing")
fails(diagnose(1:4, breaks = c(1, 4)), 'method = "custom"')
fails(diagnose(factor(c("1", "2"))), "plain numeric")
fails(diagnose(as.Date("2026-01-01") + 0:2), "plain numeric")
fails(bin_diagnose(data.frame(x = 1:3), "missing"), "exactly one")
fails(bin_diagnose(1:3, "x"), "data frame")

# Row permutation must not change membership for a given value.
v <- c(0, 0, 1, 2, 2, 2, 10, 20)
p <- c(8, 3, 1, 6, 5, 2, 7, 4)
a <- diagnose(v, bins = 3)
b <- diagnose(v[p], bins = 3)
stopifnot(identical(as.character(a$assignments)[p], as.character(b$assignments)))
original <- data.frame(x = v)
before <- serialize(original, NULL)
invisible(bin_diagnose(original, "x", warn = FALSE))
stopifnot(identical(before, serialize(original, NULL)))

# Large finite values should retain finite cutpoints and count all rows.
d <- diagnose(c(-1e308, 0, 1e308), method = "equal_width", bins = 4)
stopifnot(all(is.finite(d$breaks)), sum(d$bins$n) == 3, is.finite(d$summary$skewness))

# Warnings are controllable, but recorded notes remain available.
w <- character()
withCallingHandlers(bin_diagnose(data.frame(x = rep(0, 5)), "x"),
                    warning = function(e) { w <<- c(w, conditionMessage(e)); invokeRestart("muffleWarning") })
stopifnot(length(w) == 1L, grepl("constant", w))

# All graphical methods work for ordinary and degenerate distributions.
pdf_file <- tempfile(fileext = ".pdf")
grDevices::pdf(pdf_file)
for (v in list(1:100, rep(0, 5), 7, c(rep(0, 80), 1:20))) {
  d <- diagnose(v)
  for (type in c("histogram", "ecdf", "bins")) plot(d, type = type)
}
grDevices::dev.off()
stopifnot(file.info(pdf_file)$size > 0)
unlink(pdf_file)
cat("All binwise diagnostic tests passed.\n")

