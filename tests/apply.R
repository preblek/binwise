library(binwise)
diagnose <- function(x, ...) bin_diagnose(data.frame(x = x), "x", warn = FALSE, ...)
fails <- function(expr, pattern) {
  err <- tryCatch({ force(expr); NULL }, error = identity)
  stopifnot(inherits(err, "error"), grepl(pattern, conditionMessage(err)))
}

# Applying to the original data reproduces each diagnosis, including ties,
# empty categories, exact zeros, constants, and excluded values.
for (values in list(1:20, c(rep(0, 80), 1:20), rep(0, 5), rep(7, 5),
                    c(0, 1, NA, NaN, Inf, -Inf))) {
  for (zero in c(FALSE, TRUE)) {
    for (method in c("quantile", "equal_width")) {
      d <- diagnose(values, separate_zero = zero, method = method)
      original <- data.frame(x = values, id = seq_along(values))
      saved <- serialize(original, NULL)
      applied <- bin_apply(original, d, warn = FALSE)
      stopifnot(identical(applied$x_bin, d$assignments),
                identical(applied$x, original$x), identical(applied$id, original$id),
                identical(serialize(original, NULL), saved),
                !is.ordered(applied$x_bin),
                identical(attr(applied, "binwise")$x_bin$counts$n, d$bins$n))
    }
  }
}

# New data use saved cutpoints, not fresh quantiles. Boundary values go left.
d <- diagnose(0:8, bins = 4)
new <- data.frame(x = c(8, 2, 2.01, 0, 4, 6), id = letters[1:6])
a <- bin_apply(new, d, name = "group")
stopifnot(identical(as.character(a$group), c("B4", "B1", "B2", "B1", "B2", "B3")),
          identical(a$id, new$id),
          identical(attr(a, "binwise")$group$breaks, d$breaks))
# Data with a different column name can use the same units and definition.
renamed <- bin_apply(data.frame(measure = new$x), d, variable = "measure")
stopifnot(identical(renamed$measure_bin, a$group))

# Custom bounds retain empty levels and can cover new values outside the
# original observed range, provided they are inside the chosen bounds.
custom <- diagnose(c(2, 3), method = "custom", breaks = c(0, 1, 4, 10))
a <- bin_apply(data.frame(x = c(0, 1, 4, 10)), custom)
stopifnot(identical(as.character(a$x_bin), c("B1", "B1", "B2", "B3")))
empty <- bin_apply(data.frame(x = numeric()), custom)
stopifnot(nrow(empty) == 0, identical(levels(empty$x_bin), c("B1", "B2", "B3")),
          all(is.na(attr(empty, "binwise")$x_bin$counts$proportion)))

# Uncovered finite values error by default; explicit NA policy keeps all rows
# and reports each cause, including infinities separately from missingness.
outside <- data.frame(x = c(-1, 0, 9, NA, NaN, Inf, -Inf))
fails(bin_apply(outside, d), "2 finite observations")
w <- character()
a <- withCallingHandlers(bin_apply(outside, d, out_of_range = "NA"),
  warning = function(e) { w <<- c(w, conditionMessage(e)); invokeRestart("muffleWarning") })
report <- attr(a, "binwise")$x_bin
stopifnot(nrow(a) == 7, sum(is.na(a$x_bin)) == 6, length(w) == 1,
          report$summary$n_missing == 2, report$summary$n_infinite == 2,
          report$summary$n_out_of_range == 2,
          identical(report$excluded$row, c(1L, 3L, 4L, 5L, 6L, 7L)),
          identical(report$excluded$reason,
                    c("out_of_range", "out_of_range", "missing", "missing", "infinite", "infinite")))

# Zero rules stay fixed. A new zero does not create a previously absent level.
z <- diagnose(c(0, 1, 2), separate_zero = TRUE, method = "custom", breaks = c(-1, 1, 3))
stopifnot(as.character(bin_apply(data.frame(x = 0), z)$x_bin) == "Zero")
fails(bin_apply(data.frame(x = -.5), z), "no category")
no_zero <- diagnose(c(1, 2), separate_zero = TRUE, method = "custom", breaks = c(0, 1, 3))
fails(bin_apply(data.frame(x = 0), no_zero), "no category")
fails(bin_apply(data.frame(x = 1), diagnose(rep(0, 3), separate_zero = TRUE)), "no category")
fails(bin_apply(data.frame(x = 8), diagnose(rep(7, 3))), "no category")

# Multiple applied columns keep separate audit records and survive RDS storage.
a <- bin_apply(new, d, name = "first")
a <- bin_apply(a, d, name = "second")
stopifnot(identical(names(attr(a, "binwise")), c("first", "second")))
path <- tempfile(fileext = ".rds")
saveRDS(a, path)
stopifnot(identical(a, readRDS(path)))
unlink(path)
fails(bin_apply(a, d, name = "first"), "already exists")
fails(bin_apply(a, d, name = "x"), "already exists")
fails(bin_apply(new, bin_compare(new, "x")), "one element")
fails(bin_apply(data.frame(x = factor(1:3)), d), "plain numeric")
fails(bin_apply(new, d, variable = "absent"), "exactly one")
broken <- d
broken$bins$upper[1] <- 99
fails(bin_apply(new, broken), "unmodified")
cat("All fixed-scheme application tests passed.\n")
