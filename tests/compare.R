library(binwise)
fails <- function(expr, pattern) {
  err <- tryCatch({ force(expr); NULL }, error = identity)
  stopifnot(inherits(err, "error"), grepl(pattern, conditionMessage(err)))
}

# Known equal-count groups, explicit custom boundary membership, and empty bins.
d <- data.frame(x = c(1:8, NA, Inf))
original <- serialize(d, NULL)
c <- bin_compare(d, "x", bins = c(2, 4),
                 custom_breaks = list(cutpoints = c(0, 2, 8, 10)),
                 min_count = 1, min_prop = 0)
stopifnot(identical(names(c$diagnoses),
  c("quantile_2", "quantile_4", "equal_width_2", "equal_width_4", "cutpoints")),
  identical(c$diagnoses$quantile_4$bins$n, rep(2L, 4)),
  identical(c$diagnoses$cutpoints$bins$n, c(2L, 6L, 0L)),
  c$comparison$empty[5] == 1, c$comparison$smallest_n[5] == 0,
  c$comparison$widest_range[5] == 5, c$comparison$observed_min[5] == 3,
  c$comparison$observed_max[5] == 8,
  identical(original, serialize(d, NULL)))
for (diagnosis in c$diagnoses) {
  stopifnot(sum(diagnosis$bins$n) == 8,
            identical(is.na(diagnosis$assignments), c(rep(FALSE, 8), TRUE, TRUE)),
            length(diagnosis$assessment) > 0)
}

# Meaningful zeros are consistently separate; ties never split to fill bins.
d <- data.frame(x = c(rep(0, 80), 1:20))
z <- bin_compare(d, "x", bins = 4, separate_zero = TRUE)
stopifnot(all(z$comparison$zero_bins == 1), all(z$comparison$total_bins == 5),
          all(z$comparison$largest_n == 80),
          identical(z$diagnoses$quantile_4$bins$n, c(80L, 5L, 5L, 5L, 5L)))
q <- bin_compare(d, "x", bins = 4)
stopifnot(q$comparison$intervals[1] == 1,
          any(grepl("reduce 4 requested intervals to 1", q$diagnoses[[1]]$assessment)))

# Screening is explicit and changes flags without changing membership.
loose <- bin_compare(d, "x", bins = 4, min_count = 0, min_prop = 0)
stopifnot(!any(loose$bins$sparse), identical(q$bins$n, loose$bins$n))

# A long-tail sample demonstrates balanced quantile sizes and wide within-bin ranges.
tail <- data.frame(x = c(1:99, 10000))
t <- bin_compare(tail, "x", bins = 4, min_count = 0, min_prop = 0)
stopifnot(identical(t$diagnoses$quantile_4$bins$n, rep(25L, 4)),
          identical(t$diagnoses$equal_width_4$bins$n, c(99L, 0L, 0L, 1L)),
          t$comparison$widest_range[1] == 9924,
          any(grepl("99.00%", t$diagnoses$equal_width_4$assessment, fixed = TRUE)),
          any(grepl("no exact zeros", t$assessment)))

# Constant data and a single populated zero bin must not pick an empty bin as widest.
for (v in list(rep(0, 4), rep(7, 4), 7)) {
  a <- bin_compare(data.frame(x = v), "x", separate_zero = TRUE)
  stopifnot(all(a$comparison$widest_range == 0),
            any(grepl("cannot distinguish", a$diagnoses[[1]]$assessment)))
}
a <- bin_compare(data.frame(x = rep(0, 4)), "x", methods = character(0),
                 custom_breaks = list(zero = c(-2, -1, 0, 1)))
stopifnot(a$comparison$widest_bin == "B2", a$comparison$widest_range == 0)

# Reject ambiguous requests and incomplete custom coverage; name the failed scheme.
fails(bin_compare(d, "x", bins = c(4, 4)), "unique whole")
fails(bin_compare(d, "x", bins = c(4, NA)), "unique whole")
fails(bin_compare(d, "x", methods = "custom"), "unique choices")
fails(bin_compare(d, "x", custom_breaks = list(c(0, 20))), "named list")
fails(bin_compare(d, "x", custom_breaks = list(quantile_4 = c(0, 20))), "differ")
fails(bin_compare(d, "x", methods = character(0)), "at least one")
fails(bin_compare(d, "x", custom_breaks = list(incomplete = c(1, 20))), "incomplete")
fails(bin_compare(d, "x", custom_breaks = list(duplicate = c(0, 1, 1, 20))), "strictly increasing")
fails(bin_compare(data.frame(x = c(-1, 0, 1)), "x", separate_zero = TRUE), "nonnegative")

# Printing exposes assessments. Graphs work for ties, empty and singleton bins,
# and restore the caller's layout. Explicit subsets keep plots manageable.
stopifnot(any(grepl("Assessment", capture.output(print(z$diagnoses[[1]])))),
          any(grepl("Category counts", capture.output(print(z)))))
pdf_file <- tempfile(fileext = ".pdf")
grDevices::pdf(pdf_file, width = 12, height = 8)
before <- graphics::par("mfrow")
for (result in list(c, z, t, a,
  bin_compare(data.frame(x = rep(0, 4)), "x", separate_zero = TRUE))) {
  plot(result)
  plot(result, type = "boundaries")
}
plot(c, schemes = c("cutpoints", "quantile_4"))
stopifnot(identical(before, graphics::par("mfrow")))
fails(plot(c, schemes = "missing"), "select unique")
grDevices::dev.off()
stopifnot(file.info(pdf_file)$size > 0)
unlink(pdf_file)
cat("All binwise comparison tests passed.\n")
