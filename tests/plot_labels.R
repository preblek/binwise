library(binwise)
fmt <- getFromNamespace("format_bin_values", "binwise")
# Fraction and already-percent input conventions must produce identical labels.
fractions <- c(0, .0033, .25, 1, -.125, 1.5)
expected <- c("0%", "0.33%", "25%", "100%", "-12.5%", "150%")
stopifnot(identical(fmt(fractions, "percent", 3, 100), expected),
          identical(fmt(fractions * 100, "percent", 3, 1), expected),
          identical(fmt(c(0, 1e-9, -1e-9), "percent", 3, 100),
                    c("0%", "<0.001%", ">-0.001%")),
          identical(fmt(.000001, "percent", 4, 100), "0.0001%"),
          identical(fmt(.25, "number", 3, 100), "0.25"))

fails <- function(expr, pattern) {
  err <- tryCatch({ force(expr); NULL }, error = identity)
  stopifnot(inherits(err, "error"), grepl(pattern, conditionMessage(err)))
}
d <- data.frame(share = c(0, 1e-9, seq(.01, 1, length.out = 100)))
comparison <- bin_compare(d, "share", separate_zero = TRUE)
original <- serialize(comparison, NULL)
pdf_file <- tempfile(fileext = ".pdf")
grDevices::pdf(pdf_file, width = 10, height = 7)
labels <- c(quantile_4 = "Quantile categories", equal_width_4 = "Equal intervals")
plot(comparison, type = "boundaries", value_format = "percent",
     scheme_labels = labels, xlab = "My measured share", main = "My comparison")
# A reusable mapping can include schemes outside the currently selected subset.
plot(comparison, type = "boundaries", value_format = "percent",
     schemes = "quantile_4", scheme_labels = labels)
plot(comparison, scheme_labels = labels, main = "Counts retain their units")
plot(comparison, type = "boundaries", show_counts = FALSE)
plot(comparison, type = "counts", show_counts = FALSE)
plot(bin_compare(data.frame(x = c(0, 0, 2)), "x", methods = character(0),
                 custom_breaks = list(gaps = c(0, 1, 1.5, 2)), separate_zero = TRUE),
     type = "boundaries", show_counts = TRUE)
plot(bin_compare(data.frame(rate = c(-5, 0, 25, 80, 120)), "rate"),
     type = "boundaries", value_format = "percent", percent_scale = 1)
plot(bin_compare(data.frame(income = c(100, 1000, 2000, 10000)), "income"),
     type = "boundaries", xlab = "Income", scheme_labels = c(quantile_4 = "Income groups"))
stopifnot(identical(original, serialize(comparison, NULL)))
fails(plot(comparison, scheme_labels = c(missing = "Typo")), "scheme_labels")
fails(plot(comparison, scheme_labels = "Unnamed"), "scheme_labels")
fails(plot(comparison, scheme_labels = c(quantile_4 = "")), "scheme_labels")
fails(plot(comparison, percent_scale = 0), "percent_scale")
fails(plot(comparison, main = c("one", "two")), "main")
fails(plot(comparison, show_counts = NA), "show_counts")
fails(plot(comparison, show_counts = c(TRUE, FALSE)), "show_counts")
grDevices::dev.off()
unlink(pdf_file)
cat("Plot label and unit conversion checks passed.\n")
