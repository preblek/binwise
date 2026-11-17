# Evidence-based descriptive guidance. No distributional significance tests or
# automatic claims about the substantive meaning of a value are made here.
assess_bins <- function(x) {
  s <- x$summary
  b <- x$bins
  fmt <- function(z) format(signif(z, 5), trim = TRUE)
  out <- character()
  if (s$n_missing + s$n_infinite > 0) {
    out <- c(out, sprintf("The assessment uses %s finite observations; %s missing and %s infinite values are excluded.",
                         s$n_finite, s$n_missing, s$n_infinite))
  }
  if (s$n_unique == 1L) {
    return(c(out, sprintf("All finite observations equal %s. Binning cannot distinguish values in this sample.", fmt(s$min))))
  }
  if (s$n_zero > 0) {
    out <- c(out, sprintf("Exact zeros account for %.2f%% of finite observations. %s",
      100 * s$prop_zero,
      if (x$separate_zero) "They are kept in a separate category; interval bins describe positive values."
      else if (s$min >= 0) "If zero has a distinct substantive meaning, consider separate_zero = TRUE."
      else "Decide whether zero has a distinct substantive meaning; automatic zero separation currently requires nonnegative data."))
  } else {
    out <- c(out, "There are no exact zeros in the supplied sample; a separate zero category is unnecessary here.")
  }
  if (x$actual_interval_bins < x$requested_bins) {
    out <- c(out, sprintf("Repeated boundaries reduce %s requested intervals to %s. Equal values stay together; requesting more quantile bins may not produce equally sized groups.",
                         x$requested_bins, x$actual_interval_bins))
  }
  intervals <- b[b$bin != "Zero", , drop = FALSE]
  if (nrow(intervals)) {
    # Exclude a dedicated zero group from the interval balance calculation.
    j <- which.max(intervals$n)
    share <- 100 * intervals$n[j] / sum(intervals$n)
    out <- c(out, sprintf("The largest interval, %s, contains %s observations (%.2f%% of observations assigned to intervals). %s",
      intervals$bin[j], intervals$n[j], share,
      if (nrow(intervals) > 1L && share >= 80)
        "If balanced group sizes matter, compare quantile bins or different custom boundaries."
      else "Compare this balance with your intended use of the categories."))
    occupied <- intervals[intervals$n > 0, , drop = FALSE]
    # Normalization keeps the range comparison stable for extreme finite values.
    scale <- max(abs(c(s$min, s$max)))
    widths <- occupied$observed_max / scale - occupied$observed_min / scale
    j <- which.max(widths)
    share <- 100 * widths[j] / (s$max / scale - s$min / scale)
    out <- c(out, sprintf("The widest observed within-bin range is in %s, from %s to %s (%.2f%% of the full observed range). If distinctions within that range matter, consider additional or custom boundaries there.",
      occupied$bin[j], fmt(occupied$observed_min[j]), fmt(occupied$observed_max[j]), share))
  }
  if (any(b$n == 0)) {
    out <- c(out, sprintf("%s categories are empty. Review whether these intervals serve a substantive purpose or can be combined.", sum(b$n == 0)))
  }
  if (any(b$sparse)) {
    out <- c(out, sprintf("%s categories fall below your count or proportion screen (n < %s or proportion < %s). Small tail groups can be intentional; inspect their counts before combining them.",
                         sum(b$sparse), x$min_count, x$min_prop))
  }
  out
}
