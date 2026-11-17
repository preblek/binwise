# Public interface deliberately uses a quoted column name.
bin_diagnose <- function(data, variable, bins = 4L,
                         method = c("quantile", "equal_width", "custom"),
                         breaks = NULL, separate_zero = FALSE,
                         min_count = 20L, min_prop = 0.01, warn = TRUE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame. Supply a numeric column using `variable`.",
         call. = FALSE)
  }
  if (!is.character(variable) || length(variable) != 1L ||
      is.na(variable) || !nzchar(variable)) {
    stop('`variable` must be one quoted column name, such as "income".',
         call. = FALSE)
  }
  if (sum(names(data) == variable) != 1L) {
    stop("`variable` must identify exactly one column in `data`.", call. = FALSE)
  }
  x <- data[[variable]]
  if (!is.numeric(x) || is.object(x) || !is.null(dim(x))) {
    stop("The selected column must be a plain numeric or integer vector. ",
         "Factors, dates, labelled values, and matrices need explicit conversion.",
         call. = FALSE)
  }
  method <- match.arg(method)
  scalar_number <- function(z, lower, upper, integer = FALSE) {
    is.numeric(z) && length(z) == 1L && is.finite(z) &&
      z >= lower && z <= upper && (!integer || z == floor(z))
  }
  if (!scalar_number(bins, 2, 1000, TRUE)) {
    stop("`bins` must be a whole number from 2 to 1000.", call. = FALSE)
  }
  if (!scalar_number(min_count, 0, .Machine$integer.max, TRUE) ||
      !scalar_number(min_prop, 0, 1)) {
    stop("`min_count` must be a nonnegative whole number; ",
         "`min_prop` must be between 0 and 1.", call. = FALSE)
  }
  for (arg in c("separate_zero", "warn")) {
    z <- get(arg)
    if (!is.logical(z) || length(z) != 1L || is.na(z)) {
      stop(paste0("`", arg, "` must be TRUE or FALSE."), call. = FALSE)
    }
  }
  if (method != "custom" && !is.null(breaks)) {
    stop('Use `method = "custom"` when supplying `breaks`.', call. = FALSE)
  }
  if (method == "custom" &&
      (!is.numeric(breaks) || is.object(breaks) || !is.null(dim(breaks)) ||
       length(breaks) < 2L || any(!is.finite(breaks)) ||
       any(diff(breaks) <= 0))) {
    stop("Custom `breaks` must be at least two finite, strictly increasing ",
         "numeric boundaries, including endpoints covering the data.", call. = FALSE)
  }
  valid <- is.finite(x)
  y <- x[valid]
  if (!length(y)) {
    stop("The selected column has no finite observations to diagnose.", call. = FALSE)
  }
  if (separate_zero && any(y < 0)) {
    stop("`separate_zero = TRUE` currently requires nonnegative data.", call. = FALSE)
  }
  notes <- character()
  if (any(!valid)) {
    notes <- c(notes, sprintf("Excluded %s missing values (NA/NaN) and %s infinite values.",
                             sum(is.na(x)), sum(is.infinite(x))))
  }
  # Scale before central moments to avoid overflow for large finite values.
  scale <- max(abs(y))
  z <- if (scale == 0) y else y / scale
  centered <- z - mean(z)
  m2 <- mean(centered^2)
  skewness <- if (m2 > 0) mean(centered^3) / m2^1.5 else NA_real_
  probs <- c(0, .01, .05, .10, .25, .50, .75, .90, .95, .99, 1)
  percentiles <- data.frame(
    probability = probs,
    value = as.numeric(stats::quantile(y, probs, type = 7, names = FALSE))
  )
  summary <- data.frame(
    variable = variable, n_total = length(x), n_finite = length(y),
    n_missing = sum(is.na(x)), n_infinite = sum(is.infinite(x)),
    n_zero = sum(y == 0), prop_zero = mean(y == 0),
    n_unique = length(unique(y)), min = min(y), max = max(y),
    mean = mean(y), median = stats::median(y),
    sd = if (length(y) > 1L) stats::sd(z) * scale else NA_real_,
    iqr = stats::IQR(y, type = 7), skewness = skewness
  )

  zero_bin <- separate_zero && any(y == 0)
  pool <- if (separate_zero) y[y != 0] else y
  requested <- if (method == "custom") length(breaks) - 1L else as.integer(bins)
  pool_ids <- integer(length(pool))
  candidate_breaks <- numeric()
  used_breaks <- numeric()
  lower <- upper <- numeric()
  left_closed <- logical()
  singleton <- logical()

  if (length(pool)) {
    if (method == "custom") {
      if (breaks[1L] > min(pool) || tail(breaks, 1L) < max(pool)) {
        stop("Custom `breaks` do not cover all finite observations to be binned.",
             call. = FALSE)
      }
      candidate_breaks <- breaks
    } else if (method == "quantile") {
      candidate_breaks <- as.numeric(stats::quantile(
        pool, seq(0, 1, length.out = bins + 1L), type = 7, names = FALSE
      ))
    } else {
      # Convex combinations avoid overflow in max(pool) - min(pool).
      w <- seq(0, 1, length.out = bins + 1L)
      candidate_breaks <- (1 - w) * min(pool) + w * max(pool)
    }
    used_breaks <- unique(candidate_breaks)
    if (length(used_breaks) == 1L) {
      pool_ids[] <- 1L
      lower <- upper <- used_breaks
      left_closed <- singleton <- TRUE
    } else {
      pool_ids <- cut(pool, breaks = used_breaks, include.lowest = TRUE,
                      right = TRUE, labels = FALSE)
      lower <- head(used_breaks, -1L)
      upper <- tail(used_breaks, -1L)
      left_closed <- c(TRUE, rep(FALSE, length(lower) - 1L))
      singleton <- rep(FALSE, length(lower))
    }
    if (length(lower) < requested) {
      notes <- c(notes, sprintf(
        "Requested %s interval bins; only %s distinct intervals are possible with these boundaries. Repeated boundaries were collapsed; tied values stay together.",
        requested, length(lower)
      ))
    }
  } else {
    notes <- c(notes, "All finite values are zero; only the dedicated zero category is available.")
  }
  if (separate_zero) {
    notes <- c(notes, paste0(
      "Interval boundaries use positive values only. ",
      if (zero_bin) "The zero category is additional to the requested interval bins."
      else "There are no exact zeros, so no zero category was added."
    ))
  }
  if (zero_bin) {
    lower <- c(0, lower)
    upper <- c(0, upper)
    left_closed <- c(TRUE, left_closed)
    singleton <- c(TRUE, singleton)
  }
  finite_ids <- integer(length(y))
  if (zero_bin) {
    finite_ids[y == 0] <- 1L
    finite_ids[y != 0] <- pool_ids + 1L
  } else {
    finite_ids[] <- pool_ids
  }
  n_bins <- length(lower)
  labels <- paste0("B", seq_len(n_bins))
  if (zero_bin) labels <- c("Zero", paste0("B", seq_len(n_bins - 1L)))
  if (zero_bin && n_bins == 1L) labels <- "Zero"
  memberships <- rep(NA_integer_, length(x))
  memberships[valid] <- finite_ids
  assignments <- factor(memberships, levels = seq_len(n_bins), labels = labels)
  counts <- tabulate(finite_ids, nbins = n_bins)
  bin_table <- data.frame(
    bin = labels, lower = lower, upper = upper,
    left_closed = left_closed, right_closed = TRUE, singleton = singleton,
    n = counts, proportion = counts / length(y),
    sparse = counts < min_count | counts / length(y) < min_prop
  )
  bin_table$observed_min <- bin_table$observed_max <-
    bin_table$mean <- bin_table$sd <- rep(NA_real_, n_bins)
  groups <- split(y, factor(finite_ids, levels = seq_len(n_bins)))
  for (j in seq_len(n_bins)) {
    g <- groups[[j]]
    if (length(g)) {
      bin_table$observed_min[j] <- min(g)
      bin_table$observed_max[j] <- max(g)
      bin_table$mean[j] <- mean(g)
      s <- max(abs(g))
      bin_table$sd[j] <- if (length(g) < 2L) NA_real_ else if (s == 0) 0 else stats::sd(g / s) * s
    }
  }
  if (any(counts == 0)) notes <- c(notes, "Empty interval bins are retained in the bin table.")
  if (any(bin_table$sparse)) {
    notes <- c(notes, sprintf(
      "%s bin(s) fall below min_count = %s or min_prop = %s. These are user-set screening rules, not statistical tests.",
      sum(bin_table$sparse), min_count, min_prop
    ))
  }
  if (length(unique(y)) == 1L) notes <- c(notes, "The variable is constant; categorization cannot reveal variation.")
  result <- structure(list(
    call = match.call(), variable = variable, method = method,
    summary = summary, percentiles = percentiles, bins = bin_table,
    requested_bins = requested, actual_interval_bins = n_bins - as.integer(zero_bin),
    occupied_bins = sum(counts > 0), candidate_breaks = candidate_breaks,
    breaks = used_breaks, separate_zero = separate_zero,
    min_count = min_count, min_prop = min_prop,
    assignments = assignments, values = x, notes = unique(notes)
  ), class = "bin_diagnosis")
  result$assessment <- assess_bins(result)
  if (warn && length(result$notes)) {
    warning(paste(result$notes, collapse = "\n"), call. = FALSE)
  }
  result
}

print.bin_diagnosis <- function(x, digits = 4L, ...) {
  s <- x$summary
  cat("binwise diagnosis of ", x$variable, "\n", sep = "")
  cat("\nAssessment\n", paste0("* ", x$assessment, collapse = "\n"), "\n\n", sep = "")
  cat(s$n_finite, "finite observations out of", s$n_total, "rows\n")
  cat(s$n_missing, "missing;", s$n_infinite, "infinite;", s$n_unique, "unique finite values\n")
  cat(s$n_zero, "exact zeros (", format(round(100 * s$prop_zero, 2)),
      "% of finite values)\n", sep = " ")
  print(s[c("min", "median", "mean", "max", "sd", "skewness")],
        row.names = FALSE, digits = digits)
  cat("\nPercentiles\n")
  print(x$percentiles, row.names = FALSE, digits = digits)
  cat("\nCandidate bins using ", x$method, "\n", sep = "")
  cat("Requested", x$requested_bins, "interval bins; created",
      x$actual_interval_bins, "interval bins and",
      as.integer(x$separate_zero && s$n_zero > 0), "zero category\n")
  print(x$bins[c("bin", "lower", "upper", "n", "proportion", "sparse")],
        row.names = FALSE, digits = digits)
  cat("Intervals include the upper boundary; the first also includes the lower boundary.\n")
  if (x$separate_zero) cat("Exact zeros are assigned separately before applying intervals.\n")
  if (length(x$notes)) cat("\nNotes\n", paste0("* ", x$notes, collapse = "\n"), "\n", sep = "")
  cat("\nThese diagnostics describe the supplied data and candidate boundaries.\n")
  invisible(x)
}

plot.bin_diagnosis <- function(x, type = c("histogram", "ecdf", "bins"),
                               main = NULL, ...) {
  type <- match.arg(type)
  y <- x$values[is.finite(x$values)]
  if (type == "bins") {
    if (is.null(main)) main <- paste("Candidate bin counts for", x$variable)
    graphics::barplot(x$bins$n, names.arg = x$bins$bin,
                      col = ifelse(x$bins$sparse, "#CC6848", "#397985"),
                      ylab = "Observations", main = main, ...)
    graphics::legend("topright", legend = c("Meets screening rules", "Sparse"),
                     fill = c("#397985", "#CC6848"), bty = "n", cex = .8)
  } else if (type == "ecdf") {
    if (is.null(main)) main <- paste("Cumulative distribution of", x$variable)
    graphics::plot(stats::ecdf(y), main = main, xlab = x$variable,
                   ylab = "Proportion at or below value", col = "#397985", ...)
    if (length(x$breaks) > 2L) graphics::abline(v = x$breaks[-c(1L, length(x$breaks))], col = "#CC6848", lty = 2)
  } else {
    if (is.null(main)) main <- paste("Distribution of", x$variable)
    # All finite observations are plotted, including a dedicated zero category.
    histogram_args <- list(...)
    if (is.null(histogram_args$freq) && is.null(histogram_args$probability)) {
      histogram_args$freq <- TRUE
    }
    density_axis <- identical(histogram_args$freq, FALSE) ||
      identical(histogram_args$probability, TRUE)
    do.call(graphics::hist, c(list(
      x = y, main = main, xlab = x$variable,
      ylab = if (density_axis) "Density" else "Observations",
      col = "#B9D9DD", border = "white"
    ), histogram_args))
    if (length(x$breaks) > 2L) graphics::abline(v = x$breaks[-c(1L, length(x$breaks))], col = "#CC6848", lty = 2, lwd = 2)
    graphics::mtext("Dashed lines mark internal candidate boundaries; all finite values are shown.",
                    side = 3, line = .2, cex = .65)
  }
  invisible(x)
}
