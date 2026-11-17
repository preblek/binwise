# Compare the same observations under several explicitly identified schemes.
bin_compare <- function(data, variable, bins = 4L,
                        methods = c("quantile", "equal_width"),
                        custom_breaks = NULL, separate_zero = FALSE,
                        min_count = 20L, min_prop = 0.01) {
  if (!is.character(methods) || anyNA(methods) ||
      any(!methods %in% c("quantile", "equal_width")) || anyDuplicated(methods)) {
    stop('`methods` must contain unique choices from "quantile" and "equal_width", or character(0).', call. = FALSE)
  }
  if (!is.numeric(bins) || is.object(bins) || !is.null(dim(bins)) ||
      !length(bins) || any(!is.finite(bins)) || any(bins != floor(bins)) ||
      any(bins < 2 | bins > 1000) || anyDuplicated(bins)) {
    stop("`bins` must contain unique whole numbers from 2 to 1000.", call. = FALSE)
  }
  if (!is.null(custom_breaks) &&
      (!is.list(custom_breaks) || is.data.frame(custom_breaks) || !length(custom_breaks) ||
       is.null(names(custom_breaks)) || anyNA(names(custom_breaks)) ||
       any(!nzchar(trimws(names(custom_breaks)))) || anyDuplicated(names(custom_breaks)))) {
    stop("`custom_breaks` must be a named list of boundary vectors with unique, nonempty names.", call. = FALSE)
  }
  specs <- list()
  for (method in methods) {
    for (n in bins) {
      specs[[paste0(method, "_", n)]] <- list(method = method, bins = n)
    }
  }
  if (any(names(custom_breaks) %in% names(specs))) {
    stop("Custom scheme names must differ from automatic scheme names.", call. = FALSE)
  }
  for (name in names(custom_breaks)) {
    specs[[name]] <- list(method = "custom", breaks = custom_breaks[[name]])
  }
  if (!length(specs)) stop("Request at least one automatic or custom scheme.", call. = FALSE)
  diagnoses <- lapply(names(specs), function(name) {
    tryCatch(do.call(bin_diagnose, c(list(
      data = data, variable = variable, separate_zero = separate_zero,
      min_count = min_count, min_prop = min_prop, warn = FALSE
    ), specs[[name]])), error = function(e) {
      stop(sprintf("Scheme '%s' could not be evaluated. %s", name, conditionMessage(e)), call. = FALSE)
    })
  })
  names(diagnoses) <- names(specs)
  rows <- lapply(names(diagnoses), function(name) {
    d <- diagnoses[[name]]
    b <- d$bins
    scale <- max(abs(c(b$observed_min, b$observed_max)), na.rm = TRUE)
    width <- if (scale == 0) ifelse(b$n > 0, 0, NA_real_) else
      b$observed_max / scale - b$observed_min / scale
    j <- which.max(width)
    data.frame(
      scheme = name, method = d$method, requested = d$requested_bins,
      intervals = d$actual_interval_bins, zero_bins = sum(b$bin == "Zero"),
      total_bins = nrow(b), occupied = sum(b$n > 0), empty = sum(b$n == 0),
      sparse = sum(b$sparse), smallest_n = min(b$n), largest_n = max(b$n),
      smallest_pct = 100 * min(b$proportion), largest_pct = 100 * max(b$proportion),
      widest_bin = b$bin[j], observed_min = b$observed_min[j],
      observed_max = b$observed_max[j],
      widest_range = b$observed_max[j] - b$observed_min[j],
      stringsAsFactors = FALSE
    )
  })
  comparison <- do.call(rbind, rows)
  bin_tables <- lapply(names(diagnoses), function(name) {
    data.frame(scheme = name, diagnoses[[name]]$bins, row.names = NULL)
  })
  first <- diagnoses[[1L]]
  assessment <- c(
    sprintf("All %s schemes use the same %s finite observations; %s missing and %s infinite values are excluded.",
            length(diagnoses), first$summary$n_finite, first$summary$n_missing, first$summary$n_infinite),
    if (separate_zero && first$summary$n_zero > 0)
      "Every scheme keeps exact zeros separate. Requested intervals exclude this additional category; size and sparse-bin summaries include it."
    else if (first$summary$n_zero == 0)
      "There are no exact zeros in this sample; none of the schemes adds a zero category."
    else "Zeros remain in the interval bins. If zero has a distinct meaning, compare again with separate_zero = TRUE for nonnegative data."
  )
  for (i in seq_len(nrow(comparison))) {
    r <- comparison[i, ]
    assessment <- c(assessment, sprintf(
      "%s produces %s intervals plus %s zero category, with %s empty and %s flagged categories. Its largest category contains %.2f%% of the sample; its widest observed within-bin range runs from %s to %s.",
      r$scheme, r$intervals, r$zero_bins, r$empty, r$sparse, r$largest_pct,
      format(signif(r$observed_min, 5), trim = TRUE), format(signif(r$observed_max, 5), trim = TRUE)))
  }
  assessment <- c(assessment,
    "Use category sizes to assess balance and within-bin ranges to assess which values are grouped together. A small tail category can be intentional. These summaries do not select a best scheme.")
  structure(list(call = match.call(), variable = variable,
                 comparison = comparison, bins = do.call(rbind, bin_tables),
                 diagnoses = diagnoses, assessment = assessment,
                 separate_zero = separate_zero, min_count = min_count,
                 min_prop = min_prop), class = "bin_comparison")
}

print.bin_comparison <- function(x, digits = 4L, ...) {
  cat("binwise comparison of ", x$variable, "\n\n", sep = "")
  cat(paste0("* ", x$assessment, collapse = "\n"), "\n")
  cat("\nCategory counts and sizes\n")
  print(x$comparison[c("scheme", "requested", "intervals", "zero_bins", "empty",
                       "sparse", "smallest_n", "largest_n")], row.names = FALSE, digits = digits)
  cat("\nObserved spread within categories, in the variable's original units\n")
  print(x$comparison[c("scheme", "smallest_pct", "largest_pct", "widest_bin",
                       "observed_min", "observed_max", "widest_range")],
        row.names = FALSE, digits = digits)
  cat("\nRequested and intervals exclude any separate zero category.\n")
  cat("Smallest/largest sizes include empty bins and any zero category; percentages use all finite rows.\n")
  cat("Sparse means n < ", x$min_count, " or proportion < ", x$min_prop, ".\n", sep = "")
  cat("Inspect $bins for every boundary and count, or $diagnoses for each full assessment.\n")
  invisible(x)
}

plot.bin_comparison <- function(x, type = c("counts", "boundaries"),
                                schemes = NULL, digits = 3L,
                                value_format = c("number", "percent"),
                                percent_scale = 100, scheme_labels = NULL,
                                xlab = NULL, main = NULL, show_counts = TRUE, ...) {
  type <- match.arg(type)
  value_format <- match.arg(value_format)
  if (!is.logical(show_counts) || length(show_counts) != 1L || is.na(show_counts)) {
    stop("`show_counts` must be TRUE or FALSE.", call. = FALSE)
  }
  if (is.null(schemes)) schemes <- names(x$diagnoses)
  if (!is.character(schemes) || !length(schemes) || anyNA(schemes) ||
      any(!schemes %in% names(x$diagnoses)) || anyDuplicated(schemes)) {
    stop("`schemes` must select unique names from $diagnoses.", call. = FALSE)
  }
  if (!is.numeric(digits) || length(digits) != 1L || !is.finite(digits) ||
      digits != floor(digits) || digits < 1 || digits > 15) {
    stop("`digits` must be a whole number from 1 to 15.", call. = FALSE)
  }
  if (!is.numeric(percent_scale) || length(percent_scale) != 1L ||
      !is.finite(percent_scale) || percent_scale <= 0) {
    stop("`percent_scale` must be one positive finite number.", call. = FALSE)
  }
  if (!is.null(scheme_labels) &&
      (!is.character(scheme_labels) || is.null(names(scheme_labels)) ||
       anyNA(scheme_labels) || any(!nzchar(trimws(scheme_labels))) ||
       anyNA(names(scheme_labels)) || anyDuplicated(names(scheme_labels)) ||
       any(!names(scheme_labels) %in% names(x$diagnoses)))) {
    stop("`scheme_labels` must be a named character vector mapping existing scheme names to nonempty display labels.", call. = FALSE)
  }
  for (arg in c("xlab", "main")) {
    value <- get(arg)
    if (!is.null(value) && (!is.character(value) || length(value) != 1L || is.na(value))) {
      stop(paste0("`", arg, "` must be NULL or one character string."), call. = FALSE)
    }
  }
  ds <- x$diagnoses[schemes]
  titles <- vapply(names(ds), function(name) {
    d <- ds[[name]]
    if (d$method == "custom") gsub("_", " ", name, fixed = TRUE)
    else paste0(if (d$method == "quantile") "Quantiles" else "Equal widths",
                " (", d$requested_bins, " requested)")
  }, character(1))
  names(titles) <- names(ds)
  renamed <- intersect(names(scheme_labels), names(ds))
  titles[renamed] <- scheme_labels[renamed]
  if (is.null(xlab)) xlab <- x$variable
  if (is.null(main)) main <- paste("Binning comparison for", xlab)
  fmt <- function(z) format_bin_values(z, value_format, digits, percent_scale)
  previous <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(previous))
  cols <- min(2L, length(ds))
  graphics::par(mfrow = c(ceiling(length(ds) / cols), cols),
                mar = if (type == "counts") c(4, 4, 3, 1) else c(4, 9, 3, 1),
                oma = c(2, 0, 2, 0))
  count_max <- max(vapply(ds, function(d) max(d$bins$n), numeric(1)))
  bounds <- range(unlist(lapply(ds, function(d) c(d$bins$lower, d$bins$upper))))
  for (name in names(ds)) {
    b <- ds[[name]]$bins
    colors <- ifelse(b$sparse, "#CC6848", "#397985")
    count_labels <- format(b$n, trim = TRUE, big.mark = ",", scientific = FALSE)
    if (type == "counts") {
      at <- graphics::barplot(b$n, names.arg = b$bin, col = colors,
                             ylim = c(0, count_max * 1.18), main = NULL,
                             ylab = "Observations", ...)
      if (show_counts) graphics::text(at, b$n, labels = count_labels,
                                    pos = 3, cex = .65)
    } else {
      positions <- rev(seq_len(nrow(b)))
      labels <- ifelse(b$singleton, paste0(b$bin, " {", fmt(b$lower), "}"),
        paste0(b$bin, " ", ifelse(b$left_closed, "[", "("),
               fmt(b$lower), ", ", fmt(b$upper), "]"))
      graphics::plot(NA_real_, NA_real_, xlim = bounds,
                     ylim = c(.5, nrow(b) + if (show_counts) .85 else .5),
                     yaxt = "n", xaxt = "n",
                     xlab = xlab, ylab = "", main = NULL, ...)
      ticks <- if (value_format == "percent" && diff(bounds) > 0)
        pretty(bounds, n = 3) else graphics::axTicks(1)
      graphics::axis(1, at = ticks, labels = fmt(ticks))
      graphics::axis(2, at = positions, labels = labels, las = 1, cex.axis = .65)
      graphics::segments(b$lower, positions, b$upper, positions, col = colors, lwd = 3)
      graphics::points(b$lower, positions, pch = ifelse(b$left_closed, 16, 1), col = colors)
      graphics::points(b$upper, positions, pch = 16, col = colors)
      if (show_counts) {
        annotations <- paste0("n = ", count_labels)
        # Place counts above their own interval, including singleton/empty bins.
        # Keep labels inside the panel even when an interval hugs an endpoint.
        midpoint <- b$lower / 2 + b$upper / 2
        half_width <- graphics::strwidth(annotations, cex = .65) / 2
        limits <- graphics::par("usr")[1:2]
        padding <- graphics::strwidth(" ", cex = .65)
        label_x <- pmax(limits[1] + half_width + padding,
                        pmin(midpoint, limits[2] - half_width - padding))
        graphics::text(label_x, positions, labels = annotations,
                       pos = 3, offset = .4, cex = .65)
      }
    }
    # Titles are centered over the plotting region, so the shorter right margin
    # limits their available width on a small device. Wrap before drawing.
    title_width <- graphics::par("pin")[1] + 2 * graphics::par("mai")[4]
    title_lines <- character()
    current_line <- ""
    for (word in strsplit(titles[[name]], "[[:space:]]+")[[1]]) {
      proposed <- trimws(paste(current_line, word))
      if (nzchar(current_line) && graphics::strwidth(proposed, units = "inches",
          cex = graphics::par("cex.main"), font = 2) > title_width) {
        title_lines <- c(title_lines, current_line)
        current_line <- word
      } else current_line <- proposed
    }
    panel_title <- paste(c(title_lines, current_line), collapse = "\n")
    graphics::mtext(panel_title, side = 3, line = 1, font = 2,
                    cex = graphics::par("cex.main") * graphics::par("cex"))
  }
  graphics::mtext(main, outer = TRUE, side = 3, line = .4)
  graphics::mtext(if (type == "counts") "Shared count scale; orange marks a category below your screening rules."
    else "Shared value scale; cutpoints rounded. Orange marks categories below your screening rules.",
    outer = TRUE, side = 1, line = .4, cex = .75)
  invisible(x)
}

# Format labels only. Data, cutpoints, and plotting coordinates retain their units.
format_bin_values <- function(values, value_format, digits, percent_scale) {
  vapply(values, function(value) {
    if (value_format == "number") {
      return(format(signif(value, digits), trim = TRUE))
    }
    scaled <- value * percent_scale
    if (!is.finite(scaled)) {
      stop("Percentage labels overflow. Use a smaller `percent_scale` or numeric formatting.", call. = FALSE)
    }
    threshold <- 10^(-digits)
    if (scaled != 0 && abs(scaled) / threshold < 1 - 8 * .Machine$double.eps) {
      # Do not mislabel small positive or negative observations as exact zeros.
      return(paste0(if (scaled > 0) "<" else ">-",
                    format(threshold, scientific = FALSE, trim = TRUE), "%"))
    }
    paste0(format(round(scaled, digits), scientific = FALSE, trim = TRUE), "%")
  }, character(1), USE.NAMES = FALSE)
}
