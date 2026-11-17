# Apply fitted cutpoints; never recompute quantiles from the receiving data.
bin_apply <- function(data, scheme, name = NULL, variable = NULL,
                      out_of_range = c("error", "NA"), warn = TRUE) {
  validate_bin_scheme(scheme)
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  out_of_range <- match.arg(out_of_range)
  if (!is.logical(warn) || length(warn) != 1L || is.na(warn)) {
    stop("`warn` must be TRUE or FALSE.", call. = FALSE)
  }
  if (is.null(variable)) variable <- scheme$variable
  if (!is.character(variable) || length(variable) != 1L || is.na(variable) ||
      !nzchar(variable) || sum(names(data) == variable, na.rm = TRUE) != 1L) {
    stop("`variable` must identify exactly one column in the receiving data.", call. = FALSE)
  }
  values <- data[[variable]]
  if (!is.numeric(values) || is.object(values) || !is.null(dim(values))) {
    stop("The selected column must be a plain numeric or integer vector.", call. = FALSE)
  }
  if (is.null(name)) name <- paste0(variable, "_bin")
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(trimws(name))) {
    stop("`name` must be one nonempty column name.", call. = FALSE)
  }
  if (name %in% names(data)) {
    stop("The output column already exists. Choose a new `name` to avoid overwriting data.", call. = FALSE)
  }
  metadata <- attr(data, "binwise", exact = TRUE)
  if (!is.null(metadata) && !is.list(metadata)) {
    stop("The existing 'binwise' attribute is not a metadata list.", call. = FALSE)
  }
  b <- scheme$bins
  zero_bin <- any(b$bin == "Zero")
  valid <- is.finite(values)
  ids <- rep(NA_integer_, length(values))
  pool <- valid
  if (scheme$separate_zero) {
    pool <- valid & values > 0
    if (zero_bin) ids[valid & values == 0] <- 1L
  }
  if (length(scheme$breaks) == 1L) {
    ids[pool & values == scheme$breaks] <- 1L + as.integer(zero_bin)
  } else if (length(scheme$breaks) > 1L) {
    ids[pool] <- cut(values[pool], breaks = scheme$breaks,
                     right = TRUE, include.lowest = TRUE, labels = FALSE) + as.integer(zero_bin)
  }
  outside <- valid & is.na(ids)
  if (any(outside) && out_of_range == "error") {
    stop(sprintf(
      "%s finite observations have no category under the saved scheme. Inspect its boundaries and zero rule, or use out_of_range = \"NA\" to retain these rows with missing categories.",
      sum(outside)), call. = FALSE)
  }
  assignments <- factor(ids, levels = seq_len(nrow(b)), labels = b$bin)
  counts <- tabulate(ids, nbins = nrow(b))
  reasons <- rep(NA_character_, length(values))
  reasons[is.na(values)] <- "missing"
  reasons[is.infinite(values)] <- "infinite"
  reasons[outside] <- "out_of_range"
  excluded <- which(is.na(ids))
  report <- list(
    variable = variable, scheme_variable = scheme$variable, method = scheme$method,
    breaks = scheme$breaks, separate_zero = scheme$separate_zero,
    definition = b[c("bin", "lower", "upper", "left_closed", "right_closed", "singleton")],
    out_of_range = out_of_range,
    summary = data.frame(n_total = length(values), n_finite = sum(valid),
      n_assigned = sum(!is.na(ids)), n_missing = sum(is.na(values)),
      n_infinite = sum(is.infinite(values)), n_out_of_range = sum(outside)),
    counts = data.frame(bin = b$bin, n = counts,
      proportion = if (sum(valid)) counts / sum(valid) else rep(NA_real_, length(counts))),
    excluded = data.frame(row = excluded, reason = reasons[excluded])
  )
  # Use ordinary data-frame replacement, avoiding subclass reference semantics.
  result <- data
  class(result) <- "data.frame"
  result[[name]] <- assignments
  metadata[[name]] <- report
  attr(result, "binwise") <- metadata
  if (warn && length(excluded)) {
    warning(sprintf(
      "Created '%s' with %s unassigned rows (%s missing, %s infinite, %s outside the saved scheme). Rows are retained; see attr(result, \"binwise\")[[\"%s\"]].",
      name, length(excluded), sum(is.na(values)), sum(is.infinite(values)), sum(outside), name),
      call. = FALSE)
  }
  result
}

validate_bin_scheme <- function(scheme) {
  fail <- function() stop("`scheme` must be an unmodified bin_diagnosis from bin_diagnose(), or one element of bin_compare()$diagnoses.", call. = FALSE)
  if (!inherits(scheme, "bin_diagnosis") ||
      !is.character(scheme$variable) || length(scheme$variable) != 1L ||
      is.na(scheme$variable) || !nzchar(scheme$variable) ||
      !is.character(scheme$method) || length(scheme$method) != 1L ||
      !scheme$method %in% c("quantile", "equal_width", "custom") ||
      !is.logical(scheme$separate_zero) || length(scheme$separate_zero) != 1L ||
      is.na(scheme$separate_zero)) fail()
  breaks <- scheme$breaks
  if (!is.numeric(breaks) || is.object(breaks) || !is.null(dim(breaks)) ||
      any(!is.finite(breaks)) || any(diff(breaks) <= 0)) fail()
  b <- scheme$bins
  needed <- c("bin", "lower", "upper", "left_closed", "right_closed", "singleton")
  if (!is.data.frame(b) || !all(needed %in% names(b)) || nrow(b) < 1L ||
      !is.character(b$bin) || anyNA(b$bin) || anyDuplicated(b$bin)) fail()
  zero <- any(b$bin == "Zero")
  if (zero && !scheme$separate_zero) fail()
  n <- max(0L, length(breaks) - 1L)
  if (length(breaks) == 1L) n <- 1L
  lower <- if (length(breaks) == 1L) breaks else head(breaks, -1L)
  upper <- if (length(breaks) == 1L) breaks else tail(breaks, -1L)
  closed <- if (n) c(TRUE, rep(FALSE, n - 1L)) else logical()
  singleton <- rep(length(breaks) == 1L, n)
  labels <- if (n) paste0("B", seq_len(n)) else character()
  if (zero) {
    lower <- c(0, lower); upper <- c(0, upper)
    closed <- c(TRUE, closed); singleton <- c(TRUE, singleton)
    labels <- c("Zero", labels)
  }
  if (!identical(b$bin, labels) || !is.numeric(b$lower) || !is.numeric(b$upper) ||
      !identical(as.numeric(b$lower), as.numeric(lower)) ||
      !identical(as.numeric(b$upper), as.numeric(upper)) ||
      !identical(b$left_closed, closed) ||
      !identical(b$right_closed, rep(TRUE, length(labels))) ||
      !identical(b$singleton, singleton)) fail()
  invisible(scheme)
}
