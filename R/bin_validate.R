# Refit candidate workflows inside shared folds, then compare paired losses.
bin_validate <- function(data, outcome, variables = NULL, schemes = NULL,
                         controls = NULL, interaction = length(variables) == 2L,
                         model = c("linear", "logit"), folds = 5L, group = NULL,
                         fold_id = NULL, cluster = group, seed = 1L,
                         fit_predict = NULL, reps = 499L, level = .95,
                         spline_df = NULL, reference = NULL) {
  validate_spline_df(spline_df)
  if (!is.null(reference) && (!is.character(reference) || length(reference) != 1L ||
      is.na(reference) || !nzchar(reference)))
    stop("`reference` must be NULL or one model name.", call. = FALSE)
  if (!is.null(fit_predict) && !is.null(spline_df))
    stop("With fit_predict, implement splines inside the callback and leave spline_df = NULL.", call. = FALSE)
  model <- match.arg(model)
  validate_confidence_level(level)
  if (!is.data.frame(data) || anyDuplicated(names(data))) stop("Supply a data frame with unique column names.", call. = FALSE)
  column_names <- function(x, optional = FALSE) {
    if (optional && is.null(x)) return(TRUE)
    is.character(x) && length(x) > 0L && !anyNA(x) && !anyDuplicated(x) && all(x %in% names(data))
  }
  integer_arg <- function(x, minimum) is.numeric(x) && length(x) == 1L && is.finite(x) && x == floor(x) && x >= minimum && x <= .Machine$integer.max
  if (!column_names(outcome) || length(outcome) != 1L || !is.numeric(data[[outcome]]) || is.object(data[[outcome]]))
    stop("`outcome` must name one plain numeric column.", call. = FALSE)
  if (!integer_arg(seed, 0) || !integer_arg(folds, 2) || !integer_arg(reps, 0) || (reps > 0 && reps < 99))
    stop("Use an integer seed, at least two folds, and reps = 0 or at least 99.", call. = FALSE)
  if (!column_names(group, TRUE) || length(group) > 1L || !column_names(cluster, TRUE) || length(cluster) > 2L)
    stop("`group` names one fold-group column; `cluster` names up to two resampling columns.", call. = FALSE)
  if (anyNA(data[c(group, cluster)])) stop("Fold groups and resampling clusters must not be missing.", call. = FALSE)
  automatic <- is.null(fit_predict)
  if (!automatic && !is.function(fit_predict)) stop("`fit_predict` must be a function of training and test data.", call. = FALSE)
  if (automatic) {
    if (!column_names(variables) || !length(variables) %in% 1:2 || !column_names(controls, TRUE) ||
        anyDuplicated(c(outcome, variables, controls))) stop("Supply one or two focal variables and distinct optional controls.", call. = FALSE)
    if (!all(vapply(data[variables], function(x) is.numeric(x) && !is.object(x) && is.null(dim(x)), logical(1))))
      stop("Focal variables must be plain numeric columns.", call. = FALSE)
    if (!all(vapply(data[controls], function(x) is.null(dim(x)) &&
      ((is.numeric(x) && !is.object(x)) || is.factor(x) || is.character(x) || is.logical(x)), logical(1))))
      stop("Controls must be plain numeric, factor, character, or logical columns.", call. = FALSE)
    if (!is.logical(interaction) || length(interaction) != 1L || is.na(interaction) || (interaction && length(variables) != 2L))
      stop("An optional interaction requires two focal variables.", call. = FALSE)
    if (is.null(schemes) && !is.null(spline_df)) schemes <- list()
    if (!is.null(spline_df) && "spline" %in% names(schemes))
      stop("The candidate name 'spline' is reserved when spline_df is supplied.", call. = FALSE)
    if ((!length(schemes) && is.null(spline_df)) || !is.list(schemes) ||
        (length(schemes) && (is.null(names(schemes)) || anyNA(names(schemes)) ||
        any(!nzchar(names(schemes))) || anyDuplicated(names(schemes)) || "continuous" %in% names(schemes))))
      stop("Supply named candidate recipes, or spline_df for a spline comparator.", call. = FALSE)
    for (s in schemes) {
      if (!is.list(s) || inherits(s, "bin_diagnosis") || !length(s) || is.null(names(s)) ||
          anyDuplicated(names(s)) || any(!names(s) %in% variables))
        stop("Each candidate maps focal names to recipes, not full-data bin diagnoses.", call. = FALSE)
      for (r in s) validate_validation_recipe(r)
    }
    needed <- c(outcome, variables, controls)
  } else needed <- outcome
  usable <- lapply(data[needed], function(x) if (is.numeric(x)) is.finite(x) else !is.na(x))
  keep <- Reduce(`&`, usable)
  d <- as.data.frame(data[keep, , drop = FALSE]); original_rows <- which(keep)
  y <- d[[outcome]]
  if (length(y) < 4L || (model == "logit" && (!all(y %in% c(0, 1)) || length(unique(y)) != 2L)))
    stop("Insufficient usable outcomes; logit requires both 0 and 1.", call. = FALSE)
  # Local randomness must not alter the user's subsequent simulation stream.
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv) else
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
  set.seed(seed)
  if (is.null(fold_id)) {
    units <- if (is.null(group)) seq_len(nrow(d)) else match(d[[group]], unique(d[[group]]))
    n_units <- length(unique(units))
    if (n_units < folds) stop("There are fewer distinct groups or rows than folds.", call. = FALSE)
    assignment <- sample(rep(seq_len(folds), length.out = n_units))
    id <- assignment[units]
  } else {
    if (length(fold_id) != nrow(data) || anyNA(fold_id)) stop("`fold_id` must give one nonmissing label per original row.", call. = FALSE)
    id <- match(fold_id[keep], unique(fold_id[keep]))
    folds <- length(unique(id))
    if (folds < 2L) stop("At least two populated folds are required.", call. = FALSE)
  }
  if (!is.null(group) && any(vapply(split(id, d[[group]]), function(z) length(unique(z)) > 1L, logical(1))))
    stop("A group appears in multiple folds.", call. = FALSE)
  predictions <- NULL; details <- vector("list", folds); diagnostics <- list()
  for (k in seq_len(folds)) {
    train <- d[id != k, , drop = FALSE]; test <- d[id == k, , drop = FALSE]
    if (automatic) ans <- validation_default_fit(train, test, outcome, variables, schemes, controls, interaction, model, spline_df)
    else ans <- fit_predict(train, test)
    if (is.matrix(ans) || is.data.frame(ans)) ans <- list(predictions = ans)
    p <- as.matrix(ans$predictions)
    if (!is.numeric(p) || nrow(p) != nrow(test) || ncol(p) < 2L || is.null(colnames(p)) ||
        anyNA(colnames(p)) || any(!nzchar(colnames(p))) || anyDuplicated(colnames(p)))
      stop("Each fold must return a numeric prediction matrix with one row per test observation and at least two named models.", call. = FALSE)
    if (is.null(predictions)) predictions <- matrix(NA_real_, nrow(d), ncol(p), dimnames = list(NULL, colnames(p)))
    if (!identical(colnames(p), colnames(predictions))) stop("Model names and order must be identical in every fold.", call. = FALSE)
    invalid <- !is.finite(p) | (model == "logit" & (p < 0 | p > 1))
    p[invalid] <- NA_real_
    predictions[id == k, ] <- p
    details[[k]] <- ans$details
    diagnostics[[k]] <- data.frame(fold = k, model = colnames(p), training_n = nrow(train), test_n = nrow(test),
      unavailable = colSums(is.na(p)), messages = if (is.null(ans$messages)) "" else
        vapply(colnames(p), function(n) paste(ans$messages[[n]], collapse = " | "), character(1)))
  }
  if (is.null(reference)) reference <- colnames(predictions)[1L]
  if (!reference %in% colnames(predictions))
    stop("`reference` must name a returned model.", call. = FALSE)
  score_order <- c(reference, setdiff(colnames(predictions), reference))
  common <- apply(is.finite(predictions), 1L, all)
  if (sum(common) < 2L) stop("Fewer than two common held-out predictions remain. Inspect model support and fitting failures.", call. = FALSE)
  inference <- validation_loss_summary(y[common], predictions[common, score_order, drop = FALSE], model,
    if (length(cluster)) d[common, cluster, drop = FALSE] else NULL, reps, level)
  fold_scores <- do.call(rbind, lapply(seq_len(folds), function(k) {
    at <- common & id == k
    if (!any(at)) return(NULL)
    z <- validation_loss_summary(y[at], predictions[at, score_order, drop = FALSE], model, NULL, 0L, level)$scores
    z$fold <- k; z$n <- sum(at); z
  }))
  assessment <- c(sprintf("Every scored prediction comes from a model fitted without that row%s.",
    if (is.null(group)) "" else " or its fold group"),
    sprintf("All candidates are scored on the same %s of %s usable observations (%.1f%% coverage).", sum(common), length(common), 100 * mean(common)),
    sprintf("Lower prediction losses are better. Negative paired differences favor the candidate over %s.", reference))
  for (i in seq_len(nrow(inference$differences))) {
    z <- inference$differences[i, ]
    assessment <- c(assessment, sprintf("%s versus %s: %s difference %.5g%s.", z$model, z$reference, z$metric, z$difference,
      if (reps) sprintf("; %.0f%% paired interval [%.5g, %.5g]", level * 100, z$lower, z$upper) else ""))
  }
  if (!all(common)) assessment <- c(assessment, "Unavailable predictions restrict this comparison. Coverage is part of the result; inspect diagnostics before interpreting a ranking.")
  if (!is.null(spline_df)) assessment <- c(assessment, sprintf(
    "The spline comparator uses %s degrees of freedom per focal variable%s; knots and boundaries are learned separately in each training fold. Inspect $fold_details. Better scores than a linear model alone do not establish that bins beat this flexible comparator.",
    spline_df, if (interaction) " with all basis-term interactions" else ""))
  assessment <- c(assessment,
    "Score intervals resample the held-out losses with predictions and folds fixed; they do not include refitting or split-selection uncertainty.",
    "Prediction gains do not prove causal thresholds or invalidate categories used for interpretation. Repeatedly choosing bins from these scores needs nested validation or a new test set.")
  structure(list(scores = inference$scores, differences = inference$differences,
    fold_scores = fold_scores, predictions = data.frame(row = original_rows, fold = id, outcome = y,
      scored = common, predictions, check.names = FALSE), fold_details = details,
    diagnostics = do.call(rbind, diagnostics), excluded_rows = which(!keep),
    assessment = assessment, model = model, folds = folds, group = group, cluster = cluster,
    level = level, reps = reps, seed = seed, spline_df = spline_df, reference = reference, bootstrap_successful = inference$successful), class = "bin_validation")
}

validate_validation_recipe <- function(r) {
  if (!is.list(r) || inherits(r, "bin_diagnosis") || any(!names(r) %in% c("probs", "breaks", "separate_zero")) ||
      anyDuplicated(names(r)) || sum(c(!is.null(r$probs), !is.null(r$breaks))) != 1L)
    stop("A validation recipe supplies either `probs` learned in training or prespecified `breaks`, plus optional `separate_zero`.", call. = FALSE)
  if (!is.null(r$separate_zero) && (!is.logical(r$separate_zero) || length(r$separate_zero) != 1L || is.na(r$separate_zero)))
    stop("`separate_zero` must be TRUE or FALSE.", call. = FALSE)
  v <- if (is.null(r$probs)) r$breaks else r$probs
  if (!is.numeric(v) || length(v) < 2L || anyNA(v) || any(diff(v) <= 0) ||
      (!is.null(r$probs) && (any(!is.finite(v)) || v[1] != 0 || tail(v, 1) != 1)))
    stop("Use increasing breaks, or increasing probabilities starting at 0 and ending at 1.", call. = FALSE)
}

validation_default_fit <- function(train, test, outcome, variables, schemes, controls, interaction, model, spline_df = NULL) {
  f <- comparison_formula(outcome, variables, controls, interaction)
  spline_f <- if (!is.null(spline_df))
    comparison_formula(outcome, variables, controls, interaction, spline_df) else NULL
  specs <- c(list(continuous = list()), if (!is.null(spline_df)) list(spline = list()), schemes)
  p <- matrix(NA_real_, nrow(test), length(specs), dimnames = list(NULL, names(specs)))
  details <- messages <- list()
  for (name in names(specs)) {
    a <- train; b <- test; definitions <- list(); warning_text <- character()
    p[, name] <- tryCatch(withCallingHandlers({
      for (v in names(specs[[name]])) {
        r <- specs[[name]][[v]]; zero <- isTRUE(r$separate_zero)
        if (zero && any(c(a[[v]], b[[v]]) < 0)) stop("Separate-zero recipes require nonnegative values.")
        pool <- a[[v]][if (zero) a[[v]] > 0 else rep(TRUE, nrow(a))]
        if (length(unique(pool)) < 2L) stop("Too few distinct training values to learn this scheme.")
        breaks <- if (is.null(r$probs)) r$breaks else unique(as.numeric(stats::quantile(pool, r$probs, names = FALSE)))
        # Only quantile recipes extend their tails. Prespecified breaks are honored.
        if (!is.null(r$probs)) breaks[c(1, length(breaks))] <- c(-Inf, Inf)
        assign_bins <- function(x) {
          ids <- cut(x, breaks, labels = FALSE, include.lowest = TRUE)
          if (zero) {ids <- ids + 1L; ids[x == 0] <- 1L}
          factor(ids, levels = seq_len(length(breaks) - 1L + as.integer(zero)))
        }
        a[[v]] <- assign_bins(a[[v]]); b[[v]] <- assign_bins(b[[v]])
        if (anyNA(a[[v]])) stop("Prespecified breaks do not cover all training values.")
        definitions[[v]] <- list(breaks = breaks, separate_zero = zero, training_n = nrow(a))
      }
      fit_formula <- if (name == "spline" && !is.null(spline_df)) spline_f else f
      fit <- if (model == "linear") stats::lm(fit_formula, a, na.action = stats::na.fail) else
        stats::glm(fit_formula, a, family = stats::binomial(), na.action = stats::na.fail, control = stats::glm.control(maxit = 100))
      if (name == "spline" && !is.null(spline_df)) definitions <- fitted_spline_details(fit, variables)
      if (fit$df.residual <= 0) stop("No residual degrees of freedom remain.")
      if (fit$rank < length(stats::coef(fit)) || any(!is.finite(stats::coef(fit))) ||
          (model == "logit" && (!isTRUE(fit$converged) || isTRUE(fit$boundary)))) stop("Fit is rank deficient, nonfinite, or not converged.")
      if (model == "logit" && model_separation_detected(fit, a[c(outcome, variables, controls)], a[[outcome]], variables, interaction))
        stop("Possible complete or quasi-complete separation; predictions withheld.")
      as.numeric(stats::predict(fit, newdata = b, type = "response"))
    }, warning = function(w) {warning_text <<- c(warning_text, conditionMessage(w)); invokeRestart("muffleWarning")}),
      error = function(e) {warning_text <<- c(warning_text, conditionMessage(e)); rep(NA_real_, nrow(b))})
    messages[[name]] <- unique(warning_text); details[[name]] <- definitions
  }
  list(predictions = p, details = details, messages = messages)
}

validation_loss_summary <- function(y, p, model, cluster, reps, level) {
  metrics <- if (model == "logit") c("Brier", "log_loss") else c("RMSE", "MAE")
  losses <- lapply(metrics, function(metric) {
    if (metric %in% c("Brier", "RMSE")) (p - y)^2 else if (metric == "MAE") abs(p - y) else {
      bounded <- pmin(pmax(p, 1e-15), 1 - 1e-15)
      -(y * log(bounded) + (1 - y) * log1p(-bounded))
    }
  })
  names(losses) <- metrics
  transforms <- function(x, metric) if (metric == "RMSE") sqrt(x) else x
  scores <- differences <- list(); successful <- reps
  if (reps) {
    if (is.null(cluster)) cluster <- data.frame(row = seq_along(y))
    codes <- lapply(cluster, function(x) match(x, unique(x)))
    if (any(vapply(codes, function(x) length(unique(x)) < 2L, logical(1))))
      stop("At least two clusters per resampling dimension are needed for score intervals.", call. = FALSE)
    cell <- if (length(codes) == 1L) codes[[1]] else as.integer(interaction(codes[[1]], codes[[2]], drop = TRUE))
    ids <- sort(unique(cell)); first <- match(ids, cell)
    counts <- as.numeric(rowsum(rep(1, length(y)), cell, reorder = TRUE))
    sums <- lapply(losses, rowsum, group = cell, reorder = TRUE)
    draws <- lapply(metrics, function(z) matrix(NA_real_, reps, ncol(p)))
    for (b in seq_len(reps)) {
      weights <- rep(1, length(ids))
      for (code in codes) {
        n <- max(code); frequency <- tabulate(sample.int(n, n, replace = TRUE), nbins = n)
        weights <- weights * frequency[code[first]]
      }
      denominator <- sum(weights * counts)
      if (!denominator) next
      for (j in seq_along(metrics)) draws[[j]][b, ] <- transforms(colSums(sums[[j]] * weights) / denominator, metrics[j])
    }
    successful <- sum(is.finite(draws[[1]][, 1]))
    if (successful < .9 * reps) stop("Too many bootstrap draws contain no scored observations.", call. = FALSE)
  }
  alpha <- (1 - level) / 2
  interval <- function(x) as.numeric(stats::quantile(x, c(alpha, 1 - alpha), na.rm = TRUE))
  for (j in seq_along(metrics)) {
    values <- transforms(colMeans(losses[[j]]), metrics[j])
    for (i in seq_len(ncol(p))) {
      ci <- if (reps) interval(draws[[j]][, i]) else c(NA_real_, NA_real_)
      scores[[length(scores) + 1L]] <- data.frame(model = colnames(p)[i], metric = metrics[j], score = values[i], lower = ci[1], upper = ci[2], row.names = NULL)
      if (i > 1L) {
        ci <- if (reps) interval(draws[[j]][, i] - draws[[j]][, 1]) else c(NA_real_, NA_real_)
        differences[[length(differences) + 1L]] <- data.frame(model = colnames(p)[i], reference = colnames(p)[1],
          metric = metrics[j], difference = values[i] - values[1], lower = ci[1], upper = ci[2], row.names = NULL)
      }
    }
  }
  list(scores = do.call(rbind, scores), differences = do.call(rbind, differences), successful = successful)
}

print.bin_validation <- function(x, ...) {
  cat("Held-out model comparison\n")
  print(x$scores, row.names = FALSE)
  cat("\n", paste(x$assessment, collapse = "\n"), "\n", sep = "")
  invisible(x)
}

plot.bin_validation <- function(x, metric = if (x$model == "logit") "Brier" else "RMSE", model_labels = NULL, ...) {
  z <- x$differences[x$differences$metric == metric, ]
  if (!nrow(z)) stop("Unknown score metric.", call. = FALSE)
  labels <- gsub("_", " ", z$model, fixed = TRUE)
  if (!is.null(model_labels)) {
    if (!is.character(model_labels) || is.null(names(model_labels)) || anyNA(model_labels) ||
        anyDuplicated(names(model_labels)) || any(!names(model_labels) %in% z$model))
      stop("`model_labels` must map candidate model names to display labels.", call. = FALSE)
    labels[match(names(model_labels), z$model)] <- unname(model_labels)
  }
  old <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(old))
  margins <- graphics::par("mai")
  margins[2] <- max(margins[2], max(graphics::strwidth(labels, units = "inches")) + .35)
  graphics::par(mai = margins)
  limits <- range(c(0, z$difference, z$lower, z$upper), na.rm = TRUE)
  if (diff(limits) == 0) limits <- limits + c(-1, 1) * .01
  graphics::plot(z$difference, seq_len(nrow(z)), xlim = limits, yaxt = "n", pch = 16,
    xlab = paste(metric, "difference from", z$reference[1]), ylab = "", ...)
  graphics::axis(2, at = seq_len(nrow(z)), labels = labels, las = 1)
  graphics::abline(v = 0, lty = 2, col = "grey60")
  if (x$reps) graphics::segments(z$lower, seq_len(nrow(z)), z$upper, seq_len(nrow(z)))
  invisible(x)
}
