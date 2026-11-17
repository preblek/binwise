# A deliberately small model interface: linear or binary-logit models, one or
# two numeric focal variables, and optional additive controls.
bin_interact <- function(data, outcome, variables, schemes,
                         interaction = length(variables) == 2L,
                         model = c("linear", "logit"), controls = NULL,
                         min_count = 20L, warn = TRUE, level = 0.95,
                         spline_df = NULL) {
  validate_spline_df(spline_df)
  model <- match.arg(model)
  validate_confidence_level(level)
  if (is.character(controls) && !length(controls)) controls <- NULL
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  valid_names <- function(z) is.character(z) && length(z) && !anyNA(z) &&
    all(nzchar(z)) && !anyDuplicated(z) &&
    all(vapply(z, function(n) sum(names(data) == n, na.rm = TRUE) == 1L, logical(1)))
  if (!valid_names(outcome) || length(outcome) != 1L ||
      !valid_names(variables) || !length(variables) %in% 1:2 ||
      (!is.null(controls) && !valid_names(controls)) ||
      anyDuplicated(c(outcome, variables, controls))) {
    stop("Supply one outcome, one or two focal variables, and optional distinct controls as quoted column names.", call. = FALSE)
  }
  for (arg in c("interaction", "warn")) {
    value <- get(arg)
    if (!is.logical(value) || length(value) != 1L || is.na(value))
      stop(paste0("`", arg, "` must be TRUE or FALSE."), call. = FALSE)
  }
  if (interaction && length(variables) != 2L)
    stop("An interaction requires two focal variables.", call. = FALSE)
  if (!is.numeric(min_count) || length(min_count) != 1L || !is.finite(min_count) ||
      min_count < 0 || min_count != floor(min_count))
    stop("`min_count` must be a nonnegative whole number.", call. = FALSE)
  columns <- c(outcome, variables, controls)
  plain_numeric <- function(z) is.numeric(z) && !is.object(z) && is.null(dim(z))
  if (!all(vapply(data[c(outcome, variables)], plain_numeric, logical(1))))
    stop("The outcome and focal variables must be plain numeric or integer columns.", call. = FALSE)
  for (name in controls) {
    z <- data[[name]]
    if (!is.null(dim(z)) || !(plain_numeric(z) || is.factor(z) || is.character(z) || is.logical(z)))
      stop("Controls must be numeric, factor, character, or logical columns.", call. = FALSE)
  }
  candidates <- if (!is.null(spline_df) && is.list(schemes) && !length(schemes))
    list() else normalize_model_schemes(schemes, variables)
  if (!is.null(spline_df) && "spline" %in% names(candidates))
    stop("The candidate name 'spline' is reserved when spline_df is supplied.", call. = FALSE)
  # Establish the common sample before categorizing. No scheme can select its
  # own subset; uncovered finite values stop instead of silently changing rows.
  usable <- lapply(data[columns], function(z) if (is.numeric(z) && !is.factor(z)) is.finite(z) else !is.na(z))
  keep <- Reduce(`&`, usable)
  rows <- which(keep)
  if (length(rows) < 3L) stop("Fewer than three complete, finite observations remain.", call. = FALSE)
  analysis <- data
  class(analysis) <- "data.frame"
  analysis <- analysis[rows, columns, drop = FALSE]
  rownames(analysis) <- NULL
  for (name in controls) {
    if (!plain_numeric(analysis[[name]])) {
      analysis[[name]] <- if (is.factor(analysis[[name]]))
        factor(as.character(analysis[[name]]), levels = levels(analysis[[name]]))
      else factor(analysis[[name]])
    }
  }
  if (any(vapply(analysis[variables], function(z) length(unique(z)) < 2L, logical(1))))
    stop("Each focal variable must vary in the common sample.", call. = FALSE)
  y <- analysis[[outcome]]
  if (model == "logit" && (!all(y %in% c(0, 1)) || length(unique(y)) != 2L))
    stop("Logit models require both outcome values 0 and 1 in the common sample.", call. = FALSE)
  formula <- comparison_formula(outcome, variables, controls, interaction)
  spline_formula <- if (!is.null(spline_df))
    comparison_formula(outcome, variables, controls, interaction, spline_df) else NULL
  specs <- c(list(continuous = list()), if (!is.null(spline_df)) list(spline = list()), candidates)
  spline_details <- NULL
  models <- issues <- support <- list()
  comparison <- list()
  for (name in names(specs)) {
    specification <- specs[[name]]
    fit_data <- analysis
    for (variable in names(specification)) {
      fit_data[[variable]] <- tryCatch(model_bin_values(analysis[[variable]], specification[[variable]], "error"),
        error = function(e) stop(sprintf("Scheme '%s', variable '%s'. %s", name, variable, conditionMessage(e)), call. = FALSE))
    }
    support[name] <- list(model_bin_support(fit_data, names(specification), min_count,
                                           if (model == "logit") y else NULL))
    fit_formula <- if (name == "spline" && !is.null(spline_df)) spline_formula else formula
    messages <- character()
    fit <- tryCatch(withCallingHandlers(
      if (model == "linear") stats::lm(fit_formula, data = fit_data, na.action = stats::na.fail)
      else stats::glm(fit_formula, data = fit_data, family = stats::binomial("logit"),
                      na.action = stats::na.fail, control = stats::glm.control(maxit = 100)),
      warning = function(e) {
        messages <<- c(messages, conditionMessage(e)); invokeRestart("muffleWarning")
      }), error = function(e) {
        messages <<- c(messages, conditionMessage(e)); NULL
      })
    if (name == "spline" && !is.null(spline_df) && !is.null(fit))
      spline_details <- fitted_spline_details(fit, variables)
    status <- "ok"
    if (is.null(fit)) status <- "failed"
    else {
      if (fit$rank < length(stats::coef(fit))) {
        messages <- c(messages, "The model matrix is rank deficient; some terms cannot be estimated.")
        status <- "failed"
      }
      if (fit$df.residual <= 0) {
        messages <- c(messages, "No residual degrees of freedom remain."); status <- "failed"
      }
      if (any(!is.finite(stats::coef(fit))) || any(!is.finite(stats::fitted(fit)))) {
        messages <- c(messages, "The fit contains nonfinite coefficients or fitted values.")
        status <- "failed"
      }
      if (model == "logit" && (!isTRUE(fit$converged) || isTRUE(fit$boundary))) {
        messages <- c(messages, "The logit fit did not converge or reached a numerical boundary.")
        status <- "failed"
      }
      if (model == "logit" && model_separation_detected(fit, fit_data, y, variables, interaction))
        messages <- c(messages, "The fitted scores or categorical groups show complete or quasi-complete separation; finite logit coefficients are not established.")
      if (status == "ok" && length(messages)) status <- "warning"
    }
    # Withhold comparisons for failed or numerically flagged models, but retain
    # the fit and diagnostic messages for inspection. Do not silently drop them.
    ok <- status == "ok"
    score <- if (!ok) NA_real_ else if (model == "linear")
      sqrt(mean((y - stats::fitted(fit))^2)) else mean((y - stats::fitted(fit))^2)
    aic <- if (ok) stats::AIC(fit) else NA_real_
    bic <- if (ok) stats::BIC(fit) else NA_real_
    if (ok && (!is.finite(aic) || !is.finite(bic))) {
      aic <- bic <- NA_real_
      messages <- c(messages, "Information criteria are nonfinite (for example, an exact fit with zero residual variance); they are withheld. Point predictions remain available.")
    }
    comparison[[name]] <- data.frame(model = name, n = nrow(analysis),
      parameters = if (is.null(fit)) NA_integer_ else fit$rank + as.integer(model == "linear"),
      AIC = aic, BIC = bic,
      training_score = score, status = status, stringsAsFactors = FALSE)
    models[name] <- list(fit)
    issues[name] <- list(unique(messages))
  }
  comparison <- do.call(rbind, comparison)
  rownames(comparison) <- NULL
  comparison$delta_AIC <- comparison$AIC - comparison$AIC[1]
  excluded <- data.frame(row = which(!keep), columns = vapply(which(!keep), function(i)
    paste(columns[!vapply(usable, `[`, logical(1), i)], collapse = ", "), character(1)))
  notes <- c(
    sprintf("All models use the same %s observations; %s rows are excluded for missing or nonfinite model inputs.", length(rows), sum(!keep)),
    "The continuous benchmark uses linear terms, plus their product when an interaction is requested.",
    "Training scores describe these fitted data, not out-of-sample performance. No scheme is selected automatically.",
    "Default prediction curves hold controls fixed and may include combinations with little joint support. They are not causal effects.",
    "Confidence intervals are pointwise and model-based, conditional on the chosen model and fixed bin definitions. They do not quantify uncertainty in model differences or bin selection."
  )
  if (!is.null(spline_df)) notes <- c(notes, sprintf(
    "The spline comparator uses natural cubic splines with %s degrees of freedom per focal variable%s. Controls retain their original additive form. Knots and boundaries are stored in $spline_details.",
    spline_df, if (interaction) " and all products of their basis terms" else ""))
  if (any(comparison$status != "ok")) notes <- c(notes,
    "Fits marked failed or warning have their comparison metrics and predictions withheld; inspect $issues and $models.")
  if (any(comparison$status == "ok" & is.na(comparison$AIC))) notes <- c(notes,
    "Some otherwise usable fits have undefined information criteria; inspect $issues.")
  result <- structure(list(call = match.call(), outcome = outcome, variables = variables,
    controls = controls, interaction = interaction, model = model, formula = formula, level = level,
    min_count = min_count, spline_df = spline_df, spline_formula = spline_formula,
    spline_details = spline_details,
    models = models, schemes = specs, comparison = comparison, issues = issues,
    support = support, sample = list(n_total = nrow(data), n_used = length(rows),
      rows = rows, excluded = excluded), analysis_data = analysis,
    score_name = if (model == "linear") "training RMSE" else "training Brier score",
    notes = notes), class = "bin_interaction")
  result$grid <- model_prediction_grid(result)
  result$predictions <- predict.bin_interaction(result, result$grid$data, warn = FALSE)
  if (any(result$predictions$status != "ok")) result$notes <- c(result$notes,
    "Some default-grid predictions are unavailable; inspect $predictions$status. Curves leave those positions blank.")
  if (any(result$predictions$interval_status == "nonfinite_interval")) result$notes <- c(result$notes,
    "Some default-grid confidence intervals are unavailable; inspect $predictions$interval_status.")
  result$assessment <- assess_model_comparison(result)
  if (warn && (sum(!keep) > 0 || any(comparison$status != "ok")))
    warning(paste(c(notes[1], if (any(comparison$status != "ok")) tail(notes, 1)), collapse = "\n"), call. = FALSE)
  result
}

model_separation_detected <- function(fit, data, outcome, variables, interaction) {
  eta <- fit$linear.predictors
  if (all(is.finite(eta)) && max(eta[outcome == 0]) < min(eta[outcome == 1])) return(TRUE)
  factors <- data[vapply(data, is.factor, logical(1))]
  if (interaction && all(vapply(data[variables], is.factor, logical(1))))
    factors[["joint"]] <- do.call(base::interaction, c(unname(data[variables]), list(drop = TRUE)))
  any(vapply(factors, function(group)
    any(tapply(outcome, group, function(z) length(unique(z)) == 1L), na.rm = TRUE), logical(1)))
}

normalize_model_schemes <- function(schemes, variables) {
  if (inherits(schemes, "bin_comparison")) schemes <- schemes$diagnoses
  if (!is.list(schemes) || !length(schemes) || is.null(names(schemes)) ||
      anyNA(names(schemes)) || any(!nzchar(trimws(names(schemes)))) ||
      anyDuplicated(names(schemes)) || "continuous" %in% names(schemes))
    stop("`schemes` must be a named list of candidate schemes, or a bin_comparison. The name 'continuous' is reserved.", call. = FALSE)
  lapply(schemes, function(spec) {
    if (inherits(spec, "bin_diagnosis")) spec <- stats::setNames(list(spec), spec$variable)
    if (!is.list(spec) || !length(spec) || is.null(names(spec)) ||
        anyNA(names(spec)) || anyDuplicated(names(spec)) || any(!names(spec) %in% variables))
      stop("Each candidate must be a diagnosis or a named list mapping focal variables to diagnoses.", call. = FALSE)
    for (variable in names(spec)) {
      validate_bin_scheme(spec[[variable]])
      if (!identical(spec[[variable]]$variable, variable))
        stop("Each diagnosis must name the focal variable to which it is applied.", call. = FALSE)
    }
    spec
  })
}

model_bin_values <- function(values, scheme, out_of_range) {
  bin_apply(data.frame(value = values), scheme, variable = "value", name = "category",
            out_of_range = out_of_range, warn = FALSE)$category
}

model_bin_support <- function(data, variables, min_count, outcome = NULL) {
  if (!length(variables)) return(NULL)
  fs <- data[variables]
  if (prod(vapply(fs, nlevels, integer(1))) > 10000)
    stop("A candidate has more than 10,000 possible bin combinations. Use fewer categories.", call. = FALSE)
  counts <- as.integer(do.call(table, unname(fs)))
  grid <- expand.grid(lapply(fs, levels), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  result <- data.frame(variable_1 = variables[1], bin_1 = grid[[1]],
    variable_2 = if (length(variables) == 2) variables[2] else NA_character_,
    bin_2 = if (length(variables) == 2) grid[[2]] else NA_character_,
    n = counts, sparse = counts < min_count, stringsAsFactors = FALSE)
  if (!is.null(outcome)) {
    cell <- as.integer(do.call(interaction, c(unname(fs), list(drop = FALSE, lex.order = FALSE))))
    result$events <- tabulate(cell[outcome == 1], nbins = length(counts))
    result$non_events <- counts - result$events
  }
  result
}

model_prediction_grid <- function(object) {
  d <- object$analysis_data
  variables <- object$variables
  limits <- range(d[[variables[1]]])
  w <- seq(0, 1, length.out = 100)
  values <- (1 - w) * limits[1] + w * limits[2]
  # Include cutpoints and just-above-cutpoint positions so plotted groups reach
  # their saved boundaries without drawing a line across a discontinuity.
  cuts <- unlist(lapply(object$schemes, function(s) {
    if (is.null(s[[variables[1]]])) return(numeric())
    c(s[[variables[1]]]$breaks, if (s[[variables[1]]]$separate_zero) 0)
  }))
  shift <- max(abs(limits), .Machine$double.xmin) * .Machine$double.eps * 8
  values <- sort(unique(c(values, cuts, cuts + shift)))
  values <- values[is.finite(values) & values >= limits[1] & values <= limits[2]]
  moderator <- if (length(variables) == 2) unique(as.numeric(stats::quantile(
    d[[variables[2]]], c(.25, .5, .75), names = FALSE))) else NA_real_
  positions <- expand.grid(x = values, moderator = moderator, KEEP.OUT.ATTRS = FALSE)
  grid <- d[rep(1L, nrow(positions)), c(variables, object$controls), drop = FALSE]
  rownames(grid) <- NULL
  grid[[variables[1]]] <- positions$x
  if (length(variables) == 2) grid[[variables[2]]] <- positions$moderator
  reference <- list()
  for (name in object$controls) {
    z <- d[[name]]
    value <- if (is.factor(z)) names(which.max(table(z))) else stats::median(z)
    reference[[name]] <- value
    grid[[name]] <- if (is.factor(z)) factor(rep(value, nrow(grid)), levels = levels(z)) else rep(value, nrow(grid))
  }
  list(data = grid, moderator_values = moderator, control_reference = reference)
}

validate_confidence_level <- function(level) {
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1)
    stop("`level` must be one number strictly between 0 and 1.", call. = FALSE)
  invisible(level)
}

predict.bin_interaction <- function(object, newdata = object$grid$data, warn = TRUE,
                                    interval = c("confidence", "none"),
                                    level = if (is.null(object$level)) 0.95 else object$level, ...) {
  if (length(list(...))) stop("Additional prediction arguments are not supported in this draft.", call. = FALSE)
  interval <- match.arg(interval)
  validate_confidence_level(level)
  if (!is.data.frame(newdata)) stop("`newdata` must be a data frame.", call. = FALSE)
  if (!is.logical(warn) || length(warn) != 1L || is.na(warn))
    stop("`warn` must be TRUE or FALSE.", call. = FALSE)
  columns <- c(object$variables, object$controls)
  if (!all(vapply(columns, function(n) sum(names(newdata) == n, na.rm = TRUE) == 1L, logical(1))))
    stop("`newdata` must contain every focal variable and control exactly once.", call. = FALSE)
  d <- newdata
  class(d) <- "data.frame"
  d <- d[columns]
  valid <- rep(TRUE, nrow(d))
  extrapolated <- rep(FALSE, nrow(d))
  for (name in columns) {
    original <- object$analysis_data[[name]]
    z <- d[[name]]
    if (is.numeric(original)) {
      if (!is.numeric(z) || is.object(z) || !is.null(dim(z)))
        stop("Numeric predictors in newdata must remain plain numeric or integer columns.", call. = FALSE)
      valid <- valid & is.finite(z)
      extrapolated <- extrapolated | (is.finite(z) & (z < min(original) | z > max(original)))
    } else {
      if (!is.null(dim(z)) || !(is.factor(z) || is.character(z) || is.logical(z)))
        stop("Categorical controls in newdata must remain factor, character, or logical columns.", call. = FALSE)
      valid <- valid & !is.na(z)
      d[[name]] <- as.character(z)
    }
  }
  output <- lapply(names(object$models), function(name) {
    fit <- object$models[[name]]
    spec <- object$schemes[[name]]
    frame <- d
    reason <- ifelse(valid, "ok", "missing_or_nonfinite_predictor")
    for (variable in names(spec)) {
      frame[[variable]] <- model_bin_values(d[[variable]], spec[[variable]], "NA")
      reason[valid & is.na(frame[[variable]])] <- "uncovered_by_scheme"
    }
    estimate <- rep(NA_real_, nrow(d))
    lower <- upper <- rep(NA_real_, nrow(d))
    interval_status <- rep(if (interval == "none") "not_requested" else "prediction_unavailable", nrow(d))
    usable <- object$comparison$status[match(name, object$comparison$model)] == "ok"
    if (!usable) reason[] <- "model_unavailable"
    else {
      for (variable in names(fit$xlevels)) {
        unknown <- !is.na(frame[[variable]]) & !as.character(frame[[variable]]) %in% fit$xlevels[[variable]]
        reason[reason == "ok" & unknown] <- "unobserved_category"
        frame[[variable]] <- factor(as.character(frame[[variable]]), levels = fit$xlevels[[variable]])
      }
      eligible <- which(reason == "ok")
      if (length(eligible)) {
        new_frame <- frame[eligible, , drop = FALSE]
        if (interval == "none") {
          estimate[eligible] <- if (object$model == "linear")
            as.numeric(stats::predict(fit, newdata = new_frame)) else
            as.numeric(stats::predict(fit, newdata = new_frame, type = "response"))
        } else {
          if (object$model == "linear") {
            ci <- stats::predict(fit, newdata = new_frame, interval = "confidence", level = level)
            estimate[eligible] <- ci[, "fit"]
            lower[eligible] <- ci[, "lwr"]
            upper[eligible] <- ci[, "upr"]
          } else {
            # Form Wald intervals on the link scale, then invert the link.
            # This preserves the probability scale without symmetric clipping.
            link <- stats::predict(fit, newdata = new_frame, type = "link", se.fit = TRUE)
            critical <- stats::qnorm((1 - level) / 2, lower.tail = FALSE)
            estimate[eligible] <- fit$family$linkinv(link$fit)
            lower[eligible] <- fit$family$linkinv(link$fit - critical * link$se.fit)
            upper[eligible] <- fit$family$linkinv(link$fit + critical * link$se.fit)
            bad_link <- !is.finite(link$fit) | !is.finite(link$se.fit)
            lower[eligible[bad_link]] <- upper[eligible[bad_link]] <- NA_real_
          }
          interval_status[eligible] <- ifelse(
            is.finite(lower[eligible]) & is.finite(upper[eligible]) & lower[eligible] <= upper[eligible],
            "ok", "nonfinite_interval")
        }
      }
      bad <- reason == "ok" & !is.finite(estimate)
      reason[bad] <- "nonfinite_prediction"
      estimate[bad] <- NA_real_
      if (interval == "confidence") interval_status[bad] <- "prediction_unavailable"
    }
    lower[interval_status != "ok"] <- upper[interval_status != "ok"] <- NA_real_
    focal_bin <- if (object$variables[1] %in% names(spec)) as.character(frame[[object$variables[1]]]) else rep("continuous", nrow(d))
    data.frame(model = rep(name, nrow(d)), row = seq_len(nrow(d)), estimate = estimate,
      status = reason, extrapolated = extrapolated, focal_bin = focal_bin,
      lower = lower, upper = upper, interval_status = interval_status,
      stringsAsFactors = FALSE)
  })
  result <- do.call(rbind, output)
  rownames(result) <- NULL
  baseline <- result$estimate[result$model == "continuous"]
  result$difference <- result$estimate - baseline[result$row]
  attr(result, "interval") <- interval
  attr(result, "level") <- if (interval == "confidence") level else NA_real_
  attr(result, "interval_method") <- if (interval == "none") "none" else
    if (object$model == "linear") "model-based t interval for the mean" else "model-based logit-link Wald interval"
  if (warn && (any(result$status != "ok") || any(result$interval_status == "nonfinite_interval")))
    warning("Some predictions or confidence intervals are unavailable. Inspect status and interval_status.", call. = FALSE)
  result
}

print.bin_interaction <- function(x, digits = 4L, ...) {
  cat("binwise model comparison for ", x$outcome, "\n", sep = "")
  cat("Model", x$model, "; interaction", x$interaction, "\n")
  print(x$formula)
  if (!is.null(x$spline_formula)) {
    cat("Spline comparator\n"); print(x$spline_formula)
  }
  if (!is.null(x$assessment))
    cat("\nAssessment\n", paste0("* ", x$assessment, collapse = "\n"), "\n\n", sep = "")
  else cat("\n", paste0("* ", x$notes, collapse = "\n"), "\n\n", sep = "")
  print(x$comparison, row.names = FALSE, digits = digits)
  cat("\ntraining_score is ", x$score_name, ". delta_AIC is relative to the continuous benchmark.\n", sep = "")
  if (length(x$grid$control_reference)) {
    cat("\nControls held fixed in the default prediction grid\n")
    print(x$grid$control_reference)
  }
  if (any(lengths(x$issues) > 0)) {
    cat("\nModel issues\n"); print(x$issues[lengths(x$issues) > 0])
  }
  cat("\nInspect $predictions for common-grid estimates and differences; $support gives counts for binned combinations.\n")
  invisible(x)
}

plot.bin_interaction <- function(x, type = c("predictions", "differences"),
                                 main = NULL, xlab = NULL, ylab = NULL,
                                 show_confidence = type == "predictions",
                                 level = if (is.null(x$level)) 0.95 else x$level, ...) {
  type <- match.arg(type)
  validate_confidence_level(level)
  if (!is.logical(show_confidence) || length(show_confidence) != 1L || is.na(show_confidence))
    stop("`show_confidence` must be TRUE or FALSE.", call. = FALSE)
  if (type == "differences" && show_confidence)
    stop("Confidence intervals for model differences are not implemented. Use show_confidence = FALSE.", call. = FALSE)
  # Recalculate from the saved fits, including for objects made before bands
  # were introduced. Changing the confidence level does not refit any model.
  prediction <- if (show_confidence) predict.bin_interaction(x, warn = FALSE,
    interval = "confidence", level = level) else x$predictions
  names <- x$comparison$model[x$comparison$status == "ok"]
  if (type == "differences") names <- setdiff(names, "continuous")
  value <- if (type == "predictions") prediction$estimate else prediction$difference
  if (!length(names) || !any(is.finite(value[prediction$model %in% names])))
    stop("No usable predictions are available for this plot. Inspect $issues.", call. = FALSE)
  if (is.null(xlab)) xlab <- x$variables[1]
  if (is.null(ylab)) ylab <- if (type == "differences") "Difference from continuous model" else
    if (x$model == "logit") "Predicted probability" else "Predicted outcome"
  if (is.null(main)) main <- if (type == "differences") "Changes from the continuous benchmark" else "Continuous and binned model predictions"
  panels <- x$grid$moderator_values
  before <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(before))
  graphics::par(mfrow = c(1, length(panels)), mar = c(4, 4, 3, 1), oma = c(2, 0, 2, 0))
  colors <- rep(c("#397985", "#CC6848", "#6956A0", "#609447", "#B08932", "#555555"), length.out = length(names))
  line_types <- rep(1:6, length.out = length(names))
  selected_models <- prediction$model %in% names
  ylim <- range(c(value[selected_models], if (show_confidence)
    c(prediction$lower[selected_models], prediction$upper[selected_models])), finite = TRUE)
  if (type == "predictions" && x$model == "logit") ylim <- c(0, 1)
  for (panel in panels) {
    rows <- if (length(x$variables) == 1) seq_len(nrow(x$grid$data)) else
      which(x$grid$data[[x$variables[2]]] == panel)
    xs <- x$grid$data[[x$variables[1]]]
    title <- if (length(x$variables) == 1) x$variables[1] else paste(x$variables[2], "=", signif(panel, 3))
    graphics::plot(NA_real_, NA_real_, xlim = range(xs), ylim = ylim,
                   xlab = xlab, ylab = ylab, main = title, ...)
    if (type == "differences") graphics::abline(h = 0, lty = 3, col = "grey60")
    # Draw every ribbon before any fitted curve so shading cannot cover another
    # model's line. Split at category boundaries and unavailable grid positions.
    if (show_confidence) for (i in seq_along(names)) {
      selected <- which(prediction$model == names[i] & prediction$row %in% rows &
                          prediction$interval_status == "ok")
      groups <- split(selected, prediction$focal_bin[selected])
      for (g in groups) {
        g <- g[order(xs[prediction$row[g]])]
        runs <- split(g, cumsum(c(1L, diff(prediction$row[g]) != 1L)))
        for (run in runs) {
          xx <- xs[prediction$row[run]]
          if (length(run) > 1) graphics::polygon(
            c(xx, rev(xx)), c(prediction$lower[run], rev(prediction$upper[run])),
            col = grDevices::adjustcolor(colors[i], alpha.f = .18), border = NA)
          else graphics::segments(xx, prediction$lower[run], xx, prediction$upper[run],
                                   col = colors[i], lwd = 1.5)
        }
      }
    }
    for (i in seq_along(names)) {
      selected <- which(prediction$model == names[i] & prediction$row %in% rows & is.finite(value))
      groups <- split(selected, prediction$focal_bin[selected])
      for (g in groups) {
        g <- g[order(xs[prediction$row[g]])]
        if (length(g) > 1) graphics::lines(xs[prediction$row[g]], value[g], col = colors[i], lwd = 2, lty = line_types[i])
        else graphics::points(xs[prediction$row[g]], value[g], col = colors[i], pch = 16)
      }
    }
    if (identical(panel, panels[1])) graphics::legend("topleft", legend = names,
      col = colors, lty = line_types, lwd = 2, cex = .7, bty = "n")
  }
  graphics::mtext(main, side = 3, outer = TRUE, line = .4)
  caption <- if (type == "differences") "Prediction differences; uncertainty for differences is not estimated."
    else if (show_confidence) paste0(format(100 * level, trim = TRUE),
      "% pointwise confidence intervals for the mean; model-based, fixed model settings.")
    else "Conditional predictions with controls held fixed; confidence bands hidden."
  graphics::mtext(caption,
                  side = 1, outer = TRUE, line = .4, cex = .75)
  invisible(x)
}
