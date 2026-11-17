# Assess only fitted results, never dataset names or known simulation truths.
# No universal threshold for a practically important prediction change is used.
assess_model_comparison <- function(object) {
  fmt <- function(value) format(signif(value, 4), trim = TRUE)
  compare <- function(value, reference, label) {
    if (!is.finite(value) || !is.finite(reference))
      return(paste0(label, " comparison is unavailable"))
    delta <- value - reference
    if (!is.finite(delta)) return(paste0(label, " difference is not numerically representable"))
    if (abs(delta) < 1e-8) return(paste0(label, " is effectively unchanged"))
    paste(label, "is", fmt(abs(delta)), if (delta < 0) "lower" else "higher")
  }
  table <- object$comparison
  baseline <- table[table$model == "continuous", , drop = FALSE]
  pred <- object$predictions
  scale <- if (object$model == "logit") 100 else 1
  units <- if (object$model == "logit") "percentage points" else "outcome units"
  out <- c(sprintf(
    "All models use the same %s observations; %s rows were excluded for missing or nonfinite inputs. This assessment compares your supplied schemes with the continuous benchmark and does not choose bins for you.",
    object$sample$n_used, nrow(object$sample$excluded)),
    if (object$interaction)
      sprintf("The models include an interaction between '%s' and '%s'. Read the panels together because the relationship with one variable can depend on the other.", object$variables[1], object$variables[2])
    else "These models have no interaction. Comparisons concern the representation of the focal predictors with additive controls, if supplied.")
  # End-to-end changes are explicitly limited to the displayed range and grid.
  if (baseline$status == "ok") {
    panels <- object$grid$moderator_values
    changes <- vapply(panels, function(panel) {
      rows <- if (length(object$variables) == 1L) seq_len(nrow(object$grid$data)) else
        which(object$grid$data[[object$variables[2]]] == panel)
      rows <- rows[order(object$grid$data[[object$variables[1]]][rows])]
      endpoints <- rows[c(1L, length(rows))]
      part <- pred[pred$model == "continuous", ]
      values <- part$estimate[match(endpoints, part$row)]
      delta <- diff(values) * scale
      description <- if (!is.finite(delta)) "an unavailable change" else
        if (abs(delta) < 1e-8) "essentially no change" else
          paste("a", if (delta < 0) "decrease of" else "rise of", fmt(abs(delta)), units)
      if (length(object$variables) == 2L)
        paste0(description, " at ", object$variables[2], " = ", fmt(panel)) else description
    }, character(1))
    limits <- range(object$grid$data[[object$variables[1]]])
    out <- c(out, sprintf(
      "For the continuous benchmark, moving '%s' from %s to %s gives %s. These are end-to-end predicted changes on the displayed grid, not per-unit effects or causal estimates; controls stay fixed.",
      object$variables[1], fmt(limits[1]), fmt(limits[2]), paste(changes, collapse = "; ")))
  } else out <- c(out, "The continuous benchmark is flagged or failed. Relative fit and prediction comparisons are unavailable; inspect $issues before interpreting the alternatives.")

  for (name in table$model[table$model != "continuous"]) {
    candidate <- table[table$model == name, , drop = FALSE]
    is_spline <- identical(name, "spline") && !is.null(object$spline_df)
    text <- if (is_spline) paste0("Natural spline comparator (", object$spline_df,
      " degrees of freedom per focal variable). ") else paste0("Scheme '", name, "'. ")
    if (candidate$status != "ok") {
      text <- paste0(text, "This fit is flagged or failed, so its fit metrics and predictions are withheld. Inspect $issues; this is not evidence that its representation is better or worse.")
    } else if (baseline$status == "ok") {
      text <- paste0(text, "Compared with continuous, ",
        compare(candidate$AIC, baseline$AIC, "AIC"), "; ",
        compare(candidate$BIC, baseline$BIC, "BIC"), "; ",
        compare(candidate$training_score, baseline$training_score, object$score_name), ".")
      deltas <- c(candidate$AIC - baseline$AIC, candidate$BIC - baseline$BIC)
      if (all(is.finite(deltas)) && prod(sign(deltas)) < 0 && all(abs(deltas) >= 1e-8))
        text <- paste0(text, " AIC and BIC favor different representations, so the fit criteria do not give a consistent ordering.")
      paired <- pred[pred$model == name & is.finite(pred$difference), , drop = FALSE]
      total <- nrow(object$grid$data)
      if (nrow(paired)) {
        largest <- which.max(abs(paired$difference))
        distance <- abs(paired$difference[largest]) * scale
        at <- object$grid$data[paired$row[largest], object$variables, drop = FALSE]
        location <- paste(vapply(object$variables, function(v) {
          cuts <- unique(unlist(lapply(object$schemes, function(s) s[[v]]$breaks)))
          tolerance <- max(abs(object$analysis_data[[v]])) * .Machine$double.eps * 16
          just_below <- cuts[is.finite(cuts) & cuts < at[[v]] & at[[v]] - cuts <= tolerance]
          if (length(just_below)) paste(v, "just above", fmt(max(just_below)))
          else paste(v, "=", fmt(at[[v]]))
        }, character(1)), collapse = ", ")
        text <- paste0(text, " The largest absolute prediction difference on the default grid is ",
          fmt(distance), " ", units, " (", location, "; ", nrow(paired), " of ", total,
          " grid rows comparable). Decide whether that change matters for your application.")
      } else text <- paste0(text, " No paired predictions are available on the default grid.")
    } else text <- paste0(text, "The candidate fit is available, but comparisons with the continuous benchmark are unavailable.")
    if (is_spline) {
      out <- c(out, paste0(text, " This model retains numeric variation within the fitted curves; it has no bin-group counts. Inspect joint support and validate predictions."))
      next
    }
    support <- object$support[[name]]
    text <- paste0(text, " Its bin groups contain ", min(support$n), " to ", max(support$n),
      " observations; ", sum(support$n == 0), " groups are empty and ", sum(support$sparse),
      " fall below your count screen (n < ", object$min_count, ").")
    if (any(support$n == 0 | support$sparse))
      text <- paste0(text, " Inspect these groups before relying on their estimates; combining them requires a substantive reason.")
    if (object$model == "logit") text <- paste0(text,
      " The smallest event and non-event counts are ", min(support$events), " and ",
      min(support$non_events), " respectively, possibly in different groups. Small counts can destabilize logit estimates.")
    out <- c(out, text)
  }
  if (!is.null(object$spline_df)) {
    sp <- table[table$model == "spline", , drop = FALSE]
    out <- c(out, "The spline is a prespecified flexible continuous alternative, not an automatically tuned model. Beating the linear reference alone does not demonstrate an advantage over the spline.")
    for (name in setdiff(table$model, c("continuous", "spline"))) {
      candidate <- table[table$model == name, , drop = FALSE]
      if (sp$status == "ok" && candidate$status == "ok")
        out <- c(out, paste0("Scheme '", name, "' compared with spline: ",
          compare(candidate$AIC, sp$AIC, "AIC"), "; ",
          compare(candidate$BIC, sp$BIC, "BIC"), "; ",
          compare(candidate$training_score, sp$training_score, object$score_name),
          ". These are in-sample comparisons; use bin_validate for held-out prediction."))
    }
  }
  c(out,
    "Lower AIC or BIC indicates a better fit-complexity tradeoff within this common sample and model family. Lower training error describes these fitted data only. These comparisons are not significance tests or validation on new observations.",
    "Binning treats values within a category alike, holding other predictors fixed. Consider it when the boundaries have a defensible meaning and the lost within-bin distinctions are acceptable. Lower AIC alone cannot establish a real threshold. A smooth nonlinear alternative may improve on the linear-term reference; any supplied spline is one such alternative, not every possible continuous model.",
    "Prediction differences refer only to the displayed grid with controls fixed, not the whole sample or every possible covariate combination. Group counts describe rows, not independent units, and do not establish joint support for continuous predictors.",
    "Confidence bands are pointwise and model-based with fixed model settings, including cutpoints and spline complexity. Their width or overlap does not validate the model shape or test differences between models. Bin selection and clustered dependence are not accounted for. No scheme is selected automatically.")
}
