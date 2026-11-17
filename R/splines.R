# Natural spline terms are fitted through public R formula APIs. predict.lm/glm
# reuse the knots stored by makepredictcall.ns rather than learning on newdata.
validate_spline_df <- function(spline_df) {
  if (!is.null(spline_df) &&
      (!is.numeric(spline_df) || is.object(spline_df) || length(spline_df) != 1L ||
       !is.finite(spline_df) || spline_df < 2 || spline_df != floor(spline_df) ||
       spline_df > .Machine$integer.max))
    stop("`spline_df` must be NULL or one whole number of at least 2.", call. = FALSE)
  invisible(spline_df)
}

comparison_formula <- function(outcome, variables, controls, interaction, spline_df = NULL) {
  terms <- lapply(variables, function(v) {
    if (is.null(spline_df)) return(as.name(v))
    term <- quote(splines::ns(x, df = 3))
    term[[2L]] <- as.name(v)
    term$df <- as.integer(spline_df)
    term
  })
  rhs <- terms[[1L]]
  if (length(terms) == 2L) rhs <- call(if (interaction) "*" else "+", rhs, terms[[2L]])
  for (v in controls) rhs <- call("+", rhs, as.name(v))
  stats::as.formula(call("~", as.name(outcome), rhs), env = baseenv())
}

fitted_spline_details <- function(fit, variables) {
  bases <- fit$model[vapply(fit$model, inherits, logical(1), what = "ns")]
  stats::setNames(lapply(bases, function(b) list(
    df = ncol(b), knots = attr(b, "knots"),
    boundary_knots = attr(b, "Boundary.knots"), training_n = nrow(b)
  )), variables)
}
