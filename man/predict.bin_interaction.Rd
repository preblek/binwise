\name{predict.bin_interaction}
\alias{predict.bin_interaction}
\title{Compare Model Predictions at the Same Covariate Values}
\description{Apply each fitted model and its saved bin definitions to common
new-data rows. Original data units must be preserved.}
\usage{
\method{predict}{bin_interaction}(object, newdata = object$grid$data,
  warn = TRUE, interval = c("confidence", "none"),
  level = if (is.null(object$level)) 0.95 else object$level, ...)
}
\arguments{
\item{object}{An object returned by \code{bin_interact()}.}
\item{newdata}{A data frame containing each focal variable and control once.
The outcome is not required. Extra columns are ignored. Defaults to the stored grid.}
\item{warn}{Warn if any prediction or requested interval is unavailable. Default TRUE.}
\item{interval}{Pointwise confidence intervals for the predicted mean by default.
Use \code{"none"} for point estimates only.}
\item{level}{Confidence level strictly between 0 and 1. Defaults to the saved
level, or 0.95 for objects created before confidence intervals were introduced.}
\item{...}{Reserved. Supplying additional arguments is an error.}
}
\details{
For an optional spline comparator, predictions reuse the knots and boundary
knots saved during model fitting. They are not recalculated from newdata.
Confidence intervals condition on the specified spline degrees of freedom;
they do not account for selecting or tuning that complexity. Natural splines
are linear outside boundary knots on the model link scale. Extrapolation is
flagged and is not evidence of adequate support.


Each original covariate row is passed to the continuous model and separately
mapped to each candidate's saved categories. Predictors are not rescaled and
bins are not recalculated. Missing or nonfinite inputs, uncovered bin values,
unobserved factor levels, and unavailable fits produce missing predictions with
an explicit status. Rows are retained. A numeric value beyond its training range
sets extrapolated to TRUE, even when the continuous model supplies a prediction.
This flag checks individual numeric ranges, not joint support or causal validity.

Linear models use \code{stats::predict.lm} confidence intervals with the residual
degrees of freedom and a t critical value. These use the ordinary model
covariance, assuming independent errors with constant variance; exact finite
sample coverage also assumes normal errors and the specified mean model.
Logistic models use approximate normal Wald intervals on the logit scale,
then transform both limits to probabilities with the inverse link. They use
the ordinary GLM covariance and assume independent observations and a correctly
specified model. Limits remain on the probability scale without symmetric
clipping. Sparse data can make these approximations unreliable.

These are pointwise intervals for a conditional mean or probability, not
simultaneous bands or intervals for individual future outcomes. They condition
on the selected model, cutpoints, and covariate values. They do not account for
bin selection, cutpoint estimation, clustering, or other dependence. A narrow
interval does not establish that the functional form is correct. Failed or
flagged fits have no intervals. Numerically unavailable limits are marked
explicitly and do not discard otherwise usable point estimates.

Differences subtract the continuous benchmark at the identical original row.
For logit these are probability differences, not log-odds or odds ratios.
Multiply by 100 to express them as percentage-point differences. If the benchmark
prediction is missing, its paired differences are also missing. No confidence
interval or uncertainty estimate for a difference is calculated.
}
\value{A long data frame with model, row (the newdata row position), estimate,
status, extrapolated, focal_bin (the first focal variable's category or
\code{"continuous"}), lower, upper, interval_status, and difference.
The lower and upper columns are confidence limits for estimate, not difference.
They are NA when unavailable or not requested. interval_status is ok,
not_requested, prediction_unavailable, or nonfinite_interval. Attributes interval,
level, and interval_method record the interval settings and method.
Prediction status is ok,
missing_or_nonfinite_predictor, uncovered_by_scheme, unobserved_category,
model_unavailable, or nonfinite_prediction.}
\seealso{\code{\link{bin_interact}}, \code{\link[stats]{predict.lm}},
\code{\link[stats]{predict.glm}}}
