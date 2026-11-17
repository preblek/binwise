\name{bin_interact}
\alias{bin_interact}
\title{Compare Continuous and Binned Predictors in Regression Models}
\description{Fit a continuous benchmark and explicitly chosen binned alternatives
on one common sample. Compare conditional predictions, model fit, and bin support
with an optional two-variable interaction. A plain-language assessment describes
the fitted comparisons without selecting a best scheme.}
\usage{
bin_interact(data, outcome, variables, schemes,
  interaction = length(variables) == 2L,
  model = c("linear", "logit"), controls = NULL,
  min_count = 20L, warn = TRUE, level = 0.95,
  spline_df = NULL)
}
\arguments{
\item{data}{A data frame. It is not modified.}
\item{outcome}{One quoted name of a plain numeric or integer outcome column.
For logit, the common sample must contain both 0 and 1 and no other outcome values.}
\item{variables}{One or two distinct quoted names of plain numeric focal columns.
The first is the horizontal axis in default plots. The second, when present,
supplies the conditioning values for plot panels. Signed values are supported.}
\item{schemes}{A named list of candidates, or a \code{bin_comparison} whose
diagnoses become candidates. Each candidate is either one \code{bin_diagnosis}
or a named list mapping one or both focal variables to diagnoses. Every diagnosis
must name the variable to which it applies. Unspecified focal variables remain
continuous. Use an empty list to compare only the linear and spline models
when \code{spline_df} is supplied. Names must be unique and nonempty; \code{"continuous"} is reserved.}
\item{interaction}{With two focal variables, TRUE includes both main effects
and their product in every model. FALSE includes additive main effects only.
Defaults to TRUE for two variables and FALSE for one.}
\item{model}{\code{"linear"} fits ordinary least squares with \code{stats::lm}.
\code{"logit"} fits a binary logistic model with \code{stats::glm}.}
\item{controls}{Optional quoted names of distinct additive controls, separate
from the outcome and focal variables. Plain numeric, factor, character, and
logical columns are supported. Categorical controls become unordered factors.
Use factors when numeric codes represent categories. No automatic transformations
are applied. NULL or \code{character(0)} means no controls.}
\item{min_count}{Flag binned groups or combinations with fewer observations than
this nonnegative integer. Default 20. This is a descriptive count screen.}
\item{warn}{Warn if model inputs exclude rows or a fit is flagged or fails.
Stored diagnostics remain available with FALSE. Invalid specifications error.}
\item{spline_df}{NULL (default) preserves the original comparison. An integer
of at least 2 adds a natural cubic spline comparator named \code{spline}, with
that many degrees of freedom for each focal variable. That name is then reserved.
Three is a modest starting choice, not a universally optimal value. Controls
retain their original additive form. The same value is used for both focal
variables; when interaction is TRUE, all products of their spline basis terms
are included along with their main effects.}
\item{level}{Confidence level strictly between 0 and 1. Default 0.95.
Used for stored predictions and as the default for later predictions and plots.}
}
\details{
\strong{Assessment.} The printed assessment and \code{$assessment} describe the
common sample, continuous benchmark's end-to-end prediction changes at the
displayed moderator values, each candidate's AIC/BIC and training-score changes,
largest absolute paired prediction difference on the default grid, and group
support. Logit prediction changes are expressed in percentage points, while
linear-model changes use outcome units. Locations of maximum differences use
the first tied grid location. These maxima depend on the grid and fixed controls;
they are not whole-sample averages or maxima over every possible covariate value.

The assessment uses no dataset-specific knowledge, significance test, threshold
for practical importance, or automatic choice of bins. AIC and BIC comparisons
account for model complexity within the same family, outcome, and sample.
Differences smaller than 1e-8 in absolute magnitude are described as effectively
unchanged for display. Unavailable fits and metrics are identified explicitly.
Lower information criteria alone do not establish a real threshold or improvement
over a flexible continuous model. The assessment is stored at creation time;
rerun \code{bin_interact} after changing inputs rather than editing the result.

\strong{Common observations and fixed schemes.} Rows with missing model inputs
or nonfinite numeric model inputs are excluded once, using all specified columns.
Every model uses the remaining rows and the same outcome. \code{bin_apply}
assigns categories using the saved cutpoints and zero rule. A scheme that fails
to cover any finite focal value in the common sample stops the call. Schemes
cannot silently select different samples, and their cutpoints are not recomputed.
Their original diagnostic counts may differ from the new estimation-sample counts.

The continuous benchmark uses linear terms on the model's link scale and their
product when requested. The optional \code{spline} model uses
\code{splines::ns} for each focal variable. Knots are learned from the common
estimation sample and stored in the fitted model for reuse in prediction.
Natural splines are linear beyond their boundary knots on the model link scale.
For two focal variables with an interaction and d degrees of freedom each,
the spline contributes 2d + d*d terms, plus an intercept and controls. Sparse
support or excessive complexity can make this model unstable or rank deficient;
failed fits are reported without silently reducing the degrees of freedom.
Spline complexity is prespecified, not selected using the outcome. A spline
smooths across zero and does not automatically supply a distinct zero state.
Factor interactions include all relevant main effects. The function accepts no
arbitrary formula terms, weights, offsets, or clustered covariance specification.
Ordinary model objects are retained for inspection. This draft does not reproduce
specialized fixed-effects estimators or the paper's clustered inference.

\strong{Model comparison.} AIC and BIC are calculated within this common sample
and outcome using the same model family. Delta AIC subtracts the continuous
benchmark's AIC. Do not compare AIC between separate calls with different samples
or outcomes. Parameter counts include fitted regression rank and, for linear
models, the residual variance parameter. Training RMSE is the square root of
mean squared outcome error. Training Brier score is mean squared probability
error for the 0/1 outcome. These scores use the fitting data and are not
cross-validation or evidence of out-of-sample improvement. Coefficients for
continuous and categorical terms have different interpretations.

\strong{Fit problems.} Failed, rank-deficient, nonconverged, numerically flagged,
or separated fits are recorded rather than silently dropped. Their comparison
metrics and predictions are withheld. Separation checks identify strictly
separating fitted logit scores, pure-outcome categorical levels, and pure-outcome
joint factor cells when the interaction supplies those cell terms. These are
sufficient checks, not an exhaustive separation diagnostic. The absence of a
flag does not establish stability. Nonfinite information criteria, such as an
exact linear fit with zero residual variance, are set to NA; otherwise usable
point predictions remain available in that case.

\strong{Support.} Each binned candidate has a table of observed counts for its
categories or category combinations, retaining empty cells. Logit tables also
include events and non-events. With only one binned variable, these are marginal
counts over the other variable. They do not establish joint support throughout a
continuous predictor surface. No bin-support table is supplied for either fully
continuous comparator. Counts describe rows, not independent units. Candidates
with more than 10,000 possible bin combinations are rejected.

\strong{Default predictions.} The first focal variable spans its observed range
on a 100-point grid augmented with candidate cutpoints and just-above-cutpoint
values. A second focal variable is fixed at its unique 25th, 50th, and 75th
percentiles. Numeric controls are held at medians; categorical controls at their
most frequent level, with ties resolved by factor level order. These values and
the full grid are returned. Every model is evaluated at the same original-unit
grid, then mapped to its own saved categories. Unsupported categories leave
predictions missing. These are conditional predictions, not average marginal
effects or causal effects. Pointwise confidence intervals for the mean are
included by default. They condition on the model and fixed cutpoints and do not
account for choosing bins after examining the data. See
\code{\link{predict.bin_interaction}} for assumptions. The grid can include sparse
or unsupported joint combinations even within each variable's observed range.
Use \code{predict(result, newdata)} to choose other covariate values.
}
\value{
A \code{bin_interaction} list containing:
\item{comparison}{One row per requested model, including the continuous
benchmark. Columns are model, n, parameters, AIC, BIC, training_score, status,
and delta_AIC. Status is ok, warning, or failed.}
\item{spline_df, spline_formula, spline_details}{Optional spline complexity,
formula, and per-variable degrees of freedom, interior knots, boundary knots,
and training count. NULL when no spline is requested.}
\item{models}{Named ordinary lm or glm objects; NULL when fitting fails.}
\item{schemes}{Saved candidate definitions, with empty specifications for the fully continuous comparators.}
\item{issues}{Named lists of diagnostic messages for each model.}
\item{support}{Binned-group count tables. Columns identify variable_1, bin_1,
variable_2, bin_2, n, and sparse; logit adds events and non_events.}
\item{sample}{Original total and common-sample counts, original row positions
used, and excluded row positions with the offending column names.}
\item{analysis_data}{The common-sample outcome and original predictors, before binning.}
\item{grid}{Default prediction data, moderator values, and fixed control values.}
\item{predictions}{Common-grid predictions, confidence limits, and differences. See
\code{\link{predict.bin_interaction}} for columns.}
\item{level}{The default confidence level.}
\item{min_count}{The count screen used for model-sample support tables.}
\item{assessment}{A character vector of plain-language interpretation, printed
with the result. It describes comparisons and limitations without choosing bins.}
\item{notes}{Plain-language interpretation and limitations.}
\item{call, outcome, variables, controls, interaction, model, formula, score_name}{
Call, settings, linear-term formula, and the training-score label.}
}
\examples{
data(binwise_smooth)
choices <- bin_compare(binwise_smooth, "exposure", bins = 4)
result <- bin_interact(binwise_smooth, "outcome", "exposure", choices, spline_df = 3)
result
plot(result)

data(binwise_interaction)
x_bins <- bin_diagnose(binwise_interaction, "x", bins = 3, warn = FALSE)
z_bins <- bin_diagnose(binwise_interaction, "z", bins = 3, warn = FALSE)
result <- bin_interact(binwise_interaction, "outcome", c("x", "z"),
  schemes = list(x_only = x_bins, both = list(x = x_bins, z = z_bins)),
  controls = "control")
result$support$both
predict(result, data.frame(x = c(-1, 1), z = 0, control = 0))
}
\seealso{\code{\link{bin_apply}}, \code{\link{bin_compare}},
\code{\link{predict.bin_interaction}}, \code{\link{plot.bin_interaction}},
\code{\link[stats]{lm}}, \code{\link[stats]{glm}}, \code{\link[stats]{AIC}}}
