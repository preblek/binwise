\name{bin_validate}
\alias{bin_validate}
\title{Compare Prediction Workflows on Held-Out Observations}
\description{Refit continuous and categorical alternatives inside common folds,
learn quantile boundaries on training observations only, and compare paired
prediction losses. An advanced callback supports other estimators.}
\usage{
bin_validate(data, outcome, variables = NULL, schemes = NULL,
  controls = NULL, interaction = length(variables) == 2L,
  model = c("linear", "logit"), folds = 5L, group = NULL,
  fold_id = NULL, cluster = group, seed = 1L,
  fit_predict = NULL, reps = 499L, level = .95,
  spline_df = NULL, reference = NULL)
}
\arguments{
\item{data}{Data frame. Original rows and columns are not modified.}
\item{outcome}{Quoted numeric outcome column. Logit requires values zero and one.}
\item{variables}{One or two quoted numeric focal columns for the built-in lm/glm workflow.}
\item{schemes}{Named candidates. Each maps focal column names to recipe lists.
A recipe supplies \code{probs} starting at zero and ending at one, or prespecified
\code{breaks}, and optional \code{separate_zero = TRUE}. Full-data
\code{bin_diagnosis} objects are deliberately not accepted. When spline_df is
supplied, NULL or an empty list compares only the linear and spline models.
The names continuous and, when enabled, spline are reserved.}
\item{controls}{Optional quoted additive control columns for the built-in workflow.}
\item{interaction}{Include the two focal variables' main effects and product.
Requires two focal variables when TRUE.}
\item{model}{\code{"linear"} reports RMSE and MAE; \code{"logit"} reports Brier
score and log loss. Also selects lm or binomial glm for the built-in workflow.}
\item{folds}{Number of folds, at least two. Assignment balances numbers of groups,
not numbers of rows. Ignored when \code{fold_id} supplies the folds.}
\item{group}{Optional column identifying units that must remain together.
For dyadic data, construct an unordered country-pair identifier if both directions
should remain together.}
\item{fold_id}{Optional nonmissing fold label for every original row. At least
two populated folds are needed. Each fold is tested using all other folds for
training. This is not a forward-only temporal validation interface.}
\item{cluster}{Zero, one, or two quoted columns used to resample paired held-out
losses for score intervals. Defaults to \code{group}. NULL resamples rows.
Clustering here does not change how a model is fitted.}
\item{seed}{Nonnegative integer controlling folds and bootstrap draws. The
call restores the user's random-number state.}
\item{fit_predict}{Advanced function taking \code{train} and \code{test} data
frames. Return a numeric matrix/data frame with one prediction per test row and
at least two named model columns. The first model is the default reference. Alternatively
return a list with \code{predictions}, optional \code{details}, and optional
\code{messages} named by model. Names and order must agree across folds.
The callback owns training-only preprocessing, model fitting, and failure
diagnostics. Retain test-row order and use NA for unavailable predictions.
The automatic focal/recipe/control arguments are unused with this callback.}
\item{reps}{Number of paired bootstrap draws, zero to omit intervals or at least 99.
Default 499. Increase for more stable interval endpoints.}
\item{level}{Confidence level strictly between zero and one.}
\item{spline_df}{NULL or an integer of at least 2. Adds a natural cubic spline
comparator with this many degrees of freedom per focal variable. With two focal
variables, interaction = TRUE includes all products of their spline basis terms
plus main effects; FALSE uses additive splines. Controls are unchanged. Knots
and boundary knots are estimated only on each training fold and reused for
its test observations. Cannot be combined with fit_predict; that callback must
implement its own spline if desired.}
\item{reference}{NULL uses the first returned model (continuous in the built-in
workflow). A model name, such as spline, selects the reference for paired losses
and validation plots. All scores still use the same common observations.}
}
\details{
\strong{Training-only recipes.} Quantiles are recalculated within each training
fold. Tied cutpoints are collapsed; the first and last interval extend to
negative and positive infinity so an unseen extreme receives a tail category.
Prespecified breaks are honored without extension. Missing categories in test
data produce unavailable predictions; uncovered training values stop that fit.
Separate-zero recipes require nonnegative values and learn quantiles from
positive training values. The built-in models use linear terms or factors,
optional two-variable interactions, additive controls, and an optional natural
cubic spline comparator. Spline degrees of freedom are fixed before validation;
they are not tuned automatically. Natural splines extrapolate linearly beyond
the training boundary knots on the link scale. Per-fold spline definitions are
retained under \code{$fold_details[[k]]$spline}. More complex continuous models
can be supplied through a callback. Using many spline terms with sparse support
may produce fitting failures; the requested complexity is never silently reduced.

\strong{Common scoring.} Every model is evaluated on the intersection of rows
with finite predictions from every candidate. For logit, predictions outside
[0,1] are unavailable. Coverage and per-fold failures are reported. Log loss
clips probabilities at 1e-15 and 1-1e-15 solely to avoid infinite logarithms.
Losses average over observations, not equally over groups. Folds are never
selected according to their outcomes. Built-in missing model inputs are excluded
once; callbacks receive all rows with finite outcomes and handle their own inputs.

\strong{Intervals.} The same bootstrap weights apply to all candidate losses,
allowing paired differences. One clustering column resamples whole clusters.
Two columns independently resample each dimension and multiply their weights,
the pigeonhole bootstrap. Repeated rows within a cluster intersection remain
together. Percentile intervals use the requested confidence level. Predictions
and fold assignments are held fixed: these intervals describe evaluation-sample
uncertainty, not the full uncertainty from refitting models, learning cutpoints,
or choosing splits. This approximation can be conservative for crossed data.
They are not confidence intervals for individual predictions or causal effects.

No best binning scheme is selected automatically. A positive prediction gain
does not establish a true threshold. Selecting and reporting a winner using the
same cross-validation results requires nested validation or a new untouched test
set for an unbiased final evaluation. Grouped folds are not future-year forecasts
and do not ensure independence across groups sharing another unit.
}
\value{A \code{bin_validation} list with \code{scores}, paired
\code{differences}, \code{fold_scores}, out-of-fold \code{predictions},
\code{fold_details}, \code{diagnostics}, \code{excluded_rows}, and a
plain-language \code{assessment}. Negative loss differences favor the candidate
over the selected reference. Settings include \code{reference} and \code{spline_df}.}
\references{Owen, A. B. (2007). The pigeonhole bootstrap. The Annals of Applied
Statistics 1(2), 386--411. \url{https://arxiv.org/abs/0712.1111}}
\examples{
set.seed(12)
d <- data.frame(x = runif(400, -2, 2))
d$y <- ifelse(d$x <= 0, -1, 1) + rnorm(400, sd = .5)
v <- bin_validate(d, "y", "x", model = "linear", reps = 0, spline_df = 3,
  reference = "spline",
  schemes = list(boundary_zero = list(x = list(breaks = c(-Inf, 0, Inf)))))
print(v)
}
\seealso{\code{\link{bin_diagnose}}, \code{\link{bin_apply}}, \code{\link{bin_interact}}}
