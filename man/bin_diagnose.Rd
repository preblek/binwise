\name{bin_diagnose}
\alias{bin_diagnose}
\title{Inspect a Numeric Variable Before Binning}
\description{
Summarize a numeric column and inspect one candidate binning scheme.
No outcome variable or model is required. The input data are not modified.
}
\usage{
bin_diagnose(data, variable, bins = 4L,
  method = c("quantile", "equal_width", "custom"),
  breaks = NULL, separate_zero = FALSE,
  min_count = 20L, min_prop = 0.01, warn = TRUE)
}
\arguments{
\item{data}{A data frame, including data-frame subclasses.}
\item{variable}{One quoted column name. The column must be a plain numeric or
integer vector, without a class or dimensions. Convert factors, dates, or labelled
values explicitly after deciding their intended numeric meaning.}
\item{bins}{Requested number of interval bins, an integer from 2 to 1000.
Ignored for custom boundaries. With \code{separate_zero = TRUE}, this is the
number of positive-value intervals, in addition to a zero category if present.}
\item{method}{\code{"quantile"} uses equally spaced probabilities and R's
type-7 sample quantiles. \code{"equal_width"} divides the observed range evenly.
\code{"custom"} uses supplied numeric boundaries.}
\item{breaks}{For custom bins only, at least two finite, strictly increasing
numeric boundaries covering the finite values to be binned. Include endpoints.
When zeros are separate, boundaries only need to cover positive values.}
\item{separate_zero}{Create a category for exact zeros before computing
boundaries from positive values. Requires nonnegative data. No rounding or
near-zero tolerance is applied. No zero category is added if no zeros occur.}
\item{min_count}{Flag bins with fewer than this many observations. Default 20.}
\item{min_prop}{Flag bins with a smaller proportion of all finite observations
than this value. Default 0.01. A bin is sparse when either rule is triggered.}
\item{warn}{Emit diagnostic notes as warnings. With \code{FALSE}, notes are still
stored and displayed by the print method. Invalid inputs always produce errors.}
}
\details{
Missing values (including NaN) and infinite values are counted separately and
excluded from all distribution summaries and candidate bins. At least one finite
value is required. Bin proportions use all finite values as their denominator,
including zeros when a separate zero category is used.

Identical values are never split across bins. Repeated candidate boundaries are
collapsed, so quantile bins can be unequal in size or fewer than requested.
Interpolated quantiles can also leave empty intervals when unique values are few.
Empty intervals are retained. A constant variable produces one singleton bin for
automatic methods. Custom boundaries are retained even for constant data.

Intervals are right-closed, with the first interval also left-closed. Zero
assignment happens first when requested. Full-precision boundaries are returned;
printed values may be rounded. Bin identifiers remain unique even when displayed
boundaries look identical.

Skewness is the population-moment descriptive statistic
\eqn{m_3 / m_2^{3/2}}, not a bias-corrected estimator or a significance test.
It is undefined (NA) for constant variables. Overall and within-bin standard
deviations use the sample definition, and are NA for single observations.

Sparse-bin rules are adjustable descriptive screens. Counts of dyad-year rows
are not counts of independent cases. This function does not choose an optimal
binning scheme, test zero inflation, fit models, quantify information loss, or
establish predictive or causal validity. Boundaries describe the supplied sample;
use \code{\link{bin_apply}} to apply the saved boundaries to other observations
without recalculating them.

The plain-language assessment reports zero prevalence, collapsed boundaries,
the largest interval's share, the widest observed within-bin range, and empty
or sparse categories. The interval balance calculation excludes a dedicated
zero category; the range percentage uses the full finite sample's observed range.
A largest interval share of at least 80 percent triggers a suggestion to compare
schemes if balanced groups matter. This is a descriptive prompt, not a test or
an optimality rule. Substantive meanings of values are supplied by the user.
}
\value{
A \code{bin_diagnosis} list with the following components.
\item{summary}{One-row data frame containing counts, zero proportion, unique
values, range, mean, median, standard deviation, IQR, and skewness.}
\item{percentiles}{Probabilities and corresponding type-7 quantiles.}
\item{bins}{Bin identifiers, boundaries and closure indicators, singleton
indicator, counts, proportions, sparse flags, and observed within-bin summaries.
Empty bins have NA observed summaries.}
\item{assignments}{Factor aligned to the original rows, with NA for excluded values.}
\item{candidate_breaks, breaks}{Boundaries before and after deduplication,
respectively. A separate zero category is not included in these vectors.}
\item{requested_bins, actual_interval_bins, occupied_bins}{Requested interval
count, retained interval count, and occupied categories including a separate zero.}
\item{notes}{Human-readable diagnostic notes.}
\item{assessment}{Character vector of evidence-based descriptive guidance,
displayed before the detailed tables by the print method.}
\item{values}{Original numeric vector, used for plotting.}
\item{call, variable, method, separate_zero, min_count, min_prop}{Call and settings.}
}
\examples{
example_data <- data.frame(x = c(rep(0, 40), seq(0.01, 1, length.out = 60), NA))
d <- bin_diagnose(example_data, "x", bins = 4, warn = FALSE)
d
plot(d)
plot(d, type = "bins")
positive <- bin_diagnose(example_data, "x", bins = 3,
                         separate_zero = TRUE, warn = FALSE)
positive$bins
custom <- bin_diagnose(example_data, "x", method = "custom",
                       breaks = c(0, 0.1, 0.5, 1), warn = FALSE)
custom$assignments
}
\seealso{\code{\link{bin_compare}}, \code{\link{bin_apply}}, \code{\link{plot.bin_diagnosis}}, \code{\link[stats]{quantile}}}

