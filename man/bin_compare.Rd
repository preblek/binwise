\name{bin_compare}
\alias{bin_compare}
\title{Compare Candidate Binning Schemes}
\description{Compare category counts, cutpoints, and observed within-category
spread across several schemes applied to the same numeric column.}
\usage{
bin_compare(data, variable, bins = 4L,
  methods = c("quantile", "equal_width"),
  custom_breaks = NULL, separate_zero = FALSE,
  min_count = 20L, min_prop = 0.01)
}
\arguments{
\item{data}{A data frame.}
\item{variable}{One quoted column name identifying a plain numeric or integer vector.}
\item{bins}{Unique whole numbers from 2 to 1000. Each requested count is paired
with each automatic method. Ignored for custom schemes.}
\item{methods}{Unique choices from \code{c("quantile", "equal_width")}.
Use \code{character(0)} to compare only custom schemes.}
\item{custom_breaks}{An optional named list of numeric boundary vectors.
Each vector must be finite, strictly increasing, and cover the values to be
binned, including endpoints. Names must be unique, nonempty, and different
from automatic names such as \code{"quantile_4"}.}
\item{separate_zero}{Use a separate exact-zero category in every scheme.
Requires nonnegative data. Requested interval counts exclude this category.}
\item{min_count, min_prop}{Sparse-bin screens shared by every scheme. A category
is flagged if its count is less than \code{min_count} or its proportion of all
finite rows is less than \code{min_prop}. Defaults are 20 and 0.01.}
}
\details{
Each scheme uses \code{\link{bin_diagnose}}. Missing and infinite values are
excluded consistently. Ties remain together, duplicate automatic boundaries
collapse, and empty bins are retained. Intervals include their upper boundary;
the first includes its lower boundary. If requested, zeros are assigned first.

The comparison records requested and retained interval counts separately from
the additional zero category. Size summaries include empty bins and the zero
category. Percentages use all finite rows. The widest observed within-bin range
is the largest difference between observed extrema in a nonempty category,
not the width between its nominal cutpoints. It is measured in the original
variable's units. Empty bins are excluded from this range calculation. Ties for
widest range select the first category. An extreme range difference can overflow
to Inf even though both observed endpoints are finite.

Small tail groups may be intentional. Observed ranges describe retained
within-category variation; they are not information-loss scores or tests.
No best scheme is automatically selected. Per-scheme diagnostic notes are
stored without emitting warnings; invalid inputs stop the whole comparison
with an error naming the scheme. No partially evaluated comparison is returned.
The original data are not modified. Schemes and assignments are kept in memory,
so large data and many requested schemes can require substantial memory.
}
\value{
A \code{bin_comparison} list.
\item{comparison}{One row per scheme. Columns include scheme, method, requested
intervals, retained intervals, zero_bins, total_bins, occupied, empty, sparse,
smallest_n, largest_n, smallest_pct, largest_pct, widest_bin, observed_min,
observed_max, and widest_range. The last four describe the widest observed
within-bin range.}
\item{bins}{All per-bin tables with an added scheme column, including exact
cutpoints, observed summaries, counts, proportions, and sparse flags.}
\item{diagnoses}{Named list of full \code{bin_diagnosis} objects. Use these for
per-scheme assessments, notes, and assignments aligned to the original rows.}
\item{assessment}{Plain-language comparison as a character vector.}
\item{call, variable, separate_zero, min_count, min_prop}{Call and shared settings.}
}
\examples{
d <- data.frame(x = c(rep(0, 40), seq(.01, 1, length.out = 60)))
comparison <- bin_compare(d, "x", bins = c(3, 5), separate_zero = TRUE,
  custom_breaks = list(thresholds = c(0, .1, .5, 1)))
comparison
comparison$bins
comparison$diagnoses$quantile_3$assessment
plot(comparison, schemes = c("quantile_3", "equal_width_3"))
plot(comparison, type = "boundaries", schemes = c("quantile_3", "thresholds"))
}
\seealso{\code{\link{bin_diagnose}}, \code{\link{bin_apply}}, \code{\link{plot.bin_comparison}}}
