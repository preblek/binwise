\name{plot.bin_comparison}
\alias{plot.bin_comparison}
\alias{print.bin_comparison}
\title{Display Candidate Binning Comparisons}
\description{Print the assessment and summary tables, or plot counts and boundaries
on shared axes across schemes.}
\usage{
\method{print}{bin_comparison}(x, digits = 4L, ...)
\method{plot}{bin_comparison}(x, type = c("counts", "boundaries"),
  schemes = NULL, digits = 3L,
  value_format = c("number", "percent"), percent_scale = 100,
  scheme_labels = NULL, xlab = NULL, main = NULL, show_counts = TRUE, ...)
}
\arguments{
\item{x}{An object returned by \code{bin_compare()}.}
\item{digits}{Significant digits for printed summaries and numeric plot labels.
For percentage plot labels, decimal places instead. For plots, a whole number
from 1 to 15. Trailing zeros are omitted.}
\item{type}{\code{"counts"} draws counts on a shared vertical scale and labels
each bar with its count. \code{"boundaries"} draws each interval on a shared
horizontal value scale with rounded numeric boundaries in its row label.}
\item{schemes}{Optional character vector selecting scheme names in display
order. Defaults to all schemes. Select fewer schemes for a small graphics device.}
\item{value_format}{Boundary axis and cutpoint label format. Default
\code{"number"} retains numeric units. Choose \code{"percent"} explicitly for
percentage labels. This does not format counts as percentages.}
\item{percent_scale}{Multiplier applied only to percentage labels. Default 100
converts fractions such as 0.25 to 25 percent. Set to 1 if values are already
stored as percentages, such as 25 for 25 percent. No scale is inferred from data.}
\item{scheme_labels}{Optional named character vector mapping scheme identifiers
in \code{x$diagnoses} to display titles. Partial mappings are allowed; unmapped
automatic schemes use readable method names and requested interval counts.
Unmapped custom names have underscores replaced by spaces. A mapping can include
unselected schemes, so it can be reused for different plot subsets.}
\item{xlab}{Optional boundary-axis label. Defaults to the column name.}
\item{main}{Optional overall title. Defaults to a comparison title using
\code{xlab}, or the column name if \code{xlab} is not supplied.}
\item{show_counts}{Logical. Display observation counts above bars or, for
boundary plots, as \code{n = ...} above each interval. Defaults to TRUE.
Counts include separate-zero and empty categories and always use observation
units, even with percentage-formatted boundaries. Set FALSE to hide counts.}
\item{...}{Additional arguments to \code{barplot} or \code{plot}. Do not override
supplied axes, limits, colors, or names. Use the explicit label and title arguments. Unused by the print method.}
}
\details{
Orange marks categories below either screening threshold. Zero categories and
empty intervals remain visible. In boundary plots, filled endpoints are included,
open endpoints are excluded, and singleton categories are shown as points.
Observation counts refer to the data used to create the comparison, not to
any later application of its schemes to different data. Counts are centered
above each interval where space allows, with labels moved inward near panel
edges. Line length encodes the value range, not the number of observations.
If zeros are separate, their assignment precedes interval assignment, even if
custom interval boundaries include zero. Printed boundary labels are rounded;
the full-precision values and closure indicators are available in \code{x$bins}.
Percentage formatting applies consistently to axis ticks and interval labels.
Bin boundaries, coordinates, assignments, stored scheme names, and comparison
tables are unchanged. For instance, 0.25 is plotted at the original coordinate
but labeled 25 percent with the default percentage scale. Negative percentages
and values above 100 percent are supported when meaningful for the supplied data.
Percentage values whose nonzero magnitude is below 10 to the power of minus
\code{digits} are shown using a bound, such as \code{<0.001\%} or
\code{>-0.001\%}. Exact zero is displayed as \code{0\%}. Increase \code{digits}
to distinguish very small boundaries. Labels describe variable values, not sample
percentiles or shares of observations.
Narrow intervals may be difficult to distinguish on a shared scale; inspect
their numeric labels and the bin table. Graphics parameters are restored after
plotting. Use RStudio's Zoom button or a larger graphics device for many panels.
}
\value{The input object, invisibly. Plots are drawn to the active graphics device.}
\seealso{\code{\link{bin_compare}}}
\examples{
d <- data.frame(share = seq(0, 1, length.out = 100))
comparison <- bin_compare(d, "share")
plot(comparison, type = "boundaries", value_format = "percent",
     scheme_labels = c(quantile_4 = "Four quantile bins",
                       equal_width_4 = "Four equal-width bins"),
     xlab = "Share of total", main = "Alternative categories")
# For percentages stored from 0 to 100, also supply percent_scale = 1.
}
