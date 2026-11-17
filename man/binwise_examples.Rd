\name{binwise_examples}
\alias{binwise_examples}
\alias{binwise_zero}
\alias{binwise_tail}
\alias{binwise_smooth}
\alias{binwise_negative}
\alias{binwise_mixed}
\docType{data}
\title{Simulated Examples for Exploring Binning Decisions}
\description{Five reproducible teaching datasets that illustrate distinct
reasons to inspect a variable before categorizing it. All observations are
simulated and contain no research or personal data.}
\usage{
data(binwise_zero)
data(binwise_tail)
data(binwise_smooth)
data(binwise_negative)
data(binwise_mixed)
}
\format{
Each object is a data frame with 1000 rows and an integer \code{id} column.
\describe{
\item{binwise_zero}{Also contains numeric \code{score}, with 400 exact zeros and
600 positive beta-distributed values. In this fictional setting, zero means
no participation and positive values measure participation intensity.}
\item{binwise_tail}{Also contains numeric \code{share}, a positive fraction
strictly between zero and one, with most values near zero and a long upper tail.}
\item{binwise_smooth}{Also contains numeric \code{exposure}, \code{outcome}, and
\code{expected_outcome}. Exposure spans 0 to 100. The conditional expected
outcome is exactly \code{2 + 0.5 * exposure}; observed outcomes add normal noise
with standard deviation 3.}
\item{binwise_negative}{Also contains numeric \code{score}, with 400 observations
equal to -10 and 600 uniform draws between -9.9 and -0.1, randomly shuffled.
The numeric column \code{shifted_score} is exactly \code{score + 11}, making
every value positive without changing the ordering, ties, or differences between
observations. The score is a fictional index with an arbitrary origin; the
repeated value -10 is not assigned a distinct substantive meaning.}
\item{binwise_mixed}{Also contains numeric \code{change}, comprising 700 negative
changes and 300 positive changes, randomly shuffled, with magnitudes drawn
uniformly between 0.1 and 10. No observation equals zero. In this fictional
setting, negative values mean declines and positive values mean increases.}
}
}
\details{
The zero example illustrates a substantive reason to distinguish zero from
positive values. It does not require categorizing all positive variation.
The tail example illustrates the trade-off between balanced counts and
retaining upper-tail distinctions; skewness alone does not require bins.
The smooth example has no threshold in its generating relationship. Categorizing
exposure groups together observations with different expected outcomes. This
illustrates what arbitrary bins can hide, rather than establishing that bins
are inappropriate for every descriptive or communication purpose.

The negative example demonstrates that ties can prevent equal-sized quantile
bins at any value. Shifting the index to positive values preserves assignments
under the same automatic methods, so negative signs alone do not justify bins.
Do not split tied values just to meet a requested group count. A repeated value
does not automatically deserve a dedicated category without substantive meaning.

The mixed-sign example has balanced quartiles that mix declines and increases
within the third group. If distinguishing direction is the purpose, a custom
zero boundary gives groups of 700 and 300 observations. Unequal counts are
compatible with that purpose. Two direction groups still hide magnitude, so
additional thresholds or a continuous representation may suit other purposes.
Zero need not be observed to be a useful boundary. A zero boundary is different
from a separate zero category: intervals are right-closed, so a future exact
zero would enter the lower interval. Automatic \code{separate_zero = TRUE}
currently requires nonnegative data and is not used in these examples.

The generator uses seed 9388 with Mersenne-Twister, Inversion, and Rejection RNG
settings. In order, it samples 400 zeros and 600 Beta(2,5) draws, generates
\code{plogis(rnorm(1000, -5, 2.3))}, shuffles 1000 equally spaced exposure values
from 0 to 100, and adds normal outcome noise. It then generates and shuffles
the negative index, followed by the negative and positive changes described
above. Adding these examples preserves the three original datasets.
The source repository includes
\code{data-raw/generate_examples.R}, which restores the caller's RNG state.
These examples do not automatically determine whether to bin a user's data.
}
\examples{
data(binwise_zero)
d <- bin_diagnose(binwise_zero, "score", bins = 3,
                  separate_zero = TRUE, warn = FALSE)
applied <- bin_apply(binwise_zero, d)
table(applied$score_bin)
data(binwise_tail)
bin_compare(binwise_tail, "share")
data(binwise_smooth)
plot(binwise_smooth$exposure, binwise_smooth$outcome,
     xlab = "Exposure", ylab = "Outcome", pch = 16, cex = .5)
abline(a = 2, b = .5, col = "red", lwd = 2)
data(binwise_negative)
bin_diagnose(binwise_negative, "score", bins = 4, warn = FALSE)
data(binwise_mixed)
direction <- bin_diagnose(binwise_mixed, "change", method = "custom",
  breaks = c(min(binwise_mixed$change), 0, max(binwise_mixed$change)), warn = FALSE)
direction$bins
}
\seealso{\code{\link{bin_diagnose}}, \code{\link{bin_compare}}, \code{\link{bin_apply}}}
\keyword{datasets}
