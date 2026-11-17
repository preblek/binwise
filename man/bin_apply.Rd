\name{bin_apply}
\alias{bin_apply}
\title{Apply a Saved Binning Scheme to a Data Frame}
\description{Create a categorical column using the exact cutpoints and zero rule
of a previously inspected diagnosis. No boundaries are recomputed.}
\usage{
bin_apply(data, scheme, name = NULL, variable = NULL,
  out_of_range = c("error", "NA"), warn = TRUE)
}
\arguments{
\item{data}{A data frame containing the variable to categorize.}
\item{scheme}{An unmodified \code{bin_diagnosis} returned by \code{bin_diagnose()},
or one selected element of \code{bin_compare()$diagnoses}.}
\item{name}{New column name. Defaults to the receiving variable name followed
by \code{"_bin"}. Existing columns cannot be overwritten.}
\item{variable}{Quoted source column name in the receiving data. Defaults to the
variable name stored in the scheme. Must identify a plain numeric or integer
column in the same units as the data used to define the scheme.}
\item{out_of_range}{Default \code{"error"} stops if any finite value has no
category under the saved scheme. Explicit \code{"NA"} retains such rows with
missing categories. Boundaries are never extended and values are never clipped.}
\item{warn}{Warn about unassigned observations. With \code{FALSE}, the audit
report still records them. Invalid inputs and the default out-of-range error
are unaffected.}
}
\details{
The output is an ordinary data frame, including when the input is a data-frame
subclass. Original columns, row order, and row names are preserved, and the input
object is not modified. The new column is an unordered factor, with all saved
levels retained even when empty. Category identifiers follow the saved order.
Intervals include their upper boundary; the first includes its lower boundary.
A singleton category accepts only its exact saved value.

If zero separation was requested, exact zeros use the saved Zero category and
only positive values enter the interval bins. If no Zero category existed when
the scheme was created, a newly encountered zero is uncovered. No new category
is invented. Negative values are uncovered under a separate-zero scheme even
if custom cutpoints extend below zero. For an all-zero scheme, new positive
values are uncovered. Custom cutpoints can cover new values beyond the original
observed range, as long as those values satisfy the saved bounds and zero rule.

NA and NaN receive missing categories, as do positive and negative infinity.
All rows remain present under the explicit \code{"NA"} out-of-range policy.
The scheme's original assignment vector is not reused; each receiving value is
assigned using the saved cutpoints. Percentage display options do not convert
data units. The caller must supply values in compatible units.

An audit record is stored in \code{attr(result, "binwise")[[name]]}. It contains
the receiving and original variable names, method, exact breaks, zero rule,
category definition with closure indicators, out-of-range policy, and new-data
summaries. \code{summary} counts total, finite, assigned, missing, infinite, and
uncovered observations. \code{counts} gives each category's count and proportion
of all finite receiving values, including uncovered finite values in the
denominator. Proportions are NA if there are no finite values, and may sum to
less than one if finite values are uncovered. \code{excluded} identifies
unassigned row positions and their reasons. Counts are descriptive row counts.

Applying another scheme under a different column name preserves earlier audit
records. These records describe the data at application time; later subsetting
or editing does not recalculate them and may discard attributes. Save the whole
object with \code{saveRDS()} to retain the factor and metadata. CSV export does
not retain these attributes. The function does not choose a model or reference
category; unordered factors avoid imposing ordered-factor polynomial contrasts.
}
\value{A data frame with the new factor column and a \code{binwise} audit attribute.}
\examples{
training <- data.frame(x = c(0, 0, 1:8))
scheme <- bin_diagnose(training, "x", bins = 2,
                       separate_zero = TRUE, warn = FALSE)
binned <- bin_apply(training, scheme, name = "x_group")
table(binned$x_group, useNA = "ifany")
attr(binned, "binwise")$x_group$definition
new_data <- data.frame(x = c(0, 2, 6, 9, NA))
applied <- bin_apply(new_data, scheme, out_of_range = "NA", warn = FALSE)
attr(applied, "binwise")$x_bin$excluded
}
\seealso{\code{\link{bin_diagnose}}, \code{\link{bin_compare}}}
