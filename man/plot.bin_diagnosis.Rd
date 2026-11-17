\name{plot.bin_diagnosis}
\alias{plot.bin_diagnosis}
\alias{print.bin_diagnosis}
\title{Display a Binning Diagnosis}
\description{Print diagnostic tables or draw a histogram, cumulative distribution,
or candidate bin counts using base R graphics.}
\usage{
\method{print}{bin_diagnosis}(x, digits = 4L, ...)
\method{plot}{bin_diagnosis}(x, type = c("histogram", "ecdf", "bins"),
  main = NULL, ...)
}
\arguments{
\item{x}{An object returned by \code{bin_diagnose()}.}
\item{digits}{Significant digits in printed tables.}
\item{type}{Plot type. The histogram shows all finite values, including zeros.
The ECDF shows the proportion at or below each value. Bin-count bars include
empty bins and a separate zero category when requested.}
\item{main}{Optional title.}
\item{...}{Additional arguments to the underlying graphics function, such as
\code{breaks} for histogram resolution or \code{xlim} for a display range.
Do not override the supplied \code{xlab}, \code{ylab}, \code{col}, \code{border},
or \code{names.arg} arguments. Unused by the print method.}
}
\details{Histograms default to observation counts, including when display breaks
are unequal. Use \code{freq = FALSE} to request density instead.
Dashed lines mark internal candidate interval boundaries in histograms
and ECDF plots. Histogram bars are display bins, not the candidate categories.
Use \code{type = "bins"} to see counts of candidate categories. A separate zero
category is shown only in the bin-count plot; zeros remain included in histograms
and ECDFs. Orange count bars flag the user-set sparse-bin rules.}
\value{The input object, invisibly. Plots are drawn to the active graphics device.}
\seealso{\code{\link{bin_diagnose}}}

