\name{plot.bin_interaction}
\alias{plot.bin_interaction}
\alias{print.bin_interaction}
\title{Display Continuous and Binned Model Comparisons}
\description{Print fit summaries and diagnostics, or plot stored common-grid
predictions and their differences from the continuous benchmark.}
\usage{
\method{print}{bin_interaction}(x, digits = 4L, ...)
\method{plot}{bin_interaction}(x, type = c("predictions", "differences"),
  main = NULL, xlab = NULL, ylab = NULL,
  show_confidence = type == "predictions",
  level = if (is.null(x$level)) 0.95 else x$level, ...)
}
\arguments{
\item{x}{A \code{bin_interaction} result.}
\item{digits}{Significant digits in printed numeric summaries.}
\item{type}{Plot predictions or differences from the continuous benchmark.}
\item{main, xlab, ylab}{Optional overall title and axis labels.}
\item{show_confidence}{Show shaded pointwise confidence intervals for the mean.
Defaults to TRUE for predictions and FALSE for differences. TRUE with a
difference plot is an error because paired difference intervals are not implemented.}
\item{level}{Confidence level strictly between 0 and 1. Defaults to the saved
level, or 0.95 for older objects. Changing it does not refit the models.}
\item{...}{Unused by print. For plot, additional base plot arguments, excluding
supplied limits, labels, title, or plotting coordinates.}
}
\details{
The horizontal axis is the first focal variable in original units. With a second
focal variable, panels use its stored conditioning values, with common horizontal
and vertical scales. Controls remain at the stored reference values. Colors and
line types identify models. Each focal bin is drawn separately to avoid connecting
across a categorical discontinuity; a single supported point is marked as a dot.
Unavailable predictions leave gaps. Failed or warning fits are not plotted.
Logit predictions use a 0-to-1 probability scale. Difference plots show candidate
predictions minus the continuous benchmark; they do not report statistical
significance. The legend appears in the first panel. Use a larger device or
RStudio Zoom for the three-panel interaction view.

Prediction plots show confidence bands by default. Bands are computed from the
saved models and drawn separately within each focal bin, with a vertical interval
at an isolated point such as a separate zero category. The caption states the
confidence level. See \code{\link{predict.bin_interaction}} for the model-based
assumptions. These pointwise intervals condition on fixed model settings, including cutpoints
and any supplied spline complexity. Their
overlap or non-overlap is not a formal test of a difference between models fitted
to the same observations. Difference plots retain point estimates only.
Graphics parameters are restored after plotting.
}
\value{The input object invisibly. Plots use the active graphics device.}
\seealso{\code{\link{bin_interact}}, \code{\link{predict.bin_interaction}}}
