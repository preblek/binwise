\name{ii_case_study}
\alias{ii_case_study}
\title{Aggregate Summaries for the IIT Research Case Study}
\description{Compact summaries from Keith A. Preble's APSA preprint on
intra-industry trade, dependence, economic asymmetry, and sanctions busting,
and additional internal model-validation comparisons. These are real-data
aggregates, not simulated individual records.}
\details{
Read the list with
\code{readRDS(system.file("extdata", "ii-case-study.rds", package = "binwise"))}.
It contains metadata, provenance hashes, bin definitions and counts, distribution
summaries, alternative trade-bin summaries, observed and fitted cell means,
three paper heatmap measures across three GDP grouping rules,
pointwise cell-mean intervals, held-out scores and paired differences,
per-fold scores, cutpoints, and fit diagnostics. Numeric boundaries retain their
stored precision; fractions are not pre-multiplied by 100.

The research sample has 426,465 directed dyad-years from 1963 through 2023.
No individual records, country identifiers, fitted model objects, or row-level
predictions are distributed. Scores and intervals were computed in the separate
research workflow and are not recalculated from aggregates. The original
category-integration check used binwise 0.0.9 and validation used 0.0.10.
The current version rechecks category assignments when constructing this bundle.

The paper's categorical definitions are distinguished from the additional
continuous and flexible prediction comparisons in the case-study vignette.
The preprint is available at \doi{10.33774/apsa-2026-7wxvk}. The original
numeric research data and matching software environment are needed to refit its models.
Reading these summaries does not reproduce those regressions. The data-raw
script in the development project records the extraction process.

All score models use the same held-out rows. Bootstrap intervals hold folds and
predictions fixed and exclude refitting and model-selection uncertainty. Cell
intervals use two-way clustered covariance; recorded covariance repairs and knot
adjustments must be considered when interpreting the results. The built-in
\code{bin_interact} spline model is not the paper's advanced fixed-effects model.
}
\references{Preble, Keith (2026). \emph{Commercial Embeddedness and Sanctions Busting:
Intra-Industry Trade, Dependence, and Economic Asymmetry}. APSA Preprints.
Posted November 16, 2026. \doi{10.33774/apsa-2026-7wxvk}.}
\examples{
case <- readRDS(system.file("extdata", "ii-case-study.rds", package = "binwise"))
case$metadata$n
case$scores
}
\seealso{\code{\link{bin_diagnose}}, \code{\link{bin_apply}}, \code{\link{bin_validate}}}
\keyword{datasets}
