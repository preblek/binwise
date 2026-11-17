\name{binwise_interaction}
\alias{binwise_interaction}
\docType{data}
\title{Known Smooth and Threshold Interactions for Model Comparison}
\description{A reproducible simulated dataset with two focal variables, one
control, two continuous outcomes, and a binary outcome. The generating rules
illustrate different consequences of categorization.}
\usage{data(binwise_interaction)}
\format{A data frame with 1200 rows and these columns:
\describe{
\item{id}{Integer row identifier.}
\item{x, z}{Independent uniform draws between -2 and 2.}
\item{control}{Independent standard normal draws.}
\item{expected_outcome}{\code{1 + 0.8*x - 0.5*z + 0.9*x*z + 0.4*control}.}
\item{outcome}{The smooth expectation plus independent normal noise with SD 1.2.}
\item{expected_threshold}{\code{1 + 1.8*(x > 0) - 0.7*z + 1.1*(x > 0)*z + 0.4*control}.}
\item{threshold_outcome}{The threshold expectation plus independent normal noise with SD 1.2.}
\item{probability}{Inverse logit of \code{-0.4 + 0.6*x - 0.4*z + 0.7*x*z + 0.3*control}.}
\item{binary_outcome}{A Bernoulli draw at the corresponding probability.}
}}
\details{
The source generator uses seed 9388 and appends this simulation after the five
descriptive examples documented in \code{binwise_examples}; those datasets are
unchanged. All rows are simulated, with no research or personal data.
The continuous product term matches the smooth expectation's form. The zero
threshold matches the other continuous outcome's form. This intentional design
does not imply that either form is universally preferable. Compare model-fit
criteria within the same outcome and sample, not across the different outcomes.
The known expectations and probability are teaching aids, not model controls.
}
\examples{
data(binwise_interaction)
head(binwise_interaction)
}
\seealso{\code{\link{bin_interact}}, \code{\link{binwise_examples}}}
\keyword{datasets}
