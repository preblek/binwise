# binwise

Tools for evaluating whether and how to turn numeric variables into categories.
Inspect what proposed bins contain, compare alternatives, create categorical
columns, and assess how the choice affects fitted relationships and held-out
predictions. Plain-language assessments explain the results without selecting
bins automatically.

Version 0.0.17 includes five functions:

| Function | Purpose |
| --- | --- |
| `bin_diagnose()` | Inspect missing values, ties, zeros, boundaries, and counts. |
| `bin_compare()` | Compare quantile, equal-width, and custom categories. |
| `bin_apply()` | Add a factor column using a saved scheme, preserving numeric originals. |
| `bin_interact()` | Compare linear, optional spline, and binned predictors, with optional interactions and confidence bands. |
| `bin_validate()` | Refit workflows on shared folds and compare held-out prediction losses. |

The package includes six simulated teaching datasets. Vignettes also use small
real datasets supplied with R. The package does not require private research data,
a network connection, external executables, or compiled code to run its analyses.

## Installation and RStudio

This version is being prepared for CRAN and has not been submitted. Install a
supplied source archive through RStudio's package installer, or use
`install.packages("/path/to/binwise_0.0.17.tar.gz", repos = NULL, type = "source")`
with the actual path to the archive. Do not try `install.packages("binwise")`
until a public release is available.

To work with a downloaded copy of this repository in RStudio, first install
`knitr` and `rmarkdown` with `install.packages(c("knitr", "rmarkdown"))`.
Then open `binwise.Rproj` and run
`source("tools/start.R")` in the Console. This loads its version-specific cache
installation. After editing source files, run `source("tools/install.R")`, then
`source("tools/start.R")`. These local helpers are excluded from the source
package sent to CRAN.

After loading the package, open a guide:

```r
library(binwise)
vignette("getting-started", package = "binwise")
vignette("flexible-continuous", package = "binwise")
vignette("real-data-and-related-work", package = "binwise")
vignette("ii-case-study", package = "binwise")
```

Run each guide's code blocks in order. The getting-started guide explains basic
R notation, the output, and what conclusions users can draw from each example.
The spline guide explains why a straight line alone may be an inadequate comparator.
The II case study follows an [APSA preprint](https://doi.org/10.33774/apsa-2026-7wxvk)
through meaningful categories, joint
patterns, uncertainty, and held-out prediction. It uses verified aggregate
summaries and clearly distinguishes the paper analysis from additional checks.
Individual research records are not bundled. The extraction script in
`data-raw/build_ii_case_study.R` documents how the aggregates were created;
it requires the separate research replication project and is not needed
to use the package or follow the vignette.

## A first example

```r
library(binwise)
data(binwise_smooth)
choices <- bin_compare(binwise_smooth, "exposure", bins = 4)
plot(choices, type = "boundaries", show_counts = TRUE)
fit <- bin_interact(binwise_smooth, "outcome", "exposure", choices, spline_df = 3)
plot(fit)
```

`spline_df = 3` adds a natural cubic spline with three degrees of freedom for
each focal variable. Existing calls without it keep their original behavior.
For two focal variables with `interaction = TRUE`, the spline model includes
both spline main effects and all their basis-term products. Controls remain
additive. This can require substantially more observations than a simple linear
interaction. The package reports fitting problems instead of silently simplifying
the model.

## Held-out comparisons

```r
check <- bin_validate(
  binwise_smooth, "outcome", "exposure",
  schemes = list(quartiles = list(exposure = list(probs = c(0, .25, .5, .75, 1)))),
  spline_df = 3, reference = "spline", folds = 5, seed = 12, reps = 199
)
check$scores
plot(check)
```

Quantile cutpoints and spline knots are learned separately from each training
fold. The held-out rows reuse those definitions. `reference = "spline"` makes
negative score differences favor a candidate over the spline. `group` can keep
repeated units together. Score intervals resample evaluation losses while holding
predictions and folds fixed; they do not capture full model-selection or refitting
uncertainty. Choose complexity in advance, or use nested validation when tuning it.

## Scope and interpretation

Built-in models are ordinary linear and binary logistic regressions with one or
two focal predictors and optional additive controls. Advanced users can supply a
`fit_predict(train, test)` callback to `bin_validate()` for other estimators.
The package does not automatically identify causal thresholds, select the best
bins, or substitute for substantive knowledge. A spline is one flexible comparator,
not a guarantee of the best continuous model. Meaningful categories can serve a
descriptive purpose even when a continuous model predicts better.

The related-work vignette distinguishes this decision-focused workflow from the
broader exploration and transformation tools in dlookr, specialized interaction
methods in interflex, and binscatter estimation and inference in binsreg.

## Development and license

The analysis functions depend only on packages supplied with R. Building the
HTML vignettes requires knitr, rmarkdown, and Pandoc. Reading built guides requires
none of those build tools. MIT licensed, copyright Keith A. Preble.
