# binwise

Should you keep a variable as numbers, or put those numbers into groups?
binwise helps you see what changes when you make that choice.

Look at the values and counts in each group, compare different boundaries,
create a grouped variable, and check how the choice affects model predictions.
The package includes examples where groups are useful and examples where they
hide a relationship worth keeping.

[Read the tutorial](https://preblek.github.io/binwise/articles/first-steps.html)
| [Explore the examples](https://preblek.github.io/binwise/articles/example-gallery.html)
| [Function reference](https://preblek.github.io/binwise/reference/index.html)

## Install

Run these commands in RStudio. R 4.1 or later is required.

```r
install.packages("remotes")
remotes::install_github("preblek/binwise")
library(binwise)
```

This is an early version, not yet on CRAN.

To install the longer guides too, run this from RStudio, which supplies Pandoc.

```r
install.packages(c("knitr", "rmarkdown"))
remotes::install_github("preblek/binwise", build_vignettes = TRUE)
vignette("getting-started", package = "binwise")
```

## The main functions

| Function | What it helps you do |
| --- | --- |
| `bin_diagnose()` | Look at the values and counts in proposed groups. |
| `bin_compare()` | Compare several ways of grouping the same variable. |
| `bin_apply()` | Add the chosen categories to your data and keep the original numbers. |
| `bin_interact()` | Compare models using numbers, curves, and categories, with or without an interaction. |
| `bin_validate()` | Compare predictions for observations left out of model fitting. |

## Try a meaningful zero

```r
data("binwise_zero", package = "binwise")
groups <- bin_diagnose(binwise_zero, "score", bins = 3,
                       separate_zero = TRUE, warn = FALSE)
groups$bins
with_groups <- bin_apply(binwise_zero, groups, name = "participation_group")
table(with_groups$participation_group)
```

In this simulated example, zero means no participation. The positive values
measure how much someone participates. Keeping those situations separate may
help answer a question about participation, but it does not mean the positive
values also need categories.

## Related working paper

Preble, Keith (2026). *Commercial Embeddedness and Sanctions Busting:
Intra-Industry Trade, Dependence, and Economic Asymmetry*. APSA Preprints.
[https://doi.org/10.33774/apsa-2026-7wxvk](https://doi.org/10.33774/apsa-2026-7wxvk).

The research case study uses small summaries from the paper and additional
prediction checks prepared for binwise. It includes no individual research
records. The paper and the additional checks are identified separately.

## Working with the source

Open `binwise.Rproj` in RStudio. The local helpers `tools/install.R` and
`tools/start.R` build and load a copy of the package. Building the guides needs
knitr, rmarkdown, and Pandoc. The source script in `data-raw/build_ii_case_study.R`
requires the separate research project; you do not need to run it to use the
package or follow the examples.

The website is built with pkgdown. After installing pkgdown and pkgload, run
`source("tools/build_website.R")` from this package folder to rebuild `docs/`.

MIT licensed. Copyright Keith A. Preble.
