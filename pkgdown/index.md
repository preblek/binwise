# Think through your bins before you use them

<div class="binwise-intro">
<p class="binwise-kicker">THE BINWISE R PACKAGE</p>
<p class="binwise-lead">Should you keep a variable as continuous, or put those numbers into groups or bins? binwise helps you see what changes when you make that choice.</p>
<p>You might want to separate no participation from some participation, distinguish declines from increases, or describe combinations of trade relationships. Groups can make those differences easier to explain. They can also hide differences that matter.</p>
<p class="binwise-actions"><a class="btn btn-primary" href="articles/first-steps.html">Start the tutorial</a> <a class="btn btn-outline-primary" href="articles/example-gallery.html">Explore the examples</a></p>
</div>

<figure class="binwise-hero">
<a href="articles/example-gallery.html"><picture><source media="(max-width: 600px)" srcset="images/home-comparison-mobile.png"><img src="images/home-comparison.png" alt="Two simulated examples. In the first, four flat groups lose a steady increase. In the second, two groups follow a known jump at zero." width="1440" height="650"></picture></a>
<figcaption>Two examples that give very different answers. In the first example, grouping hides a steady change. In the second, the simulated outcome really does jump at the chosen boundary. Other predictors are fixed at zero for the second example. These pictures are a starting point, and the examples also show how to check predictions.</figcaption>
</figure>

## Why binwise?

I created binwise because I was struggling with a paper I was writing where a lot of the dependent variable was occurring in a very small, narrow band of several of my independent variables. The distribution was really bunched, and I experimented with a lot of different methods to unpack some of that compression so I could better see the differences and variation. I am not sure that the package will be very useful, but I found it useful to solve my problem, so maybe others will find it helpful, also. 

My working paper, [*Commercial Embeddedness and Sanctions Busting: Intra-Industry Trade, Dependence, and Economic Asymmetry*](https://doi.org/10.33774/apsa-2026-7wxvk), examines trade relationships involving countries under sanctions. It is available on APSA Preprints. The [paper case study](articles/working-paper.html) shows why categories can be useful for describing combinations of variables. It also explains what I learned from additional prediction checks prepared for binwise. Categories help organize the patterns and help to develop a model (or models) that keeps (or keep) the numbers while also allowing the curves to better predict.

## What can you do with binwise?

1. **Look inside the groups.** See the range of values and how many observations each proposed group contains.
2. **Compare your choices.** Try equal-sized groups, equal-width intervals, or boundaries you choose yourself.
3. **Create the grouped variable.** Add your chosen categories to your data while keeping the original numbers.
4. **Check what changes in the analysis.** Compare models using the numbers, a smooth curve, and your categories. Then test their predictions on observations left out of model fitting.

You do not have to use every step. If you are preparing a descriptive table, counts and ranges may be your main concern. If you are fitting a model, the prediction comparisons give you another way to assess the choice.

## Install and try it

Run these lines in the RStudio Console. R 4.1 or later is required.

```r
install.packages("remotes")
remotes::install_github("preblek/binwise")
library(binwise)
```

binwise is an early version and is not yet on CRAN. You can read the examples on this website without installing anything.

```r
data("binwise_zero", package = "binwise")
groups <- bin_diagnose(
  binwise_zero, "score", bins = 3,
  separate_zero = TRUE, warn = FALSE
)
groups$bins
```

This example puts zero in its own category and divides positive values into three groups. The [tutorial](articles/first-steps.html) explains why that can be useful and how to add the categories to a data frame.

## Choose an example

- **When zero means something different.** Keep no participation separate from some participation.
- **When a few large values stretch the range.** Check whether a single group hides very different observations.
- **When the outcome changes gradually.** See what gets lost when a smooth relationship becomes a set of flat steps.
- **When there is a known jump.** Try a case where a meaningful boundary helps predictions.
- **When values cross zero.** Separate declines from increases when direction is the question.

[Explore the examples and their R code](articles/example-gallery.html).

## The working paper behind the case study

My working paper, [*Commercial Embeddedness and Sanctions Busting: Intra-Industry Trade, Dependence, and Economic Asymmetry*](https://doi.org/10.33774/apsa-2026-7wxvk), examines trade relationships involving countries under sanctions. It is available on APSA Preprints. The [paper case study](articles/working-paper.html) shows why categories can be useful for describing combinations of variables. 

## A tool for making the choice yourself

binwise helps you inspect the consequences of a choice. It does not decide that bins are right simply because a variable is skewed, contains zeros, or has negative values. Nor does a lower prediction error establish a causal effect.

Start with what the variable means and what you want to learn. Keep the original numbers, describe your boundaries, and check what the groups hide as well as what they reveal.

[Report a problem or suggest an improvement](https://github.com/preblek/binwise/issues).
