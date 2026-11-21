# binwise 0.0.17

* Cite the published APSA preprint in the research vignette, help page, and
  case-study metadata using DOI 10.33774/apsa-2026-7wxvk.

# binwise 0.0.16

* Explain the research heatmaps through individual cells, comparisons across
  groups, and worked percentage-point differences.

# binwise 0.0.15

* Focus the research vignette on the reader workflow, removing manuscript
  revision history and local development notes while retaining statistical scope.

# binwise 0.0.14

* Add observed, representative-prediction, and difference heatmaps to the
  research vignette, with cell counts and a step-by-step reading guide.
* Preserve verified paper heatmap aggregates and their audit provenance.

# binwise 0.0.13

* Add a research case-study vignette on intra-industry trade and sanctions
  busting, with small real-data aggregate summaries and provenance.
* Distinguish substantive categorization, in-sample cell agreement, clustered
  mean intervals, and additional held-out prediction comparisons.
* Add a local RStudio walkthrough verifying the paper's category assignments.

# binwise 0.0.12

* Add optional natural cubic spline comparators to bin_interact() and
  bin_validate() using spline_df. Existing calls retain their behavior.
* Learn spline knots separately within each validation training fold and
  retain them for inspection. Predictions reuse fitted spline definitions.
* Add reference to bin_validate() for direct paired comparisons against
  a spline or another returned model.
* Explain spline comparisons in model assessments and documentation.
* Adopt MIT licensing with Keith A. Preble as copyright holder.

# binwise 0.0.11

* Add getting-started and real-data vignettes, with executed examples,
  interpretation guidance, related software, and primary research.
