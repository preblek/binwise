library(binwise)
x <- readRDS(system.file("extdata", "ii-case-study.rds", package = "binwise"))
stopifnot(x$metadata$n == 426465L, identical(x$metadata$years, c(1963L, 2023L)),
          x$metadata$events == 43007L, all(x$metadata$current_assignments_match),
          all(x$metadata$original_integration$assignments_match),
          sum(x$observed_cells$n) == x$metadata$n,
          sum(x$heldout_cells$n) == x$metadata$n,
          all(tapply(x$bins$n, x$bins$variable, sum) == x$metadata$n),
          all(tapply(x$trade_comparison$n, x$trade_comparison$scheme, sum) == x$metadata$n),
          nrow(x$observed_cells) == 36L, nrow(x$confidence) == 72L,
          all(x$fit_checks$converged), all(x$diagnostics$unavailable == 0),
          all(nchar(x$provenance$md5) == 32L), !any(grepl("^/", x$provenance$file)))
# Stored mean-fit diagnostics and held-out results stay distinct and aligned.
stopifnot(max(abs(x$observed_cells$observed - x$observed_cells$categorical)) < 1e-6,
          max(abs(x$heldout_cells$observed - x$heldout_cells$categorical)) > 1e-3)
for (metric in c("Brier", "log_loss")) {
  score <- x$scores[x$scores$metric == metric, ]
  values <- setNames(score$score, score$model)
  direct <- x$flexible_vs_categories[x$flexible_vs_categories$metric == metric, ]
  stopifnot(abs(direct$difference - (values['flexible'] - values['categorical'])) < 1e-12,
            values['flexible'] < values['categorical'], values['categorical'] < values['continuous'])
}
# This bundle remains small enough to be a summary artifact, not research microdata.
stopifnot(as.numeric(object.size(x)) < 500000,
          !any(c("predictions", "models", "analysis_data", "assignments", "values") %in% names(x)))
# Heatmaps retain the audited paper measures and align with the preferred sample.
h <- x$paper_heatmaps
stopifnot(nrow(h) == 108L,
          all(tapply(h$n, h$strategy, sum) == x$metadata$n),
          all(h$n > 0), all(h$events >= 0 & h$events <= h$n),
          max(abs(h$observed_rate - h$events / h$n)) < 1e-12,
          max(abs(h$observed_minus_representative -
                    (h$observed_rate - h$representative_probability))) < 1e-12,
          all(h$representative_probability >= 0 & h$representative_probability <= 1))
keys <- c('gdp_similarity_bin', 'iit_model_bin', 'tpts_bin')
z <- merge(h[h$strategy == 'quartile', ], x$observed_cells, by = keys)
stopifnot(nrow(z) == 36L, all(z$n.x == z$n.y),
          max(abs(z$observed_rate - z$observed)) < 1e-12)
