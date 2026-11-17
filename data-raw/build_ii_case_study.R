# Run from the shared project root. This builds small aggregate teaching data
# from the completed replication. It never fits models or changes paper outputs.
local({
  root <- normalizePath(".")
  pkg <- file.path(root, "binwise")
  rep_dir <- file.path(root, "replication", "2026-11-13", "outputs")
  val_dir <- file.path(root, "replication", "2026-11-13-validation", "outputs")
  stopifnot(file.exists(file.path(pkg, "DESCRIPTION")))
  api <- new.env(parent = globalenv())
  for (f in list.files(file.path(pkg, "R"), full.names = TRUE)) sys.source(f, api)
  d <- readRDS(file.path(rep_dir, "gdp_similarity_categorical_data.rds"))
  old_schemes <- readRDS(file.path(rep_dir, "binwise_schemes.rds"))
  schemes <- list(
    iit = api$bin_diagnose(d, "iit_lag", bins = 2, separate_zero = TRUE, warn = FALSE),
    trade = api$bin_diagnose(d, "thirdparty_trade_share_lag", method = "custom",
      breaks = as.numeric(quantile(d$thirdparty_trade_share_lag, c(0, .5, .75, .9, 1))), warn = FALSE),
    gdp = api$bin_diagnose(d, "gdp_similarity_lag1", method = "custom",
      breaks = as.numeric(quantile(d$gdp_similarity_lag1, c(0, .25, .75, 1))), warn = FALSE))
  vars <- c(iit = "iit_model_bin", trade = "tpts_bin", gdp = "gdp_similarity_bin")
  labels <- lapply(d[unname(vars)], levels); names(labels) <- names(vars)
  matched <- vapply(names(schemes), function(n) {
    applied <- api$bin_apply(d, schemes[[n]], name = "case_study_bin")
    identical(as.integer(applied$case_study_bin), as.integer(d[[vars[[n]]]])) &&
      identical(schemes[[n]]$breaks, old_schemes[[n]]$breaks)
  }, logical(1))
  stopifnot(all(matched), nrow(d) == 426465L)
  bins <- do.call(rbind, lapply(names(schemes), function(n) {
    b <- schemes[[n]]$bins
    data.frame(variable = n, category = labels[[n]], b, row.names = NULL)
  }))
  comparison <- api$bin_compare(d, "thirdparty_trade_share_lag", methods = "quantile",
    custom_breaks = list(paper = schemes$trade$breaks))
  # Store only aggregates. No bin_diagnosis objects, raw values, assignment
  # vectors, row-level predictions, identifiers, or fitted model objects ship.
  read_table <- function(directory, name) read.csv(file.path(directory, name), stringsAsFactors = FALSE)
  tables <- list(
    bins = bins,
    trade_comparison = comparison$bins,
    distribution = do.call(rbind, lapply(schemes, `[[`, "summary")),
    observed_cells = read_table(rep_dir, "same_sample_cell_comparison.csv"),
    paper_heatmaps = read_table(rep_dir, "heatmap_cells.csv"),
    confidence = read_table(val_dir, "cell_confidence_intervals.csv"),
    heldout_cells = read_table(val_dir, "heldout_cell_means.csv"),
    scores = read_table(val_dir, "heldout_scores.csv"),
    differences = read_table(val_dir, "heldout_differences.csv"),
    flexible_vs_categories = read_table(val_dir, "flexible_vs_categorical.csv"),
    fold_scores = read_table(val_dir, "heldout_fold_scores.csv"),
    diagnostics = read_table(val_dir, "heldout_diagnostics.csv"),
    fit_checks = read_table(val_dir, "validation_fit_checks.csv"),
    confidence_checks = read_table(val_dir, "confidence_model_checks.csv"))
  validation <- readRDS(file.path(val_dir, "paper_validation.rds"))
  tables$fold_cutpoints <- do.call(rbind, lapply(seq_along(validation$fold_details), function(k) {
    cuts <- validation$fold_details[[k]]$cutpoints
    data.frame(fold = k, positive_iit_median = cuts$iit, trade_p50 = cuts$trade[1],
      trade_p75 = cuts$trade[2], trade_p90 = cuts$trade[3], gdp_p25 = cuts$gdp[1], gdp_p75 = cuts$gdp[2])
  }))
  sources <- c(file.path(rep_dir, c("gdp_similarity_categorical_data.rds", "binwise_schemes.rds",
    "same_sample_cell_comparison.csv", "heatmap_cells.csv", "paper_figure2_audit.csv",
    "paper_predicted_audit.csv", "paper_difference_audit.csv",
    "replication_report.txt", "binwise_integration_test.rds")),
    file.path(val_dir, c("cell_confidence_intervals.csv", "heldout_cell_means.csv", "heldout_scores.csv",
    "heldout_differences.csv", "flexible_vs_categorical.csv", "heldout_fold_scores.csv",
    "heldout_diagnostics.csv", "validation_fit_checks.csv", "confidence_model_checks.csv",
    "paper_validation.rds", "validation_report.txt")))
  provenance <- data.frame(file = substring(sources, nchar(root) + 2L),
    md5 = unname(tools::md5sum(sources)), stringsAsFactors = FALSE)
  meta <- list(
    title = "Commercial Embeddedness and Sanctions Busting: Intra-Industry Trade, Dependence, and Economic Asymmetry",
    author = "Keith A. Preble", status = "Preprint published on APSA Preprints",
    doi = "10.33774/apsa-2026-7wxvk",
    preprint_url = "https://doi.org/10.33774/apsa-2026-7wxvk",
    preprint_publication_date = "2026-11-16",
    source_version = "Replication and additional validation completed 13 November 2026",
    original_integration_binwise_version = "0.0.9", original_validation_binwise_version = "0.0.10",
    packaging_binwise_version = read.dcf(file.path(pkg, "DESCRIPTION"), fields = "Version")[1, 1],
    n = nrow(d), years = range(d$year), events = sum(d$sb_kp == 1),
    levels = labels, original_integration = readRDS(file.path(rep_dir, "binwise_integration_test.rds")),
    current_assignments_match = matched,
    note = "Aggregates from real data. Vignette plots and tables use saved results; no model is refitted during package checks.")
  case <- c(list(metadata = meta, provenance = provenance), tables)
  stopifnot(sum(case$observed_cells$n) == nrow(d), sum(case$heldout_cells$n) == nrow(d),
    all(case$diagnostics$unavailable == 0), nrow(case$observed_cells) == 36L)
  for (f in c("paper_figure2_audit.csv", "paper_predicted_audit.csv", "paper_difference_audit.csv")) {
    stopifnot(all(read_table(rep_dir, f)$matches_printed))
  }
  saveRDS(case, file.path(pkg, "inst", "extdata", "ii-case-study.rds"), compress = "xz", version = 2)
  writeLines(c(paste("Verified assignments for", nrow(d), "rows."),
    paste(names(matched), matched), "No regression was refitted; paper outputs were not modified."),
    file.path(pkg, "local", "verification", "ii-heatmaps-0.0.14", "data-build.log"))
})
