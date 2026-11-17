\name{plot.bin_validation}
\alias{plot.bin_validation}
\title{Plot Paired Held-Out Loss Differences}
\description{Plot differences from the reference model and optional paired
bootstrap intervals. Negative differences favor the candidate.}
\usage{\method{plot}{bin_validation}(x,
  metric = if (x$model == "logit") "Brier" else "RMSE", model_labels = NULL, ...)}
\arguments{\item{x}{A bin_validation object.}\item{metric}{Brier or log_loss
for logit; RMSE or MAE for linear.}\item{model_labels}{Optional named character
vector mapping candidate model names to display labels. Underscores become spaces
by default.}\item{...}{Additional base plot arguments.}}
\value{The object, invisibly.}
