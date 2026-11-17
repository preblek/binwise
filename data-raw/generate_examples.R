# Run from the binwise package folder to regenerate the distributed toy data.
# Restore the caller's random-number state, including the RNG algorithm settings.
local({
  previous_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) previous_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    do.call(RNGkind, as.list(previous_kind))
    if (had_seed) assign(".Random.seed", previous_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  })
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(9388)
  n <- 1000L
  # An illustrative participation/intensity measure. Zero means no participation.
  binwise_zero <- data.frame(id = seq_len(n),
    score = sample(c(rep(0, 400), stats::rbeta(600, 2, 5))))
  # A positive fraction with a long upper tail. No zero observations are generated.
  binwise_tail <- data.frame(id = seq_len(n),
    share = stats::plogis(stats::rnorm(n, mean = -5, sd = 2.3)))
  # A known smooth relationship with no threshold in its data-generating rule.
  exposure <- sample(seq(0, 100, length.out = n))
  expected_outcome <- 2 + .5 * exposure
  binwise_smooth <- data.frame(id = seq_len(n), exposure = exposure,
    outcome = expected_outcome + stats::rnorm(n, sd = 3),
    expected_outcome = expected_outcome)
  # An arbitrary index origin makes every score negative. A repeated value is
  # not assigned a distinct substantive meaning in this fictional example.
  score <- sample(c(rep(-10, 400), stats::runif(600, -9.9, -.1)))
  binwise_negative <- data.frame(id = seq_len(n), score = score,
                                 shifted_score = score + 11)
  # Negative changes are declines and positive changes are increases.
  # Zero is a meaningful boundary even though it is absent from the sample.
  change <- sample(c(-stats::runif(700, .1, 10), stats::runif(300, .1, 10)))
  binwise_mixed <- data.frame(id = seq_len(n), change = change)
  # Known smooth and threshold interactions, plus a binary logit outcome.
  # The first five datasets are generated before this block and stay unchanged.
  x <- stats::runif(1200, -2, 2)
  z <- stats::runif(1200, -2, 2)
  control <- stats::rnorm(1200)
  expected_outcome <- 1 + .8 * x - .5 * z + .9 * x * z + .4 * control
  expected_threshold <- 1 + 1.8 * (x > 0) - .7 * z + 1.1 * (x > 0) * z + .4 * control
  probability <- stats::plogis(-.4 + .6 * x - .4 * z + .7 * x * z + .3 * control)
  binwise_interaction <- data.frame(id = seq_len(1200), x = x, z = z,
    control = control, outcome = expected_outcome + stats::rnorm(1200, sd = 1.2),
    threshold_outcome = expected_threshold + stats::rnorm(1200, sd = 1.2),
    binary_outcome = stats::rbinom(1200, 1, probability),
    expected_outcome = expected_outcome, expected_threshold = expected_threshold,
    probability = probability)
  dir.create("data", showWarnings = FALSE)
  for (name in c("binwise_zero", "binwise_tail", "binwise_smooth",
                "binwise_negative", "binwise_mixed", "binwise_interaction")) {
    save(list = name, file = file.path("data", paste0(name, ".rda")),
         envir = environment(), compress = "xz", version = 2)
  }
})
