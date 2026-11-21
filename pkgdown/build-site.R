# Run from the package root with Rscript pkgdown/build-site.R.
# GitHub Actions and local RStudio builds use this same script.
local({
  stopifnot(file.exists('DESCRIPTION'), file.exists('_pkgdown.yml'))
  required <- c('pkgdown', 'pkgload', 'pkgbuild', 'ragg', 'xml2', 'knitr', 'rmarkdown')
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop('Install the website tools first: ', paste(missing, collapse = ', '))
  if (!rmarkdown::pandoc_available()) stop('Pandoc is required. Run this inside RStudio or install Pandoc.')

  # Remove only a recognised pkgdown output directory, so deleted articles
  # cannot remain on the published site after a later rebuild.
  if (dir.exists('docs') && length(list.files('docs', all.files = TRUE, no.. = TRUE))) {
    if (!file.exists('docs/pkgdown.yml')) {
      stop('docs/ is not recognised as a pkgdown website. Inspect it before rebuilding.')
    }
    pkgdown::clean_site()
  }
  # Load the current source and recreate both homepage figures from bundled data.
  source('pkgdown/build_home_figure.R')
  # Install from this checkout into a temporary library and execute the guides.
  pkgdown::build_site(preview = FALSE)
  writeLines('Static website built with pkgdown.', 'docs/.nojekyll')
  source('pkgdown/check-site.R', local = TRUE)
  check_site('docs')
  message('Website ready in docs/.')
})
