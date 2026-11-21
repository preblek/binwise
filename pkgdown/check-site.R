# Check the generated pages before GitHub publishes them.
check_site <- function(directory) {
  root <- normalizePath(directory, winslash = '/', mustWork = TRUE)
  required <- c('index.html', 'articles/first-steps.html', 'articles/example-gallery.html',
                'articles/working-paper.html', 'reference/index.html', 'news/index.html',
                'search.json', 'images/home-comparison.png', 'images/home-comparison-mobile.png')
  absent <- required[!file.exists(file.path(root, required))]
  if (length(absent)) stop('Missing website files: ', paste(absent, collapse = ', '))
  pages <- list.files(root, pattern = '\\.html$', recursive = TRUE, full.names = TRUE)
  pages <- normalizePath(pages, winslash = '/', mustWork = TRUE)
  parsed <- lapply(pages, xml2::read_html)
  ids <- lapply(parsed, function(doc) xml2::xml_attr(xml2::xml_find_all(doc, '//*[@id]'), 'id'))
  names(ids) <- pages
  broken <- character()
  checked <- 0L
  for (i in seq_along(pages)) {
    doc <- parsed[[i]]
    refs <- c(xml2::xml_attr(xml2::xml_find_all(doc, '//*[@href]'), 'href'),
              xml2::xml_attr(xml2::xml_find_all(doc, '//*[@src]'), 'src'))
    for (ref in unique(refs[!is.na(refs)])) {
      if (startsWith(ref, 'https://preblek.github.io/binwise/')) {
        ref <- substring(ref, nchar('https://preblek.github.io/binwise/') + 1L)
        base <- root
      } else {
        if (grepl('^[A-Za-z][A-Za-z0-9+.-]*:|^//|^/', ref)) next
        base <- dirname(pages[i])
      }
      fragment <- if (grepl('#', ref, fixed = TRUE)) sub('^[^#]*#', '', ref) else ''
      rel <- utils::URLdecode(sub('[?#].*$', '', ref))
      target <- if (nzchar(rel)) file.path(base, rel) else pages[i]
      if (dir.exists(target)) target <- file.path(target, 'index.html')
      checked <- checked + 1L
      if (!file.exists(target)) {
        broken <- c(broken, paste(basename(pages[i]), ref, sep = ' -> '))
      } else if (nzchar(fragment)) {
        target <- normalizePath(target, winslash = '/', mustWork = TRUE)
        if (target %in% names(ids) && !utils::URLdecode(fragment) %in% ids[[target]]) {
          broken <- c(broken, paste(basename(pages[i]), ref, sep = ' -> '))
        }
      }
    }
  }
  for (name in c('index.html', 'articles/working-paper.html', 'articles/ii-case-study.html')) {
    links <- xml2::xml_attr(xml2::xml_find_all(xml2::read_html(file.path(root, name)), '//a[@href]'), 'href')
    if (!'https://doi.org/10.33774/apsa-2026-7wxvk' %in% links) {
      stop('Working-paper DOI link is missing from ', name)
    }
  }
  if (length(broken)) stop('Broken website links:\n', paste(unique(broken), collapse = '\n'))
  message('Checked ', length(pages), ' pages and ', checked, ' local links. Working-paper links are present.')
  invisible(TRUE)
}
