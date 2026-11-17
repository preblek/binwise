# Build and install the local package, including its HTML guides.
# Run from the binwise RStudio project. Analysis functions need only base R.
if (!file.exists("DESCRIPTION") ||
    read.dcf("DESCRIPTION", fields = "Package")[1, 1] != "binwise") {
  stop("Open binwise.Rproj first, then run source('tools/start.R').")
}
binwise_version <- read.dcf("DESCRIPTION", fields = "Version")[1, 1]
binwise_library <- file.path(tools::R_user_dir("binwise", "cache"), "library",
                             R.version$platform, paste0(R.version$major, ".",
                             strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1]),
                             binwise_version)
local({
  required <- c("knitr", "rmarkdown")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Building the guides needs ", paste(missing, collapse = ", "),
         ". Install them with install.packages(c('knitr', 'rmarkdown')), then rerun this script.")
  }
  if (!rmarkdown::pandoc_available()) {
    stop("Building the guides needs Pandoc. Run this installer inside RStudio, which includes Pandoc.")
  }
  package_source <- normalizePath(".")
  build_directory <- tempfile("binwise-build-")
  dir.create(build_directory)
  previous_directory <- setwd(build_directory)
  on.exit({setwd(previous_directory); unlink(build_directory, recursive = TRUE)}, add = TRUE)
  r_binary <- file.path(R.home("bin"), "R")
  build_status <- system2(r_binary, c("CMD", "build", shQuote(package_source)))
  archive <- file.path(build_directory, paste0("binwise_", binwise_version, ".tar.gz"))
  if (build_status != 0L || !file.exists(archive)) {
    stop("The package guides did not build. Check the messages above.")
  }
  # Install outside sync folders to avoid staged-install conflicts.
  dir.create(binwise_library, recursive = TRUE, showWarnings = FALSE)
  if ("package:binwise" %in% search()) detach("package:binwise", unload = TRUE)
  if ("binwise" %in% loadedNamespaces()) unloadNamespace("binwise")
  install_status <- system2(r_binary, c("CMD", "INSTALL", "--no-multiarch", "-l",
                                        shQuote(binwise_library), shQuote(archive)))
  if (install_status != 0L ||
      !file.exists(file.path(binwise_library, "binwise", "DESCRIPTION"))) {
    stop("Installation did not finish. Check the messages above.")
  }
})
message("Local package and guides installed. Run source('tools/start.R') to load them.")
