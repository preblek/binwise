# Open binwise.Rproj before running this script.
# Load the existing local installation; install only if it is missing.
if (!file.exists("DESCRIPTION") ||
    read.dcf("DESCRIPTION", fields = "Package")[1, 1] != "binwise") {
  stop("Open binwise.Rproj first, then run source('tools/start.R').")
}
binwise_version <- read.dcf("DESCRIPTION", fields = "Version")[1, 1]
# Installed packages must live outside sync folders to avoid staged-install conflicts.
binwise_library <- file.path(tools::R_user_dir("binwise", "cache"), "library",
                             R.version$platform, paste0(R.version$major, ".",
                             strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1]),
                             binwise_version)
if (!file.exists(file.path(binwise_library, "binwise", "DESCRIPTION"))) {
  source("tools/install.R")
}
# Avoid silently keeping a different installation already loaded in this session.
if ("binwise" %in% loadedNamespaces()) {
  expected <- normalizePath(file.path(binwise_library, "binwise"))
  loaded <- normalizePath(getNamespaceInfo("binwise", "path"))
  if (!identical(expected, loaded)) {
    if ("package:binwise" %in% search()) detach("package:binwise", unload = TRUE)
    if ("binwise" %in% loadedNamespaces()) unloadNamespace("binwise")
  }
}
.libPaths(c(binwise_library, .libPaths()))
library(binwise, lib.loc = binwise_library)
message("binwise ", packageVersion("binwise"), " is ready in this R session.")
message("For the beginner guide, run vignette('getting-started', package = 'binwise').")
