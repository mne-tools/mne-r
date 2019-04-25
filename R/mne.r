library(reticulate)

mne <- NULL

.onLoad <- function(libname, pkgname) {
  # push MNE into global environment
  mne <- reticulate::import("mne", delay_load = TRUE)
  cat(
    sprintf(
      "Importing MNE version=%s, path='%s'\n", mne$`__version__`, mne$`__path__`))
  assign("mne", mne, .GlobalEnv)
}
