context("get_data_frame")

test_that(
  "Export of MNE-objects is equivalent in R and Python", {

  # get MNE data
  fname <- paste(
    mne$datasets$testing$data_path(),
    "MEG",
    "sample",
    "sample_audvis_trunc_raw.fif",
    sep = "/")

  raw <- mne$io$read_raw_fif(fname)
  events <- mne$find_events(raw)
  storage.mode(events) <- "integer"
  event_id <- list(aud = 1L)
  tmin <- -0.1
  tmax <- 0.1
  reject <- NULL
  baseline <- NULL
  epochs <- mne$Epochs(
    raw = raw, events = events, event_id = event_id, tmin = tmin,
    tmax = tmax, baseline = baseline, preload = T, proj = F,
    reject = NULL)
  evoked <- epochs$average()

  # prepare and run tests
  data <- c(raw, epochs, evoked)
  for (inst in data) {
    out_mne_py <- inst$to_data_frame(long_format = T)
    out_mne_r <- mne:::get_long_format(
      inst, picks = NULL, index = NULL, scaling_time = 1e3,
      scalings = NULL, copy = T, start = NULL, stop = NULL)

    expect_equal(colnames(out_mne_py), colnames(out_mne_r))

    for (ii in 1:length(colnames(out_mne_py))) {
      expect_equal(out_mne_py[,ii], out_mne_r[,ii])
    }
  }
})
