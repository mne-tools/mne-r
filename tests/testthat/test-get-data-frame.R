context("get_data_frame")

test_that(
  "Export of MNE-objects is equivalent in R and Python", {

  # get MNE data
  fname <- file.path(
    mne$datasets$testing$data_path(),
    "MEG",
    "sample",
    "sample_audvis_trunc_raw.fif")

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
    # get dataframe from python method
    out_mne_py <- inst$to_data_frame(long_format = T)
    # build dataframe from instance via R
    index <- "time"
    if (inst == epochs) {
      index <- c("condition", "epoch", "time")
    }
    args <- list(inst, picks = NULL, index = index, time_format = "ms",
                 scalings = NULL, copy = TRUE)
    if (inst == raw) {
      args <- c(args, list(start = NULL, stop = NULL))
    }
    out_mne_r <- do.call(mne:::get_long_format, args)

    expect_equal(colnames(out_mne_py), colnames(out_mne_r))

    for (ii in 1:length(colnames(out_mne_py))) {
      expect_equal(out_mne_py[,ii], out_mne_r[,ii])
    }
    out_mne_r_wide <- get_data_frame(inst, index = index, long_format = FALSE)
    expect_equal(
      sum(stringr::str_detect(colnames(out_mne_r_wide), " ")), 0)
    if ("mne.epochs.BaseEpochs" %in% class(inst)) {
      expect_equal(colnames(out_mne_r_wide)[1:3],
                   c("condition", "epoch", "time"))
      expect_equal(out_mne_r_wide$condition %>% class(), "factor")
      expect_equal(out_mne_r_wide$epoch %>% class(), "factor")
      expect_equal(out_mne_r_wide$time %>% class(), "numeric")
    }
    out_mne_r_long2 <- get_data_frame(inst)
    for (ii in 1:length(colnames(out_mne_r))) {
      expect_equal(out_mne_r_long2[,ii], out_mne_r[,ii])
  }
  }
})
