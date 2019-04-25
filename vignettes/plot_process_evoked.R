## ----setup, include=T----------------------------------------------------
library(tidyverse)
library(mne)

## ------------------------------------------------------------------------
data_path <- mne$datasets$sample$data_path()
subject <- "sample"

raw_fname <- paste(data_path,
                   'MEG', 
                   subject,
                   'sample_audvis_filt-0-40_raw.fif',
                   sep = '/')

raw <- mne$io$read_raw_fif(raw_fname, preload = T)

## ------------------------------------------------------------------------
raw$filter(1, 40)

## ------------------------------------------------------------------------
events <- mne$find_events(raw)
storage.mode(events) <- "integer"  # R gets the events as floats.

tmin <- -0.2
tmax <- 0.5
baseline <- reticulate::tuple(NULL, 0)
event_id <- list("aud/l" = 1L, "aud/r" = 2L, "vis/l" = 3L, "vis/r" = 4L)
picks <- mne$pick_types(raw$info, meg = T, eeg = T)
epochs <- mne$Epochs(raw = raw, events = events, event_id =event_id,
                     tmin = tmin, tmax = tmax,
                     picks = picks %>% as.integer(),
                     baseline = baseline, reject = NULL, preload = T) 

evoked <- epochs$average()

## ------------------------------------------------------------------------
# use MNE method
evoked_df <- evoked$to_data_frame(long_format = T)  # long

## ---- fig.width=8, fig.height=6------------------------------------------
ggplot(
  data = evoked_df,
  mapping = aes(x = time, color = channel, y = observation)) +
  geom_line(mapping = aes(group = channel)) +
  facet_wrap(
    ~ch_type,
    nrow = 3,
    scales = "free",
    strip.position = "left",
    labeller = as_labeller(c(eeg = "EEG [mV]", grad = "GRAD [fT/cm]", mag = "MAG [fT]"))) +
  theme_minimal() +
  guides(color = F) +
  labs(x = 'time [ms]', y = NULL) +
  theme(text = element_text(family = "Helvetica", size = 24))


## ---- fig.width=8, fig.height=6------------------------------------------
# we look up a private function from the epochs object
pos <- epochs$`_get_channel_positions`()
# we do the same normalization as in MNE
pos <- pos - apply(pos, 1, min)
pos <- pos / apply(pos, 1, function(x) {max(x, 1e-16)})

# we set rgb values
evoked_df$r <- pos[, 1]
evoked_df$g <- pos[, 2]
evoked_df$b <- pos[, 3]

ggplot(
  data = evoked_df,
  mapping = aes(x = time, color = channel, y = observation)) +
  geom_line(mapping = aes(group = channel,
                          color = rgb(r, g, b)), size = 0.8, alpha=0.7) +
  facet_wrap(
    ~ch_type,
    nrow = 3,
    scales = "free",
    strip.position = "left",
    labeller = as_labeller(c(eeg = "EEG [mV]", grad = "GRAD [fT/cm]", mag = "MAG [fT]"))) +
  theme_minimal() +
  guides(color = F) +
  labs(x = 'time [ms]', y = NULL) +
  theme(text = element_text(family = "Helvetica", size = 24))

