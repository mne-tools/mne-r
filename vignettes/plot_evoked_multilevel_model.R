## ----setup, include = T, echo = F----------------------------------------
library(tidyverse)
library(mne)
library(lme4)
library(merTools)

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
events <- mne$find_events(raw)
storage.mode(events) <- "integer"  # R gets the events as floats.

tmin <- -0.2
tmax <- 0.5
baseline <- reticulate::tuple(NULL, 0)
event_id <- list("aud/l" = 1L, "vis/l" = 3L)
picks <- mne$pick_channels(raw$ch_names, list('MEG 1332'))
epochs <- mne$Epochs(raw = raw, events = events, event_id = event_id,
                     tmin = tmin, tmax = tmax,
                     picks = picks %>% as.integer(),
                     baseline = baseline, reject = NULL, preload = T) 

## ------------------------------------------------------------------------
# use MNE method
epochs_df <- mne::get_data_frame(epochs)  # long

## ------------------------------------------------------------------------
mod1 <- lmer(observation ~ 1 + condition + (1 + condition | time),
             data = epochs_df)
mod1 %>% summary() %>% print()

## ---- fig.width=8, fig.heigh=6-------------------------------------------
probe <- expand.grid(
  condition = c("aud/l", "vis/l") %>% as.factor(),
  time = epochs_df$time %>% unique()
)

pred_mod1 <- predict(mod1, probe)
probe$pred <- pred_mod1 

ggplot(data = epochs_df,
       mapping = aes(x = time, y = observation,
                     group = interaction(condition, epoch),
                     color = condition)) +
  geom_line(size = 0.3, alpha = 0.4) +
  geom_line(
    size = 1.5, data = probe,
    mapping = aes(x = time, y = pred, group = condition,
                  color = condition)) +
  theme_minimal() +
  theme(text = element_text(size = 24, family = "Helvetica")) +
  labs(x = "times [ms]",
       y = "predicted GRAD signal [fT/cm]") +
  ylim(-300, 300)

## ------------------------------------------------------------------------
pred_interval_mod1 <- predictInterval(
  merMod = mod1, newdata = probe, which = "full", level = 0.95,
  n.sims = 1000, stat = "mean", type = "linear.prediction",
  returnSims = T, seed = 42
)

probe_int <- bind_cols(
  probe, pred_interval_mod1)

## ---- fig.width=8, fig.height=4------------------------------------------

ggplot(
    data = probe_int,
    mapping = aes(x = time, y = pred, group = condition,
                  color = condition, ymin = lwr, ymax = upr)) +
  geom_ribbon(mapping = aes(fill = condition), alpha = 0.1) +
  geom_line(size = 1) +
  theme_minimal() +
  theme(text = element_text(size = 20, family = "Helvetica")) +
  labs(x = "times [ms]",
       y = "predicted GRAD signal [fT/cm]") +
  facet_wrap(~condition) +
  guides(color = F, fill = F)

## ------------------------------------------------------------------------
# let's subsample a few bootstrap simulations.
idx <- sample(1:1000, size = 100)

pred_sims <- attr(pred_interval_mod1, "sim.results")[,idx] %>%
  as.data.frame() %>%
  gather(key = "sim", value = "pred_hat")

pred_sims$sim <- pred_sims$sim %>% as.factor()
pred_sims$time <- probe_int$time
pred_sims$condition <- probe_int$condition
pred_sims$pred <- probe_int$pred

## ---- fig.width=8, fig.height=4------------------------------------------

ggplot(
    data = pred_sims,
    mapping = aes(x = time, y = pred,
                  group = condition,
                  color = condition)) +
  geom_line(
    alpha = 0.05, mapping = aes(
      y = pred_hat,
      group = interaction(sim, condition))) +
  stat_summary(
      fun.ymin = function(x){quantile(x, 0.025)},
      fun.ymax = function(x){quantile(x, 0.975)},
      mapping = aes(x = time,
                    y = pred_hat,
                    fill = condition,
                    group = condition,
                    color = condition),
      geom = "ribbon", alpha = 0.1) +
  geom_line(size = 1.5) +
  theme_minimal() +
  theme(text = element_text(size = 20, family = "Helvetica")) +
  labs(x = "times [ms]",
       y = "predicted GRAD signal [fT/cm]") +
  facet_wrap(~condition) +
  guides(color = F, fill = F)

