library(tidyverse)
library(reticulate)

mne <- NULL

.onLoad <- function(libname, pkgname) {
  # push MNE into global environment
  mne <<- reticulate::import("mne", delay_load = TRUE)
  cat(
    sprintf(
      "Importing MNE version=%s, path='%s'\n", mne$`__version__`,
      mne$`__path__`))
  assign("mne", mne, .GlobalEnv)
}


#' Get data from MNE into R data.rame
#'
#' \code{get_data_frame} helps importing MNE data structures
#'
#' The code will call the `to_data_frame_method` of the MNE instanc
#' passed. This makes it easy to use MNE-objects in R. The below
#' Type definitions are expressed in Python types. Please refer to
#' the reticulate documentation to learn about R-to-Python conversion
#' rules.
#' Note that this function requires, next ot MNE, a working Pandas
#' installation
#'
#' @param inst An instance of MNE data containsers, e.g,
#'        \code{mne$Epochs}, \code{mne$io$Raw}, \code{mne$Evoked}.
#' @param picks a zero-indexed integer array, a string, list, slice or
#'        None.
#' @param index The columns to be uesed as pandas index. tuple of str
#'        or None
#' @param scaling_time Scaling to be applied to time units. Float.
#' @param scalings Scaling to be applied to the channels picked.
#' @param copy Whether to make a copy of the data.
#' @param start If it is a Raw object, this defines a starting index
#'        for creating the dataframe from a slice. The times will be
#'        interpolated from the index and the sampling rate of the
#'        signal. Int or None.
#' @param stop If it is a Raw object, this defines a stop index for
#'        creating the dataframe from a slice. The times will be
#'        interpolated from the index and the sampling rate of the
#'        signal. Int or None.
#' @param long_format If True, the dataframe is returned in long
#'        format where each row is one observation of the signal at a
#'        unique coordinate of channels, time points, epochs and
#'        conditions. The number of factors depends on the data
#'        container. For convenience, a ch_type column is added when
#'        using this option that will facilitate subsetting the
#'        resulting dataframe. Defaults to False.
#' @return Returns a data.frame. The layout depends on the options.
#'         and the type of instance.
#' @export
get_data_frame <- function(inst, picks = NULL, index = NULL,
                           scaling_time = 1e3, scalings = NULL,
                           copy = T, start = NULL, stop = NULL,
                           long_format = T) {
  # handle MNE python version
  inspect <- reticulate::import("inspect")
  to_df_args <- inspect$getargspec(inst$to_data_frame)$args

  if("long_format" %in% to_df_args & long_format){
    out <- inst$to_data_frame(
      picks = picks, index = index, scaling_time = scaling_time,
      scalings = scalings, copy = copy, start = start, stop = stop,
      long_format = long_format)

    } else if (!("long_format" %in% to_df_args) & long_format) {
      out <- get_long_format(...)
    } else {
    out <- inst$to_data_frame(
      picks = picks, index = index, scaling_time = scaling_time,
      scalings = scalings, copy = copy, start = start, stop = stop)
    }

  return(out)
}

get_long_format <- function (inst, picks, index, scaling_time,
                             scalings, copy, start, stop){
  out <- inst$to_data_frame(
    picks = picks, index = index, scaling_time = scaling_time,
    scalings = scalings, copy = copy, start = start, stop = stop)
  # Order used in MNE
  # Raw + evoked
  # c('time',  'channel',  'observation', 'ch_type')
  # Epochs
  # c('condition', "epoch", 'time',  'channel',  'observation',
  #    'ch_type')

  # common steps
  ch_idx <- 0L:(inst$info["nchan"] - 1L)
  ch_type <- sapply(
    ch_idx, function(.) mne$io$pick$channel_type(inst$info, .)) %>%
    as.factor()

  if ("mne.epochs.BaseEpochs" %in% class(inst)) {
    observation <- out %>%
      as.matrix() %>%
      t() %>%
      matrix(nrow =  prod(out %>% dim(.)))
    channel <- out %>% colnames() %>% rep(., times = dim(out)[1])
    mindex <- attr(
      out, "pandas.index")$values %>% reticulate::py_to_r()

    condition <- sapply(mindex, function(.) .[[1]])
    epoch <- sapply(mindex, function(.) .[[2]])
    time <- sapply(mindex, function(.) .[[3]])

    # we mostly trust base-R grid expansion magic ...
    out_df <- data.frame(
      condition = condition,
      epoch = epoch %>% as.factor() %>% rep(each = dim(out)[2]),
      time = time %>% as.numeric() %>% rep(each = dim(out)[2]),
      channel = channel,
      observation = observation,
      ch_type = ch_type
    )
  } else {
    observation <- as.vector(t(out))
    channel <-  out %>% colnames()
    time <- out %>%
      rownames() %>%
      as.numeric() %>%
      rep(., each = length(channel))

    out_df <- data.frame(
      time = time,
      channel = channel,
      observation = observation,
      ch_type = ch_type
    )
  }
  # XXX source estimate class is not yet handled.
  # Add error or support.
  return(out_df)
}
