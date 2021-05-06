## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----package_loading, message=FALSE, warning=FALSE----------------------------
library(workloopR)
library(magrittr)
library(purrr)

## -----------------------------------------------------------------------------
workloop_trials_list<-
  system.file(
    "extdata/wl_duration_trials",
    package = 'workloopR') %>%
  read_ddf_dir(phase_from_peak = TRUE)

workloop_trials_list[1:2]

## -----------------------------------------------------------------------------
analyzed_wl_list<-
  system.file(
    "extdata/wl_duration_trials",
    package = 'workloopR') %>%
  read_analyze_wl_dir(sort_by = 'file_id',
                      phase_from_peak = TRUE,
                      cycle_def = 'lo',
                      keep_cycles = 3)

analyzed_wl_list[1:2]

## -----------------------------------------------------------------------------
analyzed_wl_list %>%
  summarize_wl_trials

## -----------------------------------------------------------------------------
non_ddf_list<-
  # Generate a vector of file names
  system.file(
    "extdata/twitch_csv",
    package = 'workloopR') %>%
  list.files(full.names = T) %>%
  # Read into a list of data.frames
  map(read.csv) %>%
  # Coerce into a workloop object
  map(as_muscle_stim, type = "twitch")

## -----------------------------------------------------------------------------
non_ddf_list<-
  non_ddf_list %>%
  map(~{
    attr(.x,"stimulus_width")<-0.2
    attr(.x,"stimulus_offset")<-0.1
    return(.x)
  }) %>%
  map(fix_GR,2)

## -----------------------------------------------------------------------------
file_ids<-paste0("0",1:4,"-",2:5,"mA-twitch.csv")

non_ddf_list<-
  non_ddf_list %>%
  map2(file_ids, ~{
    attr(.x,"file_id")<-.y
    return(.x)
  })

non_ddf_list

## -----------------------------------------------------------------------------
non_ddf_list %>%
  map_dfr(isometric_timing)

