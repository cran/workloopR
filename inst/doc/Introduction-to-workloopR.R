## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----a_p_single_file----------------------------------------------------------
library(workloopR)

## import the workloop.ddf file included in workloopR
wl_dat <-read_ddf(system.file("extdata", "workloop.ddf", 
                              package = 'workloopR'),
                  phase_from_peak = TRUE)

## select cycles 3 through 5 using a peak-to-peak definition
wl_selected <- select_cycles(wl_dat, cycle_def = "p2p", keep_cycles = 3:5)

## run the analysis function and get the full object
wl_analyzed <- analyze_workloop(wl_selected, GR = 2)
## for brevity, the print() method for this object produces a simple output
wl_analyzed
## but see the structure for the full output, e.g.
#str(wl_analyzed)

## or run the analysis but get the simplified version
wl_analyzed_simple <- analyze_workloop(wl_selected, simplify = TRUE, GR = 2)
wl_analyzed_simple

## ----a_p_batch_files----------------------------------------------------------
## batch read and analyze files included with workloopR
analyzed_wls <- read_analyze_wl_dir(system.file("extdata/wl_duration_trials",
                                               package = 'workloopR'),
                                   cycle_def = "p2p",
                                   keep_cycles = 2:4,
                                   phase_from_peak = TRUE
                                   )

## now summarize
summarized_wls <- summarize_wl_trials(analyzed_wls)
summarized_wls

## ----data_import--------------------------------------------------------------
library(workloopR)

## import the workloop.ddf file included in workloopR
wl_dat <-read_ddf(system.file("extdata", "workloop.ddf", 
                              package = 'workloopR'),
                  phase_from_peak = TRUE)

## muscle_stim objects have their own print() and summary() S3 methods
## for example:
summary(wl_dat) # some handy info about the imported file

## see the first few rows of data stored within
head(wl_dat)

## ----attributes---------------------------------------------------------------
## names(attributes(x) gives a list of all the attributes' names
names(attributes(wl_dat))

## take a look at the stimulation protocol
attr(wl_dat, "protocol_table")

## at what frequency were cyclic changes to Position performed?
attr(wl_dat, "cycle_frequency")

## at what frequency were data recorded?
attr(wl_dat, "sample_frequency")

## ----transformations----------------------------------------------------------
## this multiples Force by 2
## and multiplies Position by (1/2)
wl_fixed  <- fix_GR(wl_dat, GR = 2)

# quick check:
max(wl_fixed$Force)/max(wl_dat$Force)       #5592.578 / 2796.289 = 2
max(wl_fixed$Position)/max(wl_dat$Position) #1.832262 / 3.664524 = 0.5

