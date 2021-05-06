## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----package_loading, message=FALSE, warning=FALSE----------------------------
library(workloopR)
library(magrittr)
library(ggplot2)

## ----data_import--------------------------------------------------------------
## The file twitch.ddf is included and therefore can be accessed via
## system.file("subdirectory","file_name","package") . We'll then use
## read_ddf() to import it, creating an object of class "muscle_stim".
twitch_dat <-
  system.file(
    "extdata",
    "twitch.ddf",
    package = 'workloopR') %>%
  read_ddf()


## ----intial_plot--------------------------------------------------------------
twitch_dat %>%
  ggplot(aes(x = Time, y = Force)) +
    geom_line() +
    ylab("Force (mN)") +
    xlab("Time (sec)") +
    theme_minimal()


## ----data_cleaning------------------------------------------------------------
## Re-plot
twitch_dat %>%
  ggplot(aes(x = Time, y = Force)) +
    geom_line(lwd = 1) +
    xlim(0.075, 0.2) +
    ylim(200, 450) +
    xlab("Time (sec)") +
    ylab("Force (mN)") +
    theme_minimal()


## ----twitch_analysis----------------------------------------------------------
## Run the isometric_timing() function
twitch_analyzed <-
  twitch_dat %>%
  isometric_timing()

twitch_analyzed

## ----twitch_rising_custom-----------------------------------------------------
## Change rising supply a custom set of force development set points
twitch_rising_custom <-
  twitch_dat %>%
  isometric_timing(rising = c(5, 10, 25, 50, 75, 95))

## The returned `data.frame` contains the timing and force magnitudes
## of these set points in the "..._rising_..." columns
twitch_rising_custom

## ----tetanus------------------------------------------------------------------
tetanus_analyzed <-
  system.file(
    "extdata",
    "tetanus.ddf",
    package = 'workloopR') %>%
  read_ddf() %>%
  isometric_timing(rising = c(25, 50, 75))

tetanus_analyzed

## ----twitch_intervals---------------------------------------------------------
## Time to peak force from stimulation
twitch_analyzed$time_peak - twitch_analyzed$time_stim

## ----annotated_plot-----------------------------------------------------------
## Create a color pallete
## Generated using `viridis::viridis(6)`
## We use hard-coded values here just to avoid extra dependencies 
colz <- c("#440154FF","#414487FF","#2A788EFF",
          "#22A884FF","#7AD151FF","#FDE725FF")

twitch_dat %>%
 ggplot(aes(x = Time, y = Force)) +
   geom_line(lwd = 1) +
   xlim(0.075, 0.2) +
   ylim(200, 450) +
   xlab("Time (sec)") +
   ylab("Force (mN)") +
   geom_point(x = twitch_analyzed$time_stim, 
              y = twitch_analyzed$force_stim,
              color = colz[1], size = 3) +
   geom_point(x = twitch_analyzed$time_peak, 
              y = twitch_analyzed$force_peak,
              color = colz[4], size = 3) +
   geom_point(x = twitch_analyzed$time_rising_10, 
              y = twitch_analyzed$force_rising_10,
              color = colz[2], size = 3) +
   geom_point(x = twitch_analyzed$time_rising_90, 
              y = twitch_analyzed$force_rising_90,
              color = colz[3], size = 3) +
   geom_point(x = twitch_analyzed$time_relaxing_90, 
              y = twitch_analyzed$force_relaxing_90,
              color = colz[5], size = 3) +
   geom_point(x = twitch_analyzed$time_relaxing_50, 
              y = twitch_analyzed$force_relaxing_50,
              color = colz[6], size = 3) +
   theme_minimal()

