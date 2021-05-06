## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----package_loading, message=FALSE, warning=FALSE----------------------------
library(workloopR)
library(magrittr)
library(ggplot2)
library(dplyr)

## ----data_import--------------------------------------------------------------
## The file workloop.ddf is included and therefore can be accessed via
## system.file("subdirectory","file_name","package") . We'll then use
## read_ddf() to import it, creating an object of class "muscle_stim".
## fix_GR() multiplies Force by 2 and divides Position by 2
workloop_dat <-
  system.file(
    "extdata",
    "workloop.ddf",
    package = 'workloopR') %>%
  read_ddf(phase_from_peak = TRUE) %>%
  fix_GR(GR = 2)

summary(workloop_dat)


## ----intial_plot--------------------------------------------------------------
scale_position_to_force <- 3000

workloop_dat %>%
  # Set the x axis for the whole plot
  ggplot(aes(x = Time)) +
  # Add a line for force
  geom_line(aes(y = Force, color = "Force"), 
            lwd = 1) +
  # Add a line for Position, scaled to approximately the same range as Force
  geom_line(aes(y = Position * scale_position_to_force, color = "Position")) +
  # For stim, we only want to plot where stimulation happens, so we filter the data
  geom_point(aes(y = 0, color = "Stim"), size = 1, 
             data = filter(workloop_dat, Stim == 1)) +
  # Next we add the second y-axis with the corrected units
  scale_y_continuous(sec.axis = sec_axis(~ . / scale_position_to_force, name = "Position (mm)")) +
  # Finally set colours, labels, and themes
  scale_color_manual(values = c("#FC4E2A", "#4292C6", "#373737")) +
  labs(y = "Force (mN)", x = "Time (secs)", color = "Parameter:") +
  ggtitle("Time course of \n work loop experiment") +
  theme_bw() +
  theme(legend.position = "bottom", legend.direction = "horizontal")

## ----select_cycles------------------------------------------------------------
## Select cycles
workloop_selected <- 
  workloop_dat %>%
  select_cycles(cycle_def="lo", keep_cycles = 4:6)

summary(workloop_selected)

attr(workloop_selected, "retained_cycles")

## ----work_loop_fig------------------------------------------------------------
workloop_selected %>%
  ggplot(aes(x = Position, y = Force)) +
  geom_point(size=0.3) +
  labs(y = "Force (mN)", x = "Position (mm)") +
  ggtitle("Work loop") +
  theme_bw()


## ----analyze_workloop---------------------------------------------------------
## Run the analyze_workloop() function
workloop_analyzed <-
  workloop_selected %>%
  analyze_workloop(GR = 1)

## Produces a list of objects. 
## The print method gives a simple output:
workloop_analyzed

## How is the list organized?
names(workloop_analyzed)

## ----metrics_for_cycle--------------------------------------------------------
## What is work for the second cycle?
attr(workloop_analyzed$cycle_b, "work")

## What is net power for the third cycle?
attr(workloop_analyzed$cycle_c, "net_power")

## ----cycle_a_organization-----------------------------------------------------
str(workloop_analyzed$cycle_a)

## ----instant_power_plot-------------------------------------------------------
workloop_analyzed$cycle_b %>%
  ggplot(aes(x = Percent_of_Cycle, y = Inst_Power)) +
  geom_line(lwd = 1) +
  labs(y = "Instantaneous Power (W)", x = "Percent cycle") +
  ggtitle("Instantaneous power \n during cycle b") +
  theme_bw()

## ----simplify_TRUE------------------------------------------------------------
workloop_analyzed_simple <-
  workloop_selected %>%
  analyze_workloop(GR = 1, simplify = TRUE)

## Produces a simple data.frame:
workloop_analyzed_simple
str(workloop_analyzed_simple)

## ----select_cycles_defintions-------------------------------------------------
## Select cycles 4:6 using lo
workloop_dat %>%
  select_cycles(cycle_def="lo", keep_cycles = 4:6) %>%
  ggplot(aes(x = Time, y = Position)) +
  geom_line() +
  theme_bw()

## Select cycles 4:6 using p2p
workloop_dat %>%
  select_cycles(cycle_def="p2p", keep_cycles = 4:6) %>%
  ggplot(aes(x = Time, y = Position)) +
  geom_line() +
  theme_bw()
## here we see that via 'p2p' the final cycle is ill-defined because the return
## to L0 is considered a cycle. Using a p2p definition, what we actually want is
## to use cycles 3:5 to get the final 3 full cycles:
workloop_dat %>%
  select_cycles(cycle_def="p2p", keep_cycles = 3:5) %>%
  ggplot(aes(x = Time, y = Position)) +
  geom_line() +
  theme_bw()

## this difficulty in defining cycles may be more apparent by first plotting the 
## cycles 1:6, e.g.
workloop_dat %>%
  select_cycles(cycle_def="p2p", keep_cycles = 1:6) %>%
  ggplot(aes(x = Time, y = Position)) +
  geom_line() +
  theme_bw()


