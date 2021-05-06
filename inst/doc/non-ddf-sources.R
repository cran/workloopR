## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----package_loading, message=FALSE, warning=FALSE----------------------------
library(workloopR)
library(magrittr)
library(ggplot2)

## ----get_data-----------------------------------------------------------------
## Load in the work loop example data from workloopR
workloop_dat <-
  system.file(
    "extdata",
    "workloop.ddf",
    package = 'workloopR') %>%
  read_ddf(phase_from_peak = TRUE) %>%
  fix_GR(GR = 2)

## First we'll extract Time
Time <- workloop_dat$Time
## Now Position
Position <- workloop_dat$Position
## Force
Force <- workloop_dat$Force
## Stimulation
Stim <- workloop_dat$Stim

## Put it all together as a data.frame
my_data <- data.frame(Time = Time,
                      Position = Position,
                      Force = Force,
                      Stim = Stim)

head(my_data)

## ----as_mus_basic-------------------------------------------------------------
## Put it together
my_muscle_stim <- as_muscle_stim(x = my_data,
                                 type = "workloop",
                                 sample_frequency = 10000)

## Data are stored in columns and basically behave as data.frames
head(my_muscle_stim)

ggplot(my_muscle_stim, aes(x = Time, y = Position)) +
  geom_line() + 
  labs(y = "Position (mm)", x = "Time (secs)") +
  ggtitle("Time course of length change") +
  theme_bw()

## ----attributes---------------------------------------------------------------
str(attributes(my_muscle_stim))

## ----add_file_id--------------------------------------------------------------
## This time, add the file's name via "file_id"
my_muscle_stim <- as_muscle_stim(x = my_data,
                                 type = "workloop",
                                 sample_frequency = 10000,
                                 file_id = "workloop123")

## For simplicity, we'll just target the file_id attribute directly instead of 
## printing all attributes again
attr(my_muscle_stim, "file_id")



## -----------------------------------------------------------------------------
names(attributes(workloop_dat))

## -----------------------------------------------------------------------------
str(attributes(workloop_dat))

