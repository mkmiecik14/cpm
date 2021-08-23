# R preparation
# Matt Kmiecik
# Started 27 May 2021

# Purpose: to prepare the R workspace for processing and analysis
# Any global variables, functions, processes used across multiple 
# R scripts should be placed here

# Libraries ----
library(tidyverse); library(readxl); library(Hmisc)
library(R.matlab)



# Plotting tools ----
# use geom_flat_violin()
# see: https://neuroconscience.wordpress.com/2018/03/15/introducing-raincloud-plots/
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")
library(ggrepel)      # Plotting tool for ggplot2
library(patchwork)

# Colors
library(ghibli) # https://github.com/ewenme/ghibli
library(RColorBrewer)
rdgy_pal <- brewer.pal(11, "RdGy")
library(wesanderson)



