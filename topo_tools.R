# Topo tools
# HEAVILY inspired by Matt Craddock's code (i.e., credit should go to him):
# https://www.mattcraddock.com/blog/2017/02/25/erp-visualization-creating-topographical-scalp-maps-part-1/
# Matt Kmiecik
# Started 23 AUG 2021

# Packages ----
library(akima)
library(scales)
library(mgcv)
library(readxl)
library(RColorBrewer)
library(dplyr) # for pipe
library(ggplot2)

# Electrode Locations ----
# Loads in electrode positions and converts from polar to cartesian
# note: use little x and little y for the coordinates, not X and Y from MATLAB
elec_locs <- 
  read_excel(path = "../data/elec-locs.xlsx") %>%
  filter(labels %nin% c("Stimulator", "EMG")) %>%
  mutate(across(c(-labels, -type, -ref), .fns = ~ as.numeric(.x))) %>%
  mutate(
    radianTheta = pi/180*theta, 
    x = radius*sin(radianTheta), 
    y = radius*cos(radianTheta)
  )

# theme_topo() ----
# Plotting tools for the topo map with head shape:
theme_topo <- 
  function(base_size = 12){
    theme_bw(base_size = base_size) %+replace%
      theme(
        rect = element_blank(),
        line = element_blank(), 
        axis.text = element_blank(),
        axis.title = element_blank()
      )
}

# circleFun() ----
# function for drawing head for topo plots
circleFun <- 
  function(center = c(0,0), diameter = 1, npoints = 100) {
    r = diameter / 2
    tt <- seq(0, 2*pi, length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
}

# Headshape coordinates are determined
headShape <- 
  circleFun(
    c(0, 0), 
    diameter = round(max(elec_locs$x)), 
    npoints = 100
  )

# Nose coordinates
nose <- data.frame(x = c(-0.075, 0, .075), y = c(.495, .575, .495))

# Produces blank topography map with electrodes
ggplot(headShape,aes(x,y)) +
  geom_path() +
  geom_text(data = elec_locs, aes(x, y, label = labels)) +
  geom_line(data = nose, aes(x, y, z = NULL)) +
  theme_topo() +
  coord_equal()

# Function for interpolating
# data is a data frame containing electrode coordinates at x and y
# currently is accepting dB
# usage topo_terp(data = data, dv = "dB", gridRes = 67)
topo_interp <- 
  function(data = data, dv = dv, gridRes = 67){
    meas <- data[[dv]] # allows for assignment of different dependent variables
    tmp_topo <- 
      with(
        data,
        akima::interp(
          x = x, 
          y = y, 
          z = meas, # let's see if this works
          xo = seq(min(x)*2, max(x)*2, length = gridRes),
          yo = seq(min(y)*2, max(y)*2, length = gridRes),
          linear = FALSE,
          extrap = TRUE
        )
      )
    # Creating a matrx that is x by y filled with z
    interp_topo <- data.frame(x = tmp_topo$x, tmp_topo$z)
    names(interp_topo)[1:length(tmp_topo$y) + 1] <- tmp_topo$y
    interp_topo <- interp_topo %>% gather(key = y, value = !!dv, -x, convert = TRUE)
    
    # mark grid elements that are outside of the plotting circle
    interp_topo$incircle <- sqrt(interp_topo$x^2 + interp_topo$y^2) < .7 # original value was .7
    interp_topo <- interp_topo[interp_topo$incircle,] #remove the elements outside the circle
    return(interp_topo)
}

# BINWIDTH NEEDS TO BE DYNAMIC!
topo_plot <- 
  function(interp_data = interp_data, orig_data = orig_data, dv = dv, min = min, max = max){
  
    maskRing <- circleFun(diameter = 1.42) # creates the mask
    rdpu_pal <- brewer.pal(9, "RdPu") # color palette
    
    ggplot(interp_data, aes_(x = quote(x), y = quote(y), fill = as.name(dv))) +
      coord_equal() + # equalizes coordantes
      geom_raster() + # basis of the topo
      stat_contour(aes_(z = as.name(dv)), colour = "black") + # contour lines
      theme_topo() + # topo theme is added (white background etc.)
      # plots headshape
      geom_path(data = headShape, aes(x, y, z = NULL, fill = NULL), size = 1.5) +
      geom_point(data = orig_data, aes(x, y), size = 1) + # plots elecs
      # plots nose
      geom_path(data = nose, aes(x, y, z = NULL, fill = NULL), size = 1.5) +
      # creates a mask to remove points outside defined circle
      geom_path(
        data = maskRing,
        aes(x, y, z = NULL, fill = NULL),
        colour = "white",
        size = 6
      ) +
      # colors here
      # note: oob = squish forces everything outside the colour limits to equal
      # nearest colour boundary (i.e., below min colours = min colour)
      scale_fill_gradientn(
        colours = rdpu_pal, # jet_colors(10)
        # may want to hard code these for comparison across groups
        limits = c(min, max), # these should be determined from the uninterpolated data (i think)
        guide = "colourbar",
        oob = squish
      )
}

# For plotting the group topography
group_topo <- 
  function(interp_data = data1, orig_data = data2, freqs = x, dv = y, unit = z){
    
    this_data_interp  <- interp_data %>% filter(freq == freqs)
    this_data_orig    <- orig_data %>% filter(freq == freqs)
    this_min <- min(this_data_orig[dv]) # minumum for legend
    this_max <- max(this_data_orig[dv]) # maximum for legend
    topo_plot(this_data_interp, this_data_orig, dv = dv, min = this_min, max = this_max) + 
      labs(title = paste0(freqs,"Hz"), fill = unit) +
      facet_grid(group~stim) +
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.width = unit(.5, 'in'),
        legend.key.height = unit(.2, 'in')
      )  
}