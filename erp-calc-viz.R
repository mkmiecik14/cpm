# Computing and visualizing ERPs from Kevin's CPM task 
# Matt Kmiecik
# Started 23 AUG 2021

# Prepares workspace ----
source("r-prep.R")
source("topo_tools.R") # prepares topo tools 

# Loads electrode locations ----
this_elec_locs <- read_excel("../data/elec-locs.xlsx")

# Loads data from matlab ----
baseline  <-  readMat("../output/baseline.mat")
cpm       <-  readMat("../output/cpm.mat")
postcpm   <-  readMat("../output/postcpm.mat")

# cleans it up
baseline_data <- 
  baseline$baseline[16][[1]] %>% # subsets out the data
  array_branch(3) %>% # creates list on third dim = epochs
  map(~as_tibble(t(.x))) %>% # creates tibble on transposed mat = elecs on cols
  # inserts time and concatenates row-wise
  map_dfr(~mutate(.x, time = c(baseline$baseline[15][[1]])), .id = "trial")
colnames(baseline_data) <- c("trial", this_elec_locs$labels, "time") # adds colnames

cpm_data <-
  cpm$cpm[16][[1]] %>% # subsets out the data
  array_branch(3) %>% # creates list on third dim = epochs
  map(~as_tibble(t(.x))) %>% # creates tibble on transposed mat = elecs on cols
  # inserts time and concatenates row-wise
  map_dfr(~mutate(.x, time = c(cpm$cpm[15][[1]])), .id = "trial")
colnames(cpm_data) <- c("trial", this_elec_locs$labels, "time") # adds colnames

postcpm_data <-
  postcpm$postcpm[16][[1]] %>% # subsets out the data
  array_branch(3) %>% # creates list on third dim = epochs
  map(~as_tibble(t(.x))) %>% # creates tibble on transposed mat = elecs on cols
  # inserts time and concatenates row-wise
  map_dfr(~mutate(.x, time = c(postcpm$postcpm[15][[1]])), .id = "trial")
colnames(postcpm_data) <- c("trial", this_elec_locs$labels, "time") # adds colnames

# combines all data into one df
trial_data <-
  bind_rows(baseline_data, cpm_data, postcpm_data, .id = "cond") %>%
  mutate(
    cond = case_when(
      cond == 1 ~ "baseline",
      cond == 2 ~ "cpm",
      cond == 3 ~ "postcpm"
    )
    )

# long format
trial_data_long <- 
  trial_data %>%
  pivot_longer(c(-cond, -trial, -time), names_to = "elec", values_to = "mv")

# grandaverages
trial_data_sum <- 
  trial_data_long %>%
  filter(elec %nin% c("Stimulator", "EMG")) %>%
  group_by(cond ,time, elec) %>%
  summarise(
    m = mean(mv), 
    sd = sd(mv), 
    n = n(),
    sem = sd/sqrt(n),
    ll = quantile(mv, .025),
    ul = quantile(mv, .975)
  ) %>%
  ungroup()

# GRANDAVERAGE PLOTs
ggplot(
  trial_data_sum %>% filter(elec == "Y1"), 
  aes(time, m, group = cond, color = cond, fill = cond)
  ) +
  geom_ribbon(aes(ymin = m-sd, ymax = m+sd), alpha = 1/3) +
  geom_line() +
  scale_x_continuous(
    breaks = seq(-200, 500, 100),
    limits = c(-200, 500),
    minor_breaks = NULL
    ) +
  theme_minimal() +
  labs(x = "Time", y = "Microvolts", caption = "SD errorbars.") +
  facet_grid(elec~cond) +
  scale_color_manual(values = ghibli_palettes$PonyoMedium[3:5]) +
  scale_fill_manual(values =  ghibli_palettes$PonyoMedium[3:5]) +
  theme(legend.position = "bottom")

# 50 - 150ms window mean amplitude  
trial_data_long %>%
  filter(between(time, 50, 150)) %>% # looking at ERP I chose between 50 - 150ms
  group_by(trial, elec) %>%
  summarise(
    M = mean(mv), 
    N = n()
  ) %>%
  ungroup()





# long-format
baseline_data_long <- 
  baseline_data %>%
  pivot_longer(c(-trial, -time), names_to = "elec", values_to = "mv")

# GRANDAVERAGES
baseline_sum <-
  baseline_data_long %>%
  group_by(time, elec) %>%
  summarise(
    m = mean(mv), 
    sd = sd(mv), 
    n = n(),
    sem = sd/sqrt(mv),
    ll = quantile(mv, .025),
    ul = quantile(mv, .975)
  ) %>%
  ungroup()

ggplot(
  baseline_sum %>% filter(elec %nin% c("Stimulator", "EMG")), 
  aes(time, m, group = elec)
  ) +
  geom_line() +
  scale_x_continuous(breaks = seq(-200, 800, 100)) +
  theme_minimal()

# mean amplitude between 50 - 150ms
# first summary is to average across the window within each epoch
baseline_50_150_sum1 <-
  baseline_data_long %>%
  filter(between(time, 50, 150)) %>% # looking at ERP I chose between 50 - 150ms
  group_by(trial, elec) %>%
  summarise(
    M = mean(mv), 
    N = n()
  ) %>%
  ungroup()

# second summary is to average across epochs
baseline_50_150_sum2 <-
  baseline_50_150_sum1 %>%
  group_by(elec) %>%
  summarise(
    m = mean(M), 
    sd = sd(M), 
    n = n(),
    sem = sd/sqrt(n),
    ll = quantile(M, .025),
    ul = quantile(M, .975)
  ) %>%
  ungroup()

# Baseline original
baseline_orig <-  
  baseline_50_150_sum2 %>%
  filter(elec %nin% c("Stimulator", "EMG")) %>%
  left_join(., elec_locs, by = c("elec" = "labels"))

# Baseline interpolated
baseline_interp <- topo_interp(data = baseline_orig, dv = "m", gridRes = 67)

################################################################################
# Settings for topo plots to make them uniform 
maskRing <- circleFun(diameter = 1.42) # creates the mask
contour_alpha <- 1/3
contour_color <- "black"
headshape_size <- .25 # used to be .5
electrode_size <- 1.25
nose_size <- .25 # used to be .5
bwidth <- .5 # width of colorbar
bheight <- .1 # height of colorbar
################################################################################


# PLOT
this_interp <- baseline_interp 
this_orig <- baseline_orig
this_min <- min(this_orig$m) # minumum for legend
this_max <- max(this_orig$m) # maximum for legend
minmax <- max(abs(this_min), abs(this_max))

ggplot(this_interp, aes(x = x, y = y, fill = m)) +
  coord_equal() + # equalizes coordantes
  geom_raster(interpolate = TRUE) + # basis of the topo
  stat_contour(aes(z = m), colour = contour_color, alpha = contour_alpha) + # contour lines
  theme_topo() + # topo theme is added (white background etc.)
  # plots headshape
  geom_path(data = headShape, aes(x, y, z = NULL, fill = NULL), size = headshape_size) +
  geom_point(data = this_orig, aes(x, y), size = electrode_size) + # plots elecs
  scale_shape_manual(values = c(19)) + 
  # plots nose
  geom_path(data = nose, aes(x, y, z = NULL, fill = NULL), size = nose_size) +
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
    colours = rev(brewer.pal(n = 11, "BrBG")), # jet_colors(10)
    # may want to hard code these for comparison across groups
    limits = c(-10, 10), # these should be determined from the uninterpolated data (i think)
    breaks = c(-10, 0, 10), labels = c(-10, 0, 10),
    guide = "colourbar",
    oob = squish,
    name = "mv"
  ) + 
  guides(
    shape = FALSE, 
    fill = guide_colourbar(
      title.position = "top", 
      title.hjust = 0.5, 
      frame.colour = "black", 
      ticks.colour = "black", 
      barwidth = unit(bwidth, "in"),
      barheight = unit(bheight, "in")
    )
  ) +
  theme(legend.position = "bottom")

