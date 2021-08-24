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
  geom_ribbon(aes(ymin = m-sem, ymax = m+sem), alpha = 1/3) +
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

# SEM error bars ELECTRODE Y1 where effect is greatest
ggplot(
  trial_data_sum %>% filter(elec == "Y1", cond %nin% c("cpm")), 
  aes(time, m, group = cond, color = cond, fill = cond)
) +
  geom_vline(xintercept = 0, alpha = 1/3) +
  geom_hline(yintercept = 0, alpha = 1/3) +
  geom_ribbon(aes(ymin = m-sem, ymax = m+sem), alpha = 1/3) +
  geom_line() +
  scale_x_continuous(
    breaks = seq(-200, 500, 100),
    limits = c(-200, 500),
    minor_breaks = NULL
  ) +
  theme_classic() +
  labs(x = "Time", y = "Microvolts", caption = "SEM errorbars.") +
  scale_color_manual(values = c(rdgy_pal[10], rdgy_pal[3])) +
  scale_fill_manual(values =  c(rdgy_pal[10], rdgy_pal[3])) +
  theme(
    legend.position = "bottom",
    axis.line = element_blank()
    )
# SVG save
ggsave(
  filename = "erp.svg",
  path = "../output/",
  width = 6.5, height = 4, units = "in"
  )

# 50 - 150ms window mean amplitude ----
# Averaging within window
trial_50_150_sum1 <- 
  trial_data_long %>%
  filter(elec %nin% c("Stimulator", "EMG")) %>%
  filter(between(time, 50, 150)) %>% # looking at ERP I chose between 50 - 150ms
  group_by(cond, trial, elec) %>%
  summarise(
    M = mean(mv), 
    N = n()
  ) %>%
  ungroup()

# Running stats here (baseline n = 131, postcpm n = 319)
# Rounds up the data and forms contrast for reg modeling
trial_50_150_data <- 
  trial_50_150_sum1 %>%
  filter(cond %in% c("baseline", "postcpm")) %>% 
  mutate(cond = factor(cond))
contrasts(trial_50_150_data$cond) <- cbind(cpm = c(-.5, .5))

# Models data
trial_50_150_mod <-
  trial_50_150_data %>%
  nest_by(elec) %>%
  mutate(mod = list(lm(M ~ 1 + cond, data = data)))

# Retrieves estimates and, corrects p values, and adds elec locs
trial_50_150_est <- 
  trial_50_150_mod %>%
  summarise(broom::tidy(mod)) %>%
  ungroup() %>%
  mutate(
    term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "condcpm" ~ "CPM",
      TRUE ~ as.character(term)
    )
  ) %>%
  group_by(term) %>%
  mutate(
    sig = p.value < .05,
    p.fdr = p.adjust(p.value, method = "fdr"), 
    sig.fdr = p.fdr < .05,
    p_cor = interaction(sig, sig.fdr),
    p_cor = case_when(
      p_cor == "TRUE.TRUE" ~ "p < .05 (FDR)",
      p_cor == "FALSE.FALSE" ~ "p > .05",
      p_cor == "TRUE.FALSE" ~ "p < .05 (uncorrected)",
      TRUE ~ as.character(p_cor)
    )
  ) %>%
  ungroup() %>%
  left_join(., elec_locs, by = c("elec" = "labels"))

# Averaging across trials
trial_50_150_sum2 <- 
  trial_50_150_sum1 %>%
  group_by(cond, elec) %>%
  summarise(
    m = mean(M), 
    sd = sd(M), 
    n = n(),
    sem = sd/sqrt(n),
    ll = quantile(M, .025),
    ul = quantile(M, .975)
  ) %>%
  ungroup() %>%
  left_join(., elec_locs, by = c("elec" = "labels"))

# Interpolated data
trial_50_150_interp <-
  trial_50_150_sum2 %>%
  split(.$cond) %>%
  map_dfr(~topo_interp(data = .x, dv = "m", gridRes = 80), .id = "cond")

################################################################################
# Settings for topo plots to make them uniform 
maskRing <- circleFun(diameter = 1.25) # creates the mask circleFun(diameter = 1.42)
contour_alpha <- 1/3
contour_color <- "black"
headshape_size <- .25 # used to be .5 .25
electrode_size <- 1.25
nose_size <- .25 # used to be .5
bwidth <- .5 # width of colorbar
bheight <- .1 # height of colorbar
################################################################################

# Individual TOPO PLOTs
this_interp <- trial_50_150_interp %>% filter(cond %nin% "cpm")
this_orig <- trial_50_150_sum2 %>% filter(cond %nin% "cpm")
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
    size = 8
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
  theme(legend.position = "bottom") +
  facet_wrap(~cond)
# Standard PDF save
ggsave(
  filename = "erp-topos.pdf",
  path = "../output/",
  width = 6.5, height = 7, units = "in"
  )



# Model TOPO PLOTs
trial_50_150_est_interp <- 
  trial_50_150_est %>%
  split(.$term) %>%
  map_dfr(~topo_interp(data = .x, dv = "estimate", gridRes = 80), .id = "term")

this_interp <- trial_50_150_est_interp
this_orig <- trial_50_150_est
this_min <- min(this_orig$estimate) # minumum for legend
this_max <- max(this_orig$estimate) # maximum for legend
minmax <- max(abs(this_min), abs(this_max))

ggplot(this_interp, aes(x = x, y = y, fill = estimate)) +
  coord_equal() + # equalizes coordantes
  geom_raster(interpolate = TRUE) + # basis of the topo
  stat_contour(aes(z = estimate), colour = contour_color, alpha = contour_alpha) + # contour lines
  theme_topo() + # topo theme is added (white background etc.)
  # plots headshape
  geom_path(data = headShape, aes(x, y, z = NULL, fill = NULL), size = headshape_size) +
  geom_point(data = this_orig, aes(x, y, shape = p_cor), size = electrode_size) + # plots elecs
  scale_shape_manual(values = c(19, 17, 1)) + 
  # geom_label(data = this_orig, aes(label = elec)) + # uncomment to see elec names
  # plots nose
  geom_path(data = nose, aes(x, y, z = NULL, fill = NULL), size = nose_size) +
  # creates a mask to remove points outside defined circle
  geom_path(
    data = maskRing,
    aes(x, y, z = NULL, fill = NULL),
    colour = "white",
    size = 8
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
    #shape = FALSE, 
    fill = guide_colourbar(
      title.position = "top", 
      title.hjust = 0.5, 
      frame.colour = "black", 
      ticks.colour = "black", 
      barwidth = unit(bwidth, "in"),
      barheight = unit(bheight, "in")
    )
  ) +
  theme(legend.position = "bottom") +
  facet_wrap(~term)
# Standard PDF save
ggsave(
  filename = "reg-topos.pdf",
  path = "../output/",
  width = 6.5, height = 7, units = "in"
)

