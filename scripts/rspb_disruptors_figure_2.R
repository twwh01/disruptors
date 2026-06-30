# schematic figure of the potential range of biosphere disruptor impacts over time
# this produces a three panel figure:
#-- a. past persistent disruptors
#-- b. past transient disruptors
#-- c. the current and near-future trajectories of the human disruptor

# axes:
#-- x-axis: log base 10 time in years from onset of disruptor
#-- y-axis: impact (positive or negative) of the disruptor on the biosphere; 
#           zero-centred axis;
#           biosphere impact may be measured by biodiversity, biomass, productivity

# assumptions and parameters:
#-- a. transient disruptors last for 10e+06 years
#-- b. persistent disruptors last for 10e+08 years
#-- c. transient disruptors primarily degrade the biosphere
#-- d. persistent disruptors primarily enhance the biosphere
#-- e. linear degradation rate (biosphere degradation is uniformly distributed in time)
#-- f. linear enhancement rate (biosphere enhancement is uniformly distributed in time)
#-- g. degradation drives the biosphere to a nadir value of -1
#-- h. enhancement drives the biosphere to a peak value of +1


# clear the decks ----
rm(list = ls())


# load packages ----
library(ggplot2)
library(dplyr)


# plot parameters ----
dir_plots <- file.path("plots")
line_width <- 1/.pt # in /.pt units
point_size <- 5/.pt # in /.pt untis
plot_palette <- viridis::viridis(n = 6, begin = 0.1, end = 0.9)


# dummy data ----
## time scale ----
data_time_scale <- data.frame(
  label = c(
    # "years", 
    # "decades", 
    "centuries",
    "kiloyears", 
    "megayears", 
    "gigayears"
  ),
  value = c(
    # 0,
    # 1,
    1e+02,
    1e+03,
    1e+06, 
    1e+09
  )
)

### transient timescale ----
# transient disruptors operate on timescales of <1e+01 to 4e+06 years
# biosphere recovery and/or transformation takes place on timescales from 1e+06 to 1e+07 years
x_t <- 10**seq(from = 0.01, to = 7, by = 0.01)
x_t_min <- 1e+01
x_t_max <- 4e+06
x_t_recovery <- 1e+07

### persistent timescale ----
# persistent disruptors operate on timescales of 4e+07 years to 5e+08 years
# persistent disruptors generally enhance the biosphere on these timescales
# persistent disruptors may harm the incumbent biosphere on timescales of 1e+00 to 1e+06 years
x_p <- 10**seq(from = 0.01, to = 9, by = 0.01)
x_p_min <- 4e+07
x_p_max <- 5e+08

### human timescale ----
# human disruptor operates on timescales of 1e+01 years to 3e+03 years
# humans are currently degrading the biosphere (0 < t < 2e+02)
# humans may continue to degrade the biosphere (2e+02 < t < 3e+03)
# humans may change course and enhance the biosphere (2e+02 < t < 3e+03)
x_h <- 10**seq(from = 0.01, to = 4, by = 0.01)
x_h_min <- 1e+01
x_h_cur <- 2e+02 # human current
x_h_max <- 3e+03

## transient disruptors functions ----
### simple transient simple recovery ----
# assume 1:1 linear decline to nadir of -1
# assume 1:1 linear recovery to 0 after event
simple_t_y <- function(x) {
  ifelse(
    x >= x_t_min & x <= x_t_max,
    # first segment: linear decline
    -1 * ((x - x_t_min) / (x_t_max - x_t_min)),
    ifelse(
      x > x_t_max & x <= x_t_recovery,
      # second segment: linear recovery
      -1 + ((x - x_t_max) / (x_t_recovery - x_t_max)),
      NA  # outside the defined domain
    )
  )
}
simple_t <- data.frame(
  'disruptor' = 'transient', 
  'version' = 'simple',
  'x' = x_t, 'y' = simple_t_y(x_t)
)

### simple transient rapid recovery ----
# assume 1:1 linear decline to nadir of -1
# assume 1.5:1 rapid linear recovery to +0.5 after event
fast_t_y <- function(x) {
  ifelse(
    x >= x_t_min & x <= x_t_max,
    # first segment: linear decline
    -1 * ((x - x_t_min) / (x_t_max - x_t_min)),
    ifelse(
      x > x_t_max & x <= x_t_recovery,
      # second segment: linear recovery
      -1 + 1.5*((x - x_t_max) / (x_t_recovery - x_t_max)),
      NA  # outside the defined domain
    )
  )
}
fast_t <- data.frame(
  'disruptor' = 'transient', 
  'version' = 'fast recovery',
  'x' = x_t, 'y' = fast_t_y(x_t)
)

### simple transient slow recovery ----
# assume 1:1 linear decline to nadir of -1
# assume 0.5:1 slow linear recovery to -0.5 after event
slow_t_y <- function(x) {
  ifelse(
    x >= x_t_min & x <= x_t_max,
    # first segment: linear decline
    -1 * ((x - x_t_min) / (x_t_max - x_t_min)),
    ifelse(
      x > x_t_max & x <= x_t_recovery,
      # second segment: linear recovery
      -1 + 0.5*((x - x_t_max) / (x_t_recovery - x_t_max)),
      NA  # outside the defined domain
    )
  )
}
slow_t <- data.frame(
  'disruptor' = 'transient', 
  'version' = 'slow recovery',
  'x' = x_t, 'y' = slow_t_y(x_t)
)

### simple transient bolide impact ----
# assume 1:1 linear decline to nadir of -1 over 1e+02 years
# assume plateau to 1e+06 years
# assume 1:1 linear recovery to 0 after event
kpg_t_y <- function(x) {
  ifelse(
    x >= x_t_min & x <= 1e+02,
    # first segment: linear decline
    -1 * ((x - x_t_min) / (1e+02 - x_t_min)),
    ifelse(
      # second segment: plateau
      x > 1e+02 & x <= x_t_max, 
      -1, 
      ifelse(
        x > x_t_max & x <= x_t_recovery,
        # third segment: linear recovery
        -1 + 0.5*((x - x_t_max) / (x_t_recovery - x_t_max)),
        NA  # outside the defined domain
      )
    )
  )
}
kpg_t <- data.frame(
  'disruptor' = 'transient', 
  'version' = 'bolide impact',
  'x' = x_t, 'y' = kpg_t_y(x_t)
)

## persistent disruptors functions ----
### simple persistent simple recovery ----
# assume 1:1 linear increase to peak of +1
simple_p_y <- function(x) {
  ifelse(
    # first segment before range of disruptor impact
    x < x_p_min, 
    0, 
    ifelse(
      # second segment: linear increase for duration of impact
      x >= x_p_min & x <= x_p_max,
      (x - x_p_min) / (x_p_max - x_p_min),
      NA  # outside the defined domain
    )
  )
}
simple_p <- data.frame(
  'disruptor' = 'persistent', 
  'version' = 'simple',
  'x' = x_p, 'y' = simple_p_y(x_p)
)

### simple persistent fast ----
# assume 2:1 linear increase to peak of +2 then plateau
fast_p_y <- function(x) {
  ifelse(
    # first segment before range of disruptor impact
    x < x_p_min, 
    0, 
    ifelse(
      # second segment: linear increase gradient +2
      x >= x_p_min & (2*(x - x_p_min) / (x_p_max - x_p_min)) <= 2,
      2*(x - x_p_min) / (x_p_max - x_p_min),
      ifelse(
        # third segment: plateau at +2
        x <= x_p_max, 
        2,
        NA  # outside the defined domain
      )
    )  
  )
}
fast_p <- data.frame(
  'disruptor' = 'persistent', 
  'version' = 'fast',
  'x' = x_p, 'y' = fast_p_y(x_p)
)

### simple persistent slow ----
# assume 0.5:1 linear increase to peak of +1.5 then plateau
slow_p_y <- function(x) {
  ifelse(
    # first segment before range of disruptor impact
    x < x_p_min, 
    0, 
    ifelse(
      # second segment: linear increase gradient +1.5
      x >= x_p_min & x <= x_p_max,
      0.5*(x - x_p_min) / (x_p_max - x_p_min),
      NA  # outside the defined domain
    )  
  )
}
slow_p <- data.frame(
  'disruptor' = 'persistent', 
  'version' = 'slow',
  'x' = x_p, 'y' = slow_p_y(x_p)
)

### gain after initial harm ----
# assume linear harm for duration of transient disruptor
# then linear increase to +1
initial_harm_p_y <- function(x) {
  ifelse(
    x >= x_t_min & x <= x_t_max,
    # first segment: linear decline
    simple_t_y(x),
    ifelse(
      x > x_t_max & x <= x_p_max,
      # second segment: linear recovery
      -1 + 2*((x - x_t_max) / (x_p_max - x_t_max)),
      NA  # outside the defined domain
    )
  )
}
initial_harm_p <- data.frame(
  'disruptor' = 'persistent', 
  'version' = 'initial harm',
  'x' = x_p, 'y' = initial_harm_p_y(x_p)
)

## human disruptor functions ----
### simple human decline ----
# assume 1:1 linear decrease to nadir of -0.3
human_t_y <- function(x) {
  ifelse(
    # first segment before range of disruptor impact
    x < x_h_min, 
    0, 
    ifelse(
      # second segment: linear decrease to -0.4 at 3e+03 years
      x >= x_h_min & x < x_h_max,
      -0.4 * ((x - x_h_min) / (x_h_max - x_h_min)),
      NA  # outside the defined domain
    )
  )
}
human_t <- data.frame(
  'disruptor' = 'human', 
  'version' = 'business as usual',
  'x' = x_h, 'y' = human_t_y(x_h)
)

### human recovery ----
# assume 1:1 linear decrease towards nadir of -0.3 at 3e+03 years
# arrested at 2e+02 years with recovery to +0.2 at 3e+03 years
human_p_y <- function(x) {
  ifelse(
    # first segment before range of disruptor impact
    x < x_h_min, 
    0, 
    ifelse(
      # second segment: linear decrease to 2e+02
      x >= x_h_min & x <= 2e+02,
      -0.3 * ((x - x_h_min) / (x_h_max - x_h_min)),
      ifelse(
        # third segment: recovery
        x > 2e+02 & x <= x_h_max,
        # third segment: linear recovery
        (-0.3 * ((2e+02 - x_h_min) / (x_h_max - x_h_min))) + 0.3*((x - 2e+02) / (x_h_max - 2e+02)), 
        NA  # outside the defined domain
      )
    )
  )
}
human_p <- data.frame(
  'disruptor' = 'human', 
  'version' = 'sustainable stewardship',
  'x' = x_h, 'y' = human_p_y(x_h)
)

## collate data ----
disruptors <- rbind(
  simple_t, 
  fast_t, 
  slow_t,
  kpg_t,
  simple_p, 
  fast_p,
  slow_p, 
  initial_harm_p, 
  human_t, 
  human_p
) 

## summarise data ----
disruptors_summary <- disruptors |> 
  dplyr::mutate(
    kpg = dplyr::case_when(version == "bolide impact" ~ "K-Pg")
  ) |> 
  dplyr::group_by(x, disruptor, kpg) |> 
  dplyr::summarise(
    lower_bound = min(y, na.rm = TRUE), 
    upper_bound = max(y, na.rm = TRUE), 
    average = mean(y, na.rm = TRUE)
  ) |>
  dplyr::mutate(
    lower_bound = dplyr::case_when(is.infinite(lower_bound) ~ NA, .default = lower_bound), 
    upper_bound = dplyr::case_when(is.infinite(upper_bound) ~ NA, .default = upper_bound), 
    average = dplyr::case_when(is.infinite(average) ~ NA, .default = average), 
    disruptor = ordered(disruptor, levels = c("persistent", "transient", "human"))
  )

disruptors_summary_main <- disruptors_summary |> dplyr::filter(is.na(kpg))
disruptors_summary_kpg <- disruptors_summary |> 
  dplyr::filter(!is.na(kpg)) |>
  dplyr::mutate(
    upper_bound = disruptors_summary_main$lower_bound[
      disruptors_summary_main$disruptor == "transient" & disruptors_summary_main$x == x
    ]
  )

# plot data ----
ggplot() +
  theme_void() +
  theme(plot.margin = margin(10, 10, 7, 10, unit = 'pt'), 
        axis.text = element_blank(), 
        axis.title.y = element_text(size = 5*point_size, face = 'bold', angle = 90, vjust = 1, hjust = 0.5),
        axis.title.x = element_text(size = 5*point_size, face = 'bold', angle = 0, vjust = -1), 
        strip.background = element_blank(), 
        strip.text = element_text(size = 5*point_size, face = 'bold', vjust = 1)
  ) +
  labs(x = expression(bold("log"[10]*"(years)")), y = "planetary habitability") +
  facet_grid(~disruptor) +
  scale_x_log10(limits = c(x_t_min, 1e+09)) +
  coord_cartesian(ylim = c(-2, 2), clip = 'off') +
  scale_fill_viridis_d(aesthetics = c("fill", "colour"), begin = 0.2, end = 0.8, guide = "none") +
  # add axes and annotations
  ## zero line
  annotate(geom = "segment", x = x_t_min, xend = x_p_max, y = 0, 
           linetype = "dashed", colour = "grey50", linewidth = 0.5*line_width) +
  ## 200 year line
  annotate(geom = "segment", x = 2e+02, y = -1.3, yend = 2, 
           linetype = "dashed", colour = "grey50", linewidth = 0.5*line_width) +
  ## disruptor durations
  annotate(geom = "rect", xmin = x_t_min, xmax = x_p_max, ymin = -1.2, ymax = -1.1,
           alpha = 0.25, colour = NA, fill = viridis::viridis(n=3, begin = 0.2, end = 0.8)[1]) +
  annotate(geom = "text", x = x_p_max, y = -1.15, label = expression(italic("persistent")),
           colour = "black", size = 1.3*point_size, hjust = 1.1, vjust = 0.5) +
  annotate(geom = "rect", xmin = x_t_min, xmax = x_t_max, ymin = -1.3, ymax = -1.2,
           alpha = 0.25, colour = NA, fill = viridis::viridis(n=3, begin = 0.2, end = 0.8)[2]) +
  annotate(geom = "text", x = x_t_max, y = -1.25, label = expression(italic("transient")),
           colour = "black", size = 1.3*point_size, hjust = 1.1, vjust = 0.5) +
  annotate(geom = "rect", xmin = x_h_min, xmax = x_h_max, ymin = -1.4, ymax = -1.3,
           alpha = 0.25, colour = NA, fill = viridis::viridis(n=3, begin = 0.2, end = 0.8)[3]) +
  annotate(geom = "text", x = x_h_max, y = -1.35, label = expression(italic("humans")),
           colour = "black", size = 1.3*point_size, hjust = 1.1, vjust = 0.5) +
  ## time axis
  annotate(geom = "linerange", y = -1.4, xmin = x_t_min, xmax = max(x_p, na.rm = TRUE),
           linewidth = line_width/.pt, colour = "black") +
  geom_text(data = data_time_scale,
            aes(x = value, y = -1.46, label = label),
            angle = 90, hjust = 1, vjust = 0,
            size = 1.5*point_size, colour = 'black', fontface = "bold") +
  ## y-axis biosphere labels
  geom_segment(
    data = data.frame(
      x = 1e+02, xend = 1e+02, y = 0.1, yend = 1, 
      disruptor = ordered("persistent", levels = c("persistent", "transient", "human"))),
    aes(x = x, xend = xend, y = y, yend = yend), 
    linewidth = 1.7*line_width, colour = plot_palette[4], 
    arrow = arrow(length = unit(0.2, "cm")), lineend = "round", linejoin = "bevel"
  ) +
  geom_text(data = data.frame(
    x = 4e+01, y = 0.5, label = "net gain", 
    disruptor = ordered("persistent", levels = c("persistent", "transient", "human"))),
    aes(x = x, y = y, label = label), 
    colour = plot_palette[4], vjust = 0.3, hjust = 0.4, angle = 90, size = 1.8*point_size, fontface = "bold"
  ) +
  geom_segment(
    data = data.frame(
      x = 1e+02, xend = 1e+02, y = -0.1, yend = -1, 
      disruptor = ordered("persistent", levels = c("persistent", "transient", "human"))),
    aes(x = x, xend = xend, y = y, yend = yend), 
    linewidth = 1.7*line_width, colour = plot_palette[2], 
    arrow = arrow(length = unit(0.2, "cm")), lineend = "round", linejoin = "bevel"
  ) +
  geom_text(data = data.frame(
    x = 4e+01, y = -0.5, label = "net loss", 
    disruptor = ordered("persistent", levels = c("persistent", "transient", "human"))),
    aes(x = x, y = y, label = label), 
    colour = plot_palette[2], vjust = 0.3, hjust = 0.6, angle = 90, size = 1.8*point_size, fontface = "bold"
  ) +
  # add data
  ## main data
  geom_ribbon(data = disruptors_summary_main,
              aes(x = x, ymin = lower_bound, ymax = upper_bound, colour = disruptor, fill = disruptor), 
              linewidth = 1*line_width/.pt, alpha = 0.25) +
  geom_ribbon(data = disruptors_summary_kpg, 
              aes(x = x, ymin = lower_bound, ymax = upper_bound), #colour = "bolide impact"),
              colour = "black", fill = "grey50",
              linetype = "dotted", linewidth = 2*line_width/.pt, alpha = 0.25) +
  ## bolide impact data
  geom_text(
    data = data.frame(
      x = 4e+02, y = -0.5, label = "bolide impact", 
      disruptor = ordered("transient", levels = c("persistent", "transient", "human"))),
    aes(x = x, y = y, label = label), 
    hjust = 0, vjust = 0.5, fontface = "italic", size = 1.3*point_size)

ggsave(
  filename = file.path(dir_plots, "fig_2_schematic.png"),
  width = 170,
  height = 99,
  units = "mm",
  dpi = 600,
  bg = "white"
)


# END ----
