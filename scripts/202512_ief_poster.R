# clear the decks ----
rm(list = ls())


# load packages ----
library(openxlsx2)
library(dplyr)
library(forcats)
library(ggplot2)


# parameters ----
## directories ----
dir_data <- file.path("data")
dir_plots <- file.path("plots")

## time scale ----
data_time_scale <- data.frame(
  label = c(
    "seconds", 
    "years", 
    "decades", 
    "centuries", 
    "kiloyears", 
    "megayears", 
    "gigayears"
  ),
  value = c(
    1.00e-7, 
    1.00e0,
    1.00e1,
    1.00e2,
    1.00e3,
    1.00e6, 
    1.00e9
  )
)

## plot parameters ----
line_width <- 8 # in /.pt units
point_size <- 16 # in /.pt untis

plot_palette <- viridis::viridis(n = 5, begin = 0.1, end = 0.8)

# load data ----
infile <- openxlsx2::wb_load(file = file.path(dir_data, "deep_time_planetary_boundaries.xlsx"))
data_event_durations <- infile %>% 
  openxlsx2::wb_to_df(
    ., 
    sheet = "table_3_events", 
    start_row = 1, 
    skip_empty_rows = TRUE, 
    skip_empty_cols = TRUE
  )

data_event_biosphere <- infile %>% 
  openxlsx2::wb_to_df(
    ., 
    sheet = "event_biosphere_changes", 
    start_row = 1, 
    cols = c(1:8),
    skip_empty_rows = TRUE, 
    skip_empty_cols = TRUE
  ) %>%
  dplyr::select(-change_group) %>%
  dplyr::mutate(
    change_value = as.numeric(change_value)
  )

# sort data ----
data_to_plot <- data_event_durations %>%
  dplyr::right_join(
    ., 
    data_event_biosphere, 
    by = join_by(interval_name, interval_abbreviation)
  ) %>%
  dplyr::mutate(
    across(
      c(change_value, 
        event_duration_min_yr,
        event_duration_max_yr,
        event_duration_est_yr
        ), 
      as.numeric
    ), 
    interval_abbreviation = case_when(
      big5 == "yes" ~ paste0("†", interval_abbreviation), 
      .default = interval_abbreviation
    )
  ) %>%
  dplyr::mutate(
    event_duration_est_yr = case_when(
      is.na(event_duration_est_yr) ~ 0.5*(event_duration_max_yr + event_duration_min_yr),
      .default = event_duration_est_yr
    )
  ) %>%
  dplyr::group_by(
    interval_abbreviation, 
    age_estimate_ma,
    change_type, 
    event_duration_est_yr, 
    event_duration_min_yr, 
    event_duration_max_yr, 
    type
  ) %>%
  dplyr::summarise(
    change_min = case_when(
      min(change_value, na.rm = TRUE) == 0 ~ 0,
      abs(min(change_value, na.rm = TRUE)) <= 1 ~ 0,
      min(change_value, na.rm = TRUE) < 0 ~ -1*log10(abs(min(change_value, na.rm = TRUE))),
      .default = log10(min(change_value, na.rm = TRUE))
    ), 
    change_max = case_when(
      max(change_value, na.rm = TRUE) == 0 ~ 0,
      abs(max(change_value, na.rm = TRUE)) <= 1 ~ 0,
      max(change_value, na.rm = TRUE) < 0 ~ -1*log10(abs(max(change_value, na.rm = TRUE))),
      .default = log10(max(change_value, na.rm = TRUE))
    ), 
    change_mean = case_when(
      mean(change_value, na.rm = TRUE) == 0 ~ 0,
      abs(mean(change_value, na.rm = TRUE)) <= 1 ~ 0,
      mean(change_value, na.rm = TRUE) < 0 ~ -1*log10(abs(mean(change_value, na.rm = TRUE))),
      .default = log10(mean(change_value, na.rm = TRUE))
    ), 
    .groups = "keep"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    interval_abbreviation = ordered(
      forcats::fct_reorder2(interval_abbreviation, desc(interval_abbreviation), age_estimate_ma)
    ),
    type_labels = dplyr::case_when(
      type == "humans so far" ~ "humans",
      type == "business as usual" ~ "humans",
      type == "a hopeful future" ~ "humans",
      type == "a hopeful future" ~ "humans",
      .default = type
    ), 
    change_type = dplyr::case_when(
      change_type == "species" ~ "species diversity",
      change_type == "genus" ~ "genus diversity",
      .default = change_type
    )
  )


# plot timescales ----
fig1_timescales <- data_to_plot %>%
  ggplot(
    aes(
      y = interval_abbreviation, 
      colour = type_labels
    )
  ) +
  scale_x_log10() +
  scale_y_discrete() +
  scale_colour_manual(
    values = c(
      "persistent" = "steelblue",
      "humans" = "black",
      "transient" = "red"
    ),
    name = "Event type"
  ) +
  coord_cartesian(
    ylim = c(-5, max(data_to_plot$interval_abbreviation)),
    clip = "off"
  ) +
  theme_void() +
  theme(
    plot.margin = margin(4, 4, 4, 0, unit = "pt"),
    legend.position = "inside",
    legend.position.inside = c(0.1, 0.25),
    legend.box.background = element_rect(
      colour = "black"
    ),
    legend.box.margin = margin(4,4,4,4),
    legend.title = element_text(
      size = point_size,
      face = "bold"
    ),
    legend.text = element_text(
      size = point_size
    )
  ) +
  # add axes as annotations
  annotate(
    geom = "linerange",
    y = 0,
    xmin = 1.00e-08,
    xmax = 1.000e+10,
    linewidth = line_width/.pt,
    colour = "black"
  ) +
  annotate(
    geom = "text",
    y = -0.5,
    x = data_time_scale$value,
    label = data_time_scale$label,
    angle = 90, 
    hjust = 1, 
    vjust = 0,
    size = (point_size-2)/.pt,
    fontface = "bold"
  ) +
  annotate(
    geom = "text",
    y = data_to_plot$interval_abbreviation,
    x = data_to_plot$event_duration_min_yr,
    label = data_to_plot$interval_abbreviation,
    angle = 0, 
    hjust = 1.1, 
    vjust = 0.3,
    size = (point_size-3)/.pt, 
    fontface = "bold"
  ) +
  annotate(
    geom = "text",
    y = data_to_plot$interval_abbreviation,
    x = data_to_plot$event_duration_max_yr,
    label = paste0(
      "(", 
      as.numeric(data_to_plot$age_estimate_ma),
      " Ma)"
    ),
    angle = 0, 
    hjust = -0.1, 
    vjust = 0.3,
    size = (point_size-3)/.pt, 
    fontface = "italic"
  ) +
  # add data
  geom_linerange(
    aes(
      xmin = event_duration_min_yr, 
      xmax = event_duration_max_yr
    ), 
    linewidth = (line_width-2)/.pt
  ) +
  geom_point(
    aes(
      x = event_duration_est_yr
    ), 
    size = (point_size-2)/.pt
  )

print(fig1_timescales)

ggsave(
  filename = file.path(dir_plots, "ief_poster_event_timescales.png"),
  plot = fig1_timescales, 
  width = 259.3,
  height = 190.3,
  units = "mm",
  dpi = 450,
  bg = "white"
)  

# plot habitability change and duration ----
habitability_lim_max <- max(data_to_plot$change_max, na.rm = TRUE)
habitability_lim_min <- min(data_to_plot$change_min, na.rm = TRUE)
habitability_lim <- max(abs(habitability_lim_max), abs(habitability_lim_min), na.rm = TRUE) + 1

fig2_habitability <- data_to_plot %>% 
  dplyr::mutate(
    type = ordered(
      type, 
      levels = c(
        "persistent",
        "a hopeful future",
        "humans so far",
        "business as usual", 
        "transient"
      )
    ),
    change_type = ordered(
      change_type,
      levels = c(
        "species diversity", 
        "genus diversity", 
        "biomass", 
        "productivity"
      )
    ),
    type_labels = dplyr::case_when(
      type_labels == "humans" ~ "humans",
      .default = "geological"
    )
  ) %>%
  ggplot(
    aes(
      fill = type,
      colour = type,
      alpha = type_labels,
      size = type_labels,
      shape = change_type
    )
  ) +
  scale_x_log10() +
  scale_y_continuous(
    limits = c(
      -1*habitability_lim, habitability_lim
    )
  ) +
  scale_shape_manual(
    values = c(21:24),
    name = "Biosphere variable"
  ) +
  scale_fill_manual(
    values = c(
      "persistent" = plot_palette[5], 
      "a hopeful future" = plot_palette[4], 
      "humans so far" = "black", # plot_palette[3],
      "business as usual" = plot_palette[2], 
      "transient" = plot_palette[1] 
    ),
    aesthetics = c("colour", "fill"),
    name = "Event type"
  ) +
  scale_alpha_manual(
    values = c(
      "geological" = 0.3,
      "humans" = 0.6
    ),
    breaks = c("humans", "geological"),
    name = "Event scope"
  ) +
  scale_size_manual(
    values = c(
      "geological" = point_size/.pt,
      "humans" = 3*point_size/.pt
    ),
    breaks = c("humans", "geological"),
    name = "Event scope"
  ) +
  coord_cartesian(
    ylim = c(
      -1*(habitability_lim+(0.4*habitability_lim)), 
      habitability_lim
    ),
    clip = "off"
  ) +
  theme_void() +
  theme(
    legend.position = "inside",
    legend.justification = c(0, 1), 
    legend.position.inside = c(0.12, 0.9),
    legend.direction = "vertical",
    legend.box = "horizontal", 
    legend.box.background = element_rect(
      colour = "black"
    ),
    legend.box.margin = margin(4,4,4,4),
    legend.title = element_text(
      size = point_size+2,
      face = "bold"
    ),
    legend.text = element_text(
      size = point_size+2
    )
  ) +
  labs(
    x = "Log(duration in years)",
    y = "Planetary habitability"
  ) +
  # add axes as annotations
  annotate(
    geom = "linerange",
    y = -1*(habitability_lim), 
    xmin = 1.00e-08,
    xmax = 1.000e+10,
    linewidth = line_width/.pt,
    colour = "black"
  ) +
  annotate(
    geom = "text",
    y = -1*(habitability_lim),
    x = data_time_scale$value,
    label = data_time_scale$label,
    angle = 90, 
    hjust = 1.1, 
    vjust = 0,
    size = (point_size+4)/.pt,
    fontface = "bold"
  ) +
  annotate(
    geom = "segment",
    x = 1.00e-09,
    xend = 1.00e-09,
    y = 0.1, 
    yend = habitability_lim-1,
    linewidth = line_width/.pt,
    colour = plot_palette[5], 
    arrow = arrow(length = unit(0.3, "cm")),
    lineend = "round", 
    linejoin = "bevel"
  ) +
  annotate(
    geom = "text", 
    label = "biosphere\nnet gain",
    x = 1.00e-10,
    y = 0.5*(habitability_lim-1),
    colour = plot_palette[5], 
    angle = 90,
    size = (point_size+4)/.pt,
    fontface = "bold"
  ) +
  annotate(
    geom = "segment",
    x = 1.00e-09,
    xend = 1.00e-09,
    y = -0.1, 
    yend = -1*(habitability_lim-1),
    linewidth = line_width/.pt,
    colour = plot_palette[1], 
    arrow = arrow(length = unit(0.3, "cm")),
    lineend = "round", 
    linejoin = "bevel"
  ) +
  annotate(
    geom = "text", 
    label = "biosphere\nnet loss",
    x = 1.00e-10,
    y = -0.5*(habitability_lim-1),
    colour = plot_palette[1], 
    angle = 90,
    size = (point_size+4)/.pt,
    fontface = "bold"
  ) +
  geom_segment(
    aes(
      x = 1.00e-10,
      xend = 1.00e10,
      y = 0, 
      yend = 0
    ),
    linetype = "dashed", 
    linewidth = line_width/.pt,
    colour = "grey50"
  ) +
  # add duration range line
  geom_linerange(
    aes(
      y = change_mean,
      xmin = event_duration_min_yr, 
      xmax = event_duration_max_yr
    ),
    linewidth = line_width/.pt
  ) +
  # add habitability change line
  geom_linerange(
    aes(
      x = event_duration_est_yr,
      ymin = change_min, 
      ymax = change_max
    ),
    linewidth = line_width/.pt
  ) +
  # add central estimate point
  geom_point(
    aes(
      x = event_duration_est_yr, 
      y = change_mean
    ),
    stroke = line_width/.pt
  ) 

print(fig2_habitability)

ggsave(
  filename = file.path(dir_plots, "ief_poster_deep_time_habitability.png"),
  plot = fig2_habitability, 
  width = 365,
  height = 229.6,
  units = "mm",
  dpi = 450,
  bg = "white"
  )  

# END ----
