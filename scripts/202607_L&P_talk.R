# script for figures used in EGU26 talk
# https://doi.org/10.5194/egusphere-egu26-4549

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
dir_plots <- file.path("plots", "L&P2026")

## time scale ----
data_time_scale <- data.frame(
  label = c(
    "seconds", 
    "years", 
    "decades", 
    "centuries", 
    "kiloyears", 
    "million\nyears", 
    "billion\nyears"
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
line_width <- 4 # in /.pt units
point_size <- 8 # in /.pt untis

plot_palette <- viridis::viridis(n = 5, begin = 0.1, end = 0.8)

# load data ----
infile <- openxlsx2::wb_load(file = file.path(dir_data, "disruptors_supplementary_data_tables_s1-s5.xlsx"))
data_event_durations <- infile %>% 
  openxlsx2::wb_to_df(
    ., 
    sheet = "table_s3_events", 
    start_row = 1, 
    skip_empty_rows = TRUE, 
    skip_empty_cols = TRUE
  )

data_event_biosphere <- infile %>% 
  openxlsx2::wb_to_df(
    ., 
    sheet = "table_s5_biosphere_changes", 
    start_row = 1, 
    cols = c(1:13),
    skip_empty_rows = TRUE, 
    skip_empty_cols = TRUE
  ) %>%
  dplyr::select(-change_group) %>%
  dplyr::mutate(
    change_value = as.numeric(change_value)
  )

# sort data ----
data_to_plot <- data_event_durations %>%
  dplyr::right_join(.,data_event_biosphere, 
                    by = join_by(interval_name, interval_abbreviation, age_estimate_ma, event_duration_est_yr, type)) %>%
  dplyr::mutate(dplyr::across(c(change_value, 
                                event_duration_min_yr,
                                event_duration_max_yr,
                                event_duration_est_yr),
                              as.numeric),
                interval_abbreviation = case_when(
                  big5 == "yes" ~ paste0("†", interval_abbreviation), 
                  .default = interval_abbreviation)) %>%
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
    percentage_change_rate_Myr,
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
    change_rate_mean = case_when(
      mean(percentage_change_rate_Myr, na.rm = TRUE) == 0 ~ 0,
      abs(mean(percentage_change_rate_Myr, na.rm = TRUE)) <= 1 ~ 0,
      mean(percentage_change_rate_Myr, na.rm = TRUE) < 0 ~ -1*log10(abs(mean(percentage_change_rate_Myr, na.rm = TRUE))),
      .default = log10(mean(percentage_change_rate_Myr, na.rm = TRUE))
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
      type == "sustainable stewardship" ~ "humans",
      type == "sustainable stewardship" ~ "humans",
      .default = type
    )
  )


# plot timescales ----
fig1_timescales <- data_to_plot %>%
  dplyr::select(
    interval_abbreviation, 
    event_duration_est_yr, 
    event_duration_min_yr, 
    event_duration_max_yr, 
    age_estimate_ma, 
    type_labels
  ) %>% 
  dplyr::distinct() %>%
  ggplot(
    aes(
      x = interval_abbreviation, 
      # alpha = type_labels,
      colour = type_labels
    )
  ) +
  scale_y_log10(name = "biosphere impact duration (log-scale)") +
  scale_x_discrete() +
  scale_colour_manual(
    values = c(
      "persistent" = "steelblue",
      "humans" = "black",
      "transient" = "red"
    ),
    name = "Event type"
  ) +
  # scale_alpha_manual(
  #   values = c(
  #     "persistent" = 0.6,
  #     "humans" = 1,
  #     "transient" = 0.6
  #   ),
  #   name = "Event type"
  # ) +
  coord_cartesian(
    xlim = c(-2.5, max(data_to_plot$interval_abbreviation)),
    clip = "off"
  ) +
  theme_void() +
  theme(
    axis.title.y = element_text(size = point_size+2, face = "bold", angle = 90),
    legend.position = "inside",
    legend.justification = c(0, 0), 
    legend.position.inside = c(0.1, 0.1),
    legend.box.background = element_rect(
      colour = "black"
    ),
    legend.box.margin = margin(4,4,4,4),
    legend.title = element_text(
      size = point_size,
      face = "bold"
    ),
    legend.text = element_text(
      size = point_size, 
      face = "bold"
    ),
    plot.margin = margin(8,4,0,4)
  ) +
  # add axes as annotations
  annotate(
    geom = "linerange",
    x = 0,
    ymin = 1.00e-08,
    ymax = 1.000e+10,
    linewidth = line_width/.pt,
    colour = "black"
  ) +
  annotate(
    geom = "text",
    x = -0.5,
    y = data_time_scale$value,
    label = data_time_scale$label,
    angle = 0, 
    hjust = 1, 
    vjust = 0,
    size = (point_size+2)/.pt,
    fontface = "bold"
  ) +
  geom_text(
    aes(
      y = event_duration_min_yr, 
      label = interval_abbreviation
    ), 
    angle = 90, 
    hjust = 1.1, 
    vjust = 0.3,
    fontface = "bold",
    size = point_size/.pt
  ) +
  geom_text(
    aes(
      y = event_duration_max_yr, 
      label = paste0(
        "(", 
        as.numeric(age_estimate_ma),
        " Ma)"
      ),
    ), 
    angle = 90, 
    hjust = -0.1, 
    vjust = 0.3,
    fontface = "italic",
    size = point_size/.pt
  ) +
  # add data
  geom_linerange(
    aes(
      ymin = event_duration_min_yr, 
      ymax = event_duration_max_yr
    ),
    linewidth = line_width/3
  ) +
  geom_point(
    aes(
      y = event_duration_est_yr
    ), 
    size = point_size/2
  )

print(fig1_timescales)

ggsave(
  filename = file.path(dir_plots, "event_timescales.png"),
  plot = fig1_timescales, 
  width = 323.3,
  height = 134.9,
  units = "mm",
  dpi = 600,
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
        "sustainable stewardship",
        "humans so far",
        "business as usual", 
        "transient"
      )
    ),
    change_type = ordered(
      change_type,
      levels = c(
        "species", 
        "genus", 
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
  # scale_y_log10() +
  scale_y_log10(name = "biosphere impact duration (log-scale)") +
  scale_x_continuous(
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
      "sustainable stewardship" = plot_palette[4], 
      "humans so far" = "black", # plot_palette[3],
      "business as usual" = plot_palette[2], 
      "transient" = plot_palette[1] 
    ),
    aesthetics = c("colour", "fill"),
    name = "Episode type"
  ) +
  scale_alpha_manual(
    values = c(
      "geological" = 0.5,
      "humans" = 0.75
    ),
    breaks = c("humans", "geological"),
    name = "Episode scope"
  ) + 
  scale_size_manual(
    values = c(
      "geological" = 1.5*(point_size/.pt),
      "humans" = 3*(point_size/.pt)
    ),
    breaks = c("humans", "geological"),
    name = "Episode scope"
  ) +
  coord_cartesian(
    xlim = c(
      -1*(habitability_lim+(0.05*habitability_lim)), 
      habitability_lim
    ),
    clip = "off"
  ) +
  theme_void() +
  theme(
    axis.title.y = element_text(size = point_size+2, face = "bold", angle = 90),
    legend.position = "inside",
    legend.justification = c(1, 0),
    legend.position.inside = c(0.95, 0.15),
    legend.direction = "vertical",
    legend.box = "horizontal",
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
    ),
    plot.margin = margin(2,0,2,6)
  ) +
  labs(
    y = "Log(duration in years)",
    x = "Planetary habitability"
  ) +
  # add axes as annotations
  annotate(
    geom = "linerange",
    x = -1*(habitability_lim-0.5), 
    ymin = 1.00e-08,
    ymax = 1.000e+10,
    linewidth = line_width/.pt,
    colour = "black"
  ) +
  annotate(
    geom = "text",
    x = -1*(habitability_lim-0.25),
    y = data_time_scale$value,
    label = data_time_scale$label,
    angle = 0, 
    hjust = 1.1, 
    vjust = 0,
    size = (point_size+2)/.pt,
    fontface = "bold"
  ) +
  annotate(
    geom = "segment",
    y = 1.00e-09,
    yend = 1.00e-09,
    x = 0.1, 
    xend = habitability_lim-1,
    linewidth = 1*line_width/.pt,
    colour = plot_palette[5], 
    arrow = arrow(length = unit(0.3, "cm")),
    lineend = "round", 
    linejoin = "bevel"
  ) +
  annotate(
    geom = "text", 
    label = "biosphere net gain",
    y = 1.00e-10,
    x = 0.5*(habitability_lim-1),
    colour = plot_palette[5], 
    angle = 0,
    size = (point_size+2)/.pt,
    fontface = "bold"
  ) +
  annotate(
    geom = "segment",
    y = 1.00e-09,
    yend = 1.00e-09,
    x = -0.1, 
    xend = -1*(habitability_lim-1),
    linewidth = 1*line_width/.pt,
    colour = plot_palette[1], 
    arrow = arrow(length = unit(0.3, "cm")),
    lineend = "round", 
    linejoin = "bevel"
  ) +
  annotate(
    geom = "text", 
    label = "biosphere net loss",
    y = 1.00e-10,
    x = -0.5*(habitability_lim-1),
    colour = plot_palette[1], 
    angle = 0,
    size = (point_size+2)/.pt,
    fontface = "bold"
  ) +
  geom_segment(
    aes(
      y = 1.00e-10,
      yend = 1.00e10,
      x = 0, 
      xend = 0
    ),
    linetype = "dashed", 
    linewidth = line_width/.pt,
    colour = "grey50"
  ) +
  # add duration range line
  geom_linerange(
    aes(
      x = change_mean,
      ymin = event_duration_min_yr, 
      ymax = event_duration_max_yr
    ),
    linewidth = 0.5*(line_width/.pt)
  ) +
  # add habitability change line
  geom_linerange(
    aes(
      y = event_duration_est_yr,
      xmin = change_min, 
      xmax = change_max
    ),
    linewidth = 0.5*(line_width/.pt)
  ) +
  # add central estimate point
  geom_point(
    aes(
      y = event_duration_est_yr, 
      x = change_mean
    ),
    stroke = line_width/.pt
  ) 

print(fig2_habitability)

ggsave(
  filename = file.path(dir_plots, "deep_time_habitability.png"),
  plot = fig2_habitability, 
  width = 323.3,
  height = 134.9,
  units = "mm",
  dpi = 600,
  bg = "white"
  )  


# plot change rates ----
data_to_plot_rates <- data_to_plot %>%
  dplyr::mutate(
    type = ordered(
      type, 
      levels = c(
        "persistent",
        "sustainable stewardship",
        "humans so far",
        "business as usual", 
        "transient"
      )
    ),
    change_type = ordered(
      change_type,
      levels = c(
        "species", 
        "genus", 
        "biomass", 
        "productivity"
      )
    ),
    type_labels = dplyr::case_when(
      type_labels == "humans" ~ "humans",
      .default = "geological"
    )
  ) %>%
  dplyr::mutate(
    interval_abbreviation = forcats::fct_reorder(
      .f = interval_abbreviation,
      .x = change_rate_mean, 
      .fun = min, 
      .na.rm = TRUE
    ) 
  )

data_to_plot_rates_labels <- data_to_plot_rates %>%
  dplyr::group_by(interval_abbreviation, age_estimate_ma, type, type_labels) %>%
  dplyr::summarise(
    min_rate = min(change_rate_mean, na.rm = TRUE),
    max_rate = max(change_rate_mean, na.rm = TRUE),
  )

fig3_rates_of_change <- ggplot() +
  scale_x_continuous(expand = expansion(add = 2),
                     name = expression("rate of biosphere variable change (pseudo-log"[10]*" scale)")) +
  scale_y_discrete() +
  scale_shape_manual(values = c(21:24), name = "Biosphere variable") +
  scale_fill_manual(
    values = c(
      "persistent" = plot_palette[5], 
      "sustainable stewardship" = plot_palette[4], 
      "humans so far" = "black", 
      "business as usual" = plot_palette[2], 
      "transient" = plot_palette[1] 
    ),
    aesthetics = c("colour", "fill"),
    name = "Episode type"
  ) +
  scale_alpha_manual(
    values = c(
      "geological" = 0.5,
      "humans" = 0.9
    ),
    breaks = c("humans", "geological"),
    name = "Episode scope"
  ) +
  scale_size_manual(
    values = c(
      "geological" = point_size/.pt,
      "humans" = 2*(point_size/.pt)
    ),
    breaks = c("humans", "geological"),
    name = "Episode scope"
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(), 
    axis.title.y = element_blank(),
    axis.text.x = element_text(
      size = point_size,
      margin = margin(10,0,2,0, unit = "pt"),
      vjust = 1
    ),
    panel.grid = element_blank(), 
    legend.position = "inside",
    legend.justification = c(1, 0),
    legend.position.inside = c(0.99, 0.05),
    legend.direction = "vertical",
    legend.box = "horizontal",
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
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = (line_width/2)/.pt, colour = "grey50") +
  geom_hline(yintercept = 0, linetype = "solid", linewidth = line_width/.pt, colour = "black") +
  geom_text(
    data = data_to_plot_rates_labels, 
    aes(
      x = min_rate - 0.15,
      y = interval_abbreviation, 
      label = interval_abbreviation, 
      colour = type,
      alpha = type_labels
    ),
    angle = 0,
    hjust = 1,
    vjust = 0.5,
    fontface = "bold",
    size = point_size/.pt
  ) +
  geom_text(
    data = data_to_plot_rates_labels, 
    aes(
      x = max_rate + 0.1, 
      y = interval_abbreviation, 
      label = paste0(
        "(", age_estimate_ma, " Ma)"
      ),
      colour = type,
      alpha = type_labels
    ),
    angle = 0,
    hjust = 0,
    vjust = 0.5,
    size = point_size/.pt,
    fontface = "italic"
  ) + 
  geom_point(data = data_to_plot_rates,
             aes(x = change_rate_mean, y = interval_abbreviation, 
                 fill = type, colour = type, 
                 alpha = type_labels, size = type_labels,
                 shape = change_type))

print(fig3_rates_of_change)

ggsave(filename = file.path(dir_plots, "rates_of_change_ordered.png"),
       plot = fig3_rates_of_change,
       width = 323.3, height = 134.9, units = "mm", dpi = 600, bg = "white")  

# END ----
