# this script produces the data-derived figures and supplementary figures
# for Wong Hearing et al. 2026 (bioRxiv)


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
  label = c("seconds", "years", "decades", "centuries", "kiloyears", "megayears", "gigayears"),
  value = c(1.00e-7, 1.00e0, 1.00e1, 1.00e2, 1.00e3, 1.00e6, 1.00e9)
)

## plot parameters ----
line_width <- 4 # in /.pt units
point_size <- 10 # in /.pt untis
plot_palette <- viridis::viridis(n = 5, begin = 0.1, end = 0.8)

# load data ----
infile <- openxlsx2::wb_load(
  file = file.path(dir_data, "disruptors_supplementary_data_tables_s1-s5.xlsx")
  )
data_event_durations <- infile %>% 
  openxlsx2::wb_to_df(
    sheet = "table_s3_events", start_row = 1, skip_empty_rows = TRUE, skip_empty_cols = TRUE
  )

data_event_biosphere <- infile %>%
  openxlsx2::wb_to_df(
    sheet = "table_s5_biosphere_changes", 
    start_row = 1, cols = c(1:13), skip_empty_rows = TRUE, skip_empty_cols = TRUE
  ) %>%
  dplyr::select(-change_group) %>%
  dplyr::mutate(change_value = as.numeric(change_value))

# sort data ----
data_to_plot <- data_event_durations %>%
  dplyr::right_join(
    data_event_biosphere, 
    by = names(data_event_durations)[names(data_event_durations) %in% names(data_event_biosphere)],
  ) %>%
  dplyr::mutate(
    across(c(change_value, event_duration_min_yr, event_duration_max_yr, event_duration_est_yr),
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
      .default = type
    )
  )


# plot figure S1: timescales ----
figS1_timescales <- data_to_plot %>%
  ggplot(aes(y = interval_abbreviation, colour = type_labels)) +
  scale_x_log10() +
  scale_y_discrete() +
  scale_colour_manual(values = c("persistent" = "steelblue", "humans" = "black", "transient" = "red"),
                      name = "Disruptor type") +
  coord_cartesian(
    xlim = c(1.00e-08, 1.000e+11),
    ylim = c(-7, as.numeric(max(data_to_plot$interval_abbreviation))+0.5),
    clip = "off"
  ) +
  theme_void() +
  theme(
    legend.position = "inside",
    legend.justification = c(0, 0), 
    legend.position.inside = c(0.05, 0.25),
    legend.box.background = element_rect(colour = "black"),
    legend.box.margin = margin(4,4,4,4),
    legend.title = element_text(size = point_size, face = "bold"),
    legend.text = element_text(size = point_size)
  ) +
  # add axes as annotations
  annotate(geom = "linerange", y = 0, xmin = 1.00e-08, xmax = 1.000e+10,
           linewidth = line_width/.pt, colour = "black") +
  annotate(geom = "text", y = -0.5,x = data_time_scale$value, label = data_time_scale$label,
           angle = 90, hjust = 1, vjust = 0, size = (point_size+2)/.pt, fontface = "bold") +
  annotate(geom = "text",
           y = data_to_plot$interval_abbreviation, x = data_to_plot$event_duration_min_yr,
           label = data_to_plot$interval_abbreviation,
           angle = 0, hjust = 1.1, vjust = 0.3, size = point_size/.pt, fontface = "bold") +
  annotate(geom = "text", 
           y = data_to_plot$interval_abbreviation, x = data_to_plot$event_duration_max_yr,
           label = paste0("(", as.numeric(data_to_plot$age_estimate_ma), " Ma)"),
           angle = 0, hjust = -0.1, vjust = 0.3, size = point_size/.pt, fontface = "italic") +
  annotate(geom = "rect",
           xmin = min(data_to_plot$event_duration_min_yr[data_to_plot$type_labels == "persistent"], na.rm = TRUE),
           xmax = max(data_to_plot$event_duration_max_yr[data_to_plot$type_labels == "persistent"], na.rm = TRUE),
           ymin = 0, ymax = Inf, alpha = 0.1, colour = NA, fill = "steelblue") +
  annotate(geom = "rect",
           xmin = min(data_to_plot$event_duration_min_yr[data_to_plot$type_labels == "transient"], na.rm = TRUE),
           xmax = max(data_to_plot$event_duration_max_yr[data_to_plot$type_labels == "transient"], na.rm = TRUE),
           ymin = 0, ymax = Inf, alpha = 0.1, colour = NA, fill = "red") +
  # add data
  geom_linerange(aes(xmin = event_duration_min_yr, xmax = event_duration_max_yr)) +
  geom_point(aes(x = event_duration_est_yr))
print(figS1_timescales)

ggsave(
  filename = file.path(dir_plots, "fig_S1_event_timescales.png"),
  plot = figS1_timescales, 
  width = 169.9,
  height = 150,
  units = "mm",
  dpi = 600,
  bg = "white"
)  

# plot figure S2: habitability change and duration ----
habitability_lim_max <- max(data_to_plot$change_max, na.rm = TRUE)
habitability_lim_min <- min(data_to_plot$change_min, na.rm = TRUE)
habitability_lim <- max(abs(habitability_lim_max), abs(habitability_lim_min), na.rm = TRUE) + 1

figS2_habitability <- data_to_plot %>% 
  dplyr::mutate(
    event_duration_min_yr = dplyr::case_when(
      event_duration_min_yr < 2e-01 ~ 2e-01, 
      .default = event_duration_min_yr
    ),
    type = ordered(type, 
                   levels = c("persistent", "sustainable stewardship",
                              "humans so far", "business as usual", "transient")
    ),
    change_type = ordered(change_type,
                          levels = c("species", "genus", "biomass", "productivity")
    ),
    type_labels = dplyr::case_when(type_labels == "humans" ~ "humans", 
                                   .default = "geological")
  ) %>%
  ggplot(aes(colour = type, fill = type, 
             alpha = type_labels, size = type_labels, 
             shape = change_type)) +
  theme_void() +
  theme(
    axis.title = element_text(size = point_size), 
    axis.title.y = element_text(angle = 90, hjust = 0.6),
    legend.position = "inside",
    legend.justification = c(0, 1),
    legend.position.inside = c(0.09, 0.98),
    legend.direction = "vertical",
    legend.box = "horizontal", 
    legend.box.background = element_blank(), # element_rect(colour = "black"),
    legend.box.margin = margin(4,4,4,4),
    legend.title = element_text(size = point_size-1,face = "bold"),
    legend.text = element_text(size = point_size-2)
  ) +
  guides(colour = guide_legend(order = 1), 
         fill = guide_legend(order = 1),
         shape = guide_legend(order = 2), 
         alpha = guide_legend(order = 3), 
         size = guide_legend(order = 3)) +
  labs(x = expression(bold("log"[10]*"(Duration [years])")),
       y = expression(bold("Planetary habitability (pseudo-log"[10]*"[variable change])"))) +
  scale_x_log10() +
  scale_y_continuous() +
  scale_shape_manual(values = c(21:24), name = "Biosphere variable") +
  scale_colour_manual(values = c("persistent" = plot_palette[5],
                                 "sustainable stewardship" = plot_palette[4],
                                 "humans so far" = "black", # plot_palette[3],
                                 "business as usual" = plot_palette[2],
                                 "transient" = plot_palette[1]
                                 ),
                      aesthetics = c("colour", "fill"),
                      name = "Disruptor type"
  ) +
  scale_alpha_manual(values = c("geological" = 0.3, "humans" = 0.5),
                     breaks = c("humans", "geological"),
                     name = "Scope"
  ) +
  scale_size_manual(values = c("geological" = 0.5*point_size/.pt, "humans" = 2*(point_size/.pt)),
                    breaks = c("humans", "geological"),
                    name = "Scope"
  ) +
  coord_cartesian(
    xlim = c(1e-01, NA),
    ylim = c(-1*(habitability_lim+(0.3*habitability_lim)), habitability_lim),
    clip = "on"
  ) +
  # add biosphere midline
  geom_segment(aes(x = 2e-01, xend = 2e+09, y = 0, yend = 0), 
               linetype = "dashed", linewidth = line_width/.pt, colour = "grey50"
  ) +
  # add data
  ## add duration range line
  geom_linerange(aes(y = change_mean, xmin = event_duration_min_yr, xmax = event_duration_max_yr),
                 linewidth = 0.5*(line_width/.pt)
  ) +
  ## add habitability change line
  geom_linerange(aes(x = event_duration_est_yr, ymin = change_min, ymax = change_max),
                 linewidth = 0.5*(line_width/.pt)
  ) +
  ## add central estimate point
  geom_point(aes(x = event_duration_est_yr, y = change_mean), stroke = line_width/.pt) +
  # add axes as annotations
  ## x-axis
  annotate(geom = "linerange", y = -1*(habitability_lim), xmin = 1.5e-01, xmax = 1.000e+10,
           linewidth = line_width/.pt, colour = "black") +
  annotate(geom = "text", y = -1*(habitability_lim), x = data_time_scale$value,
           label = data_time_scale$label,
           angle = 90, hjust = 1.1, vjust = 0, size = point_size/.pt, fontface = "bold") +
  ## y-axis
  annotate(geom = "label", x = 7e-02, y = 0.5*(habitability_lim-1),
           label = "biosphere net gain", fill = "white", border.colour = "white", 
           colour = plot_palette[4], angle = 90, size = point_size/.pt, fontface = "bold"
  ) +
  annotate(geom = "label", x = 7e-02, y = -0.5*(habitability_lim-1),
           label = "biosphere net loss", fill = "white", border.colour = "white", 
           colour = plot_palette[2], angle = 90, size = point_size/.pt, fontface = "bold") + 
  annotate(geom = "segment", x = 1.5e-01, xend = 1.5e-01, y = 0.1, yend = habitability_lim-1,
           linewidth = 1*line_width/.pt, colour = plot_palette[4], 
           arrow = arrow(length = unit(0.3, "cm")), lineend = "round", linejoin = "bevel") +
  annotate(geom = "segment", x = 1.5e-01, xend = 1.5e-01, y = -0.1, yend = -1*(habitability_lim-1),
           linewidth = 1*line_width/.pt, colour = plot_palette[2], 
           arrow = arrow(length = unit(0.3, "cm")), lineend = "round", linejoin = "bevel") 
print(figS2_habitability)

ggsave(
  filename = file.path(dir_plots, "fig_S2_deep_time_habitability.png"),
  plot = figS2_habitability, 
  width = 169.9,
  height = 150,
  units = "mm",
  dpi = 600,
  bg = "white"
  )  


# plot figure 3: change rates ----
data_to_plot_rates <- data_to_plot %>%
  dplyr::mutate(
    type = ordered(type, 
                   levels = c("persistent", "sustainable stewardship", "humans so far",
                              "business as usual", "transient")
    ),
    change_type = ordered(change_type,
                          levels = c("species", "genus", "biomass", "productivity")
    ),
    type_labels = dplyr::case_when(type_labels == "humans" ~ "humans",
                                   .default = "geological")
  ) %>%
  dplyr::mutate(interval_abbreviation = forcats::fct_reorder(
    .f = interval_abbreviation, .x = change_rate_mean, .fun = min, .na.rm = TRUE) 
  )

data_to_plot_rates_labels <- data_to_plot_rates %>%
  dplyr::group_by(interval_abbreviation, age_estimate_ma, type, type_labels) %>%
  dplyr::summarise(min_rate = min(change_rate_mean, na.rm = TRUE),
                   max_rate = max(change_rate_mean, na.rm = TRUE), 
                   .groups = "keep"
  ) %>% 
  dplyr::mutate(fontface = ifelse(type_labels == "humans", "bold", "plain"))

fig3_rates_of_change <- ggplot(mapping = aes(y = interval_abbreviation,
                                             fill = type,
                                             colour = type,
                                             alpha = type_labels)) +
  scale_x_continuous(expand = expansion(add = 2),
                     name = expression(bold("Rate of biosphere change [pseudo-log"[10]*"(% variable change per Myr)]"))) +
  scale_y_discrete() +
  scale_shape_manual(values = c(21:24), name = "Biosphere variable") +
  scale_fill_manual(values = c("persistent" = plot_palette[5], 
                               "sustainable stewardship" = plot_palette[4], 
                               "humans so far" = "black", # plot_palette[3],
                               "business as usual" = plot_palette[2], 
                               "transient" = plot_palette[1]),
                    aesthetics = c("colour", "fill"),
                    name = "Disruptor type") +
  scale_alpha_manual(values = c("geological" = 0.5, "humans" = 0.8),
                     breaks = c("humans", "geological"), 
                     name = "Scope") +
  scale_size_manual(values = c("geological" = point_size/.pt, "humans" = 2*(point_size/.pt)),
                    breaks = c("humans", "geological"),
                    name = "Scope") +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(), 
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = point_size,
                               margin = margin(10,0,2,0, unit = "pt"),
                               vjust = 1),
    panel.grid = element_blank(), 
    legend.position = "inside",
    legend.justification = c(0, 1),
    legend.position.inside = c(0, 1),
    legend.box.background = element_rect(colour = "white"),
    legend.box.margin = margin(4,4,4,4, unit = "pt"),
    legend.title = element_text(size = (point_size-2), face = "bold"),
    legend.text = element_text(size = (point_size-2))) +
  geom_vline(xintercept = 0, linetype = "solid", linewidth = 0.25*line_width/.pt, colour = "grey75") +
  geom_hline(yintercept = 0, linetype = "solid", linewidth = line_width/.pt, colour = "black") +
  geom_linerange(data = data_to_plot_rates_labels, 
                 mapping = aes(xmin = min_rate, 
                               xmax = max_rate), 
                 linewidth = 3) +
  geom_point(data = data_to_plot_rates, 
             mapping = aes(x = change_rate_mean,
                           # size = type_labels,
                           shape = change_type), 
             size = point_size/.pt) +
  geom_text(data = data_to_plot_rates_labels, 
            mapping = aes(x = min_rate - 0.4,
                          y = interval_abbreviation, 
                          label = interval_abbreviation, 
                          fontface = fontface),
            alpha = 1,
            angle = 0, hjust = 1, vjust = 0.5, size = (point_size-2)/.pt) +
  geom_hline(yintercept = c(1.45, 4.5, 29.45, 30.5, 34.45, 35.5),
             linetype = "dashed", linewidth = 0.25*line_width/.pt, colour = "grey75") +
  annotate(geom = "text",
           x = data_to_plot_rates_labels$max_rate + 0.3,
           y = data_to_plot_rates_labels$interval_abbreviation, 
           label = paste0("(", data_to_plot_rates_labels$age_estimate_ma, " Ma)"),
           angle = 0, hjust = 0, vjust = 0.5, size = (point_size-2)/.pt, fontface = "italic") +
  coord_cartesian(clip = "off") +
  labs(caption = paste0("Bars show the range of rates of change for each episode.", 
                        "Vertical solid line at no biosphere change.\n", 
                        "Horizontal dashed lines and bold text highlight humans ", 
                        "in the context of geological biosphere disruptors.")) +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 5)), 
         colour = guide_legend(order = 1, override.aes = list(size = 5)),
         shape = guide_legend(order = 2, override.aes = list(size = 5)), 
         fontface = guide_none(), 
         alpha = guide_none(), # guide_legend(order = 3),
         size = guide_none()) #guide_legend(order = 3))
print(fig3_rates_of_change)

ggsave(
  filename = file.path(dir_plots, "fig_3_rates_of_change_ordered.png"),
  plot = fig3_rates_of_change, 
  width = 169.6,
  height = 190,
  units = "mm",
  dpi = 600,
  bg = "white"
)  


# END ----

