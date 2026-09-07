# ==============================================================================
# Script: visualization.R
# Purpose: Reusable, production-ready visualization functions for NEXORA
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)

# ==============================================================================
# Data Type Helper Functions & Safeguards
# ==============================================================================

get_numeric_columns <- function(data) {
  if (is.null(data) || !is.data.frame(data)) return(character(0))
  names(data)[sapply(data, is.numeric)]
}

get_categorical_columns <- function(data) {
  if (is.null(data) || !is.data.frame(data)) return(character(0))
  names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
}

create_blank_message <- function(msg) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = msg, size = 5, color = "#64748B", fontface = "italic") +
    theme_void() +
    theme(panel.background = element_rect(fill = "#F8FAFC", color = NA))
}

# ==============================================================================
# Histogram Plot (With Dynamic Bins)
# ==============================================================================

generate_histogram <- function(data, var, bins = 30) {
  if (is.null(var) || !(var %in% colnames(data))) return(create_blank_message("Please select a numeric variable."))
  
  clean_data <- data[!is.na(data[[var]]), ]
  if (nrow(clean_data) == 0) return(create_blank_message("No valid numeric data to display."))
  
  ggplot(clean_data, aes(x = .data[[var]])) +
    geom_histogram(
      fill = "#2563EB",
      color = "#FFFFFF",
      bins = bins,
      alpha = 0.85
    ) +
    labs(
      title = paste("Distribution of", var),
      x = var,
      y = "Frequency"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", color = "#1E3A8A"),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      panel.grid.minor = element_blank()
    )
}

# ==============================================================================
# Bar Chart
# ==============================================================================

generate_bar <- function(data, cat_var) {
  if (is.null(cat_var) || !(cat_var %in% colnames(data))) return(create_blank_message("Please select a categorical variable."))
  
  clean_data <- data[!is.na(data[[cat_var]]), ]
  if (nrow(clean_data) == 0) return(create_blank_message("No valid categorical data to display."))
  
  bar_data <- clean_data %>%
    count(.data[[cat_var]]) %>%
    arrange(desc(n)) %>%
    slice_head(n = 15)
  
  ggplot(bar_data, aes(x = reorder(factor(.data[[cat_var]]), -n), y = n)) +
    geom_col(fill = "#2563EB", alpha = 0.85, color = "#1E40AF") +
    labs(
      title = paste("Count Breakdown of", cat_var, if(nrow(clean_data) > 15) "(Top 15 Categories)" else ""),
      x = cat_var,
      y = "Count"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", color = "#1E3A8A"),
      axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
      panel.grid.minor = element_blank()
    )
}

# ==============================================================================
# Box Plot
# ==============================================================================

generate_boxplot <- function(data, num_var, cat_var) {
  if (is.null(num_var) || !(num_var %in% colnames(data))) return(create_blank_message("Please select a numeric variable."))
  if (is.null(cat_var) || !(cat_var %in% colnames(data))) return(create_blank_message("Please select a categorical variable."))
  
  clean_data <- data[!is.na(data[[num_var]]) & !is.na(data[[cat_var]]), ]
  if (nrow(clean_data) == 0) return(create_blank_message("No overlapping complete observations to plot."))
  
  top_cats <- clean_data %>% count(.data[[cat_var]]) %>% arrange(desc(n)) %>% slice_head(n = 10) %>% pull(1)
  clean_data <- clean_data[clean_data[[cat_var]] %in% top_cats, ]
  
  ggplot(clean_data, aes(
    x = factor(.data[[cat_var]]),
    y = .data[[num_var]],
    fill = factor(.data[[cat_var]])
  )) +
    geom_boxplot(alpha = 0.8, color = "#1E293B", outlier.color = "#EF4444", outlier.size = 2) +
    scale_fill_viridis_d(option = "mako", begin = 0.2, end = 0.8) +
    labs(
      title = paste(num_var, "Grouped by", cat_var),
      x = cat_var,
      y = num_var
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", color = "#1E3A8A"),
      legend.position = "none",
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    )
}

# ==============================================================================
# Scatter Plot
# ==============================================================================

generate_scatter <- function(data, xvar, yvar) {
  if (is.null(xvar) || !(xvar %in% colnames(data))) return(create_blank_message("Please select an X variable."))
  if (is.null(yvar) || !(yvar %in% colnames(data))) return(create_blank_message("Please select a Y variable."))
  
  clean_data <- data[!is.na(data[[xvar]]) & !is.na(data[[yvar]]), ]
  if (nrow(clean_data) == 0) return(create_blank_message("No overlapping complete observations to plot."))
  
  ggplot(clean_data, aes(x = .data[[xvar]], y = .data[[yvar]])) +
    geom_point(size = 3.5, alpha = 0.75, color = "#2563EB") +
    geom_smooth(method = "lm", se = TRUE, color = "#DC2626", fill = "#FCA5A5", linewidth = 1.2) +
    labs(
      title = paste(yvar, "vs.", xvar),
      x = xvar,
      y = yvar
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", color = "#1E3A8A"),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      panel.grid.minor = element_blank()
    )
}

# ==============================================================================
# Pie Chart
# ==============================================================================

generate_pie <- function(data, cat_var) {
  if (is.null(cat_var) || !(cat_var %in% colnames(data))) return(create_blank_message("Please select a categorical variable."))
  
  clean_data <- data[!is.na(data[[cat_var]]), ]
  if (nrow(clean_data) == 0) return(create_blank_message("No valid categorical data to display."))
  
  pie_data <- clean_data %>%
    count(.data[[cat_var]]) %>%
    arrange(desc(n)) %>%
    slice_head(n = 8) %>%
    mutate(
      pct = n / sum(n),
      label = paste0(round(pct * 100, 1), "%")
    )
  
  ggplot(
    pie_data,
    aes(
      x = "",
      y = n,
      fill = factor(.data[[cat_var]])
    )
  ) +
    geom_col(width = 1, color = "#FFFFFF") +
    coord_polar("y", start = 0) +
    scale_fill_hue() +
    labs(
      title = paste("Proportion Breakdown of", cat_var),
      fill = cat_var
    ) +
    theme_void(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", color = "#1E3A8A", hjust = 0.5)
    )
}

# ==============================================================================
# Central Dispatcher Function
# ==============================================================================

generate_selected_plot <- function(
    data,
    plot_type,
    x = NULL,
    y = NULL,
    bins = 30) {
  
  if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    return(create_blank_message("📁 Please upload a dataset to generate visual plots."))
  }
  
  switch(
    plot_type,
    "Histogram"    = generate_histogram(data, x, bins = bins),
    "Box Plot"     = generate_boxplot(data, y, x),
    "Scatter Plot" = generate_scatter(data, x, y),
    "Bar Chart"    = generate_bar(data, x),
    "Pie Chart"    = generate_pie(data, x),
    create_blank_message("Select a valid plot type.")
  )
}