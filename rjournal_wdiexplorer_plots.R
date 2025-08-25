library(htmlwidgets)
#library(wdiexplorer)
source("wdiexplorer_functions.R") 

# a folder to save interactive plots
dir.create("interactive-plots", showWarnings = FALSE)

# use the get_wdi_data() function to fetch the data set from the WDI
pm_data <- get_wdi_data(indicator = "EN.ATM.PM25.MC.M3")

# `compute_trend_shape_features()` function

pm_trend_shape <- compute_trend_shape_features(pm_data)

# `compute_diagnostic_indices()` function

pm_diagnostic_metrics <- compute_diagnostic_indices(pm_data, group_var = "region")

# The `add_group_info()` function

pm_diagnostic_metrics_group <- add_group_info(
  metric_summary = pm_diagnostic_metrics,
  pm_data
)

### The ungrouped and grouped data series trajectories with top 5% countries highlighted based on their dissimilarities.

saveWidget(plot_data_trajectories(
  pm_data, 
  metric_summary = pm_diagnostic_metrics, 
  metric_var = "country_avg_dist"
),
file = "interactive-plots/ungrouped_dissimilarity_plot.html",
selfcontained = TRUE
)

saveWidget(plot_data_trajectories(
  pm_data, 
  metric_summary = pm_diagnostic_metrics_group, 
  metric_var = "within_group_avg_dist",
  group_var = "region"
),
file = "interactive-plots/grouped_dissimilarity_plot.html",
selfcontained = TRUE
)

### The ungrouped and grouped data series trajectories with top 4% countries
#  highlighted based on the absolute values of the linearity measures.

pm_trend_shape <- pm_trend_shape |>
  dplyr::mutate(abs_linearity = abs(linearity))

pm_trend_shape_group <- add_group_info(
  metric_summary = pm_trend_shape, 
  pm_data
)

saveWidget(plot_data_trajectories(
  pm_data, 
  metric_summary = pm_trend_shape_group, 
  metric_var = "abs_linearity",
  percentile = 0.96
),
file = "interactive-plots/ungrouped_linearity_plot.html",
selfcontained = TRUE
)


saveWidget(plot_data_trajectories(
  pm_data, 
  metric_summary = pm_trend_shape_group, 
  metric_var = "abs_linearity",
  group_var = "region",
  percentile = 0.96
),
file = "interactive-plots/grouped_linearity_plot.html",
selfcontained = TRUE
)

### The ungrouped and grouped parallel coordinate plots

saveWidget(plot_parallel_coords(
  diagnostic_summary = pm_diagnostic_metrics_group,
  colour_var = "region"
),
file = "interactive-plots/ungrouped_parallel_plot.html",
selfcontained = TRUE
)


saveWidget(plot_parallel_coords(
  diagnostic_summary = pm_diagnostic_metrics_group,
  colour_var = "region",
  group_var = "region"
),
file = "interactive-plots/grouped_parallel_plot.html",
selfcontained = TRUE
)

### The ungrouped and grouped metric linkview plots

saveWidget(plot_metric_linkview(
  pm_data, 
  metric_summary = pm_diagnostic_metrics,
  metric_var = c("linearity", "curvature")
),
file = "interactive-plots/ungrouped_linkview_plot.html",
selfcontained = TRUE
)


saveWidget(plot_metric_linkview(
  pm_data, 
  metric_summary = pm_diagnostic_metrics_group,
  metric_var = c("linearity", "curvature"),
  group_var = "region"
),
file = "interactive-plots/grouped_linkview_plot.html",
selfcontained = TRUE
)



#### creating a folder for all the plots

# a list of all HTML files inside the interactive-plots folder
files <- list.files("interactive-plots", pattern = "\\.html$", full.names = FALSE)

links <- paste0("<li><a href='interactive-plots/", files, "'>", files, "</a></li>")


wdiexplorer_interactive_plot_html <- paste0(
  "<!DOCTYPE html><html><head><title>PM2.5 air pollution data interactive plots of the wdiexplorer R package gallery</title></head><body>",
  "<h1>PM2.5 air pollution data interactive plots of the wdiexplorer R package gallery</h1><ul>",
  paste(links, collapse = "\n"),
  "</ul></body></html>"
)

writeLines(wdiexplorer_interactive_plot_html, "index.html")
