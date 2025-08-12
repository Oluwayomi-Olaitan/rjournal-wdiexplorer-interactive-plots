#### - `get_wdi_data()` function

get_wdi_data <- function(indicator) {
  
  # Create a folder to store the data if it does not exist
  if (!dir.exists("wdi_data")) {
    dir.create("wdi_data")
  }
  
  wdi_path <- file.path("wdi_data", paste0(indicator, ".rds"))
  
  # check if the data for the specified wdi_path exists.
  
  if (file.exists(wdi_path)) {
    message(paste("Loading WDI indicator:", indicator, "data from data folder."))
    wdi_data <- readRDS(wdi_path)
  } else {
    message(paste("Downloading WDI indicator:", indicator, "data using the WDI R package."))
    # download data from the WDI using the WDI R package
    wdi_data <- WDI::WDI(
      indicator = indicator,
      country = "all",
      extra = TRUE
    ) |> 
      dplyr::filter(region != "Aggregates") |> # filter out countries additional entries that are not country names
      dplyr::mutate(dplyr::across(
        dplyr::where(is.character),
        ~ gsub("'", "", .)
      ))
    
    # Save the data set for this indicator
    saveRDS(wdi_data, wdi_path)
  }
  
  attr(wdi_data, "index_var") <- setdiff(
    names(wdi_data),
    c("country", "iso2c", "iso3c", "year", "status", "lastupdated",
      "region", "capital", "longitude", "latitude", "income", "lending")
  )
  
  # return the data
  return(wdi_data)
}
#-------------------------------------------------------------------------------

#### - `plot_missing()` function

plot_missing <- function(wdi_data, index = NULL, group_var){
  # Identify the name of the variable in the wdi data that contains the country-year value as index
  if(is.null(index)) {
    index_var <- attr(wdi_data, "index_var") 
    index = index_var[1]
  }
  
  data_long <- wdi_data |>
    dplyr::select(country, year, tidyselect::all_of(group_var), tidyselect::all_of(index)) |>
    dplyr::mutate(
      missing = is.na(.data[[index]]),
      group = as.factor(.data[[group_var]]),
      # colors in the order of the group levels
      colour = scales::hue_pal()(length(levels(group)))[as.integer(group)],
      country_label = paste0("<span style='color:", colour, "'>", country, "</span>"),
      # reverse the countries
      country_label = forcats::fct_rev(country_label)
    ) 
  
  # calculate the missing/present percentage for the lengend
  percentages <- data_long |>
    dplyr::summarise(
      present = round((sum(!missing) / dplyr::n() *100), 1),
      missing = round((sum(missing) / dplyr::n() *100), 1)
    )
  
  # missingness plot
  P <- data_long |>
    ggplot2::ggplot() +
    ggplot2::aes(x = year, y = country_label, fill = missing) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_manual(
      name = " ", 
      values = c("TRUE" = "black", "FALSE" = "grey90"),
      labels = c(
        paste0("Present (", percentages$present, "%)"),
        paste0("Missing (", percentages$missing, "%)")
      )
    ) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(8), expand = c(0, 0)) +
    ggplot2::labs( x = " ", y = " ") +
    ggplot2::facet_grid(ggplot2::vars(group), scales = "free_y", space = "free_y", switch = "y") +
    ggplot2::theme(
      axis.text.y = ggtext::element_markdown(size = 6.2),
      axis.ticks.y = ggplot2::element_blank(),
      legend.position = "bottom",
      panel.spacing = grid::unit(0.05, "lines"),
      strip.text.y.left = ggplot2::element_text(hjust = 1, angle = 0),
      strip.background = ggplot2::element_rect(fill = "white"),
      strip.placement = "outside"
    )
  
  
  # converting the ggplot to a graphical object
  G <- ggplot2::ggplotGrob(P)
  
  stripl <- which(grepl('strip-l', G$layout$name))  #the strip names has been switch to the left, hence the reason for the strip-l
  
  # Extract the default colors from the data
  default_colors <- data_long |> 
    dplyr::select(group, colour) |> 
    dplyr::distinct()
  
  for (i in stripl){
    j <- which(grepl('rect', G$grobs[[i]]$grobs[[1]]$childrenOrder))
    k <- which(grepl('text', G$grobs[[i]]$grobs[[1]]$childrenOrder))
    grobtext <- G$grobs[[i]]$grobs[[1]]$children[[k]]$children[[1]]$label
    st <- match(grobtext, default_colors$group)
    if (!is.na(st)){
      r <- which(grepl('text', G$grobs[[i]]$grobs[[1]]$children[[k]]$childrenOrder))
      G$grobs[[i]]$grobs[[1]]$children[[k]]$children[[r]]$gp$col <- default_colors[st, "colour"]
    }
  }
  
  grid::grid.draw(G)
}
#-------------------------------------------------------------------------------

#### - `get_valid_data()` function

get_valid_data <- function(wdi_data, index = NULL) {
  # Identify the name of the variable in the wdi data that contains the country-year value as index
  if(is.null(index)) {
    index_var <- attr(wdi_data, "index_var") 
    index = index_var[1]
  }
  
  # Identify years with no data points
  invalid_years <- wdi_data |> 
    dplyr::group_by(year) |>
    dplyr::summarise(n = sum(!is.na(.data[[index]]))) |>
    dplyr::filter(n==0) |>
    dplyr::pull(year)
  
  #return message about missing years
  year_message <- if(length(invalid_years) > 0) {
    paste("The", length(invalid_years), 
          "year(s) listed below had no available data and were excluded:\n", 
          paste(invalid_years, collapse = ", \t"))
  } else{
    "All years have at least one valid data point"
  }
  
  # Identify country with no data points
  invalid_countries <- wdi_data |> 
    dplyr::group_by(country) |>
    dplyr::summarise(n = sum(!is.na(.data[[index]]))) |>
    dplyr::filter(n==0) |> 
    dplyr::pull(country)
  
  #return message about missing countries
  country_message <- if(length(invalid_countries) > 0) {
    paste("The", length(invalid_countries), 
          "countries listed below had no available data and were excluded:\n", 
          paste(invalid_countries, collapse = "\n- "))
  } else{
    "All countries have at least one valid data point"
  }
  
  # filter valid countries and years where actual data were collected, ignoring the WDI defaults 
  valid_data <- wdi_data |>
    dplyr::arrange(year) |> # arrange year in ascending order
    dplyr::group_by(year) |>
    dplyr::filter(sum(!is.na(.data[[index]])) > 0) |> #Ensure a year level has at least one valid point
    dplyr::ungroup() |>
    dplyr::group_by(country) |>
    dplyr::filter(sum(!is.na(.data[[index]])) > 0) |> #Ensure a country level has at least one valid point 
    dplyr::ungroup()
  
  # print the messages
  cat(paste0(country_message, "\n\n", year_message), "\n")
  
  return(valid_data)
}
#-------------------------------------------------------------------------------

#### - `compute_dissimilarity()` function

compute_dissimilarity <- function(wdi_data, index = NULL, metric = "euclidean"){
  # Identify the name of the variable in the wdi data that contains the country-year value as index
  if(is.null(index)) {
    index_var <- attr(wdi_data, "index_var") 
    index = index_var[1]
  }
  
  # verify that the country, year and index columns exist in the given wdi_data
  #if the columns exist, proceed, else paste the error message
  cols <- c("country", "year", index)
  missing_cols <- cols[!cols %in% names(wdi_data)]
  if (length(missing_cols) > 0){
    stop(paste("The required column(s) are missing in the provided dataset:",
               paste(missing_cols, collapse = ", ")))
  }
  
  # convert the wdi_data to a wider form
  data_wide <- wdi_data |>
    dplyr::select(country, year, tidyselect::all_of(index)) |>
    tidyr::pivot_wider(
      names_from = year,
      values_from = tidyselect::all_of(index)
    ) |>
    tibble::column_to_rownames("country") |> # set columns to row names
    as.data.frame()
  
  # dissimilarity distance using daisy
  dist <- cluster::daisy(data_wide, metric = metric)
  
  # convert the dist to matrix
  dist_mat <- as.matrix(dist)
  
  # set the dist of a country to itself to NA
  diag(dist_mat) <- NA
  
  return(dist_mat)
}
#-------------------------------------------------------------------------------

#### - `compute_variation()` function

compute_variation <- function(wdi_data, diss_matrix = compute_dissimilarity(wdi_data), group_var)  {
  
  # Convert the dissimilarity distance matrix to a data frame
  diss_df <- as.data.frame(as.table(diss_matrix))
  colnames(diss_df) <- c("country2", "country1", "dist")
  
  # country2 as the first column because those are columns names and country1 are row names.
  # join the group_data with the diss_df
  
  # join the grouping information from the wdi_data to the diss_df
  diss_data <- diss_df |>
    dplyr::left_join(
      wdi_data |> dplyr::select(country, tidyselect::all_of(group_var)),
      by = c("country1" = "country"),
      multiple = "first"
    ) |>
    dplyr::left_join(
      wdi_data |> dplyr::select(country, tidyselect::all_of(group_var)),
      by = c("country2" = "country"),
      multiple = "first", 
      suffix = c("_1", "_2")
    ) |>
    dplyr::rename(
      group1 = all_of(paste0(group_var, "_1")),
      group2 = all_of(paste0(group_var, "_2"))
    ) 
  
  # calculate the country average dist
  country_avg_dist <- diss_data |>
    dplyr::group_by(country1) |>
    dplyr::filter(sum(!is.na(dist)) > 0) |> # Ensure that a country has at least one valid dist
    dplyr::summarise(
      country_avg_dist = mean(dist, na.rm = TRUE),
      .groups = "drop")
  
  # silhouette calculations
  
  # for each country1, find the mean of dist if group1 == group2
  a_vals <- diss_data |>
    dplyr::filter(group1 == group2) |>
    dplyr::group_by(country1, group1) |>
    dplyr::filter(sum(!is.na(dist)) > 0) |>  # Ensure that a country has at least one valid dist
    dplyr::summarise(a = mean(dist, na.rm = TRUE),
                     .groups = "drop")
  
  # for each country1, find the mean of dist if group1 != group2
  neighbour_vals <- diss_data |>
    dplyr::filter(group1 != group2) |>
    dplyr::group_by(country1, group2) |>
    dplyr::filter(sum(!is.na(dist)) > 0) |>  # Ensure that a country has at least one valid dist
    dplyr::summarise(avg_dist = mean(dist, na.rm = TRUE),
                     .groups = "drop")
  
  # select the group with the min avg_dist
  b_vals <- neighbour_vals |>
    dplyr::group_by(country1) |>
    dplyr::summarise(neighbour = group2[which.min(avg_dist)],
                     b = min(avg_dist, na.rm = TRUE),
                     .groups = "drop")
  
  # calculate the silhouette score of each country
  variation_scores <- country_avg_dist |>
    dplyr::left_join(a_vals, dplyr::join_by("country1")) |>
    dplyr::left_join(b_vals, dplyr::join_by("country1")) |>
    dplyr::mutate(sil_width = dplyr::if_else(is.na(a), 
                                             0, # if a is NA, set sil_width = 0
                                             ((b - a)/ pmax(a, b)))) |> #parrallel max value btw a and b
    dplyr::select(country = country1, group = group1, country_avg_dist,
                  within_group_avg_dist = a, sil_width)
  
  
  return(variation_scores)
}
#-------------------------------------------------------------------------------

#### - `compute_trend_shape_features()` function

compute_trend_shape_features <- function(wdi_data, index = NULL){
  # Identify the name of the variable in the wdi data that contains the country-year value as index
  if(is.null(index)) {
    index_var <- attr(wdi_data, "index_var") 
    index = index_var[1]
  }
  
  # Check if 'country', 'year', and 'index' columns are present in the data set.
  # Proceed if all exist; otherwise, return the informative error message.
  cols <- c("country", "year", index)
  missing_cols <- cols[!cols %in% names(wdi_data)]
  if (length(missing_cols) > 0){
    stop(paste("The required column(s) are missing in the provided dataset:",
               paste(missing_cols, collapse = ", ")))
  }
  
  
  # filter valid countries and years where actual data were collected, ignoring the WDI defaults 
  # using the `get_valid_data()` function
  invisible(capture.output(
    valid_data <- get_valid_data(wdi_data)
  ))
  
  # calculate smoothness
  smoothness <- valid_data |>
    dplyr::arrange(country, year) |>
    dplyr::group_by(country) |>
    dplyr::mutate(
      diff = (.data[[index]] - dplyr::lag(.data[[index]])) / 
        (year - dplyr::lag(year))
    ) |>
    dplyr::summarise(
      smoothness = stats::sd(diff, na.rm = TRUE),
      .groups = "drop"
    ) 
  
  # check if valid data has missing entries, if there are missing points, replace by linear interpolation, else return the valid data
  if(any(is.na(valid_data[[index]]))) {
    complete_data <- valid_data |>
      dplyr::group_by(country) |>
      dplyr::arrange(year) |>
      dplyr::mutate(!!index := approx(
        x = year[!is.na(.data[[index]])], # take the non-missing years as x
        y = .data[[index]][!is.na(.data[[index]])], # take the non-missing values as y
        xout = year, 
        rule = 2 # if the immediate nearest values are also NA, use the next available point for interpolation
      )$y) |>
      dplyr::ungroup()
    
    cat(
      "Note: The dataset '",deparse(substitute(wdi_data)),
      "' has missing values.\n Missing entries were replaced by linear interpolation."
    )
  } else { 
    complete_data <- valid_data 
  }
  
  # the features from the feasts package
  # convert the data to tsibble
  data_tsibble <- tsibble::as_tsibble(complete_data,
                                      index = year,
                                      key = country,
                                      regular = TRUE)
  
  trend_features <- data_tsibble |>
    fabletools::features(.data[[index]], feasts::feat_stl) |>
    dplyr::select(country, trend_strength, linearity, curvature)
  
  trend_shape_features <- trend_features |>
    dplyr::left_join(smoothness, by = "country")
  
  return(trend_shape_features)
}
#-------------------------------------------------------------------------------

#### - `compute_temporal_features()` function

compute_temporal_features <- function(wdi_data, index = NULL){
  
  # Identify the name of the variable in the wdi data that contains the country-year value as index
  if(is.null(index)) {
    index_var <- attr(wdi_data, "index_var") 
    index = index_var[1]
  }
  
  # Check if 'country', 'year', and 'index' columns are present in the data set.
  # Proceed if all exist; otherwise, return the informative error message.
  cols <- c("country", "year", index)
  missing_cols <- cols[!cols %in% names(wdi_data)]
  if (length(missing_cols) > 0){
    stop(paste("The required column(s) are missing in the provided dataset:",
               paste(missing_cols, collapse = ", ")))
  }
  
  # filter valid countries and years where actual data were collected, ignoring the WDI defaults 
  # using the `get_valid_data()` function
  invisible(capture.output(
    valid_data <- get_valid_data(wdi_data)
  ))
  
  temporal_measures <- valid_data |>
    dplyr::group_by(country) |>
    dplyr::arrange(country, year) |> # arrange year in ascending order within countries
    dplyr::summarise(
      crossing_points = feasts::n_crossing_points(.data[[index]]),
      flat_spot = feasts::longest_flat_spot(.data[[index]]),
      acf = cor(.data[[index]], dplyr::lag(.data[[index]]), use = "complete.obs")
    )
  
  return(temporal_measures)
}
#-------------------------------------------------------------------------------

#### - `add_group_info()` function

add_group_info <- function(metric_summary, wdi_data){
  # Check if 'country' exists in metric_summary
  if (!"country" %in% names(metric_summary)) {
    stop("The required column 'country' is missing in the metric_summary dataset.")
  }
  country_groups <- metric_summary |>
    dplyr::left_join(
      wdi_data |> dplyr::select(country, region, income),
      by = "country",
      multiple = "first"
    ) |>
    dplyr::select(country, region, income, dplyr::everything())
  
  return(country_groups)
}
#-------------------------------------------------------------------------------

#### - `diagnostic_indices()` function

compute_diagnostic_indices <- function(wdi_data, index = NULL, group_var) {
  # Identify the name of the variable in the wdi data that contains the country-year value as index
  if(is.null(index)) {
    index_var <- attr(wdi_data, "index_var") 
    index = index_var[1]
  }
  
  # filter valid data using the `get_valid_data()` function
  invisible(capture.output(
    valid_data <- get_valid_data(wdi_data)
  ))
  
  # compute variation
  variation <- compute_variation(wdi_data, group_var = group_var)
  
  # compute trend and shape measures
  trend_shape <- compute_trend_shape_features(wdi_data)
  
  # compute sequential measures
  sequential <- compute_temporal_features(wdi_data)
  
  combine_measures <- variation |>
    dplyr::select(-group) |>
    dplyr::left_join(trend_shape, by = "country") |>
    dplyr::left_join(sequential, by = "country")
  
  
  return(combine_measures)
}
#-------------------------------------------------------------------------------

#### `plot_metric_distribution()` function

plot_metric_distribution <- function(metric_summary, colour_var, metric_var = NULL, group_var = NULL){
  # diagnostic summary could either be the output of the diagnostic indices or a collection of the features 
  
  if(is.null(metric_var)){
    # pivot_long all the metric summaries
    summary_long <- metric_summary |>
      tidyr::pivot_longer(
        -c(country, region, income),
        names_to = "diagnostics",
        values_to = "metrics"
      ) |>
      dplyr::mutate(
        diagnostics = factor(diagnostics, levels = unique(diagnostics))
      )
  } else{
    # one or more selected features
    summary_long <- metric_summary |>
      tidyr::pivot_longer(
        c(tidyselect::all_of(metric_var)),
        names_to = "diagnostics",
        values_to = "metrics"
      ) |>
      dplyr::mutate(
        diagnostics = factor(diagnostics, levels = unique(diagnostics)),
        
      )
  }
  # the plots
  if(is.null(group_var)){
    # ungrouped plot 
    P <- summary_long |>
      ggplot2::ggplot(
        ggplot2::aes(
          x = metrics, fill = forcats::fct_infreq(.data[[colour_var]]), 
          group = NA, order = forcats::fct_infreq(.data[[colour_var]]))
      ) +
      ggdist::geom_dots(slab_color = NA) +
      ggplot2::facet_wrap(~diagnostics, scales = "free") +
      ggplot2::labs(x = "Metric Values", y = "", fill = colour_var) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        legend.position = "bottom"
      )
  } else{
    # grouped plot
    P <- summary_long |>
      ggplot2::ggplot(
        ggplot2::aes(
          x = metrics, y = forcats::fct_infreq(.data[[group_var]]), 
          fill = forcats::fct_infreq(.data[[colour_var]]), group = NA, order = .data[[group_var]])
      ) +
      ggdist::geom_dots(slab_color = NA) +
      ggplot2::facet_wrap(~diagnostics, scales = "free") +
      ggplot2::labs(x = "Metric Values", y = "", fill = colour_var) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        legend.position = "bottom"
      )
  }
  return(P)
}

#-------------------------------------------------------------------------------  

#### - `plot_metric_partition()` function

plot_metric_partition <- function(metric_summary, metric_var, group_var){
  group_metric <- metric_summary |>
    dplyr::group_by(.data[[group_var]]) |>
    dplyr::mutate(group_avg = mean(.data[[metric_var]])) |>
    dplyr::ungroup() |>
    # Re-order group_var by the group_avg
    dplyr::mutate(group = forcats::fct_reorder(.data[[group_var]], -group_avg)) |>
    # Now create colors in the order of reordered groups
    dplyr::mutate(
      color = scales::hue_pal()(length(levels(group)))[as.integer(group)],
      country_label = paste0("<span style='color:", color, "'>", country, "</span>")
    ) |>
    dplyr::group_by(group) |>
    dplyr::arrange(.data[[metric_var]], .by_group = TRUE) |>
    dplyr::mutate(country_label = forcats::fct_inorder(country_label)) |>
    dplyr::ungroup()
  
  # The plot
  P <- group_metric |>
    ggplot2::ggplot() +
    # Draw group average bars in the background
    ggplot2::geom_col(
      ggplot2::aes(x = group_avg,
                   y = country_label, fill = group),
      width = 1, alpha = 0.25) +
    ggplot2::geom_col(
      ggplot2::aes(x = .data[[metric_var]],
                   y = country_label, fill = group),
      width = 0.75) +
    ggplot2::facet_grid(ggplot2::vars(group), scales = "free_y", space = "free_y", switch = "y") +
    ggplot2::labs(x = paste0("Metric Measure: ",metric_var), y = " " ) +
    ggplot2::theme(
      axis.text.y = ggtext::element_markdown(size = 5.6),
      axis.ticks.y = ggplot2::element_blank(),
      legend.position = "none",
      panel.spacing = grid::unit(0.05, "lines"),
      strip.text.y.left = ggplot2::element_text(hjust = 1, angle = 0),
      strip.background = ggplot2::element_rect(fill = "white"),
      strip.placement = "outside"
    )
  
  # converting the ggplot to a graphical object
  
  G <- ggplot2::ggplotGrob(P)
  
  stripl <- which(grepl('strip-l', G$layout$name))  #the strip names has been switch to the left, hence the reason for the strip-l
  
  # let's extract the default color scheme used by geom_col
  # Build the ggplot to extract fill colors
  built <- ggplot2::ggplot_build(P)
  
  # Extract fill colors from the first layer (group_avg bars) and 
  # adding the default fill color to the panel_groups
  default_colors <- built$data[[1]] |>
    dplyr::select(PANEL, fill) |>
    dplyr::distinct() 
  
  panel_groups <- built$layout$layout |>
    dplyr::select(PANEL, group) 
  
  fill_colors <- panel_groups |>
    dplyr::left_join(default_colors, by = "PANEL")
  
  
  for (i in stripl){
    j <- which(grepl('rect', G$grobs[[i]]$grobs[[1]]$childrenOrder))
    k <- which(grepl('text', G$grobs[[i]]$grobs[[1]]$childrenOrder))
    grobtext <- G$grobs[[i]]$grobs[[1]]$children[[k]]$children[[1]]$label
    st <- match(gsub("\n", " ", grobtext), fill_colors$group)
    if (!is.na(st)){
      r <- which(grepl('text', G$grobs[[i]]$grobs[[1]]$children[[k]]$childrenOrder))
      G$grobs[[i]]$grobs[[1]]$children[[k]]$children[[r]]$gp$col <- fill_colors[st, "fill"]
    }
  }
  
  grid::grid.draw(G)
}
#-------------------------------------------------------------------------------

#### `plot_data_trajectories()` function

plot_data_trajectories <- function(wdi_data, index = NULL, group_var = NULL, metric_summary = NULL, metric_var = NULL, percentile = 0.95){
  
  # Identify the name of the variable in the wdi data that contains the country-year value as index
  if(is.null(index)) {
    index_var <- attr(wdi_data, "index_var") 
    index = index_var[1]
  }
  
  if(is.null(metric_summary) && is.null(metric_var)){
    # plot the data trajectories
    if(is.null(group_var)){
      # ungrouped plot
      P <- ggplot2::ggplot() +
        ggiraph::geom_line_interactive(
          data = na.omit(wdi_data),
          ggplot2::aes(
            x = year,
            y = .data[[index]],
            group = country,
            tooltip = country, 
            data_id = country
          ),
          color = "grey65"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none")
    } else{
      # grouped plot
      P <- ggplot2::ggplot() +
        ggiraph::geom_line_interactive(
          data = na.omit(wdi_data),
          ggplot2::aes(
            x = year,
            y = .data[[index]],
            group = country,
            tooltip = country, 
            data_id = country
          ),
          color = "grey65"
        ) +
        ggplot2::facet_wrap(~.data[[group_var]]) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none") 
    }
    
  } else{
    # the percentile label
    percentile_label <- paste0("<=", percentile *100, "%")
    
    # plot the metrics with the data trajectories
    if(is.null(group_var)){
      # ungrouped plot
      # global threshold
      # joining the metric_data to the wdi_data.
      metric_data <- wdi_data |>
        dplyr::select(country, region, income, year, tidyselect::all_of(index)) |>
        dplyr::left_join(metric_summary, by = "country")
      
      # calculate the cutoff for the country metrics.
      cutoff_value <- quantile(metric_summary[[metric_var]], probs = percentile, na.rm = TRUE)
      
      # the plot
      P <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = na.omit(metric_data) |>
            dplyr::filter(.data[[metric_var]] <= cutoff_value),
          ggplot2::aes(
            x = year,
            y = .data[[index]],
            group = reorder(country, .data[[metric_var]]),
            colour = "below"
          )
        ) +   
        ggplot2::scale_colour_manual(
          name = NULL,
          values = c("below" = "grey"),
          labels = c("below" = percentile_label),
          guide = ggplot2::guide_legend(order = 2)
        ) +
        ggnewscale::new_scale_color() +
        ggiraph::geom_line_interactive(
          data = na.omit(metric_data) |>
            dplyr::filter(.data[[metric_var]] > cutoff_value),
          ggplot2::aes(
            x = year,
            y = .data[[index]],
            group = reorder(country, .data[[metric_var]]),
            colour = .data[[metric_var]],
            tooltip = paste(
              "Country:", country, 
              "<br>Value:", sprintf("%.2f", .data[[metric_var]])), 
            data_id = country
          )
        ) +
        ggplot2::scale_colour_viridis_c(
          option = "C", 
          direction = -1,
          name = paste0(metric_var, "\n (Top ", round((1 - percentile) * 100), "%)"),
          guide = ggplot2::guide_colorbar(order = 1)
        ) +
        ggplot2::theme_classic()+
        ggplot2::theme(
          legend.spacing.y = ggplot2::unit(0.2, "cm")
        )
    } else{
      # compute group quantile threshold
      group_metric <- metric_summary |>
        dplyr::group_by(.data[[group_var]]) |>
        dplyr::mutate(
          threshold = quantile(.data[[metric_var]], 
                               probs = percentile, 
                               na.rm = TRUE)
        )
      
      # join the group_metric to the wdi_data   
      wdi_data |>
        dplyr::select(country, year, tidyselect::all_of(index)) |>
        dplyr::left_join(group_metric, by = "country") -> group_metric_data
      
      # the faceted plot
      P <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = na.omit(group_metric_data) |>
            dplyr::filter(.data[[metric_var]] <= threshold),
          ggplot2::aes(
            x = year,
            y = .data[[index]],
            group = reorder(country, .data[[metric_var]]),
            colour = "below"
          )
        ) +   
        ggplot2::scale_colour_manual(
          name = NULL,
          values = c("below" = "grey"),
          labels = c("below" = percentile_label),
          guide = ggplot2::guide_legend(order = 2)
        ) +
        ggnewscale::new_scale_color() +
        ggiraph::geom_line_interactive(
          data = na.omit(group_metric_data) |>
            dplyr::filter(.data[[metric_var]] > threshold),
          ggplot2::aes(
            x = year,
            y = .data[[index]],
            group = reorder(country, .data[[metric_var]]),
            colour = .data[[metric_var]],
            tooltip = paste(
              "Country:", country, 
              "<br>Value:", sprintf("%.2f", .data[[metric_var]])), 
            data_id = country
          )
        ) +
        ggplot2::scale_colour_viridis_c(
          option = "C", 
          direction = -1,
          name = paste0(metric_var, "\n (Top ", round((1 - percentile) * 100), "%)"),
          guide = ggplot2::guide_colorbar(order = 1)
        ) +
        ggplot2::facet_wrap(as.formula(paste("~", group_var))) +
        ggplot2::theme_classic()+
        ggplot2::theme(
          legend.spacing.y = ggplot2::unit(0.2, "cm")
        )
    }
  }
  
  # Interactive plot
  ggiraph::girafe(
    ggobj = P,
    width_svg = 10, 
    height_svg = 6,
    options = list(
      ggiraph::opts_hover_inv(css = "opacity: 0.1;"),  # makes non-hovered lines fade
      ggiraph::opts_hover(css = "opacity: 1; stroke-width: 2;")  # on hover: no color change
    )
  )  
}

#-------------------------------------------------------------------------------  

#### `plot_parallel_coords()` function

plot_parallel_coords <- function(diagnostic_summary, colour_var, group_var = NULL){
  
  # pivot_long all the metric summaries
  summary_long <- diagnostic_summary |>
    tidyr::pivot_longer(
      -c(country, region, income),
      names_to = "diagnostics",
      values_to = "metrics"
    ) 
  
  # scale the metrics
  if(is.null(group_var)){
    # the ungrouped parallel coordinate plot
    
    # scale the metrics - global normalisation
    scaled_metrics <- summary_long |>
      dplyr::group_by(diagnostics) |>
      dplyr::mutate(
        metrics_norm = scales::rescale(metrics, to = c(0, 1), 
                                       na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        diagnostics = factor(diagnostics, 
                             levels = unique(diagnostics))
      )
    
    P <- scaled_metrics |>
      ggplot2::ggplot() +
      ggiraph::geom_point_interactive(
        ggplot2::aes(
          x = diagnostics, 
          y = metrics_norm,
          group = country,
          color = .data[[colour_var]],
          tooltip = paste(
            "Country:", country, "<br/>",
            diagnostics, ":", sprintf("%.2f", metrics)), 
          data_id = country
        ), 
        alpha = 0.3, size = 0.8
      ) +
      ggiraph::geom_line_interactive(
        ggplot2::aes(
          x = diagnostics, 
          y = metrics_norm,
          group = country,
          color = region,
          tooltip = country,
          data_id = country
        ), alpha = 0.6
      ) +
      ggplot2::scale_x_discrete(expand = c(0, 0.2)) +
      ggplot2::labs(x ="diagnostic indices", y = " ") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 25, hjust = 0.7, size = 4),
        legend.title = ggplot2::element_blank(),
        legend.position = "bottom"
      )
  } else{
    # scale the metrics - group-wise normalisation
    scaled_metrics <- summary_long |>
      dplyr::group_by(.data[[group_var]], diagnostics) |>
      dplyr::mutate(
        metrics_norm = scales::rescale(metrics, to = c(0, 1), 
                                       na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        diagnostics = factor(diagnostics, 
                             levels = unique(diagnostics))
      )
    # the grouped parallel coordinate plot
    P <- scaled_metrics |>
      ggplot2::ggplot() +
      ggiraph::geom_point_interactive(
        ggplot2::aes(
          x = diagnostics, 
          y = metrics_norm,
          group = country,
          color = .data[[group_var]],
          tooltip = paste(
            "Country:", country, "<br/>",
            diagnostics, ":", sprintf("%.2f", metrics)), 
          data_id = country
        ), 
        alpha = 0.3, size = 0.8
      ) +
      ggiraph::geom_line_interactive(
        ggplot2::aes(
          x = diagnostics, 
          y = metrics_norm,
          group = country,
          color = region,
          tooltip = country,
          data_id = country
        ), alpha = 0.6
      ) +
      ggplot2::facet_wrap(~.data[[group_var]]) +
      ggplot2::scale_x_discrete(expand = c(0, 0.2)) +
      ggplot2::labs(x ="diagnostic indices", y = " ") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 25, hjust = 0.7, size = 4),
        legend.title = ggplot2::element_blank(),
        legend.position = "bottom"
      )
  }
  
  # the interactive hover plot with girafe
  ggiraph::girafe(ggobj = P, 
                  width_svg = 10, 
                  height_svg = 6,
                  options = list(
                    use_cursor_pos = FALSE,
                    ggiraph::opts_hover_inv(css = "opacity: 0.1;"), # makes non-hovered lines fade
                    ggiraph::opts_hover(css = "opacity: 1; stroke-width: 1.5;") #on hover: no color change
                  ))  
}


#-------------------------------------------------------------------------------  

#### `plot_metric_linkview()` function

plot_metric_linkview <- function(wdi_data, index = NULL, metric_summary, metric_var, group_var = NULL) {
  # Identify the name of the variable in the wdi data that contains the country-year value as index
  if(is.null(index)) {
    index_var <- attr(wdi_data, "index_var") 
    index = index_var[1]
  }
  
  if(is.null(group_var)){
    # the line plot
    line_plot <- ggplot2::ggplot() +
      ggiraph::geom_line_interactive(
        data = na.omit(wdi_data),
        ggplot2::aes(
          x = year,
          y = .data[[index]],
          group = country,
          tooltip = country, 
          data_id = country
        ),
        color = "grey35"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none")
    
    # dot plot
    dot_plot <- metric_summary |>
      ggplot2::ggplot() +
      ggiraph::geom_point_interactive(
        ggplot2::aes(
          x = .data[[metric_var[[1]]]],
          y = .data[[metric_var[[2]]]],
          tooltip = country,
          data_id = country
        ),
        position = ggplot2::position_jitter(height = 0.2),
        color = "grey25"
      ) +
      ggplot2::theme_minimal() 
  } else{
    line_plot <- ggplot2::ggplot() +
      ggiraph::geom_line_interactive(
        data = na.omit(wdi_data),
        ggplot2::aes(
          x = year,
          y = .data[[index]],
          group = country,
          tooltip = country, 
          data_id = country
        ),
        color = "grey35"
      ) + 
      ggplot2::facet_wrap(~.data[[group_var]], ncol = 2) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none")
    
    # dot plot
    dot_plot <- metric_summary |>
      ggplot2::ggplot() +
      ggiraph::geom_point_interactive(
        ggplot2::aes(
          x = .data[[metric_var[[1]]]],
          y = .data[[metric_var[[2]]]],
          tooltip = country,
          data_id = country
        ),
        position = ggplot2::position_jitter(height = 0.2),
        color = "grey25"
      ) + 
      ggplot2::facet_wrap(~.data[[group_var]], ncol = 2) +
      ggplot2::theme_minimal()
  }
  
  # Create interactive plot with girafe
  ggiraph::girafe(ggobj = patchwork::wrap_plots(dot_plot, line_plot), 
                  width_svg = 9.5, 
                  height_svg = 6,
                  options = list(
                    ggiraph::opts_hover_inv(css = "opacity: 0.2;")  # makes non-hovered lines fade
                  ))
}


