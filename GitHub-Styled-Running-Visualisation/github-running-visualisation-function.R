# Function for creating GitHub-type strava visualisation ----

strava_github_plot <- function(distance_date_table, year, bins, font_family = "sans"){
  
  # Required packages
  require(dplyr)
  require(tidyr)
  require(lubridate)
  require(glue)
  require(forcats)
  require(ggplot2)
  require(scales)
  
  # Define input data
  user_data <- distance_date_table
  
  # Check if required input is correct
  if("date" %in% colnames(user_data) == FALSE){stop("Column date does not exist")}
  if("distance" %in% colnames(user_data) == FALSE){stop("Column date does not exist")}
  if(is.vector(bins) == FALSE | length(bins) != 3){stop("Bins are not a vector OR provide more or less than 3 values")}
  
  # Define week order
  week_order <- c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday")

  # Create table for desired year
  distance_series <- 
    seq(ymd(glue("{year}-01-01")), ymd(glue("{year+1}-01-01"))-1, by = "1 day") |> 
    as_tibble() |> 
    rename(date = value) |> 
    left_join(user_data)
  
  # Add in custom week number
  distance_series <- 
    distance_series |> 
    mutate(
      day_of_year = row_number(),
      weekday  = weekdays(date)
    ) |> 
    group_by(weekday) |> 
    mutate(weekday_of_year = row_number()) |> 
    ungroup() |> 
    mutate(week = if_else(weekday == "Monday", weekday_of_year, NA_real_))
  
  if(distance_series$weekday[distance_series$day_of_year == 1] != "Monday"){
    distance_series$week <- distance_series$week+1
  }
  
  distance_series$week[1] <- 1
  distance_series <- 
    distance_series |> 
    fill(week) 
  

  # Add in additional required fields
  distance_series <-
    distance_series |> 
    mutate(distance = replace_na(distance, 0)) |> 
    arrange(distance) |>
    mutate(
      distance_floored = floor(distance),
      bins_group = as.character(cut(distance_floored, c(0, bins, Inf))),
      bins_group = replace_na(bins_group, "Rest"),
      bins_group = fct_inorder(bins_group),
      weekday = factor(weekday, levels = week_order),
      month = month(date, label = TRUE),
      tile_label = if_else(distance > 0, distance_floored, NA_real_),
      label_colour = if_else(tile_label <= bins[1], "col1", "col2"),
      year_month = paste(year, month, sep = ",")
    )

  # Check to see if bins provided go outside the range in the data provided
  if(bins[3]+1 > max(distance_series$distance_floored)){stop("Pick a lower top of range for bins")}

  # Check to see if the bin range provided breaks into 5 groups with data
  groups <-
    distance_series |>
    distinct(bins_group) |>
    nrow()
   if(groups != 5){stop("Bins provided not appropriate for data")}

  # Total distance
  total_distance <- round(sum(distance_series$distance, na.rm = TRUE))

  # Positions for month breaks
  breaks <-
    distance_series |>
    arrange(date) |>
    filter(weekday == "Monday") |> 
    group_by(month) |>
    summarise(week = first(week)) |>
    pull(week)

  # All month labels
  month_labels <-
    distance_series |>
    distinct(month) |>
    arrange(month) |>
    pull(month)

  # Fill order
  fill_order <-
    distance_series |>
    group_by(bins_group) |>
    summarise(max = max(distance)) |>
    arrange(desc(max)) |>
    pull(bins_group)

  # Data vis
  data_vis <-
    distance_series |>
    ggplot(aes(x = week, y = weekday, fill = bins_group)) +
    geom_tile(
      width = 0.75,
      height = 0.75
    ) +
    geom_text(
      aes(label = tile_label, colour = label_colour),
      size = 2
    ) +
    annotate(
      "tile",
      x = c(48, 49, 50, 51),
      y = 0,
      width = 0.75,
      height = 0.5,
      fill = rev(c('#6a0000', '#b30300', '#ef3f02', '#ff904a'))
    ) +
    annotate(
      "text",
      label = c("Less", "More"),
      x = c(47, 52),
      y = 0,
      hjust = c(1, 0),
      size = 3
    ) +
    scale_x_continuous(
      breaks = breaks,
      labels = month_labels,
      position = "top"
    ) +
    scale_fill_manual(
      values = c(
        'grey80',
        '#ff904a',
        '#ef3f02',
        '#b30300',
        '#6a0000'
      )
    ) +
    scale_colour_manual(
      values = c(
        "col1" = "black",
        "col2" = "white"
      )
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal(
      base_size = 12,
      base_family = font_family
    ) +
    theme(
      panel.grid = element_blank(),
      axis.text.y = element_text(margin = margin(r = -25)),
      legend.position = "none",
      panel.background = element_rect(
        fill = 'white',
        colour = 'white'
      ),
      plot.background = element_rect(
        fill = 'white',
        colour = 'black'
      )
    ) +
    labs(
      y = NULL,
      x = NULL,
      title = glue("Total of {comma(total_distance)} kilometers run in {year}")
    )

  # Return output
  return(data_vis)
  
}