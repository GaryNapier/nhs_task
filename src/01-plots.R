

# Variables ----

error_bar_wd <- 10
y_ax_title <- "Proportion %"
x_ax_title <- NA
colours <- c(
  '#CC1480', 
  '#FF9673', 
  '#E1C8B4'
)

# Functions ----

hline <- function(y = 0, color = "black", width = 1) {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(
      color = color,
      width = width
    )
  )
}

BarPlot <- function(
    data, 
    error_bar_wd = 10, 
    y_ax_title = "Proportion %", 
    x_ax_title = NA,
    title = unique(data$MetricCategoryTypeName), 
    colours = c(
      '#CC1480', 
      '#FF9673', 
      '#E1C8B4'
    )
){
  
  data$colour <- factor(data$CategoryAttribute, labels = colours)
  
  plot_ly() |> 
    add_trace(
      data = data, 
      x = ~MetricCategoryName,
      y = ~Value, 
      type = 'bar', 
      marker = list(color = ~colour),
      error_y = ~list(
        array = conf_upper, 
        color = "black", 
        width = error_bar_wd
      )
    ) |> 
    layout(
      title = title,
      yaxis = list(
        title = y_ax_title
      ), 
      xaxis = list(
        title = x_ax_title
      )
    )
  
}

# Sex ----

sex_barplot <- BarPlot(sex_data, error_bar_wd = 30)

# Sex - age standardised ----

sex_age_sd_barplot <- BarPlot(sex_age_sd_data, error_bar_wd = 30)

# Age group ----

age_grp_data$colour <- factor(age_grp_data$CategoryAttribute, labels = colours)

age_grp_barplot <- plot_ly() |> 
  add_trace(
    data = age_grp_data, 
    x = ~factor(category_metric, levels = category_metric),
    y = ~Value, 
    type = 'bar', 
    marker = list(color = ~colour),
    error_y = ~list(
      array = conf_upper, 
      color = "black", 
      width = error_bar_wd
    )
  ) |> 
  layout(
    title = unique(age_grp_data$MetricCategoryTypeName),
    yaxis = list(
      title = y_ax_title
    ), 
    xaxis = list(
      title = x_ax_title
    )
  )

# Deprivation quintile ----

dep_quint_barplot <- plot_ly() |> 
  add_trace(
    data = dep_quint_data, 
    x = ~factor(MetricCategoryName, levels = MetricCategoryName),
    y = ~Value, 
    type = 'bar', 
    error_y = ~list(
      array = conf_upper, 
      color = "black", 
      width = error_bar_wd
    )
  ) |> 
  layout(
    title = unique(dep_quint_data$MetricCategoryTypeName),
    yaxis = list(
      title = y_ax_title
    ), 
    xaxis = list(
      title = x_ax_title
    )
  )

# Deprivation quintile - age standardised ----

dep_quint_sd_barplot <- plot_ly() |> 
  add_trace(
    data = dep_quint_sd_data, 
    x = ~factor(MetricCategoryName, levels = MetricCategoryName),
    y = ~Value, 
    type = 'bar', 
    error_y = ~list(
      array = conf_upper, 
      color = "black", 
      width = error_bar_wd
    )
  ) |> 
  layout(
    title = unique(dep_quint_data$MetricCategoryTypeName),
    yaxis = list(
      title = y_ax_title
    ), 
    xaxis = list(
      title = x_ax_title
    )
  )

# Funnel plot ----

# BIG FUDGE!
plus <- 10

better_pallet <- c(
  Better = "#8fb935",
  Similar = "#e6e22e", 
  Worse = "#e64747",
  `Not compared` = "grey"
)

mrg <- list(
  l = 50, r = 50,
  b = 50, t = 50,
  pad = 20
)

funnel_plot <- plot_ly() |>
  add_trace(
    data = compare_areas,
    type = "scatter",
    mode = "markers",
    x = ~Population,
    y = ~Value,
    color = ~England_compare,
    colors = better_pallet,
    size = 10,
    text = ~AreaName
  ) |> 
  add_trace(
    x = ~Population,
    y = ~(lcl95 * 100000) + plus,
    type = "scatter",
    mode = "line",
    name = "Lower 95%",
    line = list(
      color = "grey"
    ), 
    showlegend = FALSE
  ) |>
  add_trace(
    x = ~Population,
    y = ~(ucl95 * 100000) + plus,
    type = "scatter",
    mode = "line",
    name = "95% confidence",
    line = list(
      color = "grey"
    )
  ) |>
  add_text(
    showlegend = FALSE,
    x = max_pop + (max_pop * 0.1), 
    # y = benchmark_eng$Value - (benchmark_eng$Value * 0.005),
    y = benchmark_eng$Value,
    text = "England\nbaseline"
  ) |> 
  layout(
    title = paste(
      "Funnel plot of English counties.\n", indicator
    ),
    yaxis = list(
      title = 'Admissions per 100k'
    ),
    margin = mrg,
    shapes = list(
      hline(benchmark_eng$Value),
      list(
        type = 'circle',
        xref = 'x', x0 = norfolk_pop - 30000, x1 = norfolk_pop + 30000,
        yref = 'y', y0 = norfolk_value-10, y1 = norfolk_value+10,
        fillcolor = NA, line = list(color = 'black'),
        opacity = 0.5
      )
    )
  ) 

# Bar plot - Norfolk vs England & region:

region_bar <- plot_ly() |> 
  add_trace(
    data = compare_region, 
    type = "bar", 
    x = ~AreaName, 
    y = ~Value, 
    color = ~England_compare,
    colors = better_pallet,
    error_y = ~list(
      array = abs(Value - Upper.CI.95.0.limit), 
      color = "black"
    )
  ) |> 
  layout(
    title = "Norfolk vs England and region", 
    yaxis = list(
      title = "Admissions per 100k"
    )
  )





