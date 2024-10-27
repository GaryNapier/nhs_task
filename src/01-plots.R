

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







