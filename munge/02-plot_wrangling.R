

# Data wrangling for plots

# Functions ----

SelectCvdPrevData <- function(data, 
                              filt = ""){
  data |> 
    filter(MetricCategoryTypeName == filt) |> 
    select(
      MetricCategoryTypeName, 
      CategoryAttribute, 
      MetricCategoryName,
      category_metric,
      Value,
      conf_upper
    )
}

# CVD prevalence data ----

# Convert CategoryAttribute to factor
cvd_prevalence$CategoryAttribute <- factor(
  cvd_prevalence$CategoryAttribute, 
  levels = c("Persons", "Female", "Male")
)

cvd_prevalence$MetricCategoryName <- factor(
  cvd_prevalence$MetricCategoryName, 
  levels = unique(cvd_prevalence$MetricCategoryName)
)

# Can't seem to get plotly to use raw values for conf intervals
cvd_prevalence <- cvd_prevalence |> 
  mutate(
    conf_upper = abs(Value - UpperConfidenceLimit), 
    category_metric = paste0(CategoryAttribute, ": ", MetricCategoryName)
  )

# Get data
sex_data <- SelectCvdPrevData(cvd_prevalence, "Sex")
sex_age_sd_data <- SelectCvdPrevData(cvd_prevalence, "Sex - Age Standardised")
age_grp_data <- SelectCvdPrevData(cvd_prevalence, "Age group")
dep_quint_data <- SelectCvdPrevData(cvd_prevalence, "Deprivation quintile")
dep_quint_sd_data <- SelectCvdPrevData(cvd_prevalence, "Deprivation quintile - Age Standardised")

# Compare areas data ----

# For funnel plot:

# Calc upper / lower 95 conf limit
compare_areas <- compare_areas %>%
  dplyr::mutate(
    rate =  Count/Population,
    overall_rate = sum(Count)/sum(Population),
    se = sqrt(overall_rate * ((1 - overall_rate) / Population)),
    lcl95 = overall_rate - (1.96 * se),
    ucl95 = overall_rate + (1.96 * se)
  )

# Variables needed for plots 
max_pop <- max(compare_areas$Population, na.rm = TRUE)
compare_areas <- compare_areas |> arrange(Population)
norfolk_pop <- compare_areas |> filter(AreaName == "Norfolk") |> select(Population) |> pull()
norfolk_value <- compare_areas |> filter(AreaName == "Norfolk") |> select(Value) |> pull()




















