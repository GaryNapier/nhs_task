

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

# Wrangle ----

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






