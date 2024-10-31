# Preprocessing script.

# CVD prevalence ----

# Change var name of data sheet from Excel file
cvd_prevalence <- cvd_prevalence.sheet1

# Subset to just Norfolk and waveney
cvd_prevalence <- cvd_prevalence |> 
  filter(AreaName == "Nhs Norfolk And Waveney Integrated Care Board")

# Compare areas ----

# compare_areas <- read.csv("data/compare_areas.csv")

compare_areas_raw <- compare_areas

indicator <- unique(compare_areas_raw$Indicator.Name)

# Clean count col
compare_areas$Count <- gsub(",", "", compare_areas$Count) |> as.numeric()

# Rename
compare_areas <- compare_areas |> 
  rename(
    Population = Denominator, 
    England_compare = Compared.to.England.value.or.percentiles
  ) |> 
  select(
    -Indicator.ID,
    -Indicator.Name,
    -Parent.Code,
    -Parent.Name,
    -Area.Code,
    -Area.Type,
    -Sex,
    -Age,
    -Category.Type,
    -Category,
    -Time.period,
    -Lower.CI.95.0.limit,
    -Upper.CI.95.0.limit,
    -Lower.CI.99.8.limit,
    -Upper.CI.99.8.limit,
    -Value.note,
    -Compared.to.parent.value.or.percentiles,
    -Time.period.Sortable,
    -New.data,
    -Compared.to.goal,
    -Time.period.range,
  ) |> 
  filter(!is.na(Value), !is.na(Population), !is.na(Count))

# Take out England as benchmark
benchmark_eng <- compare_areas |> filter(AreaName == "England")
compare_areas <- compare_areas |> filter(AreaName != "England")
















