# Preprocessing script.

# CVD prevalence ----

# Change var name of data sheet from Excel file
cvd_prevalence <- cvd_prevalence.sheet1

# Subset to just Norfolk and waveney
cvd_prevalence <- cvd_prevalence |> 
  filter(AreaName == "Nhs Norfolk And Waveney Integrated Care Board")

# Compare areas ----

# compare_areas <- read.csv("data/compare_areas.csv")

# Save copy to use directly in plotly
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


# MSOA Norfolk comparison ----

MSOA <- read.csv("data/MSOA_comparison.csv")

MSOA |> colnames() |> sort()

MSOA <- MSOA |> 
  filter(
    Sex == "Persons",
    Indicator.Name %in% c(
      "Deaths from circulatory disease, all ages, standardised mortality ratio", 
      "Deaths from coronary heart disease, all ages, standardised mortality ratio",         
      "Deaths from stroke, all ages, standardised mortality ratio"   
    )
  ) |> 
  select(
    -Parent.Code, 
    -Indicator.ID, 
    -Parent.Name, 
    -Area.Type, 
    -Category.Type, 
    -Category,  
    -Time.period,
    -Lower.CI.99.8.limit, 
    -Upper.CI.99.8.limit, 
    -Value.note, 
    -Recent.Trend, 
    -Compared.to.parent.value.or.percentiles, 
    -Time.period.Sortable, 
    -New.data, 
    -Compared.to.goal, 
    -Time.period.range
  )

# Do aggregation of the three indicators
MSOA_aggregate <- MSOA |> group_by(AreaName) |>
  summarise(
    Value = mean(Value),
    Lower.CI.95.0.limit = mean(Lower.CI.95.0.limit),
    Upper.CI.95.0.limit = mean(Upper.CI.95.0.limit),
    Count = sum(Count),
    Denominator = sum(Denominator)
  )

# Join the area code back in
MSOA <- MSOA |> select(AreaName, Area.Code) |> distinct() |> 
  inner_join(MSOA_aggregate, by = "AreaName")


# Split out England and Norfolk
MSOA_eng <- MSOA |> filter(AreaName == "England")
MSOA_norfolk <- MSOA |> filter(AreaName == "Norfolk")
MSOA <- MSOA |> filter(!(AreaName %in% c("Norfolk", "England")))

MSOA <- MSOA |> mutate(
  compare_england = case_when(
    Value < MSOA_eng$Lower.CI.95.0.limit ~ "Better", 
    Value > MSOA_eng$Upper.CI.95.0.limit ~ "Worse",
    .default = "Similar"
  )
) |> arrange(Value)


MSOA_top_bottom <- rbind(
  MSOA |> 
    filter(
      Value <= MSOA |> summarize(
        Q90 = quantile(Value, probs = 0.1)
      ) |> pull()
    ),
  MSOA |> 
    filter(
      Value >= MSOA |> summarize(
        Q90 = quantile(Value, probs = 0.9)
      ) |> pull()
    )
) |> data.frame()


MSOA_top_bottom

# https://maczokni.github.io/crime_mapping_textbook/making-maps-in-r.html

library(sf)
# Remember to use the appropriate pathfile in your case
shp_name <- "data/BoundaryData/england_msoa_2021.shp"

norfolk_shp <- st_read(shp_name)




norfolk_shp <- inner_join(
  norfolk_shp, MSOA, by = c("msoa21cd" = "Area.Code")
)

ggplot() + 
  geom_sf(
    data = norfolk_shp, 
    aes(fill = Value)
    )+
  scale_fill_gradient(low = "yellow", high = "red")+ 
  theme_void()

ggplot() + 
  geom_sf(
    data = norfolk_shp, 
    aes(fill = compare_england)
  )+
  scale_fill_manual(values = better_pallet)+
  theme_void()















