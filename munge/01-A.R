# Preprocessing script.

# CVD prevalence ----

# Change var name of data sheet from Excel file
cvd_prevalence <- cvd_prevalence.sheet1

# Subset to just Norfolk and waveney
cvd_prevalence <- cvd_prevalence |> 
  filter(AreaName == "Nhs Norfolk And Waveney Integrated Care Board")

# Compare areas ----

# Clean count col
compare_areas$Count <- gsub(",", "", compare_areas$Count) |> as.numeric()

# Rename
compare_areas <- compare_areas |> rename(
  Population = Denominator
)

# Take out England as benchmark
benchmark_eng <- compare_areas |> filter(AreaName == "England")
compare_areas <- compare_areas |> filter(AreaName != "England")


compare_areas <- compare_areas |> mutate(
  overall_rate = sum(Count, na.rm = TRUE) / sum(Population, na.rm = TRUE),
  # overall_rate = sum(Count, na.rm = TRUE) / benchmark_eng$Population, 
  se = sqrt(
    # overall_rate * ((1 - overall_rate) / Population)
    overall_rate * ((1 - overall_rate) / benchmark_eng$Population)
  ), 
  lcl95 = overall_rate - (1.96 * se), 
  ucl95 = overall_rate + (1.96 * se), 
  lcl99.7 = overall_rate - (3 * se), 
  ucl99.7 = overall_rate + (3 * se), 
)



# comorbidity <- c('asthma'        , 'cardio'  , 'diabetes'    , 'downsyn'    , 'hematologic','immuno'
#                  ,'neurological' , 'obesity' , 'pneumopathy' , 'puerperium' , 'renal')
# cases <-       c(145, 2768, 2369, 36, 179,289
#                  ,198, 168, 44, 318, 14)
# deaths <-   c(42, 1069 , 914 ,13, 63,  127
#               , 62,  81,  6 ,137 ,7)
# df <- data.frame(comorbidity,cases,deaths)
# 
# df2 <- df%>%
#   dplyr::mutate(DeathRatebyComorb =  deaths/cases
#                 , OverallDeathRate = sum(deaths)/sum(cases)
#                 , se = sqrt( OverallDeathRate * ((1- OverallDeathRate) /cases))
#                 , lcl95 = OverallDeathRate   - (1.96* se)
#                 , ucl95 = OverallDeathRate   + (1.96* se)
#                 , lcl99.7 = OverallDeathRate - (3* se)
#                 , ucl99.7 = OverallDeathRate + (3* se)
#                 
#   )
# 
# df2 
# 
# pl <- ggplot(data = df2, aes(x = cases, group =OverallDeathRate  ))
# 
# pl <- pl + geom_smooth(aes(y =lcl95),se = FALSE,linetype ="solid",color = "red", size = 0.5)
# pl <- pl + geom_smooth(aes(y =ucl95),se = FALSE, linetype ="solid",color = "red", size = 0.5)
# 
# pl <- pl + geom_smooth(aes(y =lcl99.7),se = FALSE, linetype ="solid",color = "blue", size = 0.5)
# pl <- pl + geom_smooth(aes(y =ucl99.7),se = FALSE, linetype ="solid",color = "blue", size = 0.5)
# 
# pl <- pl + geom_smooth(aes(y =OverallDeathRate),se = FALSE, color = "blue")
# 
# pl <- pl + geom_point(aes(y =DeathRatebyComorb), color ="red")
# 
# pl <- pl + geom_text(aes(y =DeathRatebyComorb,label=comorbidity),color = "blue",size = 3,hjust= -0.2, vjust= -0.3)






better_pallet <- c(
  Better = "#8fb935",
  Similar = "#e6e22e", 
  Worse = "#e64747"
)

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

max_pop <- max(compare_areas$Population, na.rm = TRUE)

plot_ly() |> 
add_trace(
  data = compare_areas, 
  x = ~Population,
  y = ~Value, 
  color = ~Compared.to.England.value.or.percentiles,
  colors = better_pallet,
  type = "scatter", 
  mode = 'markers',
  text = ~AreaName
) |> 
  add_trace(
    data = compare_areas,
    x = ~Population, 
    y = ~lcl95,
    type = 'scatter', 
    mode = 'lines'
  ) 


# |> 
#   layout(
#     title = "Funnel plot", 
#     shapes = list(
#       hline(benchmark_eng$Value)
#     )
#   ) |> 
#   add_text(
#     showlegend = FALSE, 
#     x = max_pop - (max_pop * 0.1),
#     y = benchmark_eng$Value - 20, 
#     text = "England baseline"
#   )



compare_areas |> data.frame()

str(compare_areas)














