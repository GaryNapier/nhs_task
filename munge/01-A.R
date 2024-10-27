# Preprocessing script.

# Change var name of data sheet from Excel file
cvd_prevalence <- cvd_prevalence.sheet1

# Subset to just Norfolk and waveney
cvd_prevalence <- cvd_prevalence |> 
  filter(AreaName == "Nhs Norfolk And Waveney Integrated Care Board")

