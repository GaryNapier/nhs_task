

# MSOA table ----

MSOA_top_bottom_tbl <- cbind(
  
  MSOA_top_bottom |> 
    filter(top_bottom == "top 10%") |> 
    select(AreaName, Value) |> 
    mutate_if(is.numeric, round, digits = 2) |> 
    # Reactable can't handle same names
    rename(
      Area_top = AreaName,
      Value_top = Value
    ),
  
  MSOA_top_bottom |> 
    filter(top_bottom == "bottom 10%") |> 
    select(AreaName, Value) |> 
    rename(
      Area_bottom = AreaName,
      Value_bottom = Value
      ) |> 
    mutate_if(is.numeric, round, digits = 2)
  
)

MSOA_top_bottom_reactable <- reactable(
  MSOA_top_bottom_tbl,
  columns = list(
    Value_top = colDef(
      style = list(
        color = c("#8fb935")
      )
    ), 
    Value_bottom = colDef(
      style = list(
        color = c("#e64747")
      )
    )
  ), 
  defaultPageSize = 100
)
