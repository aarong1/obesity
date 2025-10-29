
pop <- read.fst('./populations/k20_population.fst')


pop <- pop |> 
  rowwise() |> 
  mutate(
    comorbidities = sum(
      !is.na(diabetes),
      !is.na(atrial_fibrillation),
      !is.na(pad),
      !is.na(ckd),
      !is.na(hypertension),
      !is.na(cholesterol),
      
      !is.na(bmi),
      !is.na(smoking),
      !is.na(alcohol),
      !is.na(diet),
      !is.na(pa)#,
      #!is.na(wellbeing)#,
      #!is.na(pm25g)
    )
  ) |> 
  ungroup()

pop <- pop |> 
  mutate(qrisk_percentile = rank(qrisk_score)/max(rank(qrisk_score))) 


# Create sample ECharts graphs
sales_chart <- mtcars |> 
  e_charts(mpg) |> 
  e_line(cyl,legend = F, smooth = TRUE, name = "Sales Trend") |>
  e_color("#3498db") |>
  e_theme("walden") |>
  e_tooltip(trigger = "axis") |>
  e_title("Sales Performance", left = "center", textStyle = list(fontSize = 14)) |>
  e_grid(left = "10%", right = "10%", top = "20%", bottom = "15%") 

revenue_chart <- mtcars |> 
  e_charts(wt) |> 
  e_scatter(hp, qsec,legend = F, scale = e_scale, name = "Revenue vs Growth") |>
  e_color("#e74c3c") |>
  e_theme("walden") |>
  e_tooltip() |>
  e_title("Revenue Analysis", left = "center", textStyle = list(fontSize = 14)) |>
  e_grid(left = "10%", right = "10%", top = "20%", bottom = "15%")

performance_chart <- data.frame(
  category = c("Q1", "Q2", "Q3", "Q4"),
  value = c(120, 200, 150, 300)
) |>
  e_charts(category) |>
  e_bar(value, name = "Performance",legend = F) |>
  e_color("#27ae60") |>
  e_theme("walden") |>
  e_tooltip(trigger = "axis") |>
  e_title("Quarterly", left = "center", textStyle = list(fontSize = 12)) |>
  e_grid(left = "15%", right = "10%", top = "20%", bottom = "15%")
