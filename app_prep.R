
# pop <- read.fst('./populations/k20_population.fst')
# pop <- slice_sample(pop, n=50000)
# write.fst(pop,'./populations/k5_population.fst')


pop <- read.fst('./populations/k5_population.fst')




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


metric_card <- function( top ='top', 
                         change = 'change', 
                         text ='text',
                         change_icon =  '',
                         color = 'red',
                         opacity='opacity-50'){
  
  change_class = if(change_icon=='negative'){  "fa-arrow-down me-1"
  }else if(change_icon=='negative'){ "fa-arrow-up me-1"
  }else{''}
  
  color_class = case_when(color == '#8F00FF' ~ 'theme-purple',
                          color == 'teal' ~ 'theme-teal',
                          color == 'steelblue' ~ 'theme-teal',
                          
                          
                          color == '#dc3545' ~ 'theme-red', 
                          color == 'mediumseagreen' ~ 'theme-green'
  )
  
  
  div(class = paste("grid-item grid-item--small",opacity),
      div(class = "grid-item-content",
          div(class = "metric-card",
              #tags$i(class = "fas fa-external-link-alt fa-2x mb-3", style = "color: #dc3545;"),
              div(class = "metric-value", style = paste("color:",color), format(top,big.mark = ',',digits=3)),
              div(class = "metric-label", text),
              div(class = paste("metric-change", change_icon),
                  tags$i(class = change_class, change)
              )
          )
      )
  )
}
