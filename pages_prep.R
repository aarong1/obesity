# install.packages(c("shiny", "dplyr", "tidyr", "forcats", "echarts4r", "stringr"))
library(shiny)
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)
library(echarts4r)

# ---- helpers --------------------------------------------------------------

# 1) Ensure we have a BMI category column named `bmi_cat`
as_bmi_cat <- function(df, bmi_col = "bmi") {
  if (bmi_col %in% names(df)) {
    # if numeric, bin; if already character/factor (Normal/Overweight/Obese), keep as is
    if (is.numeric(df[[bmi_col]])) {
      df %>%
        mutate(
          bmi_cat = case_when(
            .data[[bmi_col]] < 25 ~ "Normal",
            .data[[bmi_col]] < 30 ~ "Overweight",
            is.finite(.data[[bmi_col]]) ~ "Obese",
            TRUE ~ NA_character_
          )
        )
    } else {
      df %>% mutate(bmi_cat = as.character(.data[[bmi_col]]))
    }
  } else if ("bmi_cat" %in% names(df)) {
    df %>% mutate(bmi_cat = as.character(.data[["bmi_cat"]]))
  } else {
    stop("No `bmi` or `bmi_cat` column found. Provide one of them.")
  }
}

# 2) Tidy shares per panel
compute_panel_shares <- function(df) {
  df %>%
    mutate(
      # Make sure these exist and are readable
      mdm_quintile_soa = coalesce(.data[["mdm_quintile_soa"]]) %>% as.character(),
      HSCT = .data[["HSCT"]] %>% as.character()
    ) %>%
    filter(!is.na(mdm_quintile_soa), !is.na(HSCT), !is.na(bmi_cat)) %>%
    mutate(
      bmi_cat = fct(bmi_cat, levels = c("Normal", "Overweight", "Obese"))
    ) %>%
    count(HSCT, mdm_quintile_soa, bmi_cat, name = "n") %>%
    group_by(HSCT, mdm_quintile_soa) %>%
    mutate(share = n / sum(n)) %>%
    ungroup()
}

# 3) Minimal echarts bar for a single panel
render_panel_chart <- function(dat, title_line1, title_line2 = NULL) {
  # dat: rows for one HSCT x quintile, columns bmi_cat + share
  graph <- dat %>%
    #group_by(bmi_cat) |> 
    e_charts(bmi_cat,width='200',height='250',textStyle=list(fontStyle=12)) %>%#
    e_bar(share,name = paste (title_line1,str_remove(title_line2,pattern = 'quintile (SOA)')), roundCap = TRUE) %>%
    e_tooltip(trigger = "item", formatter = htmlwidgets::JS("
      function(p){return p.marker + p.name + ': ' + (p.value[[1]]*100).toFixed(0);  } 
    ")) |> 
    e_theme('roma') %>%  #+ '%'+ '/n'+p.seriesName.split(' ')[0]+ '\n'+p.seriesName.split(' ')[1]; 
    # e_color(c("#4add8c", "#f4d35e", "#ee6c4d")) |> 
    e_y_axis(min=0,max=0.75,
      show = TRUE,
      axisLabel = list(formatter = htmlwidgets::JS("function(v){return (v*100)+'%';}")),
      splitLine = list(show = TRUE)
    ) %>%
    e_tooltip(confine=T) |> 
    e_axis_labels() |> 
    e_x_axis(axisTick = list(show = FALSE)) %>%
    e_grid(top = '30%', right = 10, bottom = 30, left = 40) %>%
    e_title(
      text = title_line1,
      subtext = title_line2,
      left = "left",
      textStyle = list(fontSize = 16, fontWeight = 500),
      subtextStyle = list(fontSize = 14, color = "#666")
    ) %>%
    e_legend(show = F) 
  print(graph)
  
  return((graph))
}

# ---- demo UI --------------------------------------------------------------

# ---- server ---------------------------------------------------------------

  
  # Replace this with your real dataframe `df`
  # Expecting columns: HSCT, mdm_quintile_soa (or townsend_quintile), and either bmi or bmi_cat
  df <- tibble::tibble(
    HSCT = sample(c("Belfast", "Northern", "Southern", "South Eastern", "Western"), 2000, TRUE),
    mdm_quintile_soa = sample(1:5, 2000, TRUE),
    bmi = rnorm(2000, 28, 5) # numeric demo; will be binned to categories
  )
  
  # Prepare data
  dat_panels <- 
    df %>%
      as_bmi_cat(bmi_col = "bmi") %>%
      compute_panel_shares()
  
  
  # Create one output per panel
  
    panels <- dat_panels %>%
      distinct(HSCT, mdm_quintile_soa) %>%
      arrange(HSCT, mdm_quintile_soa) %>%
      mutate(panel_id = paste0("panel_", str_replace_all(HSCT, "\\W+", "_"), "_q", mdm_quintile_soa))
    
    # UI for all panels
    
      lapply(seq_len(nrow(panels)), function(i) {
        div(
          class = "panel-card",
          echarts4rOutput(panels$panel_id[i], height = "220px")
        )
      })
    
    # Render each chart
      group_echarts = lapply(seq_len(nrow(panels)), function(i) {
      id <- panels$panel_id[i]
      hsct_i <- panels$HSCT[i]
      q_i    <- panels$mdm_quintile_soa[i]
      
      dat_i <- dat_panels %>% filter(HSCT == hsct_i, mdm_quintile_soa == q_i)
      
     render_panel_chart(
          dat_i,
          title_line1 = paste0(hsct_i),
          title_line2 = paste0("MDM quintile (SOA): ", q_i)
        )
      })
      
  
