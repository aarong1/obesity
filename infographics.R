# infographics.R




style <- list(
  normal = list(opacity = 0.5), # normal
  emphasis = list(opacity = 1) # on hover
)

y <- rnorm(10, 10, 2)
df <- data.frame(
  x = 'M',
  y = 1,
  z = 'Males'
)
df2 <- data.frame(
  x = 'F',
  y = 2,
  z = 'Females'
)
th <- read_json('theme.json')

bmi_sex_df <- pop |> 
  filter(!is.na(bmi)) |> 
  count(bmi=bmi%in% c('obese','overweight'),sex) |> 
  #pivot_wider(names_from = sex,values_from = n) |> 
  filter(bmi==TRUE) |> 
  mutate(x = c( 0.5, 1.5)) |>
  mutate(x = c( 'Males', 'Females')) |>
  
  mutate(symbols=symbols)
   

males_bmi_sex_df <- bmi_sex_df|> 
  select('Males') |> 
  mutate(x=1.5)

females_bmi_sex_df <- bmi_sex_df|> 
  select(c(1)) |> 
  mutate(x=0.5)
(
overweight_obese_sex <- bmi_sex_df |> 
   group_by(sex) |>
  e_charts(x) |> 
  e_pictorial(name = c('Male','Female'),n, symbol = symbols, 
              barCategoryGap = "50%", legend = T,
              itemStyle = style) |> 
    e_labels(formatter = '{c}') |> 
  # e_data(females_bmi_sex_df) |> 
  # e_pictorial(name = 'Females',Females, symbol = symbols[2], 
  #             barCategoryGap = "50%", 
  #             itemStyle = style) |> 
  e_axis(axis = 'y',    
         splitLine = list(show = FALSE),
         axisLabel = list(show = FALSE),
         axisTick = list(show = FALSE),
         axisLine = list(show = FALSE)
) |>
    # e_title('Sex') |> 
  # e_mark_line( serie = "n",name='Male'#,  
  #              data = 
  #   list(yAxis = bmi_sex_df$n[1]
  # )
  # ) |> 
    # e_mark_line(name='Female',  data = 
    #               list(xAxis =c(1),yAxis = bmi_sex_df$n[2]
    #               )
    #             ) |> 
  e_x_axis(
    # max = 1,
    splitLine = list(show = FALSE),
    axisLabel = list(show = FALSE),
    axisTick = list(show = FALSE),
    axisLine = list(show = FALSE)
    
  ) |>
  e_legend() |> 
  e_tooltip() |> 
  # e_title("SVG path") |> 
  # e_theme_custom("westeros", th) |>
   e_theme('roma')
)



df <- data.frame(
  price = rnorm(5, 10),
  amount = rnorm(5, 15),
  letter = LETTERS[1:5]
)

BMI_parallel_chart <- reduced_pop |> 
  filter(!is.na(bmi)) |> 
  filter(qrisk_score>=0.01) |> 
  arrange(desc(age20)) |>
  mutate(bmi = 
           case_when(bmi == 'normal' ~ 25+runif(min = -1,n()),
                     bmi == 'overweight' ~ 30+runif(min = -1,n()),
                     bmi == 'obese' ~ 35+runif(min = -1,n()),
                     TRUE ~ 0)) |>
  mutate(comorbidities = comorbidities+runif(n())/2) |>
  # group_by(mdm_quintile_soa) |>
  e_charts() |> 
  e_parallel(
             mdm_rank,
             qrisk_score, 
             bmi,
             comorbidities,
             age,
            opts = list(
              lineStyle = list(
                opacity = 0.1))) |> 
  
  e_title("BMI Chart with contributing and adjacent characteristics") |> 
  e_theme('walden')

# ------


# -----------------------------------------------------------
# Click-down hierarchy of obesity-related disease (Sunburst)
# -----------------------------------------------------------
library(dplyr)
library(tibble)
library(echarts4r)

# 1) Define the hierarchy (replace `value` with your own metrics)
hier <- tribble(
  ~item,                      ~parent,                      ~value,
  "Obesity-related disease",  '',                           1,
  # Cardiovascular
  "Cardiovascular disease",   "Obesity-related disease",    100,
  "Cardiology",               "Cardiovascular disease",     40,    # CHD, HF, AF, HTN
  "Cerebrovascular",          "Cardiovascular disease",     25,    # Stroke, TIA
  "Peripheral vascular",      "Cardiovascular disease",     15,    # PAD
  "Other CVD",                "Cardiovascular disease",     20,
  # Metabolic
  "Metabolic",                "Obesity-related disease",    80,
  "Type 2 diabetes",          "Metabolic",                  50,
  "Chronic kidney disease",   "Metabolic",                  30,    # CKD
  # Respiratory
  "Respiratory",              "Obesity-related disease",    55,
  "Obstructive sleep apnoea", "Respiratory",                30,
  "Asthma (worse control)",   "Respiratory",                25,
  # Musculoskeletal
  "Musculoskeletal",          "Obesity-related disease",    40,
  "Osteoarthritis",           "Musculoskeletal",            40,
  # Gastro-hepatic
  "Gastro-hepatic",           "Obesity-related disease",    45,
  "NAFLD/NASH",               "Gastro-hepatic",             30,
  "GERD",                     "Gastro-hepatic",             15,
  # Cancer (illustrative)
  "Cancer",                   "Obesity-related disease",    60,
  "Breast",                   "Cancer",                     15,
  "Colorectal",               "Cancer",                     15,
  "Endometrial",              "Cancer",                     10,
  "Pancreatic",               "Cancer",                     10,
  "Other cancers",            "Cancer",                     10
)

# 2) Build the interactive Sunburst
#    - Click a segment to drill down (zoom)
#    - Click the center to zoom back out



x <- pop |> 
  count(HSCT=first(HSCT),DEA2014_name=first(DEA2014_name),soa_name)

x0 <- pop |>
  count(item = HSCT, parent = 'NI', name='value')

x1 <- pop |>
  count(parent = HSCT, item = DEA2014_name, name='value')

x2 <- pop |>
  count(parent = DEA2014_name, item = Urban_mixed_rural_status, name='value')

x2 <- pop |>
  group_by(item = soa_name) |> 
           summarise( parent = first(DEA2014_name),  value = n()) 
  # count(item) |> 
  # filter(n>1)

xx <- rbind(
data.frame(item = "NI",  parent = '', value = 1),
x0,
x1,
x2[1:500,]
)

geo <- data.tree::FromDataFrameNetwork(xx)

#add decal

geo_sunburst <- geo |> 
  e_charts() |> 
  e_sunburst() |> 
  e_theme('roma') |> 
  e_labels(show = FALSE) |> 
  e_tooltip()

geo_treemap <- geo |> 
  e_charts() |> 
  e_treemap(upperLabel = list(show=F),
            leafDepth = 1) |> 
  e_theme('roma')  |> 
e_labels(show = T, position='insidetop') |> 
  e_tooltip()



df <- data.frame(
  parents = c("","CVD", "CVD", "cancer", "cancer", "cariology", "cariology", "peripheral", "peripheral", "fish", "fish", "Everything", "Everything", "Everything"),
  labels = c("Everything", "cardiology", "peripheral", "valley", "crater", "forest", "river", "diabetes", "fish", "shark", "tuna", "resp","CVD", "cancer"),
  value = c(0, 30, 40, 10, 10, 20, 10, 20, 20, 8, 12, 10, 70, 20)
)

universe <- data.tree::FromDataFrameNetwork(hier)

# create a tree object
universe <- data.tree::FromDataFrameNetwork(df)

obesity_effects_sunburst <- universe |> 
  e_charts() |> 
  e_sunburst(    universalTransition = TRUE,
                 animationDurationUpdate = 2000L) |> 
  e_theme('london')

obesity_effects_treemap <- universe |> 
  e_charts() |> 
  e_treemap(    universalTransition = TRUE,
                animationDurationUpdate = 2000L,
                upperLabel = list(
    show = F,
    height = 30,
    color='grey',
    # backgroundColor='black',
    opacity=1),
    itemStyle = list(
      borderColor = '#fff'
    )
  ) |> 
  e_title("Treemap chart") |> 
  e_tooltip() |> 
  e_theme('london')

cb <- "() => {
  let x = 0;
  setInterval(() => {
    x++
    chart.setOption(opts[], true);
  }, 5000);
}"

e_morph(sunburst, treemap, callback = cb)


bmi_sya_age <- pop |> 
  count(bmi,age) |>  #=as.character(age)
  filter(!is.na(bmi)) |> 
  # filter(age>20,age<90) |>
  pivot_wider(names_from = bmi, values_from = n) |>
  # mutate(  apples = runif(n()),
  #          bananas = runif(n()),
  #          pears = runif(n()),
  #          dates = seq(1, n()),
  #          dates = as.character(seq.Date(Sys.Date() - n()+1 , Sys.Date(), by = "day"))) |> 
   # group_by(bmi) |>
  e_charts(age) |>
  # e_single_axis(index = 1,type = 'category') |> 
  # e_tooltip,(trigger='axis') |> 
  # e_river(bmi) |>
    # e_single_axis(index = 2,type = 'value',max = 'dataMax') |>  #|>,    coord_system = "singleAxis"
  # echarts_from_json(txt='t')
  # e_river(apples) #|>
  # e_river(bananas) |> 
  e_river(obese) |>
  e_single_axis(index = 0,type = 'value',max = 'dataMax') |>  #|>,    coord_system = "singleAxis"
  e_theme('roma') |>
  # e_x_axis(type = 'category') |>
  e_river(overweight) |>     
  e_single_axis(index = 0,type = 'value',max = 'dataMax') |>  #|>,    coord_system = "singleAxis"
  

  e_river(normal)  |> 
  e_single_axis(index = 0,type = 'value',max = 'dataMax') |> 
  
  e_tooltip(trigger = "axis") #|> 
  # e_title("BMI with Age", "Continuouse Single year of Age")





trusts <- sf::read_sf('./data/trustboundaries.geojson')

trusts <- st_transform(trusts, 'WGS84')

trusts <- st_make_valid(trusts)

trusts1 <- st_simplify(trusts,
                       preserveTopology = TRUE,
                       dTolerance = 2000)

file.remove("./data/trusts.geojson")
st_write(trusts1,'./data/trusts.geojson', append = FALSE)

trusts_json <- jsonlite::read_json("./data/trusts.geojson")
object.size(trusts_json)


trusts <- pop |> 
  count(HSCT,overweight=bmi%in%c('obese','overweight')) |> 
  filter(overweight == T) |> 
  rename(c( 'TrustName' = 'HSCT')) |> 
  mutate(Name = sort(trusts1$TrustName))

trusts1 <- trusts1 |> 
  left_join(trusts, by =c('TrustName' = 'Name'))


#(
#  pick_canvas_map <-

cb <- "() => {
  let x = 0;
  setInterval(() => {
    x++
    chart.setOption(opts[x % 2], true);
  }, 5000);
}"

cb <- "() => {
  let x = 0;

  
    chart.on('click', function(e) {
      x = x + 1;
      chart.setOption(opts[x % 2], true);
    });
  
}"

  

bar_map_morph <- e_morph(trusts_map, trust_bar, callback = cb)

(
trust_bar <-  
    trusts %>% 
  #head() |> 
  # group_by(Name) |>
  e_charts(Name,reorder = FALSE ) %>% #height = '450px', width='450px'
  e_bar(universalTransition = TRUE,
        animationDurationUpdate = 2000L,
        name='TrustName',
        legend = F,
        tooltip = T,
        serie = n) |> 
    e_tooltip(backgroundColor='white') |> 
    e_x_axis(axisLabel = list(rotate = 15)) |> 
  # e_flip_coords() |>
  e_visual_map(serie = n) |>
  e_grid(left='30%',bottom='20%') |> 
  e_theme('london')
  )
(
 trusts_map <-    trusts %>% 
    #head() |> 
    e_charts(Name,
             reorder = FALSE) %>%
    e_map_register("custom_map", trusts_json) |> 
      e_visual_map(n) |> 
      e_theme('london') |> 
    e_map(universalTransition = TRUE,
          animationDurationUpdate = 2000L,
          serie=n, 
          name='TrustName',
          nameProperty = "TrustName",
          roam=F,
          map = "custom_map",
          itemStyle=list( borderColor='white'))
)



########################################
########################################
BMI_parallel_chart
overweight_obese_sex
bmi_sya_age
obesity_effects_sunburst
obesity_effects_treemap

bar_map_morph
trust_bar
trusts_map
########################################
########################################


library(leaflet)
library(htmlwidgets)

{ pal <- colorNumeric(palette = "Blues", domain = trusts1$n, na.color = "transparent")
  
 leaflet_trust <-  leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    htmlwidgets::onRender("
    function(el, x) {
      L.control.zoom({ position: 'topright' }).addTo(this);
    }
  ") %>%
    addTiles() %>%
    addPolygons(
      data = trusts1,
      stroke = TRUE,
      color = "transparent",
      weight = 1,
      opacity = 1,
      fill = TRUE,
      fillColor = ~pal(n),
      fillOpacity = 0.7,
      label = ~lapply(
        paste0(
          "<b> ", TrustCode, "</b><br>",
          "<b>n:</b> ", format(n, big.mark = ",")
        ),
        htmltools::HTML
      ),
      labelOptions = labelOptions(
        direction = "auto",
        sticky = TRUE,
        textsize = "13px",
        offset = c(5, -5)#,
        # style = list(
        #   "background-color" = "rgba(255,255,255,0.9)",
        #   "border" = "1px solid grey",
        #   "border-radius" = "4px",
        #   "padding" = "4px"
        # )
      ),
      highlight = highlightOptions(
        weight = 2,
        color = '#555',
        fillOpacity = 0.8,
        bringToFront = TRUE
      )
    ) %>%
    setView(lng = -6.988054, lat = 54.60701, zoom = 7)
}


lrg_leaflet <- leaflet(elementId = 'map',width='100vw',height='100vh',
        #options = leafletOptions(zoomControl = FALSE)
) %>%
  # htmlwidgets::onRender("function(el, x) {
  #   L.control.zoom({ position: 'bottomright' }).addTo(this)}") |> 
  addTiles() |> 
  setView(lng = -5.9576, lat = 54.904, zoom = 8) |> 
  # addMarkers(lng = -0.1276, lat = 51.5074, popup = "London") |> 
  
  addCircles(data = parks,
             weight = 15,
             # radius = 150, 
             fillOpacity = 1,
             fillColor  = 'mediumseagreen',
             fill = F,
             opacity=0.5,
             color = 'mediumseagreen',
             stroke = T,
             label = ~name#,
             
             #popup = ~as.character(name)
  ) |> 
  addCircles(data = fast_food,
             weight = 15,
             fillOpacity = 1,
             fillColor  = 'steelblue',
             fill = F,
             opacity=0.5,
             color = 'steelblue',
             stroke = T,
             label = ~name) |> 
  addLegend(position = 'bottomright',
            colors = c('mediumseagreen','steelblue'),
            labels = c('Parks','Fast Food Outlets'),
            opacity = 1) 
