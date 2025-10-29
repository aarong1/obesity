library(fst)
# source('app_prep.R')

#Risk Stratification

pop |> count(bmi)

pop$deprivation
pop$custom_townsend_rank
pop$custom_townsend_score_dz
pop$income_dm_decile_soa
pop$employment_dm_decile_soa
# Urban_mixed_rural_status
# NRA_name

# pop |> count(age10,bmi) |>
#   group_by(bmi) |> 
#       dplyr::filter(!is.na(bmi)) |>
#       echarts4r::e_charts(age10) |>
#       echarts4r::e_bar(n, stack = "BMI", name = ~bmi) |>
#       echarts4r::e_title("BMI distribution across Age Bands") |>
#       echarts4r::e_tooltip(trigger = "axis") |>
#       echarts4r::e_legend() |>
#       echarts4r::e_x_axis(name = "Age Band") |>
#       echarts4r::e_y_axis(name = "Population Count") |>
#       echarts4r::e_datazoom(xy = TRUE)
  
metric_chart_bmi_age <- pop |> 
  dplyr::count(age10, bmi, name = "n") |>
  dplyr::filter(!is.na(bmi)) |>  
  group_by(bmi) |> 

  echarts4r::e_charts(age10) |>
  echarts4r::e_bar(n) |>
   #echarts4r::e_title("BMI distribution across Age Bands") |>
  echarts4r::e_tooltip(trigger = "axis") |>
  echarts4r::e_legend(show=T)
  
metric_chart_bmi_sex <- pop |> count(sex,bmi) |> 
  dplyr::filter(!is.na(bmi)) |>  
  group_by(bmi) |> 
  echarts4r::e_charts(sex) |>
  echarts4r::e_bar(n) |>
  #echarts4r::e_title("BMI distribution across Age Bands") |>
  echarts4r::e_tooltip(trigger = "axis")


fn_plt <- function(df,x_col,y_col=n, lab= ''){
  
  x_col      <- rlang::enquo(x_col)
  x_label <- rlang::quo_label(x_col)
  print(x_label)
  print(class(x_label))
  df <- df |> 
    rename(value = {{y_col}}) |>
    rename(x_col = {{x_col}}) 
  
if(is.logical(df$x_col)){
  print('logical')
  df <- df |> mutate(x_col =ifelse(x_col==T,
                             as.character(lab),
                             'normal')
    )
}
  
  print(df)
  df |> 
  dplyr::filter(!is.na(bmi)) |>  
  group_by(bmi) |> 
  echarts4r::e_charts(x_col) |>
  echarts4r::e_bar(value) |>
  #echarts4r::e_title("BMI distribution across Age Bands") |>
  echarts4r::e_tooltip(trigger = "axis")
}

income_plot <- pop |> 
  count(income_dm_decile_soa,bmi) |> 
  fn_plt(x_col=income_dm_decile_soa)

employment_plot <- pop |> count(bmi,employment_dm_decile_soa)|> 
  fn_plt(x_col=employment_dm_decile_soa)

NRA_plot <- pop |> 
  # mutate(NRA_name = na_if(NRA_name, 'NA')) |> 
  count(bmi, NRA_name = 'NA'==NRA_name)|> 
  fn_plt(x_col = NRA_name, lab='NRA')

HSCT_plot <- pop |> 
  # mutate(NRA_name = na_if(NRA_name, 'NA')) |> 
  count(bmi, HSCT)|> 
  fn_plt(x_col = HSCT)

hypertension_plot <- pop |> count(bmi,hypertension = !is.na(hypertension))|> 
  fn_plt(x_col=hypertension, lab = 'Hypertension') #|> 
  # rename(value = n) |>
  # rename(x_col = hypertension) |>
  # mutate(x_col= ifelse(is.logical(x_col),
  #               ifelse(x_col==T,as.character(x_col),'normal'),
  #               x_col))) |>
  # dplyr::filter(!is.na(bmi)) |>  
  # group_by(bmi) |> 
  # echarts4r::e_charts(x_col) |>
  # echarts4r::e_bar(value) |>
  # #echarts4r::e_title("BMI distribution across Age Bands") |>
  # echarts4r::e_tooltip(trigger = "axis")

af_plot <- pop |> count(bmi,atrial_fibrillation = !is.na(atrial_fibrillation))|> 
  fn_plt(x_col=atrial_fibrillation, lab = 'Atrial Fibrillation')
ethnicity_plot <- pop |> count(bmi,ethnicity = broad_ethnicity)|> 
  fn_plt(x_col=ethnicity)
pad_plot <- pop |> count(bmi,pad = !is.na(pad))|> 
  fn_plt(x_col=pad, lab = 'Peripheral Arterial Disease')
ckd_plot <- pop |> count(bmi,ckd = !is.na(ckd))    |> 
  fn_plt(x_col=ckd, lab = 'Chronic Kidney Disease')
cholesterol_plot <- pop |> count(bmi,cholesterol = !is.na(cholesterol))|> 
  fn_plt(x_col=cholesterol, lab = 'High Cholesterol')

smoke_plot <- pop |> count(bmi,smoking = !is.na(smoking))  |> 
  fn_plt(x_col=smoking, lab = 'Smokes')
alcohol_plot <- pop |> count(bmi,alcohol = !is.na(alcohol)) |> 
  fn_plt(x_col=alcohol, lab = 'Unsafe Alcohol Consumption')
diet_plot <- pop |> count(bmi,diet = !is.na(diet))     |> 
  fn_plt(x_col=diet, lab = 'Unhealthy Diet')
pa_plot <- pop |> count(bmi,pa = !is.na(pa))      |> 
  fn_plt(x_col=pa, lab = 'Insufficient Physical Activity')

# pop |> count(bmi,wellbeing = !is.na(wellbeing))
# pop |> count(bmi,pm25g = !is.na(pm25g)) 

# pop <- pop |> 
#   rowwise() |> 
#   mutate(
#   comorbidities = sum(
# !is.na(diabetes),
# !is.na(atrial_fibrillation),
# !is.na(pad),
# !is.na(ckd),
# !is.na(hypertension),
# !is.na(cholesterol),
# 
# !is.na(bmi),
# !is.na(smoking),
# !is.na(alcohol),
# !is.na(diet),
# !is.na(pa)#,
# #!is.na(wellbeing)#,
# #!is.na(pm25g)
# )
# )

comorbidities_plot <- pop |> 
  filter(!is.na(bmi)) |>
  count(bmi,comorbidities) |> 
  fn_plt(x_col = comorbidities)

pop |> 
  ggplot(aes(comorbidities, n, fill=bmi))+
  geom_col(position = 'dodge')

hist(pop$comorbidities)
hist((pop$qrisk_score))

#Causes of obesity
# DIET


#Physical Activity

(pm25g_urban_chart <- pop |>   
  filter(!is.na(bmi)) |> 
    mutate(Urban_mixed_rural_status=factor(Urban_mixed_rural_status,
                                           levels=c('Urban','Mixed','Rural'))) |> 
  group_by(Urban_mixed_rural_status) |> 
  summarise(pm25g = mean(pm25g)) |> 
  e_charts(Urban_mixed_rural_status) |> 
  e_bar(pm25g) |> 
  e_color(color = c('lightblue')) |> 
  e_line(pm25g,
         color='mediumseagreen',
         itemStyle = list(opacity=1, size=10),
         lineStyle = list(width=5)) |> 
  e_y_axis(min=2,max=6) 
  )
  #!is.na(pm25g)

#Depression
depression_obesity_chart <- pop |>   
  filter(!is.na(bmi)) |> 
  group_by(bmi) |> 
  summarise(depression_percentile = mean(depression_percentile)) |> 
  e_charts(bmi) |> 
  e_bar(depression_percentile) |> 
  e_color(color = c('lightblue')) |> 
  e_line(depression_percentile,
         color='mediumseagreen',
         itemStyle = list(opacity=1),
         lineStyle = list(width=8)) |> 
  e_y_axis(min=0.4,max=0.6) 

##################################
##################################

####### Include Sleep percentile #########

##################################
##################################

# pop |>   
#   filter(!is.na(bmi)) |> 
#   group_by(bmi) |> 
#   summarise(sleep_percentile = mean(sleep_percentile)) |> 
#   e_charts(bmi) |> 
#   e_bar(sleep_percentile) |> 
#   e_color(color = c('lightblue')) |> 
#   e_line(sleep_percentile,
#          color='mediumseagreen',
#          itemStyle = list(opacity=1),
#          lineStyle = list(width=8)) |> 
#   e_y_axis(min=0.4,max=0.6) 
(
townsend_distribution_chart <- pop |> 
  # group_by(bmi) |>
  e_chart() |> 
  e_density(stack = 'bmi',serie = custom_townsend_score_dz,       
           itemStyle = list(opacity=0),
           lineStyle = list(width=2)#,color='white'
) |> 
  e_grid(left='20%') |> 
  e_theme('walden')
)
(
qrisk_distribution_chart <- pop |> 
    filter(qrisk_score>0.02) |> 
  # group_by(bmi) |>
  e_chart() |> 
  e_density(serie = qrisk_score,         
            itemStyle = list(opacity=0),
            lineStyle = list(width=2)#,color='white'
  ) |> 
  e_theme('walden') |> 
    e_grid(left='20%')
)
# pop |>   
#   filter(!is.na(bmi)) |> 
#   group_by(bmi) |> 
#   summarise(pm25g = median(pm25g)) |> 
#   e_charts(bmi) |> 
#   e_bar(pm25g) |> 
#   e_line(pm25g,color='lightgreen') |> 
#   e_y_axis(min=4,max=4.5) 


pop |> 
  filter(!is.na(bmi)) |> 
  group_by(bmi) |> 
  e_charts() |> 
  e_boxplot(depression_percentile)

metric_chart_bmi_sex
metric_chart_bmi_age

depression_obesity_chart
pm25g_urban_chart

townsend_distribution_chart
qrisk_distribution_chart

comorbidities_plot


income_plot
employment_plot
NRA_plot
HSCT_plot

hypertension_plot
af_plot
ethnicity_plot
pad_plot
ckd_plot
cholesterol_plot
smoke_plot
alcohol_plot
diet_plot
pa_plot

