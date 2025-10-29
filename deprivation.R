library(fst)

# pop <- read.fst('populations/k20_population.fst')
# source('app_prep.R')

pop |> 
  group_by(dz_id) |> 
  summarise(big = sum(bmi %in% c('overweight','obese')),
            tot = n(),
 dep=mean(custom_townsend_rank)
#dep  = mean(mdm_rank)
) |> 
  filter(tot>10) |> 
  mutate(prev = big/tot) |> 
  ggplot(aes(prev,dep))+
  geom_point(alpha=0.4)+
  geom_smooth(method='lm')


deprivation_bmi_age_chart <- pop |> 
  group_by(dz_id,age20) |> 
  summarise(big = sum(bmi %in% c('overweight','obese')),
            tot = n(),
            dep=mean(custom_townsend_rank)
            #dep  = mean(mdm_rank)
  ) |> 
  filter(tot>20) |> 
  
  mutate(prev = big/tot) |> 
  ungroup() |> 
  group_by(age20) |> 
  
  e_charts(prev) |> 
  e_scatter(dep) |> 
  e_lm(dep~prev,
       name = c('0-20', 
                '20-40', 
                '40-60', 
                '60-80', 
                 '80-100'#, 
                #'100-120'
                ))
  

top_dea_overweight_prev <- pop |> 
  group_by(DEA2014_name) |> 
  summarise(big = sum(bmi %in% c('overweight','obese')),
            overweight = sum(bmi == 'overweight',na.rm=T),
            obese = sum(bmi == 'obese',na.rm = T),
            tot = n(),
            dep=mean(custom_townsend_rank)
            #dep  = mean(mdm_rank)
  ) |> 
  filter(tot>10) |> 
  mutate(big_prev = big/tot,
         obese_prev = obese/tot,
         overweight_prev = overweight/tot) |> 
  arrange(desc(big_prev))


top_town_overweight_prev <- pop |> 
  group_by(SETTLEMENT2015_name) |> 
  summarise(big = sum(bmi %in% c('overweight','obese')),
            overweight = sum(bmi == 'overweight',na.rm=T),
            obese = sum(bmi == 'obese',na.rm = T),
            tot = n(),
            dep=mean(custom_townsend_rank)
            #dep  = mean(mdm_rank)
  ) |> 
  filter(tot>10) |> 
  mutate(big_prev = big/tot,
         obese_prev = obese/tot,
         overweight_prev = overweight/tot) |> 
  arrange(desc(big_prev))


pop |> 
  group_by( Urban_mixed_rural_status) |> 
  summarise(big = sum(bmi %in% c('overweight','obese')),
            overweight = sum(bmi == 'overweight',na.rm=T),
            obese = sum(bmi == 'obese',na.rm = T),
            tot = n(),
            dep=mean(custom_townsend_rank)
            #dep  = mean(mdm_rank)
  ) |> 
  #filter(tot>10) |> 
  mutate(big_prev = big/tot,
         obese_prev = obese/tot,
         overweight_prev = overweight/tot) |> 
  arrange(desc(big_prev)) #|> # View()
  #filter(SETTLEMENT2015_name=='BELFAST CITY')


pop |> 
  group_by(soa_code,soa_name) |> 
  summarise(big = sum(bmi %in% c('overweight','obese')),
            tot = n(),
            dep=mean(custom_townsend_score_dz),
            unique(DEA2014_name),
            unique(SETTLEMENT2015_name),
            unique(NRA_name)
            
  ) |> 
  filter(tot>40) |> 
  mutate(prev = big/tot) |> 
  arrange(desc(dep)) |> 
  head(20)


#top deprived areas
top_town_dep_table <-pop |> 
  filter(!is.na(SETTLEMENT2015_name)) |> 
  group_by(SETTLEMENT2015_name) |> 
  summarise(big = sum(bmi %in% c('overweight','obese')),
            tot = n(),
            dep=mean(mdm_rank),
            first(DEA2014_name),
            first(HSCT),
            first(SETTLEMENT2015_name),
            first(NRA_name)
  ) |> 
  filter(tot>40) |> 
  mutate(prev = big/tot) |> 
  arrange((dep)) |> 
  head(20)



top_soa_dep_table <- pop |> 
  group_by(soa_code) |> 
  summarise(big = sum(bmi %in% c('overweight','obese')),
            tot = n(),
            dep=mean(mdm_rank),
            townsend=mean(custom_townsend_score_dz),
            
            DEA = first(DEA2014_name),
            #first(HSCT),
            town = first(SETTLEMENT2015_name),
            rwnewal_area = first(NRA_name)
  ) |> 
  filter(tot>40) |> 
  mutate(prev = big/tot) |> 
  arrange(desc(dep)) |> 
  head(20)


deprivation_bmi_age_chart
top_town_dep_table
top_soa_dep_table

top_dea_overweight_prev
top_town_overweight_prev

