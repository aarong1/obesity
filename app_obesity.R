

library(htmltools)
library(shiny)
library(echarts4r)
library(bslib)
library(leaflet)
library(reactable)
library(fst)
library(tidyverse)
library(sf)
library(profvis)
# load(".RData")

# rm(past_populations,
#    instantiate_base_pop,
#    default_fracture4_female,
#    th,
#    test_poopulation,
#    teset_population,stroke_incidence,
#    population_w_established_prevalence,new_year_pop,wrapping_examples_in_function)
# rm(list=ls())
print(paste('running','/pivottable.R'));# source('modules/pivot_module/pivottable.R')
print(paste('running','/pivottable_module.R'));source('modules/pivot_module/pivottable_module.R')
print(paste('running','/chart_update_module.R'));# source('modules/chart_update_module/chart_update_module.R')
print(paste('running','/intervention_module.R'));source('modules/intervention_module/intervention_module.R')
print(paste('running','app_prep.R'));source('app_prep.R')
# print(paste('running','pages_prep_geo.R'));source('pages_prep_geo.R')

load( file = "data/csv_pts_wgs84.RData") #csv_pts_wgs84
print(paste('running','pages_prep.R'));source('pages_prep.R')
print(paste('running','obesity_causes.R'));source('obesity_causes.R')
print(paste('running','comorbidity.R'));source('comorbidity.R')
print(paste('running','risk_stratification.R'));source('risk_stratification.R')
print(paste('running','deprivation.R'));source('deprivation.R')
# print(paste('running','tables.R'));# source('tables.R')
print(paste('running','bed_days_estimate.R'));source("bed_days_estimate.R")
print(paste('running','sick_days_estimate.R'));source("sick_days_estimate.R")
print(paste('running','infographics.R'));source('infographics.R')
#print(paste('running','obesity_prevalence_tables.R'));source('obesity_prevalence_tables.R')

graph_wrapper <- function(..., header =NULL){

div(class = "grid-item grid-item--graph theme-green",
    div(class = "grid-item-content",
        div(class = "chart-card",
            if(!is.null(header)){
               div(class = "card-header",# style = "font-size: 0.5em;",
                header
            )},
            ...
        )
    )
)
}

# theme_x <- readLines('theme.json')

ui <- page_fluid( id = 'main-content',
                  theme = bs_theme(version = 5, font_scale = 0.8,
                                   bootswatch = 'litera',
                                   primary = '#2196F3'),
                  #e_theme_register(paste0(theme_x,collapse =""), name = "myTheme"),

                  
                  # Include external dependencies
                  tags$head(
                      # Packery CSS and JS from CDN (no Draggabilly needed)
                      tags$script(src = "https://unpkg.com/packery@2/dist/packery.pkgd.min.js"),
                      HTML('<script>
                        window.FontAwesomeConfig = {
                          searchPseudoElements: true
                        }
                      </script>'),
                      # tags$script(src = "roma.js"),
                      #includeScript("roma.js"),
                      HTML('<style>
.dashboard-nav{
--primary-navy: #1e3a8a;
  --primary-blue: #3b82f6;
  --secondary-blue: #60a5fa;
  --accent-orange: #f97316;
  --success-green: #10b981;
  --warning-yellow: #f59e0b;
  --danger-red: #ef4444;
  --neutral-gray: #6b7280;
  --light-gray: #f8fafc;
  --medium-gray: #e2e8f0;
  --dark-gray: #374151;
  --white: #ffffff;
  --glass-bg: rgba(255, 255, 255, 0.85);
--glass-border: rgba(255, 255, 255, 0.2);
--shadow-light: 0 1px 3px rgba(0, 0, 0, 0.1);
--shadow-medium: 0 4px 6px rgba(158, 123, 123, 0.07);
--shadow-heavy: 0 10px 25px rgba(0, 0, 0, 0.1);
--border-radius: 12px;
--border-radius-lg: 16px; 
--transition: all 0.2s cubic-bezier(0.4, 0, 0.2, 1);
--fa-style-family-brands: "Font Awesome 6 Brands";
--fa-font-brands: normal 400 1em/1 "Font Awesome 6 Brands";
--fa-font-regular: normal 400 1em/1 "Font Awesome 6 Free";
--fa-style-family-classic: "Font Awesome 6 Free";
--fa-font-solid: normal 900 1em/1 "Font Awesome 6 Free";
color: var(--dark-gray);
line-height: 1.6;
margin: 0;
box-sizing: border-box;
animation-duration: 0.01ms !important;
animation-iteration-count: 1 !important;
transition-duration: 0.01ms !important;
background: var(--white);
border-bottom: 1px solid var(--medium-gray);
padding: 1rem 2rem;
margin: 4rem 0rem;

box-shadow: var(--shadow-light);
}
</style>'),
                      # Custom CSS styling
                      tags$style("
* { box-sizing: border-box; } 

body { 
  /*font-family: sans-serif;  */
  margin: 0;
  padding: 0;
}

/* ---- Layout ---- */

.main-container {
  display: flex;
  min-height: 100vh;
}

.sidebar {
  width: 250px;
  background: linear-gradient(180deg, #2c3e50 0%, #34495e 100%);
  color: white;
  position: fixed;
  height: 100vh;
  left: 0;
  top: 0;
  overflow-y: auto;
  box-shadow: 2px 0 10px rgba(0,0,0,0.1);
  z-index: 1000;
}

.sidebar-header {
  padding: 20px;
  border-bottom: 1px solid rgba(255,255,255,0.1);
  text-align: center;
}

.sidebar-title {
  font-size: 1.4em;
  font-weight: bold;
  margin: 0;
}

.nav-section {
  padding: 15px 0;
}

.nav-section-title {
  padding: 10px 5px;
  font-size: 1em;
  color: darkgrey;
  /*text-transform: uppercase;*/
  letter-spacing: 1px;
  margin: 0;
}

.nav-item {
  color: grey;
  padding: 3px 3px;
  margin: 2px 0px;
  cursor: pointer;
  transition: all 0.3s ease;
  border-left: 3px solid transparent;
}

.nav-item:hover {
  background: rgba(52, 152, 219, 0.1);
  border-radius:10px;

}

.nav-item.active {
  /*background: rgba(52, 152, 219, 0.5);
  color:white; */
  font-weight: bold;
  border-radius:10px;

}

.nav-icon {
  margin-right: 10px;
  width: 16px;
  display: inline-block;
}

.content-area {
  margin:auto;
  flex: 1;
  /*padding: 20px;*/
}
2
/* ---- grid ---- */
.grid {
  padding: 10px;
  border-radius: 40px;
  max-width: 70vw;
  min-height: 100vh;
}
2
/* clear fix */
.grid:after {
  content: '';
  display: block;
  clear: both;
}

/* ---- .grid-item ---- */
.grid-item {
  float: left;
  background: white;
  border-radius:20px;
  box-shadow: 0 1px 3px rgba(100, 0, 0, 0.01);
  border: 1px solid hsla(0, 0%, 100%, 0.5);
  transition: all 0.3s ease;
  cursor: pointer;
  overflow: hidden;
  margin-right: 2px;
  margin-bottom: 2px;
}

.grid-item-content {22
  padding: 20px;
  border-radius: 20px;
  width: 100%;
  height: 100%;
  transition: all 0.4s ease;
}

.grid-item--width2 { width: 150px; }
.grid-item--height2 { height: 150px; }
.grid-item--small { width: 150px; height: 150px; }
.grid-item--graph { width: 300px; height: 300px; }

/* Expanded state */
.grid-item.is-expanded {
  z-index: 100;
}

.grid-item.is-expanded.grid-item--small {
  width: 400px;
  height: 300px;
}

.grid-item.is-expanded.grid-item--graph {
  width: 700px;
  height: 600px;
}

.grid-item.is-expanded .grid-item-content {
  transform: scale(1.02);
}

.grid-item:hover {
 /* 
  margin-right: 0px;
  margin-bottom: 0px; 
  */
  transform: translate(2px 2px);
  z-index:1000;
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
}

.grid-item:hover .grid-item-content {
  background: rgba(255, 255, 255, 0.95);
}

/* Click indicator */
.grid-item::after {
  position: absolute;
  top: 10px;
  right: 10px;
  opacity: 0;
  transition: opacity 0.3s ease;
  font-size: 12px;
  background: rgba(0, 0, 0, 0.3);
  color: white;
  padding: 4px 6px;
  border-radius: 4px;
}

.grid-item:hover::after {
  opacity: 1;
}

.grid-item.is-expanded::after {
  content: '\f16c';
    /* >> Name of the FA free font (mandatory), e.g.:
               - 'Font Awesome 5 Free' for Regular and Solid symbols;
               - 'Font Awesome 5 Pro' for Regular and Solid symbols (Professional License);
               - 'Font Awesome 5 Brand' for Brands symbols. */
    font-family: 'Font Awesome 5 Free';
    /* >> Weight of the font (mandatory):
               - 400 for Regular and Brands symbols;
               - 900 for Solid symbols;
               - 300 for Light symbols. */
    font-weight: 400;
    opacity:1;
}

/* Metric Cards */
.metric-card {
  margin:auto;
  text-align: center;
  height: 100%;
  width: 100px;
  display: flex;
  flex-direction: column;
  justify-content: center;
}

.handle{
  position:relative;
  top:1px;
  left:15px;
}

.metric-value {
  font-size: 2.1em;
  font-weight: bold;
  margin-bottom: 10px;
}

.metric-label {
  font-size: 1em;
  color: #6c757d;
  margin-bottom: 15px;
}

.metric-change {
  font-size: 0.8em;
  font-weight: 600;
  padding: 5px 15px;
  border-radius: 20px;
  display: inline-block;
}

.metric-change.positive {
  background: rgba(40, 167, 69, 0.1);
  color: #28a745;
}

.metric-change.negative {
  background: rgba(220, 53, 69, 0.1);
  color: #dc3545;
}

/* Chart Cards */
.chart-card {
  padding: 15px;
  height: 100%;
  width: 100%;
  display: flex;
  flex-direction: column;
}

.chart-card .echarts4r {
  flex: 1;
  min-height: 0;
  width: 100% !important;
  height: 100% !important;
}

.card-header {
  font-weight: bold;
  margin-bottom: 15px;
  color: #495057;
  border-bottom: 2px solid #e9ecef;
  padding-bottom: 10px;
}

/* Control Panel */
.control-panel {
  background: white;
  padding: 20px;
  border-radius: 12px;
  margin-bottom: 20px;
  box-shadow: 0 2px 10px rgba(0,0,0,0.1);
}

/* Navigation Cards*/
.nav-card {
  color: black;
  opacity:0.7;
  display: flex;
  flex-direction: column;
  justify-content: center;
  cursor: pointer;
  margin:0.5rem;
  padding:0.5rem;
  border-radius:0.5rem;
  transition:all 0.5s ease;
} 

.nav-card:hover {
    opacity:0.9;
  transform: scale(1.02);
  z-index:10;
  transition:all 0.3s ease;
}

.nav-card-icon {
  color:white;
  text-align: right;
  font-size: 1.4em;
  margin-bottom: 0.5rem;
  opacity: 0.9;
}

/* Tab Content Styling */
.tab-content {
  display: block;
  padding: 20px;
  animation: fadeIn 0.3s ease-in-out;
}

.tab-content.active {
  display: block;
}

@keyframes fadeIn {
  from { opacity: 0; }
  to { opacity: 1; }
}

.nav-card-icon i:hover {
  box-shadow: 0 1px 1px rgba(255,255,255,0.1);
}

.nav-card-title {
  font-size: 1.3em;
  font-weight: bold;
  margin-bottom: 8px;
}

.nav-card-description {
  font-size: 0.8em;
  opacity: 0.8;
}

.nav-card.reports {
  background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);
}

.nav-card.settings {
  background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%);
}

.nav-card.analytics {
  background: linear-gradient(135deg, #43e97b 0%, #38f9d7 100%);
}

  .glass-card {
  background: rgba(255, 255, 255, 0.44); /* 0.14 */
  backdrop-filter: blur(5px);
  -webkit-backdrop-filter: blur(5px);
  border-radius: 20px;
  border: 1px solid rgba(255, 255, 255, 0.3);
  box-shadow: 
    0 8px 32px rgba(0, 0, 0, 0.1),
    inset 0 1px 0 rgba(255, 255, 255, 0.5),
    inset 0 -1px 0 rgba(255, 255, 255, 0.1),
    inset 0 0 10px 5px rgba(255, 255, 255, 0.5);
  overflow: hidden;
}

.glass-card::before {
  content: '';
  position: absolute;
  top: 0;
  left: 0;
  right: 0;
  height: 1px;
  background: linear-gradient(
    90deg,
    transparent,
    rgba(255, 255, 255, 0.8),
    transparent
  );
}

.glass-card::after {
  content: '';
  position: absolute;
  top: 0;
  left: 0;
  width: 1px;
  height: 100%;
  background: linear-gradient(
    180deg,
    rgba(255, 255, 255, 0.8),
    transparent,
    rgba(255, 255, 255, 0.3)
  );
}

")
                  ),
                  
                  # Main layout container
                  
                  # Control Panel
                 
                  
                 # HTML(' <!-- Navigation -->
                 #    <nav class="dashboard-nav">
                 #    <div class="nav-content">
                 #    <div class="nav-items">
                 #    <a href="#overview" class="nav-item active" data-section="overview" data-bs-toggle="tab" data-bs-target="#overview">
                 #    <i class="fas fa-tachometer-alt"></i>
                 #    <span>Overview</span>
                 #    </a>
                 #    <a href="#geography" class="nav-item" data-section="geography"  data-bs-toggle="tab" data-bs-target="#geography">
                 #    <i class="fas fa-map-marked-alt"></i>
                 #    <span>Geographic Analysis</span>
                 #    </a>
                 #    <a href="#population" class="nav-item" data-section="population"  data-bs-toggle="tab" data-bs-target="#population">
                 #    <i class="fas fa-users"></i>
                 #    <span>Population Insights</span>
                 #    </a>
                 #    <a href="#risk-factors" class="nav-item" data-section="risk-factors"  data-bs-toggle="tab" data-bs-target="#risk-factors">
                 #    <i class="fas fa-exclamation-triangle"></i>
                 #    <span>Risk Factors</span>
                 #    </a>
                 #    <a href="#data-explorer" class="nav-item" data-section="data-explorer"  data-bs-toggle="tab" data-bs-target="#data-explorer">
                 #    <i class="fas fa-table"></i>
                 #    <span>Data Explorer</span>
                 #    </a>
                 #    </div>
                 #    </div>
                 #    </nav>'),
                 
                 shiny::tags$nav(
                   
                   class = "navbar fixed-top shadow-lg glass-card mb-3 p-2 rounded", # bg-light
                   shiny::h4(  "Population Health ", span(class = 'lead','Population Health Model'),
                               span(HTML('<h7><span class="badge rounded-pill text-white bg-opacity-75 bg-primary">Obesity', '' ,'</span></h7>')),
                               
                               HTML('<h7><span class="badge rounded-pill bg-opacity-75 bg-warning ">SPPG </span></h7>'),
                               HTML('<h7><span class="badge rounded-pill bg-opacity-75 bg-info ">PHA </span></h7>'),
                               HTML('<h7 id = "move" style = "float:right;">
                       <a href="https://apply-for-innovation-funding.service.gov.uk/competition/2186/overview/e51c18bc-21b3-450d-bdbc-2f43dad3b268">
                       <span class="badge rounded-pill bg-light">
                        <i class="fa-solid fa-arrow-up-right-from-square"></i>
                       OPIP bid</span>
                       </a>
                       </h7>'),
                               
                               class = "navbar-brand mb-0 w-100"
                   )
                 ),
                 
                
                 div(style='top:12vh;width:12vw;z-index:1000',class = ' ms-2 p-3 d-flex flex-column display-absolute position-fixed left-0 shadow-sm glass-card',

                     
                     div(class = "nav-section",
                         h6(class = "bg-opacity-50 border-5  p-2 rounded-3 text-bg-success", "Health Burden"), #text-body-secondary
                         div(class = "nav-item",
                             span(class = "nav-icon"),
                             "Dashboard"
                         ),
                         div(class = "nav-item active",
                             span(class = "nav-icon"),
                             "Analytics"
                         ),
                         div(class = "nav-item",
                             span(class = "nav-icon"),
                             "Reports"
                         )
                     ),
                     
                     div(class = "nav-section",
                         h6(class = "bg-opacity-50 border-5  p-2 rounded-3 text-bg-success",
                            "OPIP"),
                         
                         div(class = "nav-item",
                             span(class = "nav-icon"),
                             "Obesity Risk"
                         ),  #
                         div(class = "nav-item",
                             span(class = "nav-icon"),
                             "Geography"
                         ),
                         div(class = "nav-item",
                             span(class = "nav-icon"),
                             "Population"
                         ),
                         
                         div(class = "nav-item",
                             span(class = "nav-icon"),
                             "Northern Trust"
                         ),
                         div(class = "nav-item",
                             span(class = "nav-icon"),
                             "Lifestyle"
                         ),
                         div(class = "nav-item",
                             span(class = "nav-icon"),
                             "Deprivation"
                         ),
                         div(class = "nav-item",
                             span(class = "nav-icon"),
                             "Society"
                         )
                     ),
                     div(class = "nav-section",
                         h6(class = "bg-opacity-50  border-5  p-2 rounded-3 text-bg-success", "Intervention"), #nav-section-title
                         div(class = "nav-item",
                             span(class = "nav-icon"),
                             "Specify"
                         ),
                         div(class = "nav-item",
                             span(class = "nav-icon"),
                             "Scenarios"
                         )
                     )
                     
                 ),
                  
                  div(class='mt-5 pt-5'),
                  div(class = "main-container",
                      
                      # Main content area
                      div(class = "content-area",
                         # div( class="tab-pane active", id="overview", role="tabpanel", `aria-labelledby`="overview",
              
            div(class = "tab-content",
                              
                              # Dashboard Tab Content
                            
              div(id = "dashboard-tab", class = "tab-pane show active", #
                  div(class = "container-fluid",
                      style = "padding-left: 17%;",

                      fluidRow(

                              column(12,offset=0,

                                         # Dashboard Tab Content (default active)
                                             div(id = "grid", class = "grid",


                                         div(class ='grid-item',style = 'border-width:5px 0px 0px 0px;width:70vw;border-style: solid;border-color: grey;'),

                                         div(class = "grid-item grid-item--graph theme-blue",
                                             div(class = "grid-item-content",
                                                 div(class = "chart-card",
                                                     div(class = "card-header",
                                                         "BMI sex"
                                                     ),
                                                     metric_chart_bmi_sex


                                                 )
                                             )
                                         ),

                                         div(class = "grid-item grid-item--graph theme-blue",
                                             div(class = "grid-item-content",
                                                 div(class = "chart-card",
                                                     div(class = "card-header",
                                                         "BMI age"
                                                     ),
                                                     metric_chart_bmi_age


                                                 )
                                             )
                                         ),


                                         div(class = "grid-item grid-item--graph theme-blue",
                                             div(class = "grid-item-content",
                                                 div(class = "chart-card",
                                                     div(class = "card-header",
                                                         "Townsend Material Deprivation "
                                                     ),
                                                     comorbidities_bmi_townsend_extreme


                                                 )
                                             )
                                         ),

                                         div(class ='grid-item',style = 'border-width:5px 0px 0px 0px;width:70vw;border-style: solid;border-color: grey;'),

                                         div(class = "grid-item grid-item--graph theme-green",
                                             div(class = "grid-item-content",
                                                 div(class = "chart-card",
                                                     #no card header

                                                     overweight_obese_sex
                                                 )
                                             )
                                         ),
                                         
                                         div(class = "grid-item grid-item--graph theme-blue",
                                             div(class = "grid-item-content",
                                                 div(class = "chart-card",
                                                     div(class = "card-header",
                                                         "Townsend Material Deprivation "
                                                     ),
                                                     townsend_distribution_chart
                                                 )
                                             )
                                         ),

                                         div(class = "grid-item grid-item--graph theme-blue",
                                             div(class = "grid-item-content",
                                                 div(class = "chart-card",
                                                     div(class = "card-header",
                                                         "Risk Stratification and Priority"
                                                     ),
                                                     qrisk_distribution_chart
                                                 )
                                             )
                                         ),
                                         div(class ='grid-item',style = 'border-width:5px 0px 0px 0px;width:70vw;border-style: solid;border-color: grey;'),

                                         div(class = "grid-item grid-item--graph theme-blue",
                                               div(class = "grid-item-content",
                                                   div(class = "chart-card",
                                                       div(class = "card-header",
                                                           "Comorbidities"
                                                       ),  comorbidities_plot
                                                   )
                                               )
                                         ),
                                         div(class = "grid-item grid-item--graph theme-blue",
                                               div(class = "grid-item-content",
                                                   div(class = "chart-card",
                                                       div(class = "card-header",
                                                           "Depression and Obesity"
                                                       ),  depression_obesity_chart
                                                   )
                                               )
                                         ),

                                         div(class = "grid-item grid-item--graph",
                                             div(class = "grid-item-content",
                                                     leaflet_trust



                                             )
                                             )

                                         ),
                                         div(class ='grid-item',style = 'border-width:5px 0px 0px 0px;width:70vw;border-style: solid;border-color: grey;'),
                                         
                                         metic_card_prev_total_obesity,
                                         metric_card_total_bed_days,
                                         metric_card_total_episodes,
                                         metric_card_total_deaths_obesity,
                                         metric_card_YLL_total,
                                         metic_card_daly_total_obesity,
                                         metic_card_yld_total_obesity,
                                         metic_card_prev_cancer_obesity,
                                         metric_card_costs_total_obesity,


                                         # Sales Performance Chart (Wide)
                                         div(class = "grid-item grid-item--graph theme-blue",
                                             div(class = "grid-item-content",
                                                 div(class = "chart-card",
                                                     div(class = "card-header",
                                                         "Risk with BMI"
                                                     ),  risk_bmi


                                                 )
                                             )
                                         ),
                                         div(class ='grid-item',style = 'border-width:5px 0px 0px 0px;width:70vw;border-style: solid;border-color: grey;'),

                                         # Revenue Analysis Chart (Tall)
                                         div(class = "grid-item grid-item--graph theme-red",
                                             div(class = "grid-item-content",
                                                 div(class = "chart-card",
                                                     div(class = "card-header",
                                                         tags$i(class = "fas fa-chart-scatter me-2"),

                                                         "Risk by Age",tags$small(class=' text-muted','1-yr prob of serious CVD')
                                                     ),
                                                     # revenue_chart
                                                     risk_prob_density_fn_age10
                                                 )
                                             )
                                         ),

                                         # Performance Chart (Small)
                                         div(class = "grid-item grid-item--graph theme-green",
                                             div(class = "grid-item-content",
                                                 div(class = "chart-card",
                                                     # div(class = "card-header", style = "font-size: 0.5em;",
                                                     #     "Risk Correlations"
                                                     # ),
                                                     # performance_chart

                                                     risk_graph
                                                 )
                                             )
                                         ),

                                         div(class = "grid-item grid-item--graph theme-green",
                                             div(class = "grid-item-content",
                                                 div(class = "chart-card",
                                                     div(class = "card-header",
                                                         "BMI with Age"
                                                     ),
                                                     bmi_sya_age
                                                 )
                                             )
                                         ),

                                         div(class = "grid-item nav-card analytics bg-opacity-50",
                                              div(onclick = "$('.tab-pane').removeClass('active show');$('#' + 'analytics-tab').addClass('active show')",
                                                  class = "nav-card-icon",
                                                  icon("arrow-up-right-from-square")),
                                              div(class = "nav-card-title", "Morbidity"),
                                              div(class = "nav-card-description", "View detailed analytics"),
                                                        ),
                                         div(class = "grid-item nav-card settings bg-opacity-50",
                                             div(onclick = "$('.tab-pane').removeClass('active show');$('#' + 'geography-tab').addClass('active show')",
                                                 class = "nav-card-icon",
                                                 icon("arrow-up-right-from-square")),
                                             div(class = "nav-card-title", "Geography"),
                                             div(class = "nav-card-description", "View detailed analytics"),
                                         ),
                                         div(class = "grid-item nav-card reports bg-opacity-50",
                                             div(onclick = "$('.tab-pane').removeClass('active show');$('#' + 'deprivation-tab').addClass('active show')",
                                                 class = "nav-card-icon",
                                                 icon("arrow-up-right-from-square")),
                                             div(class = "nav-card-title", "Deprivation"),
                                             div(class = "nav-card-description", "View detailed analytics"),
                                         ),


                                     ) # End of dashboard grid
                                 )#, # End of dashboard tab content
                              
                         # ) # End of fluidRow
                  ) #end of fluid container
                                 ),
                                 
                                 # Analytics Tab Content
                                 div(id = "analytics-tab", class = "tab-pane active show ", #style = "width:100%",
                                     div(style = 'position:absolute;top:0;left:0;margin-inline:-12px;',#class = "container-fluid",
                                         div(
                                          leafletOutput("mymap", width = '100vw', height = '100vh'),
                                           ),
                                          ),
                                         
                                     div(class = 'glass-card p-3 me-3',
                                         style = 'position:absolute;top:12vh;right:0;height:75vh; overflow:scroll; background: rgba(255, 255, 255, 0.64)!important; /* 0.14 */',#class = "container-fluid",
                                         # style='top:15vh;width:15vw;z-index:1000',class = ' ms-3 p-4 d-flex flex-column display-absolute position-fixed left-0 shadow-sm glass-card'
                                         div(class = 'd-flex flex-column gap-3 justify-content-between',
                                       div( span(class = '','BMI'),
                                         div(style = 'height:150px;width:200px;',echarts4rOutput('bmi_chart',height = 120,width=200))
                                         ),
                                       div( span(class = '','Age'),
                                         div(style = 'height:150px;width:200px;',echarts4rOutput('age_chart',height = 120,width=200))
                                         ),
                                        div(span(class = '','Sex'),
                                         div(style = 'height:150px;width:200px;',echarts4rOutput('sex_chart',height = 120,width=200))
                                         ),
                                        div(span(class = '','Deprivation'),
                                         div(style = 'height:150px;width:200px;',echarts4rOutput('depriv_chart',height = 120,width=200))
                                         ),
                                       div(span(class = '','Risk'),
                                           div(style = 'height:250px;width:200px;',echarts4rOutput('qrisk_chart1',height = 220,width=200))
                                       ),
                                       div(span(class = '','Risk'),
                                           div(style = 'height:200px;width:200px;',echarts4rOutput('qrisk_chart',height = 180,width=200))
                                       ),
                                       
                                        span( class='ms-4',
                                          h6('Population:',textOutput('headline_count'))
                                             ),
                                       span( class='ms-4',
                                             h6('Qrisk Score:',textOutput('qrisk_average'))
                                       )
                                         )
                                     )

                                 ),

                                 # Reports Tab Content
                                 div(id = "reports-tab", class = " tab-pane active show",# style = "display: none;",
                                     div(class = "container-fluid", style = "padding-left: 20%;",
                                         h3("Population Health Data - Interactive Pivot Analysis"),
                                         p(class='lead',"Drag columns to create custom analysis. Use dimensions for grouping, measures for aggregation."),
                                         div(style = "font-size: 0.6rem !important;",
                                         # Pivot module UI
                                         pivot_module_ui("pivot_reports",
                                                        data_names = c("sex", "age_risk", "county", "hsct", "bmi",
                                                                      "Urban_status", "mdm_quintile_soa_name", "ethnicity",
                                                                      "stroke", "chd", "diabetes", "dementia", "heart_failure"),
                                                        fun_names = c("sum","mean","median","min","max","count","n_distinct")),

                                         # Results section
                                         # div(style = "margin-top: 30px;",
                                         #     #h4("Pivot Analysis Results"),
                                         #     div(id = "pivot_reports-table_container",
                                         #         reactableOutput("pivot_reports-table")
                                         #     ),
                                         #     )
                                         )
                                     )
                                 ),

                                

                             
      
                                 # Orders Tab Content
              div(id = "ObesityRisk-tab", class = "  tab-pane show active ", style = "",
                  div( style = "padding-left: 17%;",
                      h3("Obesity Contributions to Obesity"),
                      p(class = 'lead', "Factors leading to the cause of Obesity and often accompany it "),
                      div(class = "d-flex flex-row gap-3 flex-wrap justify-content-center",
                      div(class = "d-flex flex-column gap-3 flex-wrap justify-content-center",
                      metric_cards_parks,
                      metric_cards_fast_food),
                      graph_wrapper(header = 'BMI in Income Quintiles', income_plot),
                      graph_wrapper(header = 'Distribution of Modifiable Risk', comorbidities_plot),
                      graph_wrapper(header = span(class='text-muted',
                      'Higher BMI correlates with greater propensity for depression'),
                      depression_obesity_chart)
                      ),
                  
                      div(class = "d-flex flex-column gap-3 flex-wrap justify-content-center",
                      div(style = 'width:70vw', class= 'grid-item p-5 m-5',BMI_parallel_chart )),
                      
                      div(class = "d-flex flex-row gap-3 flex-wrap justify-content-center",
                          
                      graph_wrapper(header = 'BMI Age', metric_chart_bmi_age),
                      graph_wrapper(header = 'BMI Sex', metric_chart_bmi_sex),
                      graph_wrapper(header = 'BMI and Diet Risk', diet_plot),
                      graph_wrapper(header = 'BMI and Adequate Physical Activity', pa_plot)
                      )
                    )
                ),
              
              div(id = "geography-tab", class = "tab-pane show active", style = " ",
                  div(class = "container-fluid",   style = "padding-left: 14%", #;width:100%
                      h3("Geography Analytics"),
                      p(class = 'lead',"A supplement to the <a onclick= alert('hello')>analytics</a> showing the heirarchical 
                        'hotspots'broken down by geographical, administrative or statistical areas"),
                      div(class = " rounded-5 bg-light d-flex flex-row gap-2 justify-content-around",
                          div(style = 'width:30vw;height:60vh;', class= 'grid-item p-1 m-3',
                              div(class = "chart-card",
                                    div(class = "card-header",# style = "font-size: 0.5em;",
                                        'Investigate Hierachical Geography',
                                  span(class = 'text-secondary',
                                      icon('warning'),
                                      'Due to the large data size involved, only most deprived and most prevalent Geographies are plotted.'
                                        )
                                    ),
                              geo_sunburst)), 
                              div(style = 'width:30vw;height:60vh;', class= 'grid-item p-1 m-3',
                                  div(class = "chart-card",
                                      
                              div(class = "card-header",# style = "font-size: 0.5em;",
                                     'Trust Level Obesity Prevalence',
                                  span(class = 'text-bg-secondary',
                                       icon('mouse'),'Use mouse to scroll to zoom in/out, click to transform'
                                  )
                                  )
                                  ),
                                  
                              bar_map_morph
                              )
                              
                      ),
                      
                      div(class = "rounded-5 bg-light d-flex flex-row gap-3 m-5 flex-wrap justify-content-centre",
                          div(style = 'width:70vw', class= 'grid-item p-1 m-2',
                          geo_treemap
                          )
                      )
                  )
              ),
              
              div(id = "population-tab", class = "tab-pane show active ", style = " ",
                  div(class = "container-fluid",   style = "padding-left: 17%;",
                      h3(" Obesity Analytics"),
                      
                      p(class = 'lead', "Analysis of Obesity dynamic in the Population"),
                      div(class = "rounded-5 bg-light d-flex flex-row gap-3 flex-wrap justify-content-around",
                          div(style = 'width:70vw', class= 'grid-item p-5 m-5',
                              div(class = "card-header",# style = "font-size: 0.5em;",
                                  'Prevalence of Obesity and Overweight with Deprivation by age',
                                  span(class = 'text-bg-secondary',
                                  'The gradient is neutral to positive, to varying degrees among age groups')
                              ),
                              deprivation_bmi_age_chart
                              )
                          ),

                      br(),br(),br(),
                      #h6('Top Obesity Towns by Prevalence'),
                      br(),br(),br(),
                      
                      div(style='font-size:12px;',
                          #formatted_table
                      ),
                      
                      div(class='d-flex flex-row flex-wrap gap-3 justify-content-between',
                          
                          div(style = 'width:70vw',bar_map_morph),
                          graph_wrapper(obesity_effects_treemap),
                          graph_wrapper(obesity_effects_sunburst),
                          
                          
                          graph_wrapper(metric_chart_bmi_sex),
                          graph_wrapper(metric_chart_bmi_age),
                          
                          graph_wrapper(depression_obesity_chart),
                          graph_wrapper(pm25g_urban_chart),
                          
                          graph_wrapper(townsend_distribution_chart),
                          graph_wrapper(qrisk_distribution_chart),
                          
                          graph_wrapper(comorbidities_plot),
                          graph_wrapper(metric_chart_bmi_age),
                          graph_wrapper(metric_chart_bmi_sex),
                          graph_wrapper(income_plot),
                          graph_wrapper(employment_plot),
                          graph_wrapper(NRA_plot),
                          graph_wrapper(HSCT_plot),
                          
                          graph_wrapper(hypertension_plot),
                          graph_wrapper(af_plot),
                          graph_wrapper(ethnicity_plot),
                          graph_wrapper(pad_plot),
                          graph_wrapper(ckd_plot),
                          graph_wrapper(cholesterol_plot),
                          graph_wrapper(smoke_plot),
                          graph_wrapper(alcohol_plot),
                          graph_wrapper(diet_plot),
                          graph_wrapper(pa_plot)
                      )
                  )
              ),
              
              
              
              # div(id = "NorthernTrust-tab", class = "tab-pane active show", style = " ",
              #     div(class = "container-fluid",   style = "padding-left: 17%;",
              #         h3(" Northern Trust Analytics"),
              #         p("Obesity analysis content will be displayed here when the Revenue nav item is clicked."),
              #         tags$ul(tags$li('Top SOA DEA Settlements'),
              #                 tags$li('Split'),
              #                 tags$li(' Overview'),
              #                 metric_card_costs_total_obesity,
              #                 metric_card_nhs_obesity,
              #                 metric_card_society_obesity
              #         )
              #     )
              # ),
              
              div(id = "lifestyle-tab", class = "  tab-pane show active ", style = " ",
                 div(style = "padding-left: 17%;",
                      h3("Lifestyle Dashboard"),
                      p(class = 'lead',"Deprivation content will be displayed here when the Users nav item is clicked."),
                      h6('Health Burden Attributable to obesity'),
                     div(class = "d-flex flex-row flex-wrap gap-3 justify-content-between",
                      metric_card(top = 'Health Metric','','',color='teal'),
                      metic_card_prev_total_obesity,
                      metic_card_inc_total_obesity,
                      metric_card_YLL_total,
                      metic_card_daly_total_obesity,
                      metic_card_yld_total_obesity
                      
                      ),
                      #h6('Resource'),
                     div(class = "d-flex flex-row flex-wrap gap-3 justify-content-between",
                         metric_card(top = 'Resource','','',color='Purple',opacity = 'opacity-75'),
                         
                     metric_card_total_bed_days,
                     
                     metric_card_costs_total_obesity,
                     metric_card_obesity_days_lost_obesity,
                     metric_card_obesity_cost_obesity
                         
                      ),
                      # h6('Comorbidities'),
                     div(class = "d-flex flex-row flex-wrap gap-3 justify-content-between",
                      
                      metric_card(top = 'Resource','','',color='purple',opacity = 'opacity-100'),
                         
                      graph_wrapper(header = span(div('BMI with deprivation'),
                                                     span(class='text-muted','Positive Trend')),
                                                     deprivation_risk_by_bmi_chart),
                      graph_wrapper(comorbidities_curve_wo_0_or_1),
                      graph_wrapper(comorbidities_age),
                      graph_wrapper(comorbidities_bmi),
          
                      ),
                 #     
                 #     h6('Relative Risks'),
                 # 
                 #      obesity_rr_table(obesity_rr_demo, p_obesity = 0.30),
                 # 
                 # h6('Disability Weights'),
                 # 
                 #     prevalence_dw_table(morbidity_prev_dw_demo, show_yld = TRUE)
                 
                  )
              ),
              
              # Users Tab Content
              div(id = "deprivation-tab", class = "  tab-pane active show", style = " ",
                  div(class = "container-fluid",   style = "padding-left: 15%;",
                      h3("HotSpots "),
                      
                      h6(class='lead', "Deprivation content will be displayed here when the Users nav item is clicked."),
                      div(class="grid-5x5 mx-3 px-3",
                          style= 'display: grid;
                                             grid-template-columns: repeat(5, 1fr);
                                             grid-auto-rows: 1fr;            /* equal heights within a fixed container */
                                               gap: 10px;
                                             /* optional: fix container height so rows are equal; or remove and let content size */
                                              ',
                          group_echarts),
                  )
              ),

            
              
              div(id = "society-tab", class = "tab-pane show active", style = " ",
                  div(class = "container-fluid",   style = "padding-left: 17%;",
                      h3(" Society"),
                      p(class = 'lead', "Analysis on societal impact of Obesity"),
                      # tags$ul(tags$li('Cost'),
                      #         tags$li('Sick days'),
                      #         tags$li('Bed days'),
                      #         tags$li('Avg LoS for a bed related morbidity'),
                      div(class = 'd-flex flex-row flex-wrap justify-content-center gap-3',
                      metric_card_nhs_obesity,
                      metric_card_society_obesity,
                      metric_card_inpatient_obesity,
                      metric_card_outpatient_obesity,
                      metric_card_AE_obesity,
                      metric_card_primary_care_obesity,
                      metric_card_med_obesity,
                      metric_card_medical_device_obesity,
                      metric_card_long_term_obesity,
                      metric_card_morbidity_obesity,
                      metric_card_mortality_obesity,
                      metric_card_informal_care_obesity),
                      div(class = 'd-flex flex-row flex-wrap justify-content-center gap-3',
                      metric_card_obesity_days_lost_obesity,
                      metric_card_obesity_spells_obesity,
                      metric_card_obesity_cost_obesity,
                      metric_card_obesity_days_lost_population,
                      metric_card_obesity_spells_population,
                      metric_card_obesity_cost_population
                      )
                      )
              ),

              div(id = "scenarios-tab", class = "tab-pane active show", style = " ",
                   div(
                    style = "padding-left: 17%;",
                      h3("Scenarios Overview"),
                      p(class='lead',"Scenarios overview content is coming soon")
                  

                   )
              ),

              div(id = "specify-tab", class = "tab-pane show active ", style = "width:100vw",
                  div(
                      style = "padding-left: 17%;",
                      h3("Intervene "),
                      # In UI
                        intervention_module_ui("intervention1"),

                      h5(class = 'lead mb-5 ',
                         "Simulate Interventions and innovative pathways to treat obesity by
                         estimating the disease output before and after modulating population prevalence for obesity"),
                      actionButton(class='float-right pb-2 mb-3 me-5 pe-5',inputId = 'e','run',icon = icon('play')),
                      br(),
                      br(),
                      #chartUpdateModuleUI("demo-chart"),
                      br(),
                      br()
                  )
              )
       ) # End of tab-content-container
                        
                          ), # Close content-area
            
                          # Initialize Packery with Click and Expand JavaScript
                          tags$script(HTML("
$(document).ready(function() {
  // Initialize Packery when the page loads
  setTimeout(function() {
    var $grid = $('.grid').packery({
      itemSelector: '.grid-item',
      columnWidth: 80,
      gutter:10
    });

    // Handle click events on grid items
    $grid.on('doubleclick', '.grid-item-content', function(event) {
      event.preventDefault();
      event.stopPropagation();
      
      var itemElem = event.currentTarget.parentNode;
      var $item = $(itemElem);
      var isExpanded = $item.hasClass('is-expanded');
      
      // Toggle the expanded class
      $item.toggleClass('is-expanded');
      
      // Force a layout update after the CSS transition
      setTimeout(function() {
        if (isExpanded) {
          // If contracting, use shiftLayout to compact everything
          $grid.packery();
        } else {
          // If expanding, first layout normally, then fit the expanded item
          $grid.packery('shiftLayout');
          setTimeout(function() {
            $grid.packery('shiftLayout', itemElem);
          }, 50);
        }
      }, 50); // Small delay to let CSS transition start
      
      // Also trigger layout after CSS transition completes
      setTimeout(function() {
        $grid.packery();
      }, 150); // Match CSS transition duration (0.4s + buffer)
    });
    
    $('.chart-card').on('click', function() {
      console.log('resize');
      // Trigger ECharts resize
      setTimeout(function() {
        $('.echarts4r').each(function() {

          //if (this.echartsInstance) {
          console.log('resizeIn');
          console.log(this);
          echarts.getInstanceByDom(this).resize();
            //this.echartsInstance.resize();
          //}
        });
      }, 300);
    });
    
    // Tab Navigation Functionality
    $('.nav-item').on('click', function() {
      // Remove active class from all nav items
      $('.nav-item').removeClass('active');
      // Add active class to clicked item
      $(this).addClass('active');
      
      // Hide all tab content
      $('.tab-pane').removeClass('active show')
      
      // Show corresponding tab content based on nav item text
      var navText = $(this).text().trim().toLowerCase();
      var tabId = '';
      
      switch(navText) {
        case 'dashboard':
          tabId = 'dashboard-tab';
          break;
        case 'analytics':
          tabId = 'analytics-tab';
          break;
        case 'reports':
          tabId = 'reports-tab';
          break;
        case 'geography':
          tabId = 'geography-tab';
          break;
        case 'deprivation':
          tabId = 'deprivation-tab';
          break;
        case 'population':
          tabId = 'population-tab';
          break;
        case 'intervention':
          tabId = 'intervention-tab';
          break;
        case 'specify':
          tabId = 'specify-tab';
          break;
        case 'scenarios':
          tabId = 'scenarios-tab';
          break;
        case 'obesity risk':
          tabId = 'ObesityRisk-tab';
          break;
        case 'northern trust':
          tabId = 'NorthernTrust-tab';
          break;
        case 'lifestyle':
          tabId = 'lifestyle-tab';
          break;
         case 'society':
          tabId = 'society-tab';
          break;
        default:
          tabId = 'dashboard-tab';
      }
      
      $('#' + tabId).addClass('active show')
      
      // Re-initialize packery if dashboard tab is shown
      if (tabId === 'dashboard-tab') {
        setTimeout(function() {
          $('.grid').packery();
        }, 100);
      }
      
      // Initialize pivot module if reports tab is shown
      if (tabId === 'reports-tab') {
        setTimeout(function() {
          // Trigger pivot module column update
          if (window.Shiny && window.Shiny.setInputValue) {
            window.Shiny.setInputValue('pivot_reports_tab_shown', Math.random());
          }
          
          // Re-trigger any drag-drop initialization if needed
          if (typeof window.setupDropzone === 'function') {
            console.log('Re-initializing pivot drag-drop zones');
            window.setupDropzone('#column-pool');
            window.setupDropzoneCat('#groups-drop');
            window.setupDropzoneCat('#wide-by-drop');
            window.setupDropzone('#values-drop');
            window.setupDropzoneValueFunc('#value-func-drop');
          }
        }, 200);
      }
    });
    
    
    // setTimeout(function() {
    //  $('.tab-pane').removeClass('active show')
    //  $('#' + 'specify-tab').addClass('active show')
    // }, 1000);
     
      setTimeout(function() {
          $('.grid').packery();
        }, 100);
     
  }, 500); // Small delay to ensure DOM is ready
   
});
"
                      ))), # tab pane
                      HTML('<!-- Footer -->
                                 <footer class="dashboard-footer">
                                 <div class="footer-content">
                                 <div class="footer-info">
                                 <p>&copy;  Obesity - Data runs of the Population health Models microsimulation, data are estimates </p>
                                 <p>Data is updated periodically when underlying datasets are released from their respective providers </p>
                                 </div>
                                 <div class="footer-links">
                                 <p> Public Health Agency - Population Health Model 2025  </p>
                                 <!--<a href="#methodology">Methodology</a>
                                 <a href="#privacy">Privacy Policy</a>
                                 <a href="#contact">Contact</a> -->
                                 </div>
                                 </div>
                                </footer>
                                 <style>
                                 .dashboard-footer {
                                 margin-inline:-12px;
                                 width:100vw;
                                   position:relative;
                                   display:fixed;
                                   bottom: 0;
                                   background: #f8f9fa;
                                   padding: 10px 15px;
                                   border-top: 1px solid #e9ecef;
                                   margin-top: 20px;
                                 }
                                 
                                 .footer-content {
                                   display: flex;
                                   justify-content: space-between;
                                   align-items: center;
                                   flex-wrap: wrap;
                                 }
                                 .footer-info p {
                                   margin: 0;
                                   font-size: 0.9em;
                                   color: #6c757d;
                                 }
                                 .footer-links a {
                                   margin-left: 15px;
                                   font-size: 0.9em;
                                   color: #007bff;
                                   text-decoration: none;
                                 }
                                 .footer-links a:hover {
                                   text-decoration: underline;
                                 }
                                 @media (max-width: 600px) {
                                   .footer-content {
                                     flex-direction: column;
                                     align-items: flex-start;
                                   }
                                   .footer-links {
                                     margin-top: 10px;
                                   }
                                   .footer-links a {
                                     margin-left: 0;
                                     margin-right: 15px;
                                   }
                                 }
                                 </style>')

)

# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {

observeEvent(input$`intervention1-draggable_data`,
             {print(input$`intervention1-draggable_data`)}
             )
  observeEvent(input$draggable_data,
               {print(input$draggable_data)}
  )
  # Intervention module server ----
result <- intervention_module_server("intervention1", reactive({runButton()}))

observe({
  print(result)}
)

  # Chart update module server ----
  runButton <- reactiveVal(NULL)

  observeEvent(input$e,{
    runButton(input$e+1)
    })

  # simulation_state <- chartUpdateModuleServer("demo-chart", reactive({runButton()}))

  # observe({
  #   print(simulation_state)
  #   })

  # Reactive data source for pivot module
  population_data <- reactive({
    # df <- read.csv("./populations/test_population.csv", stringsAsFactors = FALSE)
    df <- read.fst('./populations/k20_population.fst')

    # Convert logical health conditions to factors for better pivoting
    health_cols <- c("stroke", "chd", "diabetes", "dementia", "heart_failure",
                     "atrial_fibrillation", "hypertension", "chronic_kidney_disease")
    df[health_cols] <- lapply(df[health_cols], function(x) factor(ifelse(x, "Yes", "No")))

    # Ensure proper factor ordering for age groups
    if("age_risk" %in% names(df)) {
      df$age_risk <- factor(df$age_risk, levels = c("0-15", "16-34", "35-44", "45-54", "55-64", "65-74", "75-110"))
    }

    # Convert BMI to factor with proper ordering
    if("bmi" %in% names(df)) {
      df$bmi <- factor(df$bmi, levels = c("normal", "overweight", "obese"))
    }

    return(df)
  })

  # Pivot module server
  pivot_result <- pivot_module_server("pivot_reports", data = population_data)




  # output$custom <- renderLeaflet({
  # 
  #   leaflet(elementId = 'map1') |>
  #     addTiles() |>
  #     setView(lng = -5.9576, lat = 54.904, zoom = 8)
  # 
  #   })

  output$mymap <- renderLeaflet({

    lrg_leaflet

  })

  map_filtered_chart <- reactiveVal(pop)

  debounced_bounds <- reactive({
    input$mymap_bounds
  }) |>
    debounce(4000)   # 4000 milliseconds = 4 seconds

  # 2. Replace input$mymap_bounds with debounced_bounds() in your observeEvent
  observeEvent(debounced_bounds(), {
    req(debounced_bounds())
    print('printing bounds')
    print(input$mymap_bounds)


    bbox_poly <- st_as_sfc(
      st_bbox(c(
        xmin = input$mymap_bounds$west,
        ymin = input$mymap_bounds$south,
        xmax = input$mymap_bounds$east,
        ymax = input$mymap_bounds$north
      ), crs = 4326)
    )

    req(input$mymap_bounds$west != input$mymap_bounds$west)
    req(input$mymap_bounds$north != input$mymap_bounds$south)
    
    inside_mat <- st_within(csv_pts_wgs84, bbox_poly, sparse = FALSE)
    #st_within(csv_pts_wgs84, x, sparse = FALSE)
    
    csv_pts_wgs84 <- csv_pts_wgs84 |>
      mutate(in_bbox = as.logical(inside_mat[, 1]))

    dz_in_bbox <- csv_pts_wgs84 |>
      filter(in_bbox)

    # dz_in_bbox$DZ2021_code
    # dz_in_bbox$DZ2021_name
    # 
    # print(head(pop))
    # print(head(dz_in_bbox$DZ2021_code))
    # print(head(map_filtered_chart()))
    map_filtered_chart(pop |> 
                         filter(dz_id %in% dz_in_bbox$DZ2021_code ))
  })


  output$group_echarts <- renderEcharts4r({
    map_filtered_chart() |>
      group_by(Urban_status) |>
      summarise(count = n()) |>
      e_charts(Urban_status) |>
      echarts4r::e_tooltip(trigger = "axis",confine =T) |>

      e_bar(count) |>
      e_title("Population by Urban Status") |>
      e_theme('walden')
  })

  
  # --- charts ---------------------------------------------------------------
  output$bmi_chart <- renderEcharts4r({
    dat <-count(map_filtered_chart(),bmi)## bmi_counts(); validate(need(nrow(dat) > 0, "BMI not available."))
    dat |>
      echarts4r::e_charts(bmi,height = '150',width='200',textStyle = list( fontSize=9)) |>
      echarts4r::e_bar(n, name = "Count") |>
      #echarts4r::e_title("BMI distribution (filtered)") |>
      echarts4r::e_tooltip(trigger = "axis",confine =T) |>
      e_legend(show=F) |>
      e_color(c('#2AFEB7','yellow')) |>

      #echarts4r::e_x_axis(name = "BMI band") |>
      echarts4r::e_y_axis(name = "People") |>
      echarts4r::e_grid(top = 40, right = 20, bottom = 40, left = 50)
  })

  output$age_chart <- renderEcharts4r({
    dat <- count(map_filtered_chart(),age20)##age_counts(); validate(need(nrow(dat) > 0, "Age not available."))
    dat |>
      echarts4r::e_charts(age20,height = '150',width='200') |>
      echarts4r::e_bar(n, name = "Count") |>
      #echarts4r::e_title("Age bands (filtered)") |>
      echarts4r::e_tooltip(trigger = "axis",confine =T) |>
      e_legend(show=F) |>
      #echarts4r::e_x_axis(name = "Age band") |>
      echarts4r::e_y_axis(name = "People") |>
      e_legend(show=F) |>
      e_theme('walden') |>
      #e_color(c('#2AFEB7','yellow')) |>
      echarts4r::e_grid(top = 40, right = 20, bottom = 40, left = 50)
  })

  output$sex_chart <- renderEcharts4r({
    dat <- count(map_filtered_chart(),sex)#sex_counts(); validate(need(nrow(dat) > 0, "Sex not available."))
    dat |>
      group_by(sex) |>
      echarts4r::e_charts(sex,height = '150',width='200') |>
      echarts4r::e_bar(n) |>
      e_legend(show=F) |>
      #echarts4r::e_title("Sex split (filtered)", textStyle = list( fontSize=9)) |>
      echarts4r::e_tooltip(formatter = "{b}: {c} ({d}%)",confine = T) |>
      #e_color(c('#2AFEB7','yellow')) |>
      echarts4r::e_grid(top = 40, right = 20, bottom = 40, left = 50) |>
      e_text_style(
        #color = "white",
        #fontStyle = "italic"
        textStyle = list(fontSize = 9)
      ) |>
      e_theme('roma')
  })

  output$depriv_chart <- renderEcharts4r({
    dat <- count(map_filtered_chart(), mdm_quintile_soa_name)#depriv_counts(); validate(need(nrow(dat) > 0, "Deprivation not available."))
    # Pick the x column dynamically
   # xcol <- if ("mdm_quintile_soa_name" %in% names(dat)) "mdm_quintile_soa_name" else "mdm_quintile_soa"

    dat |>
      mutate(mdm_quintile_soa_name = factor(mdm_quintile_soa_name,
                                            levels = c("Most Deprived","Quintile 2","Quintile 3","Quintile 4","Least Deprived"))) |>
      arrange(mdm_quintile_soa_name) |>
      echarts4r::e_charts(mdm_quintile_soa_name,height = '150',width='200') |>
      echarts4r::e_bar(n, name = "Count") |>
      e_legend(show = F) |>
      #echarts4r::e_title("Deprivation quintile (filtered)",textStyle = list( fontSize=9)) |>
      #echarts4r::e_tooltip(trigger = "axis") |>
      echarts4r::e_tooltip(trigger = "axis",confine =T) |>
      # e_color(c('#2AFEB7','yellow')) |>

      #echarts4r::e_x_axis(name = "MDM quintile") |>
      echarts4r::e_y_axis(name = "People") |>
      echarts4r::e_grid(top = 40, right = 20, bottom = 40, left = 50)
  })

  output$qrisk_chart <- renderEcharts4r({
      dat <- map_filtered_chart() |> 
        slice_sample(n = 500)

      y_max <- ceiling(max(dat$qrisk_score, na.rm = TRUE))
      y_max <- max(y_max, 1)  # ensure sensible upper bound

      dat |>
        echarts4r::e_charts(id,height = '150',width='200') |>
        # points (strip/list)
        echarts4r::e_scatter(qrisk_percentile,
                             name = "QRisk",
                             symbolSize = 6,
                             large = TRUE,
                             largeThreshold = 2000,
                             itemStyle=list(opacity=0.2)
        ) |>
        # mean & median reference lines
        echarts4r::e_mark_line(data = list(
          list(type = "average", name = "Mean"),
          list(type = "median", name = "Median")
        )) |>
        # axes & layout
        echarts4r::e_y_axis(
          name = "QRisk (%)",
          min = 0, max = y_max,
          axisLabel = list(formatter = "{value}%")
        ) |>
        echarts4r::e_x_axis(show = FALSE) |>
        echarts4r::e_grid(top = 40, right = 16, bottom = 40, left = 60) |>
        e_theme('walden')

  })


  output$qrisk_chart1 <- renderEcharts4r({
    dat <- map_filtered_chart()|>
      slice_sample(n = 500)

    dat |>
      filter(age>25) |>
      filter(!is.na(bmi)) |>
      group_by(bmi) |>
      e_charts(height=290) |>
      e_density(qrisk_percentile,breaks=5) |>

      e_mark_line(title = 'Baseline',
                  data = list(
                    type = "average",
                    name = "Average"
                  )) |>
      e_theme('walden')

  })


  # --- headline card (optional) --------------------------------------------
  output$headline_count <- renderText({

    format(nrow(map_filtered_chart())*10, big.mark = ",")
  })


  output$qrisk_average <- renderText({
    signif(digits = 2,mean(map_filtered_chart()$qrisk_percentile))
  })

}


shinyApp(ui = ui, server = server)
