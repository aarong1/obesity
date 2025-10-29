# ============================================================================
# PACKERY DASHBOARD APP - CLICK AND EXPAND VERSION
# ============================================================================
# Interactive dashboard with click-to-expand widgets, metrics, and charts

library(shiny)
library(echarts4r)
library(bslib)
library(leaflet)
library(reactable)

# source('modules/pivot_module/pivottable.R')
source('modules/pivot_module/pivottable_module.R')


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

ui <- page_fluid( id = 'main-content',
                  theme = bs_theme(version = 5, font_scale = 0.8,
                                   bootswatch = 'litera',
                                   primary = '#2196F3'),
                  #titlePanel("Packery - Click and Expand Demo"),
                  
                  # Include external dependencies
                  tags$head(
                      # Packery CSS and JS from CDN (no Draggabilly needed)
                      tags$script(src = "https://unpkg.com/packery@2/dist/packery.pkgd.min.js"),
                      HTML('<script>
                        window.FontAwesomeConfig = {
                          searchPseudoElements: true
                        }
                      </script>'),
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
  padding: 5px 10px;
  cursor: pointer;
  transition: all 0.3s ease;
  border-left: 3px solid transparent;
}

.nav-item:hover {
  background: rgba(52, 152, 219, 0.1);
  border-radius:10px;

}

.nav_section.nav-item.active {
  background: rgba(52, 152, 219, 0.5);
  text:white;
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
  padding: 20px;
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

.grid-item--width2 { width: 200px; }
.grid-item--height2 { height: 200px; }
.grid-item--small { width: 200px; height: 200px; }
.grid-item--graph { width: 500px; height: 500px; }

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
  margin-right: 0px;
  margin-bottom: 0px;
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
  width: 150px;
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
  font-size: 2.5em;
  font-weight: bold;
  margin-bottom: 10px;
}

.metric-label {
  font-size: 1.1em;
  color: #6c757d;
  margin-bottom: 15px;
}

.metric-change {
  font-size: 1em;
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
  box-shadow: 0 1px 3px rgba(255,255,255,0.3);
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
                   shiny::h4(  "Population Health ",
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
                 
                

                 div(style='top:15vh;width:15vw;z-index:1000',class = ' ms-3 p-4 d-flex flex-column display-absolute position-fixed left-0 shadow-sm glass-card',
                     # div(id='myTab', class = "nav-section",
                     #     h6(class = "nav-section-title", "Obesity"),
                     #     div(class = "nav-item active", `data-section`="overview", `data-bs-toggle`="tab", `data-bs-target`="#overview",
                     #         "Settings"
                     #     ),
                     #     div(class = "nav-item", `data-section`="geography", `data-bs-toggle`="tab", `data-bs-target`="#geography",
                     #         span(class = "nav-icon" ),
                     #         "Support"
                     #     )
                     #     
                     # ),
                     
                     div(class = "nav-section",
                         h6(class = "nav-section-title", "Health Burden"),
                         div(class = "nav-item active",
                             span(class = "nav-icon"),
                             "Dashboard"
                         ),
                         div(class = "nav-item",
                             span(class = "nav-icon"),
                             "Analytics"
                         ),
                         div(class = "nav-item",
                             span(class = "nav-icon"),
                             "Reports"
                         )
                     ),
                     
                     div(class = "nav-section",
                         h6(class = "nav-section-title", "OPIP"),
                         div(class = "nav-item",
                             span(class = "nav-icon"),
                             "Population"
                         ),
                         div(class = "nav-item",
                             span(class = "nav-icon"),
                             "Geography"
                         ),
                         div(class = "nav-item",
                             span(class = "nav-icon"),
                             "Deprivation"
                         )
                     ),
                     div(class = "nav-section",
                         h6(class = "nav-section-title", "Intervention"),
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
                            
              div(id = "dashboard-tab", class = "tab-pane   show active",
                          fluidRow(
                              column(2, offset = 0,
                                   
                              ),
                              # column(2,offset=0,
                              #        div(class = "nav-card analytics bg-info",
                              #            div(class = "nav-card-icon",  icon("arrow-up-right-from-square")),
                              #            div(class = "nav-card-title", "Analytics"),
                              #            div(class = "nav-card-description", "View detailed analytics")
                              #        ),
                              #        
                              #        div(class = "nav-card analytics",
                              #            div(class = "nav-card-icon",  icon("arrow-up-right-from-square")),
                              #            div(class = "nav-card-title", "Map"),
                              #            div(class = "nav-card-description", "View detailed analytics")
                              #        ),
                              #        
                              #        div(class = "nav-card analytics bg-opacity-50",
                              #            div(class = "nav-card-icon",  icon("arrow-up-right-from-square")),
                              #            div(class = "nav-card-title", "Morbidity"),
                              #            div(class = "nav-card-description", "View detailed analytics")
                              #        )
                              # ),
                              
                              column(8,offset=0,
                                     # Tab Content Areas (controlled by left navigation)
                                    # div(id = "tab-content-container",
                                         
                                         # Dashboard Tab Content (default active)
                                             div(id = "grid", class = "grid",
                                         
                                         # Revenue Metric Card
                                         div(class = "grid-item grid-item--small theme-green",
                                             div(class = "grid-item-content",
                                                 div(class = "metric-card",
                                                     tags$i(class = "fas fa-dollar-sign fa-2x mb-3", style = "color: #28a745;"),
                                                     div(class = "metric-value", style = "color: #28a745;", "$127.5K"),
                                                     div(class = "metric-label", "Monthly Revenue"),
                                                     div(class = "metric-change positive", 
                                                         tags$i(class = "fas fa-arrow-up me-1"), "12.5%"
                                                     )
                                                 )
                                             )
                                         ),
                                         div(style = 'border:10px 0px 0px 0px solid black;'),
                                         
                                         # Users Metric Card  
                                         div(class = "grid-item grid-item--small theme-blue",
                                             div(class = "grid-item-content",
                                                 div(class = "metric-card",
                                                     tags$i(class = "fas fa-users fa-2x mb-3", style = "color: #007bff;"),
                                                     div(class = "metric-value", style = "color: #007bff;", "8,421"),
                                                     div(class = "metric-label", "Active Users"),
                                                     div(class = "metric-change positive",
                                                         tags$i(class = "fas fa-arrow-up me-1"), "8.2%"
                                                     )
                                                 )
                                             )
                                         ),
                                         
                                         # Conversion Rate
                                         div(class = "grid-item grid-item--small theme-orange",
                                             div(class = "grid-item-content",
                                                 div(class = "metric-card",
                                                     tags$i(class = "fas fa-chart-line fa-2x mb-3", style = "color: #fd7e14;"),
                                                     div(class = "metric-value", style = "color: #fd7e14;", "3.2%"),
                                                     div(class = "metric-label", "Conversion Rate"),
                                                     div(class = "metric-change negative",
                                                         tags$i(class = "fas fa-arrow-down me-1"), "1.1%"
                                                     )
                                                 )
                                             )
                                         ),
                                         
                                         # Sales Performance Chart (Wide)
                                         div(class = "grid-item grid-item--graph theme-blue",
                                             div(class = "grid-item-content",
                                                 div(class = "chart-card",
                                                     div(class = "card-header",
                                                         "Sales Performance Trend"
                                                     ),
                                                     sales_chart
                                                 )
                                             )
                                         ),
                                         # Revenue Analysis Chart (Tall)
                                         div(class = "grid-item grid-item--graph theme-red",
                                             div(class = "grid-item-content",
                                                 div(class = "chart-card",
                                                     div(class = "card-header",
                                                         tags$i(class = "fas fa-chart-scatter me-2"),
                                                         "Revenue vs Growth Analysis"
                                                     ),
                                                     revenue_chart
                                                 )
                                             )
                                         ),
                                         
                                         # Performance Chart (Small)
                                         div(class = "grid-item grid-item--graph theme-green",
                                             div(class = "grid-item-content",
                                                 div(class = "chart-card",
                                                     div(class = "card-header", style = "font-size: 0.9em;",
                                                         "Quarterly"
                                                     ),
                                                     performance_chart
                                                 )
                                             )
                                         ),
                                         
                                         # Orders Metric
                                         div(class = "grid-item grid-item--small theme-purple",
                                             div(class = "grid-item-content",
                                                 div(class = "metric-card",
                                                     tags$i(class = "fas fa-shopping-cart fa-2x mb-3", style = "color: #6f42c1;"),
                                                     div(class = "metric-value", style = "color: #6f42c1;", "1,249"),
                                                     div(class = "metric-label", "Orders Today"),
                                                     div(class = "metric-change positive",
                                                         tags$i(class = "fas fa-arrow-up me-1"), "15.3%"
                                                     )
                                                 )
                                             )
                                         ),
                                         
                                         # Page Views
                                         div(class = "grid-item grid-item--small theme-teal",
                                             div(class = "grid-item-content",
                                                 div(class = "metric-card",
                                                     tags$i(class = "fas fa-eye fa-2x mb-3", style = "color: #20c997;"),
                                                     div(class = "metric-value", style = "color: #20c997;", "45.2K"),
                                                     div(class = "metric-label", "Page Views"),
                                                     div(class = "metric-change positive",
                                                         tags$i(class = "fas fa-arrow-up me-1"), "6.8%"
                                                     )
                                                 )
                                             )
                                         ),
                                         
                                         # Bounce Rate
                                         div(class = "grid-item grid-item--small theme-red",
                                             div(class = "grid-item-content",
                                                 div(class = "metric-card",
                                                     tags$i(class = "fas fa-external-link-alt fa-2x mb-3", style = "color: #dc3545;"),
                                                     div(class = "metric-value", style = "color: #dc3545;", "28.4%"),
                                                     div(class = "metric-label", "Bounce Rate"),
                                                     div(class = "metric-change negative",
                                                         tags$i(class = "fas fa-arrow-down me-1"), "3.2%"
                                                     )
                                                 )
                                             )
                                         ),
                                         div(class = "grid-item grid-item--small theme-green",
                                             
                                         echarts4rOutput('sales')),
                                         div(class = "grid-item grid-item--small theme-green",
                                             
                                             
                                              ( leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
                                                 htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)}") |> 
                                                 addTiles() |> 
                                                 setView(lng = -5.9576, lat = 54.904, zoom = 7) |> 
                                                 addMarkers(lng = -0.1276, lat = 51.5074, popup = "London"))
                                         
                                         ),
                                         # Customer Satisfaction
                                         div(class = "grid-item grid-item--small theme-green",
                                             div(class = "grid-item-content",
                                                 div(class = "metric-card",
                                                     tags$i(class = "fas fa-smile fa-2x mb-3", style = "color: #28a745;"),
                                                     div(class = "metric-value", style = "color: #28a745;", "94.7%"),
                                                     div(class = "metric-label", "Customer Satisfaction"),
                                                     div(class = "metric-change positive",
                                                         tags$i(class = "fas fa-arrow-up me-1"), "2.1%"
                                                     )
                                                 )
                                             )
                                         ),
                                         
                                         # Load Time
                                         div(class = "grid-item grid-item--small theme-orange",
                                             div(class = "grid-item-content",
                                                 div(class = "metric-card",
                                                     tags$i(class = "fas fa-stopwatch fa-2x mb-3", style = "color: #fd7e14;"),
                                                     div(class = "metric-value", style = "color: #fd7e14;", "1.8s"),
                                                     div(class = "metric-label", "Avg Load Time"),
                                                     div(class = "metric-change positive",
                                                         tags$i(class = "fas fa-arrow-up me-1"), "0.3s faster"
                                                     )
                                                 )
                                             )
                                         ),
                                         
                                         div(class = "grid-item grid-item--small theme-orange",
                                             div(class = "grid-item-content",
                                                 div(class = "metric-card",
                                                     tags$i(class = "fas fa-stopwatch fa-2x mb-3", style = "color: #fd7e14;"),
                                                     div(class = "metric-value", style = "color: #fd7e14;", "1.8s"),
                                                     div(class = "metric-label", "Morbidity "),
                                                     div(class = "metric-change positive",
                                                         tags$i(class = "fas fa-arrow-up-right-from-square me-1"), "0.3s faster"
                                                     )
                                                 )
                                             )
                                         ),
                                         div(class = "grid-item grid-item--small theme-orange",
                                             div(class = "grid-item-content",
                                                 div(class = "metric-card",
                                                     tags$i(class = "fas fa-stopwatch fa-2x mb-3", style = "color: #fd7e14;"),
                                                     div(class = "metric-value", style = "color: #fd7e14;", "1.8s"),
                                                     div(class = "metric-label", "Maps"),
                                                     div(class = "metric-change positive",
                                                         tags$i(class = "fas fa-arrow-up-right-from-square me-1"), "0.3s faster"
                                                     )
                                                 )
                                             )
                                         ),
                                         div(class = "grid-item grid-item--small theme-orange",
                                             div(class = "grid-item-content",
                                                 div(class = "metric-card",
                                                     tags$i(class = "fas fa-stopwatch fa-2x mb-3", style = "color: #fd7e14;"),
                                                     div(class = "metric-value", style = "color: #fd7e14;", "1.8s"),
                                                     div(class = "metric-label", "Population"),
                                                     div(class = "metric-change positive",
                                                         tags$i(class = "fas fa-arrow-up-right-from-square me-1"), "0.3s faster"
                                                     )
                                                 )
                                             )
                                         )
                                     ) # End of dashboard grid
                                 ), # End of dashboard tab content
                              column(2,
                                     div(style='top:15vh;right:1vw;width:15vw;',class = ' ms-3 p-4 d-flex flex-column',#  display-absolute position-fixed shadow-sm glass-card
                                         
                                         div(class = "nav-card analytics bg-info",
                                             div(class = "nav-card-icon",  icon("arrow-up-right-from-square")),
                                             div(class = "nav-card-title", "Analytics"),
                                             div(class = "nav-card-description", "View detailed analytics")
                                         ),
                                         
                                         div(class = "nav-card analytics",
                                             div(class = "nav-card-icon",  icon("arrow-up-right-from-square")),
                                             div(class = "nav-card-title", "Map"),
                                             div(class = "nav-card-description", "View detailed analytics")
                                         ),
                                         
                                         div(class = "nav-card analytics bg-opacity-50",
                                             div(class = "nav-card-icon",  icon("arrow-up-right-from-square")),
                                             div(class = "nav-card-title", "Morbidity"),
                                             div(class = "nav-card-description", "View detailed analytics")
                                         )
                                     ))
                          ) # End of fluidRow
                                 ),
                                 
                                 # Analytics Tab Content
                                 div(id = "analytics-tab", class = "tab-pane active show", style = "width:100%",
                                     div(style = 'position:absolute;top:0;left:0;margin-inline:-12px;',#class = "container-fluid",
                                        
                                         leafletOutput("mymap", width = '100vw', height = '100vh'),
                                           
                                         #      (leaflet(width='100vw',height='100vh') |> 
                                         #     addTiles() |> 
                                         #     setView(lng = -0.1276, lat = 51.5074, zoom = 10) |> 
                                         #     addMarkers(lng = -5.1276, lat = 55.5074, popup = "NI")
                                          ),
                                         h3("Analytics Dashboard"),
                                         p("Reports content will be displayed here when the Reports nav item is clicked."),
                                           
                                     #)
                                 ),
                                 
                                 # Reports Tab Content  
                                 div(id = "reports-tab", class = " tab-pane ",# style = "display: none;",
                                     div(class = "container-fluid", style = "padding-left: 20%;",
                                         h3("Population Health Data - Interactive Pivot Analysis"),
                                         p("Drag columns to create custom analysis. Use dimensions for grouping, measures for aggregation."),
                                         div(style = "font-size: 0.6rem !important;",
                                         # Pivot module UI
                                         pivot_module_ui(id = "pivot_reports", 
                                                        data_names = c("sex", "age_risk", "county", "hsct", "bmi", 
                                                                      "Urban_status", "mdm_quintile_soa_name", "ethnicity",
                                                                      "stroke", "chd", "diabetes", "dementia", "heart_failure"),
                                                        fun_names = c("sum","mean","median","min","max","count","n_distinct")),
                                         
                                         # Results section
                                         div(style = "margin-top: 30px;",
                                             h4("Pivot Analysis Results"),
                                             div(id = "pivot_reports-table_container",
                                                 reactableOutput("pivot_reports-table")
                                             ),
                                             )
                                         )
                                     )
                                 ),
                                 
                                 # Users Tab Content
                                 div(id = "deprivation-tab", class = "  tab-pane  ", style = " ",
                                     div(class = "container-fluid",
                                         h3("Deprivation Dashboard"),
                                         
                                         p("Deprivation content will be displayed here when the Users nav item is clicked.")
                                     )
                                 ),
                                 
                                 # Revenue Tab Content
                                 div(id = "population-tab", class = "tab-pane", style = " ",
                                     div(class = "container-fluid",
                                         h3(" population"),
                                         p("population analysis content will be displayed here when the Revenue nav item is clicked.")
                                     )
                                 ),
                                 
                                 # Orders Tab Content
                                 div(id = "geography-tab", class = "tab-pane ", style = " ",
                                     div(class = "container-fluid",
                                         h3("geography Overview"),
                                         p("geography overview content will be displayed here when the Orders nav item is clicked.")
                                     )
                                 )
                                 
                             ) # End of tab-content-container
                              #),
                              #                              column(2,offset=0,
                              #        div(class = "nav-card analytics bg-info",
                              #            div(class = "nav-card-icon",  icon("arrow-up-right-from-square")),
                              #            div(class = "nav-card-title", "Analytics"),
                              #            div(class = "nav-card-description", "View detailed analytics")
                              #        ),
                              #        
                              #        div(class = "nav-card analytics",
                              #            div(class = "nav-card-icon",  icon("arrow-up-right-from-square")),
                              #            div(class = "nav-card-title", "Map"),
                              #            div(class = "nav-card-description", "View detailed analytics")
                              #        ),
                              #        
                              #        div(class = "nav-card analytics bg-opacity-50",
                              #            div(class = "nav-card-icon",  icon("arrow-up-right-from-square")),
                              #            div(class = "nav-card-title", "Morbidity"),
                              #            div(class = "nav-card-description", "View detailed analytics")
                              #        )
                              # )

                          ), # Close content-area
            
                          # Initialize Packery with Click and Expand JavaScript
                          tags$script(HTML("
$(document).ready(function() {
  // Initialize Packery when the page loads
  setTimeout(function() {
    var $grid = $('.grid').packery({
      itemSelector: '.grid-item',
      columnWidth: 100
    });

    // Handle click events on grid items
    $grid.on('click', '.grid-item-content', function(event) {
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
          $grid.packery('shiftLayout');
        } else {
          // If expanding, first layout normally, then fit the expanded item
          $grid.packery();
          setTimeout(function() {
            $grid.packery('fit', itemElem);
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
    
     $('.tab-pane').removeClass('active show')
     $('#' + 'dashboard-tab').addClass('active show')
     
  }, 500); // Small delay to ensure DOM is ready
   
});
"
                          
                      ))), # tab pane
                      HTML('<!-- Footer -->
                                 <footer class="dashboard-footer">
                                 <div class="footer-content">
                                 <div class="footer-info">
                                 <p>&copy;  Obesity - Data runs of the Population health Models microsimulation, data are estimates. 
                                 Data is updated periodically when underlying datasets are released from their respective providers </p>
                                 </div>
                                 <div class="footer-links">
                                 <p> PHM - Population Health Model 2025  </p>
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
  
  # Reactive data source for pivot module
  population_data <- reactive({
    df <- read.csv("test_population.csv", stringsAsFactors = FALSE)
    
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
    
  output$`pivot_reports-table` <- renderReactable({
    print(pivot_result())
    pivot_result()
  })
    
    
    
  output$sales <- renderEcharts4r({
    sales_chart
  })
  
  
  output$mymap <- renderLeaflet({
    leaflet(width='100vw',height='100vh',options = leafletOptions(zoomControl = FALSE)) %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this)}") |> 
      addTiles() |> 
      setView(lng = -5.9576, lat = 54.904, zoom = 8) |> 
      addMarkers(lng = -0.1276, lat = 51.5074, popup = "London")
  })

}

# ============================================================================
# RUN APP
# ============================================================================
shinyApp(ui = ui, server = server)