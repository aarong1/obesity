# ============================================================================
# PACKERY DASHBOARD APP
# ============================================================================
# Interactive dashboard with draggable widgets, metrics, and charts

library(shiny)
library(echarts4r)
library(bslib)

# Create sample ECharts graphs
sales_chart <- mtcars |> 
    e_charts(mpg, height = 450, width = 450) |> 
    e_line(cyl,legend = F, smooth = TRUE, name = "Sales Trend") |>
    e_color("#3498db") |>
    e_theme("walden") |>
    e_tooltip(trigger = "axis") |>
    e_title("Sales Performance", left = "center", textStyle = list(fontSize = 14))

revenue_chart <- mtcars |> 
    e_charts(wt, height = 450, width = 450) |> 
    e_scatter(hp, qsec,legend = F, scale = e_scale, name = "Revenue vs Growth") |>
    e_color("#e74c3c") |>
    e_theme("walden") |>
    e_tooltip() |>
    e_title("Revenue Analysis", left = "center", textStyle = list(fontSize = 14))

performance_chart <- data.frame(
    category = c("Q1", "Q2", "Q3", "Q4"),
    value = c(120, 200, 150, 300)
) |>
    e_charts(category, height = 450, width = 450) |>
    e_bar(value, name = "Performance",legend = F) |>
    e_color("#27ae60") |>
    e_theme("walden") |>
    e_tooltip(trigger = "axis") |>
    e_title("Quarterly", left = "center", textStyle = list(fontSize = 12))

ui <- page_fluid( id = 'main-content',
                  theme = bs_theme(version = 5, font_scale = 0.8,
                                   bootswatch = 'litera',
                                   primary = '#2196F3'),
                  #titlePanel("Packery - Draggabilly Demo"),
                  
                  # Include external dependencies
                  tags$head(
                      # Packery CSS and JS from CDN
                      tags$script(src = "https://unpkg.com/packery@2/dist/packery.pkgd.min.js"),
                      tags$script(src = "https://unpkg.com/draggabilly@3/dist/draggabilly.pkgd.min.js"),
                      
                      # Custom CSS styling
                      tags$style("
* { box-sizing: border-box; } 

body { 
  font-family: sans-serif; 
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
  padding: 10px 20px;
  font-size: 0.9em;
  color: #bdc3c7;
  text-transform: uppercase;
  letter-spacing: 1px;
  margin: 0;
}

.nav-item {
  padding: 12px 10px;
    width:80%;
  cursor: pointer;
  transition: all 0.3s ease;
  border-left: 3px solid transparent;
}

.nav-item:hover {
  background: rgba(52, 152, 219, 0.1);
  border-radius:10px;

}

.nav-item.active {
  background: rgba(52, 152, 219, 0.5);
  text:white;
  border-radius:10px;

}
/*
  border-left-color: grey;
  border-left-color: #3498db;
*/

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



/* ---- grid ---- */
.grid {
  padding: 10px;
  border-radius: 40px;
  max-width: 70vw;
  min-height: 100vh;
}

/* clear fix */
.grid:after {
  content: '';
  display: block;
  clear: both;
}

/* ---- .grid-item ---- */

.grid-item {
padding:20px;
border-radius:20px;
  float: left;
  /*width: 300px;
  height: 300px;*/
  background: white;
  box-shadow: 0 1px 3px rgba(100, 0, 0, 0.01);
  border: 1px solid hsla(0, 0%, 100%, 0.5);
  transform: translate(-1px, -1px);
  transition: box-shadow 0.2s;
  
} 

.grid-item--width2 { width: 200px; }
.grid-item--height2 { height: 200px; }
.grid-item--small { width: 200px; height: 200px; }
.grid-item--graph { width: 500px; height: 500px; }


.grid-item:hover {
  transform: translate(2px, 2px);
  box-shadow: 0 1px 3px rgba(100, 0, 0, 0.1);  cursor: move;
}

.grid-item.is-dragging,
.grid-item.is-positioning-post-drag {
  background: #3333;
  z-index: 2;
}

.packery-drop-placeholder {
  outline: 3px dashed hsla(0, 0%, 0%, 0.5);
  border-radius:15px;
  outline-offset: -6px;
  -webkit-transition: -webkit-transform 0.2s;
          transition: transform 0.2s;
}

/* Metric Cards */
.metric-card {
  margin:auto;
  text-align: center;
  height: 100%;
  /*
  */
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
padding:15px;
  height: 450px;
  width: 450px;
  display: flex;
  flex-direction: column;
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
  /*height: 150px;
  width: 150px;*/
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
  /*width: 240px;
  height: 360px;*/
  background: rgba(255, 255, 255, 0.14);
  backdrop-filter: blur(5px);
  -webkit-backdrop-filter: blur(5px);
  border-radius: 20px;
  border: 1px solid rgba(255, 255, 255, 0.3);
  box-shadow: 
    0 8px 32px rgba(0, 0, 0, 0.1),
    inset 0 1px 0 rgba(255, 255, 255, 0.5),
    inset 0 -1px 0 rgba(255, 255, 255, 0.1),
    inset 0 0 10px 5px rgba(255, 255, 255, 0.5);
  /*position: relative;*/
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
                  shiny::tags$nav(
                      
                      class = "navbar fixed-top shadow-lg glass-card mb-3 p-2 rounded", # bg-light
                    #   HTML("<h7> <span class='badge bg-dark m-2 p-2 me-5 position-relative'> 
                    #   <img src = 'inverse_logo.png',  width = '200px'/>
                    # <span class='fs-6 position-absolute top-10 start-90 translate-middle badge rounded-pill bg-info'>
                    # PHA</span>
                    #  </span>
                    #        </h7>"),
                      shiny::h4(  "                Population Health ",
                                span(HTML('<h7><span class="badge rounded-pill text-white bg-opacity-75 bg-primary">Obesity', '' ,'</span></h7>')),
                                    
                                  HTML('<h7><span class="badge rounded-pill bg-opacity-75 bg-warning ">SPPG </span></h7>'),
                                  HTML('<h7><span class="badge rounded-pill bg-opacity-75 bg-info ">PHA </span></h7>'),
                                 #h4(class = 'float-middle', "                Obesity "),
                                  # last_run_ui("pivot_run"),
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
                  
                  # tags$nav(class = "navbar navbar-light fixed-top glass-card mb-3 p-2 rounded",
                  #     div(class = "row align-items-center",
                  #         div(class = "col-md-8",
                  #             h4(class = "mb-0", 
                  #                " Obesity Dashboard, in support of the Obesity Pathway Innovation programme Bid"
                  #             )
                  #         ),
                  #         div(class = "col-md-4 text-end",
                  #             span(class = "badge pill-rounded bg-info", "Live Data"),
                  #             span(class = "text-muted ms-3", "Last updated: ", format(Sys.time(), "%H:%M"))
                  #         )
                  #     )
                  # ),
                  div(class='mt-5 pt-5'),
                  div(class = "main-container",
                      
                      # Fixed Left Sidebar
                      
                      
                      
                      
                      
                      # Main content area
                      div(class = "content-area",
                          
                          fluidRow(
                              column(2, offset = 0,
                                     div(class = 'd-flex flex-column',
                                         div(class = "nav-section",
                                             h6(class = "nav-section-title", "Obesity"),
                                             div(class = "nav-item",
                                                 span(class = "nav-icon"),
                                                 "Settings"
                                             ),
                                             div(class = "nav-item",
                                                 span(class = "nav-icon" ),
                                                 "Support"
                                             )
                                             
                                         ),
                                         
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
                                                 "Users"
                                             ),
                                             div(class = "nav-item",
                                                 span(class = "nav-icon"),
                                                 "Revenue"
                                             ),
                                             div(class = "nav-item",
                                                 span(class = "nav-icon"),
                                                 "Orders"
                                             )
                                         )
                                         
                                     )
                              ),
                              
                              column(8,offset=0,
                                     # Main Dashboard Grid
                                     div(id = "grid", class = "grid",
                                         
                                         # Revenue Metric Card
                                         div(class = "grid-item theme-green",
                                             div(class = "metric-card",
                                                 tags$i(class = "fas fa-dollar-sign fa-2x mb-3", style = "color: #28a745;"),
                                                 div(class = "metric-value", style = "color: #28a745;", "$127.5K"),
                                                 div(class = "metric-label", "Monthly Revenue"),
                                                 div(class = "metric-change positive", 
                                                     tags$i(class = "fas fa-arrow-up me-1"), "12.5%"
                                                 )
                                             )
                                         ),
                                         div(style = 'border:10px 0px 0px 0px solid black;'),
                                         
                                         # Users Metric Card  
                                         div(class = "grid-item theme-blue",
                                             div(class = "metric-card",
                                                 tags$i(class = "fas fa-users fa-2x mb-3", style = "color: #007bff;"),
                                                 div(class = "metric-value", style = "color: #007bff;", "8,421"),
                                                 div(class = "metric-label", "Active Users"),
                                                 div(class = "metric-change positive",
                                                     tags$i(class = "fas fa-arrow-up me-1"), "8.2%"
                                                 )
                                             )
                                         ),
                                         
                                         # Conversion Rate
                                         div(class = "grid-item  theme-orange",
                                                 tags$i(class=" fas fa-solid text-secondary float-left fa-grip-vertical"),
                                             div(class = "metric-card",
                                                 
                                                 tags$i(class = "fas fa-chart-line fa-2x mb-3", style = "color: #fd7e14;"),
                                                 div(class = "metric-value", style = "color: #fd7e14;", "3.2%"),
                                                 div(class = "metric-label", "Conversion Rate"),
                                                 div(class = "metric-change negative",
                                                     tags$i(class = "fas fa-arrow-down me-1"), "1.1%"
                                                 )
                                             )
                                         ),
                                         
                                         # Sales Performance Chart (Wide)
                                         div(class = "grid-item grid-item--graph theme-blue",
                                             div(class = "chart-card",
                                                 div(class = "card-header",
                                                     tags$i(class=" fas text-secondary me-2 fa-solid fa-grip-vertical"),
                                                     
                                                     "Sales Performance Trend",
                                                     # tags$i(class = "ms-2  fas fa-chart-line me-2")
                                                 ),
                                                 sales_chart
                                             )
                                         ),
                                         # Revenue Analysis Chart (Tall)
                                         div(class = "grid-item grid-item--graph theme-red",
                                             div(class = "chart-card",
                                                 div(class = "card-header",
                                                     tags$i(class = "fas fa-chart-scatter me-2"),
                                                     "Revenue vs Growth Analysis"
                                                 ),
                                                 revenue_chart
                                             )
                                         ),
                                         
                                         # Performance Chart (Small)
                                         div(class = "grid-item grid-item--graph theme-green",
                                             div(class = "chart-card",
                                                 div(class = "card-header", style = "font-size: 0.9em;",
                                                     "Quarterly"
                                                 ),
                                                 performance_chart
                                             )
                                         ),
                                         
                                         # Orders Metric
                                         div(class = "grid-item theme-purple",
                                             div(class = "metric-card",
                                                 tags$i(class = "fas fa-shopping-cart fa-2x mb-3", style = "color: #6f42c1;"),
                                                 div(class = "metric-value", style = "color: #6f42c1;", "1,249"),
                                                 div(class = "metric-label", "Orders Today"),
                                                 div(class = "metric-change positive",
                                                     tags$i(class = "fas fa-arrow-up me-1"), "15.3%"
                                                 )
                                             )
                                         ),
                                         
                                         # Page Views
                                         div(class = "grid-item  theme-teal",
                                             div(class = "metric-card",
                                                 tags$i(class = "fas fa-eye fa-2x mb-3", style = "color: #20c997;"),
                                                 div(class = "metric-value", style = "color: #20c997;", "45.2K"),
                                                 div(class = "metric-label", "Page Views"),
                                                 div(class = "metric-change positive",
                                                     tags$i(class = "fas fa-arrow-up me-1"), "6.8%"
                                                 )
                                             )
                                         ),
                                         
                                     
                                         
                                         # Bounce Rate
                                         div(class = "grid-item theme-red",
                                             div(class = "metric-card",
                                                 tags$i(class = "fas fa-external-link-alt fa-2x mb-3", style = "color: #dc3545;"),
                                                 div(class = "metric-value", style = "color: #dc3545;", "28.4%"),
                                                 div(class = "metric-label", "Bounce Rate"),
                                                 div(class = "metric-change negative",
                                                     tags$i(class = "fas fa-arrow-down me-1"), "3.2%"
                                                 )
                                             )
                                         ),
                                         
                                     
                                         
                                         # Customer Satisfaction
                                         div(class = "grid-item theme-green",
                                             div(class = "metric-card",
                                                 tags$i(class = "fas fa-smile fa-2x mb-3", style = "color: #28a745;"),
                                                 div(class = "metric-value", style = "color: #28a745;", "94.7%"),
                                                 div(class = "metric-label", "Customer Satisfaction"),
                                                 div(class = "metric-change positive",
                                                     tags$i(class = "fas fa-arrow-up me-1"), "2.1%"
                                                 )
                                             )
                                         ),
                                         
                                         # Load Time
                                         div(class = "grid-item theme-orange",
                                             div(class = "metric-card",
                                                 tags$i(class = "fas fa-stopwatch fa-2x mb-3", style = "color: #fd7e14;"),
                                                 div(class = "metric-value", style = "color: #fd7e14;", "1.8s"),
                                                 div(class = "metric-label", "Avg Load Time"),
                                                 div(class = "metric-change positive",
                                                     tags$i(class = "fas fa-arrow-up me-1"), "0.3s faster"
                                                 )
                                             )
                                         ),
                                         
                                         div(class = "grid-item theme-orange",
                                             div(class = "metric-card",
                                                 tags$i(class = "fas fa-stopwatch fa-2x mb-3", style = "color: #fd7e14;"),
                                                 div(class = "metric-value", style = "color: #fd7e14;", "1.8s"),
                                                 div(class = "metric-label", "Morbidity "),
                                                 div(class = "metric-change positive",
                                                     tags$i(class = "fas fa-arrow-up-right-from-square me-1"), "0.3s faster"
                                                 )
                                             )),
                                         div(class = "grid-item theme-orange",
                                             div(class = "metric-card",
                                                 tags$i(class = "fas fa-stopwatch fa-2x mb-3", style = "color: #fd7e14;"),
                                                 div(class = "metric-value", style = "color: #fd7e14;", "1.8s"),
                                                 div(class = "metric-label", "Maps"),
                                                 div(class = "metric-change positive",
                                                     tags$i(class = "fas fa-arrow-up-right-from-square me-1"), "0.3s faster"
                                                 )
                                             )
                                         ),
                                         div(class = "grid-item theme-orange",
                                             div(class = "metric-card",
                                                 tags$i(class = "fas fa-stopwatch fa-2x mb-3", style = "color: #fd7e14;"),
                                                 div(class = "metric-value", style = "color: #fd7e14;", "1.8s"),
                                                 div(class = "metric-label", "Population"),
                                                 div(class = "metric-change positive",
                                                     tags$i(class = "fas fa-arrow-up-right-from-square me-1"), "0.3s faster"
                                                 )
                                             )
                                         )),
                                     
                                     # # Navigation Cards in Grid
                                     
                                     
                                     # Close grid
                                     
                              ),
                              column(2,offset=0,
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
                              )
                          ), # Close content-area
                          
                          # Initialize Packery with JavaScript
                          
                          # Initialize Packery with JavaScript
                          tags$script(HTML("
    $(document).ready(function() {
     // external js: packery.pkgd.js, draggabilly.pkgd.js

var $grid = $('.grid').packery({
  itemSelector: '.grid-item',
  columnWidth: 100
});

// make all grid-items draggable
$grid.find('.grid-item').each( function( i, gridItem ) {
  var draggie = new Draggabilly( gridItem );
  // bind drag events to Packery
  $grid.packery( 'bindDraggabillyEvents', draggie );
});
});"
                          ))
                      )
                  )
)


# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {
    # Server logic can be added here if needed
    # The demo currently runs entirely on the client side with JavaScript
}

# ============================================================================
# RUN APP
# ============================================================================
shinyApp(ui = ui, server = server)