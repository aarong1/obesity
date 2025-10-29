library(shiny)
library(htmltools)

# Define UI
ui <- fluidPage(
  # Add external JavaScript libraries
  tags$head(
    # Include jQuery (already included by Shiny, but ensuring it's loaded first)
    tags$script(src = "https://code.jquery.com/jquery-3.6.0.min.js"),
    # Include Packery library
    tags$script(src = "https://unpkg.com/packery@2/dist/packery.pkgd.min.js"),
    # Include our custom JavaScript
    includeScript("www/packery_app.js"),
    
    # Add custom CSS
    tags$style(HTML("
      * { box-sizing: border-box; }
      
      /* force scrollbar */
      html { overflow-y: scroll; }
      
      body { font-family: sans-serif; }
      
      .grid {
        background: #EEE;
      }
      
      /* item is invisible, but used for layout */
      .grid-item,
      .grid-sizer {
        width: 20%;
      }
      
      .grid-item {
        float: left;
        height: 100px;
      }
      
      /* grid-item-content is visible, and transitions size */
      .grid-item-content {
        width: 100%;
        height: 100%;
        background: #C09;
        border: 2px solid hsla(0, 0%, 0%, 0.5);
        -webkit-transition: width 0.4s, height 0.4s;
                transition: width 0.4s, height 0.4s;
      }
      
      .grid-item:hover .grid-item-content {
        background: #C90;
        cursor: pointer;
      }
      
      /* both item and item content change size */
      .grid-item.is-expanded {
        width: 60%;
        height: 200px;
        z-index: 2;
      }
      
      .grid-item.is-expanded .grid-item-content {
        background: #0C9;
      }
    "))
  ),
  
  # Title
  h1("Packery - animate responsive item size"),
  
  # Instructions
  p("Click items to toggle size"),
  
  # Main grid container
  div(class = "grid",
      div(class = "grid-sizer"),
      div(class = "grid-item",
          div(class = "grid-item-content")
      ),
      div(class = "grid-item",
          div(class = "grid-item-content")
      ),
      div(class = "grid-item",
          div(class = "grid-item-content")
      ),
      div(class = "grid-item",
          div(class = "grid-item-content")
      ),
      div(class = "grid-item",
          div(class = "grid-item-content")
      ),
      div(class = "grid-item",
          div(class = "grid-item-content")
      ),
      div(class = "grid-item",
          div(class = "grid-item-content")
      ),
      div(class = "grid-item",
          div(class = "grid-item-content")
      ),
      div(class = "grid-item",
          div(class = "grid-item-content")
      ),
      div(class = "grid-item",
          div(class = "grid-item-content")
      ),
      div(class = "grid-item",
          div(class = "grid-item-content")
      ),
      div(class = "grid-item",
          div(class = "grid-item-content")
      ),
      div(class = "grid-item",
          div(class = "grid-item-content")
      ),
      div(class = "grid-item",
          div(class = "grid-item-content")
      ),
      div(class = "grid-item",
          div(class = "grid-item-content")
      )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Server logic can be added here if needed for dynamic content
}

# Run the application
shinyApp(ui = ui, server = server)