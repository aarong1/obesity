theme_x = '{"seriesCnt":4,"backgroundColor":"rgba(0,0,0,0)","titleColor":"#333333","subtitleColor":"#aaaaaa","textColorShow":false,"textColor":"#333","markTextColor":"#eeeeee","color":["#c12e34","#e6b600","#0098d9","#2b821d","#005eaa","#339ca8","#cda819","#32a487"],"borderColor":"#ccc","borderWidth":0,"visualMapColor":["#1790cf","#a2d4e6"],"legendTextColor":"#333333","kColor":"#c12e34","kColor0":"#2b821d","kBorderColor":"#c12e34","kBorderColor0":"#2b821d","kBorderWidth":1,"lineWidth":2,"symbolSize":4,"symbol":"emptyCircle","symbolBorderWidth":1,"lineSmooth":false,"graphLineWidth":1,"graphLineColor":"#aaaaaa","mapLabelColor":"#c12e34","mapLabelColorE":"#c12e34","mapBorderColor":"#eee","mapBorderColorE":"#ddd","mapBorderWidth":0.5,"mapBorderWidthE":1,"mapAreaColor":"#ddd","mapAreaColorE":"#e6b600","axes":[{"type":"all","name":"通用坐标轴","axisLineShow":true,"axisLineColor":"#333","axisTickShow":true,"axisTickColor":"#333","axisLabelShow":true,"axisLabelColor":"#333","splitLineShow":true,"splitLineColor":["#ccc"],"splitAreaShow":false,"splitAreaColor":["rgba(250,250,250,0.3)","rgba(200,200,200,0.3)"]},{"type":"category","name":"类目坐标轴","axisLineShow":false,"axisLineColor":"#333","axisTickShow":false,"axisTickColor":"#333","axisLabelShow":false,"axisLabelColor":"#333","splitLineShow":false,"splitLineColor":["#ccc"],"splitAreaShow":false,"splitAreaColor":["rgba(250,250,250,0.3)","rgba(200,200,200,0.3)"]},{"type":"value","name":"数值坐标轴","axisLineShow":false,"axisLineColor":"#333","axisTickShow":false,"axisTickColor":"#333","axisLabelShow":true,"axisLabelColor":"#a09494","splitLineShow":false,"splitLineColor":["#ccc"],"splitAreaShow":false,"splitAreaColor":["rgba(250,250,250,0.3)","rgba(200,200,200,0.3)"]},{"type":"log","name":"对数坐标轴","axisLineShow":true,"axisLineColor":"#333","axisTickShow":true,"axisTickColor":"#333","axisLabelShow":true,"axisLabelColor":"#333","splitLineShow":true,"splitLineColor":["#ccc"],"splitAreaShow":false,"splitAreaColor":["rgba(250,250,250,0.3)","rgba(200,200,200,0.3)"]},{"type":"time","name":"时间坐标轴","axisLineShow":true,"axisLineColor":"#333","axisTickShow":true,"axisTickColor":"#333","axisLabelShow":true,"axisLabelColor":"#554a4a","splitLineShow":true,"splitLineColor":["#ccc"],"splitAreaShow":false,"splitAreaColor":["rgba(250,250,250,0.3)","rgba(200,200,200,0.3)"]}],"axisSeperateSetting":true,"toolboxColor":"#06467c","toolboxEmphasisColor":"#4187c2","tooltipAxisColor":"#cccccc","tooltipAxisWidth":1,"timelineLineColor":"#005eaa","timelineLineWidth":1,"timelineItemColor":"#005eaa","timelineItemColorE":"#005eaa","timelineCheckColor":"#005eaa","timelineCheckBorderColor":"#316bc2","timelineItemBorderWidth":1,"timelineControlColor":"#005eaa","timelineControlBorderColor":"#005eaa","timelineControlBorderWidth":0.5,"timelineLabelColor":"#005eaa","datazoomBackgroundColor":"rgba(47,69,84,0)","datazoomDataColor":"rgba(47,69,84,0.3)","datazoomFillColor":"rgba(167,183,204,0.4)","datazoomHandleColor":"#a7b7cc","datazoomHandleWidth":"100","datazoomLabelColor":"#333333"}'

serializeJSON(theme_x)
theme_x <- read_json(theme_x)
fromJSON(  'theme.json')
theme_x <- readLines('theme.json')
browsable(
div(  
  e_theme_register(paste0(theme_x,collapse =""), name = "myTheme"),
(
 mtcars |> 
    head() |> 
    tibble::rownames_to_column('model') |> 
    e_charts(model) |> 
    e_pie(carb) |> 
   e_title('Pie chart') |> 
   e_theme(name = 'myTheme') #|>
 # e_theme_custom('theme.json',#'{"color":["#ff715e","#ffaf51"]}',
 #                name = 'custom') 
 
  )
)
)


library(shiny)
library(echarts4r)

ui <- bslib::page_fluid(  
  e_theme_register(paste0(theme_x,collapse =""), name = "myTheme"),
  (
    mtcars |> 
      head() |> 
      tibble::rownames_to_column('model') |> 
      e_charts(model) |> 
      e_pie(carb) |> 
      e_title('Pie chart') |> 
      e_theme(name = 'myTheme') #|>
    # e_theme_custom('theme.json',#'{"color":["#ff715e","#ffaf51"]}',
    #                name = 'custom') 
  )
)

server <- function(input, output){
  e <- e_charts(cars, speed) |> 
    e_scatter(dist) 
  
  output$chart1 <- renderEcharts4r({
    e_theme(e, "myTheme")
  })
  
  output$chart2 <- renderEcharts4r({
    e_theme(e, "myTheme")
  })
  
}

shinyApp(ui, server)
