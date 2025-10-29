library(sf)
library(leaflet)
library(ggplot2)

fast_food_belfast <- st_read("data/export (2).geojson") |> 
  st_centroid() 
parks <- st_read("data/export (1).geojson")|> 
  st_centroid() 
fast_food <- st_read("data/export.geojson")|> 
  st_centroid() 

(parks) |> 
  leaflet() |> 
  addTiles() |> 
  addCircles(radius = 50, 
             label = ~name,
             popup = ~as.character(name))

(fast_food) |> 
  leaflet() |> 
  addTiles() |> 
  addCircles(radius = 50, popup = ~as.character(name))

pts <- parks[sf::st_geometry_type(parks) %in% c("POINT", "MULTIPOINT"), ]
polys <- parks[sf::st_geometry_type(parks) %in% c("POLYGON", "MULTIPOLYGON"), ]


metric_cards_parks <- metric_card('Parks and Green Spaces', 633,'NI',color = 'green',opacity = 'bg-opacity-100')
metric_cards_fast_food <- metric_card(890,'Fast Food Outlets','NI',color = 'green')
browsable(metric_cards_parks)

# var map = L.map('custom_map1').setView([51.505, -0.09], 13);


