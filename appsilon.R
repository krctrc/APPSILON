distance <- function(lat1,lon1,lat2,lon2) {
  R = 6371
  dLat = (lat2-lat1) * (pi/180) 
  dLon = (lon2-lon1) * (pi/180) 
  a = (sin(dLat/2) * sin(dLat/2) +
  cos((lat1)* (pi/180)) * cos((lat2) * (pi/180)) * 
  sin(dLon/2) * sin(dLon/2))
  c = 2 * atan2(sqrt(a), sqrt(1-a))
  d = R * c
  print(d)
}


distbtw <- function(a,b,d) {
  distance(d[a,1],d[a,2],d[b,1],d[b,2])
}

leaflet(data = bynp[1:20,]) %>% addTiles() %>%
  addMarkers(~LON, ~LAT, popup = ~as.character(SPEED), label = ~as.character(SPEED))


karoli =data %>% 
  filter(data$SHIPNAME == "KAROLI" & data$is_parked == 0)

byship <- function (n) {
  ships %>% filter(SHIPNAME == n & is_parked == 0)
}

formap <- function (mp) {
leaflet(data = mp[1:nrow(mp),]) %>% addTiles() %>%  
  addMarkers(~LON, ~LAT, popup = ~as.character(SPEED), label = ~as.character(SPEED))
} 


data = transform(data, Time = substr(DATETIME, 12, 19))



remainseconds <- function(a,b) {
  h=ships[a,24]-ships[b,24]
  m=ships[a,25]-ships[b,25]
  s=ships[a,26]-ships[b,26]
  sec=h*3600+m*60+s
  print(sec)
}

calcdist <- function(a,b) {
  v = (ships[a,3]+ships[b,3])/7.2
  h=ships[a,24]-ships[b,24]
  m=ships[a,25]-ships[b,25]
  s=ships[a,26]-ships[b,26]
  sec=h*3600+m*60+s
  road=sec*v
  print(road)
}

library(shiny)
library(shiny.semantic)
ui <- semanticPage(
  title = "My page",
  div(class = "ui button", icon("user"),  "Icon button")
)
server <- function(input, output) {}
shinyApp(ui, server)

output$map <- renderLeaflet({
  leaflet(zo) %>%
    addTiles() %>%
    addMarkers(~LON, ~LAT, popup = ~as.character(SPEED), label = ~as.character(SPEED))
})

