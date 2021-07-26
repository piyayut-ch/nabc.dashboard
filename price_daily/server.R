function(input, output, session) { 
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Stamen.Toner) %>%
      setView(lng = 101, lat = 13, zoom = 6)
  })
}