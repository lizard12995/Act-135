library(shiny)

function(input, output, session) {
  
  #POINTS
  
  # "All Points with Entity Type" --> all data, color: all_properties_geocoded_flags$resp_entity
  # "People vs. Non-Human Entities" --> all data, color: all_properties_geocoded_flags$resp_human
  # "Predicted Race or Ethnicity" --> filter to all_properties_geocoded_flags$resp_human == TRUE & !is.na(race)
  
  #POLYGONS
  
  # "Neighborhoods" --> neighborhoods
  # "Displacement Risk Ratio 2016" --> bgs16, color: bgs16$DRR1516C
  # "Displacement Risk Ratio 2021" --> bgs21, color: bgs21$DRR2122C
  # "Percent Non-White 2021" --> bgs21, color: bgs21$pct_non_white
  # "Percent Asian 2021" --> bgs21, color: bgs21$pct_asian

  #----MAP----
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lat = 39.971943, lng = -75.159994, zoom = 11) %>%
    
      addMapPane("polygons", zIndex = 410) %>%
      addMapPane("nbrhd", zIndex = 420) %>%
      addMapPane("point", zIndex = 430) 

  })
  
  
  #----POINT SELECTION----
  
  point_data <- reactive({

    if (input$points == "Entity Type" | input$points ==  "Human vs. Corporate Entities"
        | input$points == "Default") {

      all_properties_geocoded_flags

    }
    else if (input$points == "Predicted Race or Ethnicity"){

      all_properties_geocoded_flags %>%
        filter(resp_human & !is.na(race))
    }

  })
  
  observe({
    if (input$points == "Entity Type" | input$points ==  "Human vs. Corporate Entities"
        | input$points == "Default") {
      
      content2 <<- paste("<p> Address:", all_properties_geocoded_flags$RecordMatch, "<br>",
                        "OPA Number:", all_properties_geocoded_flags$opanum, "<br>",
                        "Petitioner:", all_properties_geocoded_flags$petitioner, "<br>",
                        "Respondent:", all_properties_geocoded_flags$respondent, "<br>",
                        "Respondent Entity Type:", all_properties_geocoded_flags$resp_entity, "<br>",
                        "# of Properties:", all_properties_geocoded_flags$number_props, "<br>",
                        "Predicted Ethnicity:", all_properties_geocoded_flags$race) %>%
        lapply(htmltools::HTML)
      
      
    }
    else if (input$points == "Predicted Race or Ethnicity"){
      
      content2 <<- paste("<p> Address:", human_properties$RecordMatch, "<br>",
                        "OPA Number:", human_properties$opanum, "<br>",
                        "Petitioner:", human_properties$petitioner, "<br>",
                        "Respondent:", human_properties$respondent, "<br>",
                        "Respondent Entity Type:", human_properties$resp_entity, "<br>",
                        "# of Properties:", human_properties$number_props, "<br>",
                        "Predicted Ethnicity:", human_properties$race) %>%
        lapply(htmltools::HTML)
      
    }
  })

  observe({
  
    colorBy <<- input$points
    
    if (colorBy == "Entity Type") {
      
      colorData <<- point_data()$resp_entity

      points_pal <<- colorFactor(
        palette = c("#d7191c","#fc8d59","#fee08b","#e6f598","#99d594","#3288bd"),
        domain = point_data()$resp_entity)
      
    }
      
      if (input$points == "Human vs. Corporate Entities") {
        
        colorData <<- point_data()$resp_human
          
        points_pal <<- colorFactor(
          palette = c("#d7191c","#3288bd"),
          domain = point_data()$resp_human)
        
      }
      
      else if (input$points == "Predicted Race or Ethnicity"){
        
        colorData <<- point_data()$race
        
        points_pal <<- colorFactor(
          palette = c("#d7191c","#fc8d59","#99d594","#3288bd"),
          domain = point_data()$race)
        
      }
  })
  
  observe({
    
    if (input$points == "Default"){
      leafletProxy("map") %>%
        
        clearMarkers() %>%
        clearControls() %>%
        
        addCircleMarkers(data = point_data(),
                         color = "blue",
                         fillOpacity = .4,
                         radius = 4,
                         stroke = TRUE,
                         weight = 1,
                         label = content2,
                         labelOptions = c(direction='right'),
                         options = pathOptions(pane="point")) 
    } 
    
    else{
      leafletProxy("map") %>%
        
        clearMarkers() %>%
        clearControls() %>%
        
        addCircleMarkers(data = point_data(),
                         fillColor= points_pal(colorData),
                         fillOpacity = .7,
                         radius = 4,
                         stroke = TRUE,
                         weight = .5,
                         label = content2,
                         labelOptions = c(direction='right'),
                         options = pathOptions(pane="point")) %>%
        
        addLegend("bottomleft", 
                  pal=points_pal, 
                  values=colorData, 
                  title=colorBy)
    }
    
  })

  
  
  #----POLYGON SELECTION----

  observe({

    colorBy2 <<- input$polygons
  
    if (colorBy2 == "Displacement Risk Ratio 2016") {

      colorData2 <<- bgs21$DRR1516C

      poly_pal <<- colorFactor(
        palette = "magma",
        domain = colorData2)

    }
    if (colorBy2 == "Displacement Risk Ratio 2021") {

      colorData2 <<- bgs21$DRR2122C

      poly_pal <<- colorFactor(
        palette = "magma",
        domain = colorData2)

    }
    if (colorBy2 == "Percent Non-White 2021") {

      colorData2 <<- bgs21$pct_non_white

      poly_pal <<- colorNumeric(
        palette = "viridis",
        domain = colorData2)

    }
    else if (colorBy2 == "Percent Asian 2021"){

      colorData2 <<- bgs21$pct_asian

      poly_pal <<- colorNumeric(
        palette = "viridis",
        domain = colorData2)


    }
    
  })
  
  observe({
    
    if(input$polygons == "Neighborhoods"){

    leafletProxy("map") %>%

      clearShapes() %>%
      clearControls() %>%
      
      addPolygons(data = neighborhoods,
                  stroke = TRUE,
                  weight = 1,
                  fillOpacity = 0,
                  label = neighborhoods@data$mapname,
                  options = pathOptions(pane="nbrhd"),
                  group = "neighborhoods")}
    
    else{
      leafletProxy("map") %>%
        
        clearShapes() %>%
        clearControls() %>%
        
        addPolygons(data = bgs21,
                    stroke = TRUE,
                    weight = .5,
                    fillOpacity = .5,
                    color = poly_pal(colorData2),
                    options = pathOptions(pane="polygons")) %>%
        
        addLegend("bottomright", pal=poly_pal,
                  values=colorData2,
                  title=colorBy2)
    }

  })

}