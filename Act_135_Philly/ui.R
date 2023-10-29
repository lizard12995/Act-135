
  # Application title
  
  #--------Create application with two tabs-------
  fluidPage(theme = shinytheme("united"),
            shinyjs::useShinyjs(),
            
            tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
            
            titlePanel(h2("Philadelphia Act 135 Petition Properties"), windowTitle="Act 135 in Philly"),
            
            absolutePanel(top = 150, left = 50, draggable = TRUE, width = "10%", height = "40%", 
                          style = "z-index:500; min-width: 170px;",
                          
              selectInput("points",
                                       "Select Petition Data",
                          
                                       choices = c("Default",
                                         "Entity Type",
                                                   "Human vs. Corporate Entities",
                                                   "Predicted Race or Ethnicity")),
                           
                           selectInput("polygons",
                                       
                                       "Select Map Background Data",
                                       
                                       choices = c("Neighborhoods",
                                         "Displacement Risk Ratio 2016",
                                                   "Displacement Risk Ratio 2021",
                                                   "Percent Non-White 2021",
                                                   "Percent Asian 2021"))),
            
            leafletOutput("map", width="100%", height = "90vh")
            
  )
            
    # sidebarLayout(
    #   div(sidebarPanel(
    # 
    #     selectInput("points",
    #                 "Select Petition Data",
    #                 choices = c("Entity Type",
    #                             "Human vs. Corporate Entities",
    #                             "Predicted Race or Ethnicity")),
    # 
    #     selectInput("polygons",
    #                 "Select Map Background Data",
    #                 choices = c("Displacement Risk Ratio 2016",
    #                             "Displacement Risk Ratio 2021",
    #                             "Percent Non-White 2021",
    #                             "Percent Asian 2021")),
    # 
    #     actionButton("change","Display Map Data")
    # 
    #   ), style = "max-width:750px"),
    # 
    #   # Show the map
    #   mainPanel(
    # 
    #     #Map output
    #     leafletOutput("map", width="100%", height = "100vh")
    # 
    # 
    #   )
    # ))

