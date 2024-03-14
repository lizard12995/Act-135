
  #--------Create application-------
  fluidPage(theme = shinytheme("united"),
            shinyjs::useShinyjs(),
            
            tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
            
            titlePanel(h2("Philadelphia Act 135 Petition Properties"), windowTitle="Act 135 in Philly"),
            
            absolutePanel(top = 150, left = 50, draggable = TRUE, width = "10%", height = "40%", 
                          style = "z-index:500; min-width: 170px;",
                          
              selectInput("points",
                                       "Select Petition Data",
                          
                                       choices = c("Default",
                                         "Respondent Entity Type",
                                                   "Human vs. Corporate Respondents",
                                                   "Only Human Respondents")),
                           
                           selectInput("polygons",
                                       
                                       "Select Map Background Data",
                                       
                                       choices = c(
                                         "Council Districts",
                                         # "Neighborhoods",
                                         "Displacement Risk Ratio 2016",
                                                   "Displacement Risk Ratio 2021",
                                                   "Percent Non-White 2021",
                                                   "Percent Asian 2021"))),
            
            leafletOutput("map", width="100%", height = "90vh")
            
  )


