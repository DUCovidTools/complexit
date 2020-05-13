# ui.R for complex-it model

## panel ideas:
# Data view, where you can plot data for the various clusters, or choose your different areas

ui <- shinyUI(fluidPage(
  titlePanel("COMPLEX-IT Covid19 model"),
  
  sidebarLayout(
    sidebarPanel( 
      selectInput("NEregion", label ="North East Region", choices= NEnames, selected = NEnames[1]),
      sliderInput("proxy_range", label = "Range for proxy", min = 0, 
                  max = 2, value = c(0.9, 1.25), step=0.05),
      checkboxInput("showPast", label ="Show past week", value = FALSE),
      checkboxInput("showCluster", label = "Show rest of cluster", value = FALSE),
      checkboxInput("addNumbers", label = "Show numbers", value = FALSE),
      checkboxInput("showUK", label = "Show UK data from same cluster", value = FALSE),
      checkboxInput("showEst", label = "Show estimate from past week", value = FALSE)
      
    ),
    mainPanel(
      plotOutput("NEname_proxyplot", height = "600px")
#      plotOutput("NEname_projplot"),
#      htmlOutput("ProjPlot_sources")
      
      ## Next plans 
      # - option to see past week 
      # - option to see whole cluster / quadrant?
      # - Would a more conservative estimate be to use the greatest and least increase over the quadrant?
    )
  )
  
))