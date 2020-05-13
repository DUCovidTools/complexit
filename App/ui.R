# ui with grid layout

## panel ideas:
# Data view, where you can plot data for the various clusters, or choose your different areas

ui <- shinyUI(fluidPage(
  titlePanel("COMPLEX-IT Covid19 model"),
  
  fluidRow(
    column(2, 
           selectInput("NEregion", label ="Region", choices= NEnames, selected = "County Durham"),
           sliderInput("proxy_range", label = "Range for proxy", min = 0, 
                       max = 2, value = c(0.9, 1.25), step=0.05),
           checkboxInput("showCluster", label = "Show rest of Italian cluster", value = FALSE),
           checkboxInput("addNumbers", label = "Show numbers", value = FALSE),
           checkboxInput("showUK", label = "Show UK data from same cluster", value = FALSE),
           checkboxInput("showEst", label = "Show estimate from past week", value = FALSE)
           ),
    column(10,
           tabsetPanel(
             type = "tabs",
             id = "timeview",
             tabPanel(
               "Summary",
               includeMarkdown("markdown/exp_text.md")
             ),
             tabPanel(
               "Dashboard Options",
               includeMarkdown("markdown/dash_opt.mdown")
             ),
             tabPanel(
               "Clustering",
               includeMarkdown("markdown/clustering_exp.mdown")
             ),
             tabPanel(
               "Estimate",
               includeMarkdown("markdown/estimate.mdown")
             )
           )
           )
  ),
  plotOutput("NEname_proxyplot", height = "500px")

))