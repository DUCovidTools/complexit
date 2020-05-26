# ui with grid layout

## panel ideas:
# Data view, where you can plot data for the various clusters, or choose your different areas

ui <- shinyUI(fluidPage(
  chooseSliderSkin("Modern", color = "#72246C"),
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
  #   tags$style(type="text/css", "padding:10px;")
  # ),
  tags$style('.container-fluid {
                             margin:10px;
              }'),
  titlePanel(
    windowTitle = "Complex-IT Covid-19 Model",
    h2(
      div(style="display:inline-block;",img(src="DULogo.png", height = 50,style="left;")),
      div(style="display:inline-block;","COMPLEX-IT Covid-19 Model",style="padding-left:10px;font-weight:bold;")
    )
),

  fluidRow(
    tabsetPanel(
      type = "tabs",
      id = "timeview",
      tabPanel(
        "Summary",
        includeMarkdown("markdown/exp_text.md")
      ),
      tabPanel(
        "Plot notes",
        includeMarkdown("markdown/plot_notes.mdown")
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
        "Models",
        includeMarkdown("markdown/models.mdown")
      )
    )),
  fluidRow(
    column(4,
           selectInput("NEregion", label ="Area", choices= NEnames, selected = "County Durham"),
           ),
    column(4,           dateInput("first_reg", label = "Earliest date in dataset to use for forecast", value = last_week, 
                                  min = last_week - 7, max = date_format(lastActualDay),
                                  format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                  language = "en", width = NULL)),
    column(4,            ## Don't allow 'glm' for now as I don't know how to do predictive variance
           selectInput("ModelType", 
                       label = "Model type (see Models tab for explanation)", 
                       choices = c(
                         "Linear Regression" = "lm", 
                         "Linear Regression (non-constant variance)" = "lmvar",
                         "Log-linear regression" = "log_lm"), 
                       selected = "log_lm")),
  ),
fluidRow(
    column(2,
           downloadButton('downloadPlot', label = "Download plot"),
           checkboxInput("usepast", label = "Use past in regression", value = TRUE),
           checkboxInput("showUK", label = "Show UK data from same cluster", value = FALSE),
           checkboxInput("addNumbers", label = "Show forecast bound numbers", value = FALSE),
           checkboxInput("useproxy", label = "Use Italian proxy data if available", value = TRUE),
           conditionalPanel(
             condition = "input.useproxy == true",
             sliderInput("n_RMSE", label = "Select Italian proxies based on how many days?", min = 3, 
                         max = 14, value = 5, step=1, width="80%"),
             sliderInput("max_RMSE", label = "Max RMSE for Italian proxies",
                         min=0, max=100, value = 40, step=0.5, width="80%")
           )
           ),
    column(10,     plotOutput("reg_plot", height = "500px"))
)))
