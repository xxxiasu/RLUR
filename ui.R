require(shiny)
require(shinydashboard)
require(leaflet)
require(shinyBS)

header <- dashboardHeader(title = "RLUR")

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem(
      "Variable Creation", icon = icon("wrench", lib = "font-awesome"), tabName = "variables"
    ),
    menuItem(
      "Training Set", icon = icon("list", lib = "font-awesome"), tabName = "training"
    ),
    menuItem(
      "Model Builder", icon = icon("flash", lib = "font-awesome"), tabName = "models"
    ),
    menuItem(
      "Predictions", icon = icon("magic", lib = "font-awesome"), tabName = "predictions"
    ),
    menuItem(
      "Descriptions", icon = icon("info", lib = "font-awesome"), tabName = "descriptions"
    ),
    menuItem(
      "Help", icon = icon("question", lib = "font-awesome"), tabName = "help"
    )
  )
)

body <- dashboardBody(
  tags$head(tags$style(
    HTML('.skin-blue .wrapper{background-color:#ecf0f5}')
  )),
  tabItems(
    tabItem(
      tabName = "variables",
      bsAlert("alert0"),
      fluidRow(column(
        width = 4,
        box(
          title = "Monitoring", status = "primary", width = 12, solidHeader = TRUE,
          fileInput('file.monitor', 'Monitoring data', multiple = TRUE)
        )
      ),
      column(
        width = 4,
        box(
          title = "Spatial Reference", status = "primary", width = 12, solidHeader = TRUE,
          textInput("epsg", label = h4("EPSG code"), value = "27700")
        )
      )),
      fluidRow(
        column(
          width = 4,
          box(
            title = "Landcover", status = "primary", width = 12, solidHeader = TRUE,
            fileInput('file.landcover', 'CORINE landcover data', multiple = TRUE)
          )
        ),
        column(
          width = 4,
          box(
            title = "Traffic", status = "primary", width = 12, solidHeader = TRUE,
            fileInput('file.roads', 'Road and traffic data', multiple = TRUE)
          )
        ),
        column(
          width = 4,
          box(
            title = "Population", status = "primary", width = 12, solidHeader = TRUE,
            fileInput('file.population', 'Population data', multiple = TRUE)
          )
        )
      ),
      fluidRow(
        column(
          width = 4,
          box(
            title = NULL, status = "primary", width = 12, solidHeader = FALSE, height = 250,
            uiOutput("corinecode")
          )
        ),
        column(
          width = 4,
          box(
            title = NULL, status = "primary", width = 12, solidHeader = FALSE, height = 250,
            uiOutput("roadcodetotal"),
            uiOutput("roadcodeHGV"),
            uiOutput("roadmajor")
          )
        ),
        column(
          width = 4,
          box(
            title = NULL, status = "primary", width = 12, solidHeader = FALSE, height = 250,
            uiOutput("popnpop"),
            uiOutput("popnhhold")
          )
        )
      ),
      fluidRow(
        column(width = 4,
               uiOutput("landcoverGenerate")),
        column(width = 4,
               uiOutput("roadGenerate")),
        column(width = 4,
               uiOutput("popnGenerate"))
      )
    ),
    tabItem(
      tabName = "training",
      fluidRow(column(
        width = 6,
        box(
          title = "Load existing dataset", status = "primary", width = 12, height = 150, solidHeader = TRUE,
          fileInput('filecsv', 'CSV file', multiple = FALSE)
        )
      ),
      column(
        width = 6,
        box(
          title = "Outlier selection", status = "primary", width = 12, height = 150, solidHeader = TRUE,
          fluidRow(
            column(width = 2,
                   actionButton("action.clear.out", label = "Clear")),
            column(width = 10,
                   verbatimTextOutput('out.count'))
          )
        )
      )),
      fluidRow(column(
        width = 6,
        box(
          title = "Training set attributes", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
          div(DT::dataTableOutput('table.trainingset'), style = "font-size: 65%")
        )
      ),
      column(
        width = 6,
        box(
          title = "Training set map", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
          fluidRow(column(
            width = 11, sliderInput("pointsize", NULL, 1, 50, 10, 1, ticks = FALSE)
          ),
          column(
            width = 1,
            div(actionButton(
              "pointextent", label = NULL, icon = icon("globe", lib = "font-awesome")
            ), style = "float:right")
          )),
          leafletOutput("monitormap", width = "100%", height = 600)
        )
      ))
    ),
    tabItem(tabName = "models",
            fluidRow(
              column(
                width = 4,
                box(
                  title = "Variables", status = "primary", width = 12, solidHeader = TRUE,
                  uiOutput("response"),
                  actionButton("action.clear", label = "Clear selection"),
                  br(),
                  br(),
                  div(DT::dataTableOutput('training.set'), style = "font-size: 65%")
                )
              ),
              column(
                width = 8,
                tabBox(
                  id = "plotTabset", width = 12,# height = "250px",
                  tabPanel("Correlation", plotOutput("pairs")),
                  tabPanel(
                    "Scatter", checkboxInput("labels", "Show row numbers"),
                    plotOutput("lm.obs.exp")
                  ),
                  tabPanel("Change", plotOutput("lm.aic.r2")),
                  tabPanel("Residuals-Fitted", plotOutput("lm.plot.1")),
                  tabPanel("Q-Q", plotOutput("lm.plot.2")),
                  tabPanel("Scale-Location", plotOutput("lm.plot.3")),
                  tabPanel("Cook's D", plotOutput("lm.plot.4")),
                  tabPanel("Residuals-Leverage", plotOutput("lm.plot.5"))
                ),
                box(
                  title = "Model details", status = "primary", width = 12, solidHeader = TRUE,
                  DT::dataTableOutput('model.stats')
                ),
                box(
                  title = "Cross validation", status = "primary", width = 12, solidHeader = TRUE,
                  numericInput(
                    "kfolds", "Number of k-folds", 10, min = 2, max = NA , step = 1,
                    width = 150
                  ),
                  verbatimTextOutput("model.summary")
                )
              )
            )),
    tabItem(
      tabName = "predictions",
      div(
        bsAlert("alert"),
        class = "outer",
        tags$head(includeCSS("styles.css")),
        leafletOutput("mymap", width = "100%", height = "100%"),             ## From Superzip example
        absolutePanel(
          id = "controls", class = "panel panel-default", fixed = TRUE,
          draggable = TRUE, top = 160, left = "auto", right = 20, bottom = "auto",
          width = 330, height = "auto",
          h3("Prediction extent"),
          checkboxInput("cbtrunc", label = "Truncate values", value = FALSE),
          checkboxInput("cbrecept", label = "Specify point file", value = FALSE),
          conditionalPanel(
            "input.cbrecept == false",
            checkboxInput("cb.extent", label = "Maximum from input", value = FALSE),
            fluidRow(column(
              8, numericInput("gres", label = NULL, value = 500)
            ),
            column(4, h5("Resolution")))
          ),
          conditionalPanel(
            "input.cbrecept == true",
            fileInput('file.recept', 'Receptor locations', multiple = TRUE)
          ),
          actionButton("action.clear.box", label = "Clear"),
          h3("Run prediction"),
          fluidRow(column(
            6, actionButton("action.grid", label = "Grid")
          ),
          column(
            6, actionButton("action.pred", label = "Predict")
          )),
          h3("Export"),
          fluidRow(column(6, selectInput(
            "downloadItem", NULL,
            c(
              "Prediction" = 1,
              "Training Set" = 2,
              "Model Details" = 3,
              "R Workspace" = 4,
              "Correlation Plot" = 5,
              "Scatter Plot" = 6,
              "Change Plots" = 7,
              "Residual-Fitted Plot" = 8,
              "Normal Q-Q Plot" = 9,
              "Scale-Location Plot" = 10,
              "Cooks D Plot" = 11,
              "Residual-Leverage Plot" = 12
            )
          )),
          column(
            6, downloadButton("downloadData", label = "Export")
          ))
        )
      )
    ),
    tabItem(
      tabName = "descriptions",
      box(
        title = "Variable description", status = "primary", width = 12, solidHeader = TRUE,
        div(DT::dataTableOutput('table.escape'), style = "font-size: 65%")
      )
    ),
    tabItem(tabName = "help",
            fluidRow(column(
              4,
              box(
                title = "Contents", status = "primary", width = 12, solidHeader = TRUE,
                div(actionLink("help1", "Introduction"), style = "font-size: 150%"),
                div(actionLink("help2", "Test data"), style = "font-size: 150%"),
                div(actionLink("help3", "Variable creation"), style = "font-size: 150%"),
                div(actionLink("help4", "Training set"), style = "font-size: 150%"),
                div(actionLink("help5", "Model Builder"), style = "font-size: 150%"),
                div(actionLink("help5a", "Model guidelines"), style = "font-size: 150%"),
                div(actionLink("help6", "Predictions"), style = "font-size: 150%"),
                div(actionLink("help7", "Further information"), style = "font-size: 150%"),
                div(actionLink("help8", "Worked example"), style = "font-size: 150%")
              )
            )
            ,
            column(
              8, htmlOutput('instructions')
            )))
  )
)

dashboardPage(header,
              sidebar,
              body)
