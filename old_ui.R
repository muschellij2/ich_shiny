
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
options(shiny.maxRequestSize = 50 * 1024 ^ 2)
# library(cttools)
# library(ichseg)
library(shiny)
library(shinyBS)
library(shinyjs)

shinyUI(fluidPage(
  shinyjs::useShinyjs(),

  # Application title
  titlePanel("ICH Segmentation"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput('img_fname', 'Choose NIfTI image to upload'
      ),
      div(style = "display:inline-block",
          bsButton("ss", "Skull Strip",
                   disabled = TRUE, width = "100%")
          ),
      div(style = "display:inline-block",
          checkboxInput("robust", "Robust Version",
                    value = FALSE, width = "100%")
      ),
      br(),
      br(),
      bsButton("reg", "Register Skull-Stripped Image", disabled = TRUE),
      br(),
      br(),
      bsButton("make_pred", "Create Predictors", disabled = TRUE),
      br(),
      br(),
      bsButton("predict", "Create Prediction Images", disabled = TRUE),
      br(),
      br(),
      downloadButton('download', 'Download All Output'),
      br(),
      br(),
      downloadButton('download_pred', 'Download Prediction Image Only')
      # actionButton("process", "Process Skull-stripped Image")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        id = 'results',
        tabPanel('Original Data',
                 plotOutput("origPlot")
                 ),
        tabPanel('Skull Stripped',
                 plotOutput("ssPlot")
        ),
        tabPanel('Registration',
                 plotOutput("regPlot")
        ),
        tabPanel('Predictor',
                 plotOutput("candPlot")
        ),
        tabPanel('Prediction',
                 plotOutput("predPlot")
        ),
        tabPanel('Diagnostics', textOutput("nText")),
        singleton(
          tags$head(tags$script(src = "message-handler.js"))
        )
      )

    )
  )
))
