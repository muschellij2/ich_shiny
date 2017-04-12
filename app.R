library(ichseg)
library(extrantsr)
library(fslr)
# library(divest)
library(dcm2niir)
library(crosstalk)
library(oro.dicom)
library(neurobase)
library(dcmtk)
library(dplyr)
library(tools)

options(shiny.maxRequestSize = 50 * 1024 ^ 2)

## app.R ##
library(shiny)
library(shinydashboard)

#####################
# Consider IRANGES
#####################
get_ext = function(file) {
  file = tolower(file)
  file = basename(file)
  ext = tools::file_ext(file)
  if (length(ext) > 0) {
    if (ext %in% "gz") {
      file = file_path_sans_ext(file)
      ext = file_ext(file)
      ext = paste0(ext, ".gz")
    }
  } else {
    ext = NA
  }
  if (ext %in% "") {
    ext = NA
  }
  return(ext)
}

ui <- dashboardPage(
  dashboardHeader(title = "ICH Segmentation"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Upload Images",
      tabName = "upload_data",
      icon = icon("glyphicon glyphicon-upload")
    ),
    menuItem(
      "Processing",
      tabName = "widgets",
      icon = icon("glyphicon glyphicon-cog")
    )
  )),
  dashboardBody(tabItems(
    # First tab content
    tabItem(
      tabName = "upload_data",
      fileInput(
        inputId = 'fnames',
        label = 'Choose Images (NIfTI or DICOM series)',
        multiple = TRUE
      ),
      # accept = c('.dcm')),
      textOutput("dcm_out"),
      uiOutput("which_series"),
      plotOutput("each_dicom")
      
    ),
    # Second tab content
    tabItem(
      tabName = "widgets",
      # plotOutput("each_dicom"),
      actionButton("run_processing", label = "Run Processing"),
      verbatimTextOutput("proc_output"),
      plotOutput("preprocess_plot"),
      plotOutput("unsmooth_plot"),
      plotOutput("smooth_plot")
      
    )
  ))
)



server <- function(input, output, session) {
  # Objects in this file are defined in each session
  # source('read_img.R', local=TRUE)
  
  get_data = reactive({
    inFile <- input$fnames
    print(inFile)
    if (is.null(inFile)) {
      return(NULL)
    }
    # copying file extensiosn to differentiate NIfTI from DICOM
    # print("getting Image extensions")
    # print(inFile$name)
    inFile$ext = get_ext(inFile$name)
    print(inFile$ext)
    
    ###################
    # renmaing the niftis
    ###################
    niis = grepl("nii", inFile$ext)
    if (any(niis)) {
      xx = inFile[niis,]
      outfiles = file.path(dirname(xx$datapath), xx$name)
      file.rename(xx$datapath, outfiles)
      inFile$datapath[niis] = outfiles
    }
    
    print("grepped it")
    print(inFile)
    
    
    # inFile$ext[ !is.na(inFile$ext)] = paste0(".",
    #                                          inFile$ext[ !is.na(inFile$ext)])
    #
    # inFile$ext[is.na(inFile$ext)] = ""
    # # files now have extensions
    # inFile$outfile = paste0(inFile$datapath, inFile$ext)
    # file.rename(inFile$datapath, inFile$outfile)
    
    # need the path to run dcm2nii
    outdir = unique(dirname(inFile$datapath))
    # print("directory is ")
    # print(outdir)
    # print(inFile)
    if (!is.null(outdir)) {
      sd = dcm2nii(basedir = outdir,
                   dcm2niicmd = "dcm2niix",
                   copy_files = FALSE)
      print(dir(outdir))
    } else {
      sd = NULL
    }
    sd
  })
  
  get_simple_data = reactive({
    dcm_output = get_data()
    print("dcm_output is")
    print(dcm_output)
    if (!is.null(dcm_output)) {
      # just getting filenames
      nifti_images = check_dcm2nii(dcm_output)
      nifti_images = c(dcm_output$nii_before, nifti_images)
      # nifti_images$nifti_
      if (length(nifti_images) == 0) {
        nifti_images = NULL
      } else {
        print("pre unique")
        print(nifti_images)
        nifti_images = unique(nifti_images)
        print("post unique")
        print(nifti_images)
      }
    } else {
      nifti_images = NULL
    }
    nifti_images
  })
  
  output$dcm_out = renderText({
    sd = get_simple_data()
    sd
  })
  
  
  ################################
  # Gets series filenames
  ################################
  get_unique_series = reactive({
    sd = get_simple_data()
    if (!is.null(sd)) {
      sd = unique(basename(sd))
    } else {
      sd = NULL
    }
    sd
  })
  
  ## CHANGE HERE
  ## Set up buffer to keep the index
  series_indexer <- reactiveValues(which_index = 1)
  
  ## CHANGE HERE
  ## Save the selection when occurs
  observe({
    if (!is.null(input$selected_series)) {
      series_indexer$which_index <- input$selected_series
    }
  })
  
  # find if more than one
  output$which_series = renderUI({
    usd = get_unique_series()
    print(usd)
    if (length(usd) == 0) {
      return(NULL)
    }
    if (length(usd) > 1) {
      selectInput("selected_series",
                  label = "Which Series",
                  choices = usd)
    } else {
      
    }
  })
  
  
  the_image = reactive({
    sd = get_simple_data()
    if (!is.null(sd)) {
      print("sd is")
      print(sd)
      usd = unique(basename(sd))
      names(sd) = usd
      sd = check_nifti(sd)
      # sd = readnii(sd)
      if (is.nifti(sd)) {
        sd = list(sd)
        names(sd) = usd
      }
      sd = lapply(sd,
                  window_img,
                  window = c(-1024, 3071),
                  replace = "window")
      # } else {
      print("Series Index stuff")
      print(series_indexer$which_index)
      img = sd[[series_indexer$which_index]]
      # }
    } else {
      img = NULL
    }
    img 
  })
  
  output$each_dicom = renderPlot({
    img = the_image()
    if (!is.null(img)) {
      ortho2(img, window = c(0, 100))
    }
  })
  
  # run_processing
  v <- reactiveValues(run_the_stuff = FALSE)
  
  observeEvent(input$run_processing, {
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
    v$run_the_stuff <- input$run_processing
  })
  
  
  values <- reactiveValues()
  
  
  
  
  logText <- reactive({
    img = the_image()
    values[["log"]] <- capture.output({
      result <- ich_segment(img = img)
    })
    result
  })
  
  observe({
    if (v$run_the_stuff == FALSE) return()
    isolate({
      print("Why are you already in the thing")
      
      result = logText()
      output$proc_output <- renderPrint({
        logText()
        return(print(values[["log"]]))
        # You could also use grep("Warning", values[["log"]]) to get warning messages and use shinyBS package
        # to create alert message
      })  
      output$preprocess_plot = renderPlot({
        ortho2(img, result$preprocess$mask,
               col.y = scales::alpha("red", 0.5))
      })
      output$unsmooth_plot = renderPlot({
        ortho2(img, result$native_prediction$prediction_image,
               col.y = scales::alpha("red", 0.5))
      })      
      output$smooth_plot = renderPlot({
        ortho2(img, result$native_prediction$smoothed_prediction_image,
               col.y = scales::alpha("red", 0.5))
      })          
    })  
    
  })
  
  
  
  
  
}

shinyApp(ui, server)
