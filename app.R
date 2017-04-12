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


options(shiny.maxRequestSize = 50 * 1024 ^ 2)

## app.R ##
library(shiny)
library(shinydashboard)

#####################
# Consider IRANGES
#####################
add_sd_path = function(sd,       
                       cn = c("seriesNumber", "seriesDescription", 
                              "studyDate", "patientName")) {
  paths = attributes(sd)$paths
  sd = sd[, cn]
  for (i in seq_along(paths)) {
  # mapply(function(data, paths){
    p = data.frame(path = paths[[i]], stringsAsFactors = FALSE)
    p = merge(p, sd[i,], all = TRUE)
    paths[[i]] = p
  }
  paths = do.call("rbind", paths)
  paths
}

ui <- dashboardPage(
  dashboardHeader(title = "ICH Segmentation"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Images", 
               tabName = "upload_data", 
               icon = icon("glyphicon glyphicon-upload")),
      menuItem("Processing", tabName = "widgets", 
               icon = icon("glyphicon glyphicon-cog"))
    )    
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "upload_data",
              fileInput(inputId = 'fnames', 
                        label = 'Choose Images (NIfTI or DICOM series)',
                        multiple = TRUE),
              # accept = c('.dcm')),
              dataTableOutput("dcm_out"),
              uiOutput("which_series"),
              plotOutput("each_dicom")
              
      )
    )
  )
)



server <- function(input, output) {
  # Objects in this file are defined in each session
  # source('read_img.R', local=TRUE)
  
  get_data = reactive({
    inFile <- input$fnames
    print(inFile)
    if (is.null(inFile)) {
      return(NULL)
    }
    inFile$ext = neurobase::parse_img_ext(input$name)
    print(inFile)
    outdir = unique(dirname(inFile$datapath))
    if (!is.null(outdir)) {
      sd = scanDicom(path = outdir)
      sd = add_sd_path(sd)
      sd = sd %>% group_by(seriesNumber)
      # sdata <- SharedData$new(sd)
      # sd <- sdata$data(withSelection = TRUE)
    } else {
      sd = NULL
    }
    sd
  })
  
  get_simple_data = reactive({
    dcm_output = get_data()
    if (!is.null(dcm_output)){
      nifti_images = check_dcm2nii(dcm_output)
    } else {
      nifti_images = NULL
    }
    nifti_images
  })
  
  output$dcm_out = renderDataTable({
    sd = get_simple_data()
    sd
  })
  
  
  get_unique_series = reactive({
    sd = get_simple_data()
    unique(basename(sd))
  })
  
  # find if more than one 
  output$which_series = renderUI({
    usd = get_unique_series()
    print(usd)
    if (length(usd) == 1) {
      selectInput("selected_series", label = "which_series", choices = usd)
    } else {
      NULL
    }
  })
  
  
  output$each_dicom = renderPlot({
    sd = get_data()
    if (!is.null(sd)) {
      sd = sd %>% dplyr::slice(1)
      imgs = lapply(sd$path, readDICOMFile)
      print(names(imgs))
      imgs = lapply(imgs, function(x) {
        x$img
      })
      names(imgs) = paste0("Series Number:", sd$seriesNumber)
      n_imgs = length(imgs)
      par(mfrow = c(1, n_imgs))
      mapply(function(img, main) {
        graphics::image(img, col = gray(0:64/64), main = main)
      }, imgs, names(imgs))
      par(mfrow =c(1,1))
      # sapply()
    }
    # plot(0, 0)
  })  
  
  
  
  
}

shinyApp(ui, server)
