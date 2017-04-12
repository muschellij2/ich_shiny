read_img = reactive({
  inFile <- input$img_fname
  
  if (is.null(inFile)) {
    return(NULL)
  } else {
    fname = inFile$name
    
    dpath = inFile$datapath
    #######################
    # Renaming Images so they have the filename and .nii.gz
    #######################
    filename = file.path(dirname(dpath), fname)
    if (file.exists(dpath)) {
      if (file.exists(filename)) {
        file.remove(filename)
      }
      file.rename(dpath, filename)
    }
    return(img)
  }
})

