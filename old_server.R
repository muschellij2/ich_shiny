
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# library(cmaker)
library(shiny)
library(shinyBS)
library(ITKR)
# library(rgl)
library(ANTsR)
library(ichseg)
library(extrantsr)
library(fslr)
# library(matlabr)
library(R.matlab)
# if (!have_matlab()) {
#   options(matlab.path = '/Applications/MATLAB_R2014b.app/bin')
# }

filename =  "~/_AXIAL_HEAD_STD_20140115181000_2_Eq_1.nii.gz"
verbose = TRUE

download_img = function(outimg, file, gzipped = TRUE){
  ff = nii.stub(file)
  # print(ff)
  writenii(outimg,
           filename = ff,
           gzipped = gzipped)
  ext = ifelse(gzipped, ".nii.gz", ".nii")
  file.rename(paste0(ff, ext), file)
  return(file)
}

# Simple Function that will get the messages out
add_expr = function(expr){
  withCallingHandlers(
    expr,
    message = function(m) {
      shinyjs::html(id = "nText",
                    html = paste0(m$message, "<br/>"), add = TRUE)
    },
    warning = function(w) {
      shinyjs::html(id = "nText",
                    html = paste0(w$message, "<br/>"), add = TRUE)
    },
    error = function(e) {
      shinyjs::html(id = "nText",
                    html = paste0(e$message, "<br/>"), add = TRUE)
    })
}

shinyServer(function(input, output, session) {
  values <- reactiveValues(ss = NULL,
                           preprocess = NULL,
                           df = NULL,
                           nim = NULL,
                           res = NULL)


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
      add_expr({
        if (verbose) {
          message("<h3># Reading in Data</h3>")
        }
        img = readnii(filename)
      })
      return(img)
    }
  })
  #
  #   proc_img = reactive({
  #
  #     img = read_img()
  #     if (!is.null(img)) {
  #       preprocess = ich_preprocess(img = img,
  #                                   mask = NULL,
  #                                   verbose = TRUE)
  #       return(preprocess)
  #     } else {
  #       return(NULL)
  #     }
  #
  #   })

  output$origPlot <- renderPlot({
    shinyjs::disable(id = "download")
    shinyjs::disable(id = "download_pred")

    img = read_img()
    if (!is.null(img)) {
      add_expr({
        if (verbose) {
          message("<h3># Plotting Original in Data</h3>")
        }
      })
      ortho2(img, window = c(0, 100), text = "Original\nImage")

      updateButton(session, "ss",
                   disabled = FALSE,
                   icon = icon("chevron-right"))
    } else {
      return()
    }
  })

  #########################################
  # Skull Stripping
  #########################################
  observe({
    if (!is.null(input$ss)) {

      if (input$ss > 0) {

        updateButton(session, "ss", disabled = TRUE,
                     icon = icon("spinner fa-spin"))

        img = read_img()
        add_expr({
          if (verbose) {
            message("<h3># Skull-Stripping Images</h3>")
          }
          robust = input$robust
          ss = CT_Skull_Stripper(img, retimg = TRUE,
                                 robust = robust)
          # ss = CT_Skull_Strip(img, retimg = TRUE)
        })
        mask = ss > 0
        img = check_nifti(img)
        ss = mask_img(img, mask)
        ss = window_img(ss, window = c(0, 100),
                        replace = "zero")
        values$ss = ss
        rm(list = c("ss", "mask")); gc(); gc();
        icon = icon("check")
        updateButton(session, "ss",
                     disabled = TRUE,
                     icon = icon("check"))
        updateButton(session, "reg", disabled = FALSE,
                     icon = icon("chevron-right"))
      }
    }
  })

  #########################################
  # Skull Stripping Plot
  #########################################
  output$ssPlot <- renderPlot({
    # print(input$ss)
    ss = values$ss
    if (!is.null(ss)) {
      add_expr({
        if (verbose) {
          message("<h3># Plotting Skull-Stripped Data<h3>")
        }
      })
      ortho2(ss, window = c(0, 100), xyz = xyz(ss > 0),
             text = "Skull-Stripped\nImage")

    } else {
      return()
    }
  })


  #########################################
  # Registration
  #########################################
  observe({
    # print(input$reg)
    if (!is.null(input$reg)) {

      if (input$reg > 0) {
        updateButton(session, "reg", disabled = TRUE,
                     icon = icon("spinner fa-spin"))

        ss = values$ss
        mask = ss > 0
        outprefix = tempfile()
        typeofTransform = "Rigid"
        template.file = system.file(
          "scct_unsmooth_SS_0.01.nii.gz",
          package = "ichseg")
        template = readnii(template.file)
        interpolator = "Linear"
        add_expr({
          if (verbose) {
            message("<h3># Registering Images<h3>")
          }
          preprocess = registration(
            filename = ss,
            skull_strip = FALSE,
            correct = FALSE,
            outfile = NULL,
            retimg = TRUE,
            typeofTransform = typeofTransform,
            template.file = template.file,
            interpolator = interpolator,
            remove.warp = FALSE,
            outprefix = outprefix,
            verbose = verbose)

          omask = ants_apply_transforms(
            fixed = template.file,
            moving = mask,
            typeofTransform = typeofTransform,
            interpolator = interpolator,
            transformlist = preprocess$fwdtransforms)
        })
        preprocess$mask = mask
        preprocess$ss_image = ss
        preprocess$transformed_image = preprocess$outfile
        preprocess$transformed_mask = omask
        preprocess$outfile = NULL
        preprocess$fixed = template

        preprocess$fwd_mat = readMat(preprocess$fwdtransforms)
        preprocess$inv_mat = readMat(preprocess$invtransforms)

        rm(list = c("template", "mask", "omask")); gc(); gc();

        values$preprocess = preprocess
        rm(list = c("preprocess")); gc(); gc();
        updateButton(session, "reg", disabled = TRUE,
                     icon = icon("check"))
        updateButton(session, "make_pred", disabled = FALSE,
                     icon = icon("chevron-right"))

      }
    }
  })


  #########################################
  # Registration Plot
  #########################################
  output$regPlot <- renderPlot({
    # print(input$ss)
    preprocess = values$preprocess
    if (!is.null(preprocess)) {
      add_expr({
        if (verbose) {
          message("<h3># Plotting Registered Data<h3>")
        }
        double_ortho(preprocess$transformed_image,
                     preprocess$fixed,
                     window = c(0, 100),
                     text = "Registered\nImage\nand\nTemplate")
      })
    } else {
      return()
    }
    rm(list = c("preprocess")); gc(); gc();
  })


  #########################################
  # Making Predictors
  #########################################
  observe({
    print(input$make_pred)
    if (!is.null(input$make_pred)){
      if (input$make_pred > 0) {
        preprocess = values$preprocess
        if (!is.null(preprocess)){
          updateButton(session, "make_pred", disabled = TRUE,
                       icon = icon("spinner fa-spin"))

          timg = preprocess$transformed_image
          tmask = preprocess$transformed_mask > 0.5
          rm(list = c("preprocess")); gc(); gc();
          add_expr({
            if (verbose) {
              message("<h3># Making Predictors<h3>")
            }
            img.pred = make_predictors(
              timg,
              mask = tmask,
              roi = NULL,
              save_imgs = FALSE,
              verbose = verbose)
          })
          df = img.pred$df
          nim = img.pred$nim
          rm(list = "img.pred"); gc(); gc();
          df$multiplier = ich_candidate_voxels(df)


          values$df = df
          values$nim = nim
          rm(list = c("df", "nim")); gc(); gc();

          updateButton(session, "make_pred",
                       disabled = TRUE, icon = icon("check"))
          updateButton(session, "predict", disabled = FALSE,
                       icon = icon("chevron-right"))
        }
      }
    }
  })

  #########################################
  # Making Predictors Plot
  #########################################
  output$candPlot <- renderPlot({

    df = values$df
    nim = values$nim
    preprocess = values$preprocess

    if (!is.null(preprocess) & !is.null(df) & !is.null(nim)) {
      cand = remake_img(df$multiplier, nim)
      xyz = xyz(cand)
      timg = preprocess$transformed_image
      add_expr({
        if (verbose) {
          message("<h3># Plotting Candidate Data<h3>")
        }
      })
      ortho2(timg, cand,
             window = c(0, 100),
             text = "Registered\nImage\nand\nCandidate Mask")
      rm(list = c("timg", "cand", "xyz")); gc(); gc();

    } else {
      return()
    }
    rm(list = c("df", "nim", "preprocess")); gc(); gc();

  })

  #########################################
  # Predict Image
  #########################################
  observe({
    print(input$predict)
    if (!is.null(input$predict)) {
      if (input$predict > 0) {
        img = read_img()
        df = values$df
        nim = values$nim
        preprocess = values$preprocess
        if (!is.null(preprocess)) {
          invtransforms = values$preprocess$invtransforms
          interpolator = values$preprocess$interpolator
          print(invtransforms)
          print(interpolator)

          message(invtransforms)
          message(interpolator)
          updateButton(session, "predict", disabled = TRUE,
                       icon = icon("spinner fa-spin"))

          model = "rf"
          add_expr({
            if (verbose) {
              message("<h3># Making Prediction Images<h3>")
            }
            L = ich_predict(
              df = df,
              native_img = img,
              nim = nim,
              model = model,
              verbose = verbose,
              transformlist = invtransforms,
              interpolator = interpolator)
          })
          L$preprocess = values$preprocess
          # values$preprocess = NULL
          values$res = L
          rm(list = c("L")); gc(); gc();

          updateButton(session, "predict", disabled = TRUE,
                       icon = icon("check"))
          shinyjs::enable(id = "download")
          shinyjs::enable(id = "download_pred")
        }
      }
    }
  })

  #########################################
  # Predict Image Plot
  #########################################
  output$predPlot <- renderPlot({
    scc = values$res$native_prediction$smoothed_prediction_image
    # cc = res$native_prediction$prediction_image
    ss = values$ss

    if (!is.null(ss) & !is.null(scc)) {
      xyz = xyz(scc > 0.5)
      add_expr({
        if (verbose) {
          message("<h3># Plotting Prediction Mask<h3>")
        }
      })
      ortho2(ss, scc,
             window = c(0, 100),
             xyz = xyz,
             text = "Skull-Stripped\nImage\nand\nPrediction Mask")
      rm(list = c("scc", "ss", "xyz")); gc(); gc();
    } else {
      return()
    }
  })

  output$download <- downloadHandler(
    filename = 'output.rda',
    content = function(file) {
      isolate({
        vv = reactiveValuesToList(values)
      })
      res = vv$res

      save(res, file = file)
    }
  )


  output$download_pred <- downloadHandler(
    filename = 'smoothed_prediction_image.nii.gz',
    content = function(file) {
      isolate({
        vv = reactiveValuesToList(values)
      })
      outimg = vv$res$native_prediction$smoothed_prediction_image

      message("outimg")
      # message(outimg)
      print(outimg)
      message("file")
      message(file)
      print(file)


      rm(list = c("vv")); gc(); gc();
      download_img(outimg, file, gzipped = TRUE)

    }
  )


})

