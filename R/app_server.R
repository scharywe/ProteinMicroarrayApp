#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny shinyFiles EBImage
#' @importFrom shinythemes shinytheme
#' @importFrom DT DTOutput renderDT datatable
#' @import plotly
#' @import tidyr
#' @noRd
app_server <- function(input, output, session) {
  
  ###### FIRST TAB
  
  options(shiny.maxRequestSize=50*1024^2) #file can be up to 50 mb; default is 5 mb
  shinyImageFile <- reactiveValues(shiny_img_origin = NULL, shiny_img_cropped = NULL,
                                   shiny_img_final = NULL, Threshold = NULL)
  IntensData <- NULL
  
  #checks upload for file input
  observe({
    #default: upload image
    if(input$upload == 1){
      output$plot1 <- renderPlot({
        if(is.null(input$file1)) {
          output$rotatePanel <- renderUI({})
        }
        validate(need(!is.null(input$file1), "Must upload a valid jpg, png, or tiff"))
      })
    }
    if(input$upload == 2){
      # using sample image
      img <- readImage(system.file("images", "Sample.jpeg", package="ProteinMicroarrayApp"))
      shinyImageFile$shiny_img_origin <- img
      shinyImageFile$shiny_img_cropped <- img
      shinyImageFile$shiny_img_final <- img
      
      shinyImageFile$filename <- "Sample.jpeg"
      #outputs image to plot1 -- main plot
      output$plot1 <- renderPlot({ EBImage::display(shinyImageFile$shiny_img_final, method = "raster") })
      drawRotatePanel()
    }
  }) # end of observe
  
  
  # NOTE renameUpload is completely unecessary. The app works without it perfectly.
  
  # #the datapath is different from the one needed to properly recognize photo
  # #so this function renames the file
  # renameUpload <- function(inFile){
  #   if(is.null(inFile))
  #     return(NULL)
  # 
  #   oldNames <- inFile$datapath
  #   newNames <- file.path(dirname(inFile$datapath), inFile$name)
  #   file.rename(from = oldNames, to = newNames)
  #   inFile$datapath <- newNames
  # 
  #   return(inFile$datapath)
  # }
  
  #if they enter a new file, their file will become the new imageFile
  observeEvent(input$file1, {
    shinyImageFile$filename <- input$file1$name
    # img <- readImage(renameUpload(input$file1)) # Commented it, because I think its redundant.
    img <- readImage(input$file1$datapath)
    shinyImageFile$shiny_img_origin <- img
    shinyImageFile$shiny_img_cropped <- img
    shinyImageFile$shiny_img_final <- img
    output$plot1 <- renderPlot({EBImage::display(img, method = "raster")})
    drawRotatePanel()
  })
  
  
  # NOTE This function draws rotation panel.
  drawRotatePanel <- function() {
    output$rotatePanel <- renderUI({
      tagList(
        sliderInput("rotate", "Rotate image",
                    min=-90, max=90, value=0),
        actionButton("rotateCCW", "-90"),
        actionButton("rotateCW", "+90"),
        actionButton("fliphor", "FH"),
        actionButton("flipver", "FV"),
      )
    })
  }
  
  
  observe({reactiveRotation()})
  
  reactiveRotation <- eventReactive(input$rotate, {
    isolate({
      if (!is.null(shinyImageFile$shiny_img_cropped)) {
        shinyImageFile$shiny_img_final <- EBImage::rotate(shinyImageFile$shiny_img_cropped, input$rotate, bg.col="white")
        output$plot1 <- renderPlot({EBImage::display(shinyImageFile$shiny_img_final, method = "raster")})
        session$resetBrush("plot_brush")
      }
    })
  })
  
  observe({reactiveRotationCCW()})
  
  reactiveRotationCCW <- eventReactive(input$rotateCCW, {
    isolate({
      if (!is.null(shinyImageFile$shiny_img_cropped)) {
        shinyImageFile$shiny_img_cropped <- EBImage::rotate(shinyImageFile$shiny_img_cropped, -90)
        shinyImageFile$shiny_img_final <- shinyImageFile$shiny_img_cropped
        output$plot1 <- renderPlot({EBImage::display(shinyImageFile$shiny_img_final, method = "raster")})
        session$resetBrush("plot_brush")
      }
    })
  })
  
  observe({reactiveRotationCW()})
  
  reactiveRotationCW <- eventReactive(input$rotateCW, {
    isolate({
      if (!is.null(shinyImageFile$shiny_img_cropped)) {
        shinyImageFile$shiny_img_cropped <- EBImage::rotate(shinyImageFile$shiny_img_cropped, 90)
        shinyImageFile$shiny_img_final <- shinyImageFile$shiny_img_cropped
        output$plot1 <- renderPlot({EBImage::display(shinyImageFile$shiny_img_final, method = "raster")})
        session$resetBrush("plot_brush")
      }
    })
  })
  
  observe({reactiveRotationFlip()})
  
  reactiveRotationFlip <- eventReactive(input$fliphor, {
    isolate({
      if (!is.null(shinyImageFile$shiny_img_cropped)) {
        shinyImageFile$shiny_img_cropped <- EBImage::flip(shinyImageFile$shiny_img_cropped)
        shinyImageFile$shiny_img_final <- shinyImageFile$shiny_img_cropped
        output$plot1 <- renderPlot({EBImage::display(shinyImageFile$shiny_img_final, method = "raster")})
        session$resetBrush("plot_brush")
      }
    })
  })
  
  observe({reactiveRotationFlop()})
  
  reactiveRotationFlop <- eventReactive(input$flipver, {
    isolate({
      if (!is.null(shinyImageFile$shiny_img_cropped)) {
        shinyImageFile$shiny_img_cropped <- EBImage::flop(shinyImageFile$shiny_img_cropped)
        shinyImageFile$shiny_img_final <- shinyImageFile$shiny_img_cropped
        output$plot1 <- renderPlot({EBImage::display(shinyImageFile$shiny_img_final, method = "raster")})
        session$resetBrush("plot_brush")
      }
    })
  })
  
  croppedImage <- function(image, xmin, ymin, xmax, ymax){
    if(length(dim(image)) == 2)
      image <- image[xmin:xmax, ymin:ymax, drop = FALSE]
    else if(length(dim(image)) == 3)
      image <- image[xmin:xmax, ymin:ymax, ,drop = FALSE]
    return(image)
  }
  
  observe({resetImage()})
  
  resetImage <- eventReactive(input$reset,{
    isolate({
      # For now, resetting only the grid is enough. If there is a need of resseting the image
      # and rotation settings, uncommennt following three lines.
      shinyImageFile$shiny_img_cropped <- shinyImageFile$shiny_img_origin
      shinyImageFile$shiny_img_final <- shinyImageFile$shiny_img_cropped
      output$plot1 <- renderPlot({EBImage::display(shinyImageFile$shiny_img_final, method = "raster")})
      session$resetBrush("plot_brush")
      updateSliderInput(session, "rotate", value=0)
      shinyjs::disable("segmentation")
    })
  })
  
  #prompts shiny to look at recursive crop
  observe({recursiveCrop()})
  
  #only executes when keep is clicked
  recursiveCrop <- eventReactive(input$plot_dblclick,{
    isolate({
      p <- input$plot_brush
      validate(need(p$xmax <= dim(shinyImageFile$shiny_img_cropped)[1], 
                    "Highlighted portion is out of bounds on the x-axis"))
      validate(need(p$ymax <= dim(shinyImageFile$shiny_img_cropped)[2], 
                    "Highlighted portion is out of bounds on the y-axis"))
      validate(need(p$xmin >= 0, 
                    "Highlighted portion is out of bounds on the x-axis"))
      validate(need(p$ymin >= 0, 
                    "Highlighted portion is out of bounds on the y-axis"))
      shinyImageFile$shiny_img_cropped <- croppedImage(shinyImageFile$shiny_img_final, p$xmin, p$ymin, p$xmax, p$ymax)
      shinyImageFile$shiny_img_final <- shinyImageFile$shiny_img_cropped
      output$plot1 <- renderPlot({
        EBImage::display(shinyImageFile$shiny_img_final, method = "raster")
      })
      session$resetBrush("plot_brush")
      shinyjs::enable("reset")
    })
    session$resetBrush("plot_brush")
    shinyjs::disable("segmentation")
  })
  
  observe({recursiveGrid()})
  
  recursiveGrid <- eventReactive(input$plot_brush,{
    isolate({
      p <- input$plot_brush
      output$plot1 <- renderPlot({
        EBImage::display(shinyImageFile$shiny_img_final, method = "raster")
        
        colcuts <- seq(p$xmin, p$xmax, length.out = input$hor + 1)
        rowcuts <- seq(p$ymin, p$ymax, length.out = input$ver + 1) 
        
        for (x in colcuts) {
          lines(x = rep(x, 2), y = c(p$ymin, p$ymax), col="red")
        }
        for (y in rowcuts) {
          lines(x = c(p$xmin, p$xmax), y = rep(y, 2), col="red")
        }
      })
      shinyjs::enable("reset")
      shinyjs::enable("segmentation")
    })
  })
  
  observe({recursiveSegmentation()})
  
  #only executes when Apply Segmentation is clicked
  recursiveSegmentation <- eventReactive(input$segmentation,{
    isolate({
      p <- input$plot_brush
      MAX <- dim(shinyImageFile$shiny_img_final)[1:2]
      colcuts <- seq(1, MAX[1], length.out = input$hor+1)
      rowcuts <- seq(1, MAX[2], length.out = input$ver+1)
      
      segmentation.list <- vector("list", length = input$hor)  
      count <- 0
      for(i in 1:input$hor){
        tmp.list <- vector("list", length = input$ver)
        for(j in 1:input$ver){
          img <- shinyImageFile$shiny_img_final
          if(length(dim(img)) == 2)
            img <- img[colcuts[i]:colcuts[i+1], rowcuts[j]:rowcuts[j+1]]
          if(length(dim(img)) == 3)
            img <- img[colcuts[i]:colcuts[i+1], rowcuts[j]:rowcuts[j+1], , drop = FALSE]
          tmp.list[[j]] <- img
        }
        segmentation.list[[i]] <- tmp.list
      }
      shinyImageFile$cropping_grid <- list("columns" = colcuts, "rows" = rowcuts)
      shinyImageFile$segmentation_list <- segmentation.list
      updateTabsetPanel(session, "tabs", selected = "tab2")
    })
  })
  
  ################# END OF THE FIRST TAB  
  
  
  
  ############### SECOND TAB
  
  observe({input$thresh})
  
  observe({input$channel})
  
  observe({recursiveThreshold()})
  
  #only executes when Segmentation is clicked
  recursiveThreshold <- eventReactive(input$threshold,{
    isolate({
      seg.list <- shinyImageFile$segmentation_list
      op <- par(no.readonly = TRUE)
      if(input$thresh == 1){
        Background.Threshold <- matrix(ncol=input$ver,nrow=input$hor)
        output$plot3 <- renderPlot({
          concat_img <- NULL
          for(i in 1:input$ver){
            row_segment <- NULL
            for(j in 1:input$hor){
              img <- seg.list[[i]][[j]]
              if(colorMode(img) > 0){
                img <- 1-channel(img, input$channel)
              }
              Background.Threshold[i,j] <- otsu(img)
              signal <- imageData(img) > Background.Threshold[i,j]
              row_segment <- EBImage::abind(row_segment,signal, along=2)
            }
            concat_img <- EBImage::abind(concat_img, row_segment, along=1)
          }
          display(concat_img, method="raster")
          shinyImageFile$Threshold <- Background.Threshold
        })
        shinyImageFile$Mean_Intensities <- matrix(0, ncol=input$hor*input$ver, nrow=1)
        shinyImageFile$Median_Intensities <- matrix(0, ncol=input$ver*input$hor, nrow=1)
        output$plot4 <- renderPlot({
          concat_img <- NULL
          count <- 0
          for(i in 1:input$ver){
            row_segment <- NULL
            for(j in 1:input$hor){
              count <- count + 1
              img <- seg.list[[i]][[j]]
              if(colorMode(img) > 0){
                img <- 1-channel(img, input$channel)
              }
              thr <- otsu(img)
              signal <- imageData(img) > thr
              imageData(img) <- (imageData(img) - thr)*signal
              row_segment <- EBImage::abind(row_segment, imageData(img), along=2)
              shinyImageFile$Mean_Intensities[1,count] <- mean(imageData(img)[signal])
              shinyImageFile$Median_Intensities[1,count] <- median(imageData(img)[signal])
              
            }
            concat_img <- EBImage::abind(concat_img, row_segment, along=1)
          }
          display(concat_img, method="raster")
        })
      }
      
      if(input$thresh == 2){
        Background <- vector(mode= "list", length=input$ver*input$hor)
        count <- 0
        for(i in 1:input$ver){
          for(j in 1:input$hor){
            count <- count + 1
            img <- seg.list[[i]][[j]]
            if(colorMode(img) > 0){
              img <- 1-channel(img, input$channel) 
            }            
            Background[[count]] <- as.numeric(imageData(img))
          }
        }
        Background.Threshold <- quantile(unlist(Background), 
                                         probs = input$quantile1/100)
        shinyImageFile$Threshold <- Background.Threshold
        output$plot3 <- renderPlot({
          concat_img <- NULL
          for(i in 1:input$ver){
            row_segment <- NULL
            for(j in 1:input$hor){
              img <- seg.list[[i]][[j]]
              if(colorMode(img) > 0){
                img <- 1-channel(img, input$channel)
              }
              signal <- imageData(img) > Background.Threshold
              row_segment <- EBImage::abind(row_segment,signal, along=2)
            }
            concat_img <- EBImage::abind(concat_img, row_segment, along=1)
          }
          display(concat_img, method="raster")
        })
        shinyImageFile$Mean_Intensities <- matrix(0, ncol=input$hor*input$ver, nrow=1)
        shinyImageFile$Median_Intensities <- matrix(0, ncol=input$ver*input$hor, nrow=1)
        output$plot4 <- renderPlot({
          concat_img <- NULL
          count <- 0
          for(i in 1:input$ver){
            row_segment <- NULL
            for(j in 1:input$hor){
              count <- count+1
              img <- seg.list[[i]][[j]]
              if(colorMode(img) > 0){
                img <- 1-channel(img, input$channel)
              }
              signal <- imageData(img) > Background.Threshold
              imageData(img) <- (imageData(img) - Background.Threshold)*signal
              row_segment <- EBImage::abind(row_segment,imageData(img), along=2)
              shinyImageFile$Mean_Intensities[1,count] <- mean(imageData(img)[signal])
              shinyImageFile$Median_Intensities[1,count] <- median(imageData(img)[signal])
            }
            concat_img <- EBImage::abind(concat_img, row_segment, along=1)
          }
          display(concat_img, method="raster")
        })
      }
    })
  })
  
  recursiveData <- eventReactive(input$data,{
    isolate({
      AM <- shinyImageFile$Mean_Intensities
      colnames(AM) <- paste0("Mean", 1:input$hor)
      Med <- shinyImageFile$Median_Intensities
      colnames(Med) <- paste0("Median", 1:input$hor)
      if(input$thresh == 2){
        BG.method <- matrix(c("Otsu", NA), nrow = 1, 
                            ncol = 2, byrow = TRUE)
        colnames(BG.method) <- c("Background", "Probability")
      }
      if(input$thresh == 1){ 
        BG.method <- matrix(c("quantile", input$quantile1), 
                            nrow = 1, ncol = 2, byrow = TRUE)
        colnames(BG.method) <- c("Background", "Probability")
      }
      
      DF <- data.frame("Date" = input$testdate,
                       "File" = shinyImageFile$filename,
                       BG.method, AM, Med, 
                       check.names = FALSE)
      
      all_colnames <- colnames(DF)
      
      table_info <- all_colnames[1:5]
      mean_colnames <- all_colnames[grepl("Mean", all_colnames, fixed=TRUE)]
      median_colnames <- all_colnames[grepl("Median", all_colnames, fixed=TRUE)]
      
      mean_table <- gather(DF[mean_colnames], mean, mean)
      median_table <- gather(DF[median_colnames], median, median)
      
      DF <- cbind(mean_table, median_table)
      DF <- subset(DF, select= c(-1,-3))
      
      letters <- LETTERS
      letters <- substr(letters, 1, input$ver)
      numbers <- 1:input$hor
      
      pos <- NULL
      for (i in unlist(strsplit(letters, split=""))) {
        for (j in numbers) {
          pos <- c(pos, paste(i,j, sep=""))
        }
      }
      
      date <- data.frame("date" = rep(input$testdate, nrow(DF)))
      
      DF <- cbind(date, pos, DF)
      
      
      if(inherits(try(IntensData, silent = TRUE), "try-error"))
        IntensData <<- DF
      else
        IntensData <<- rbind(IntensData, DF)
      
      output$intens <- renderDT({
        DF <- IntensData
        datatable(DF)
      })
      output$plot3 <- renderPlot({})
      output$plot4 <- renderPlot({})
      if(!is.null(shinyImageFile$Threshold))
        shinyImageFile$Threshold <- NULL
      if(!is.null(shinyImageFile$Mean_Intensities))
        shinyImageFile$Mean_Intensities <- NULL
      if(!is.null(shinyImageFile$Median_Intensities))
        shinyImageFile$Median_Intensities <- NULL
    })
  })  
  observe({recursiveData()})
  
  recursiveData <- eventReactive(input$data,{
    isolate({
      AM <- shinyImageFile$Mean_Intensities
      colnames(AM) <- paste0("Mean", 1:(input$hor*input$ver))
      Med <- shinyImageFile$Median_Intensities
      colnames(Med) <- paste0("Median", 1:(input$hor*input$ver))
      if(input$thresh == 1){
        BG.method <- matrix(c("Otsu", NA), nrow = 1, 
                            ncol = 2, byrow = TRUE)
        colnames(BG.method) <- c("Background", "Probability")
      }
      if(input$thresh == 2){ 
        BG.method <- matrix(c("quantile", input$quantile1), 
                            nrow = 1, ncol = 2, byrow = TRUE)
        colnames(BG.method) <- c("Background", "Probability")
      }
      seg.list <- shinyImageFile$segmentation_list
      img <- seg.list[[1]][[1]]
      if(colorMode(img) > 0){
        MODE <- input$channel
        DF <- data.frame("Date" = input$testdate,
                         "File" = shinyImageFile$filename,
                         "Mode" = MODE,
                         #"Strip" = input$selectStrip,
                         BG.method, AM, Med, 
                         check.names = FALSE)
      }else{
        DF <- data.frame("Date" = input$testdate,
                         "File" = shinyImageFile$filename,
                         "Mode" = NA,
                         #"Strip" = input$selectStrip,
                         BG.method, AM, Med, 
                         check.names = FALSE)
      }
      
      all_colnames <- colnames(DF)
      
      table_info <- all_colnames[1:5]
      mean_colnames <- all_colnames[grepl("Mean", all_colnames, fixed=TRUE)]
      median_colnames <- all_colnames[grepl("Median", all_colnames, fixed=TRUE)]
      
      mean_table <- gather(DF[mean_colnames], mean, mean)
      median_table <- gather(DF[median_colnames], median, median)
      
      DF <- cbind(mean_table, median_table)
      DF <- subset(DF, select= c(-1,-3))
      
      letters <- "ABCDEFGFIJKLMNOPQRSTUVWXYZ"
      letters <- substr(letters, 1, input$ver)
      numbers <- 1:input$hor
      
      pos <- NULL
      for (i in unlist(strsplit(letters, split=""))) {
        for (j in numbers) {
          pos <- c(pos, paste(i,j, sep=""))
        }
      }
      
      date <- data.frame("date" = rep(input$testdate, nrow(DF)))
      
      DF <- cbind(date, pos, DF)
      
      if(inherits(try(IntensData, silent = TRUE), "try-error"))
        IntensData <<- DF
      else
        IntensData <<- rbind(IntensData, DF)
      
      output$intens <- renderDT({
        DF <- IntensData
        datatable(DF)
      })
      output$plot3 <- renderPlot({})
      output$plot4 <- renderPlot({})
      if(!is.null(shinyImageFile$Threshold))
        shinyImageFile$Threshold <- NULL
      if(!is.null(shinyImageFile$Mean_Intensities))
        shinyImageFile$Mean_Intensities <- NULL
      if(!is.null(shinyImageFile$Median_Intensities))
        shinyImageFile$Median_Intensities <- NULL
    })
  })
  
  observe({recursiveShowIntensData()})
  recursiveShowIntensData <- eventReactive(input$showIntensData,{
    isolate({
      updateTabsetPanel(session, "tabs", selected = "tab3")
    })
  })
  
  observeEvent(input$intensFile,{
    output$intens <- renderDT({})
    suppressWarnings(rm(IntensData, pos = 1))
  })
  
  observe({recursiveUploadIntens()})
  recursiveUploadIntens <- eventReactive(input$intensFile,{
    isolate({
      req(input$intensFile)
      tryCatch(
        DF <- read.csv(input$intensFile$datapath, header = TRUE, 
                       check.names = FALSE),
        error = function(e){stop(safeError(e))}
      )
      IntensData <<- DF
      output$intens <- renderDT({
        datatable(DF)
      })
    })
  })
  
  observe({recursiveDelete()})
  recursiveDelete <- eventReactive(input$deleteData,{
    isolate({
      IntensData <<- NULL
    })
  })
  
  
  observe({recursiveRefresh()})
  recursiveRefresh <- eventReactive(input$refreshData,{
    isolate({
      output$intens <- renderDT({
        DF <- IntensData
        datatable(DF)
      })
    })
  })
  
  ##HELPFUL TEXTS##
  
  #creates the textbox below plot2 about the plot_brush details and etc
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    
    paste0(
      "click: ", xy_str(input$plot_click),
      "dblclick: ", xy_str(input$plot_dblclick),
      "hover: ", xy_str(input$plot_hover),
      "brush: ", xy_range_str(input$plot_brush)
    )
  })
  output$thresh <- renderText({
    if(!is.null(shinyImageFile$Threshold))
      paste0("Threshold(s): ", paste0(signif(shinyImageFile$Threshold, 4), collapse = ", "))
  })
  output$meanIntens <- renderText({
    if(!is.null(shinyImageFile$Threshold))
      paste0("Mean intensities: ", paste0(signif(shinyImageFile$Mean_Intensities, 4), collapse = ", "))
  })
  output$medianIntens <- renderText({
    if(!is.null(shinyImageFile$Threshold))
      paste0("Median intensities: ", paste0(signif(shinyImageFile$Median_Intensities, 4), collapse = ", "))
  })
  output$intens <- renderDT({
    DF <- IntensData
    datatable(DF)
  })
  output$folder <- renderPrint({
    paste0("Folder for Results: ", parseDirPath(c(wd=fs::path_home()), input$folder))
  })
  
  
  ##DOWNLOAD##
  
  #allows user to download data
  output$downloadData <- downloadHandler(
    filename = "IntensityData.csv",
    content = function(file) {
      write.csv(IntensData, file, row.names = FALSE)
    }
  )
  
  ##STOP SHINY APP## 
  #When user clicks the return to command line button
  #stops the shiny app
  # prevents user from quitting shiny using ^C on commandline 
  observe({recursiveStop()})
  
  recursiveStop <- eventReactive(input$stop,{
    isolate({
      suppressWarnings(rm(IntensData, pos = 1))
      suppressWarnings(rm(ExpInfo, pos = 1))
      suppressWarnings(rm(MergedData, pos = 1))
      suppressWarnings(rm(CalibrationData, pos = 1))
      stopApp()
    })
  })
  ####################################################################
  # TIMESERIES MODULE
  ####################################################################
  #
  # LOOP FOR GENERATING TABS
  
  observe({recursiveUploadIntens()})
  recursiveUploadIntens <- eventReactive(input$intensFile,{
    isolate({
      req(input$intensFile)
      tryCatch(
        DF <- read.csv(input$intensFile$datapath, header = TRUE,
                       check.names = FALSE),
        error = function(e){stop(safeError(e))}
      )
      IntensData <<- DF
      output$intens <- renderDT({
        datatable(DF)
      })
    })
  })
  
  
  
  observe({timetables()})
  
  timetables <- eventReactive(input$readIntensData, {
    isolate({
      # LOADING of intensity data
      
      intensityData <- IntensData      
      
      # LOOP FOR GENERATING TABS
      output$subtabs <- renderUI({
        do.call(tabsetPanel, c(id='t',lapply(unique(intensityData$date), function(i) {
          tabPanel(
            title=paste0(i), 
            DTOutput(paste0('date-',i))
          )
        })))
      })
      
      tables <- list()
      
      for(d in unique(intensityData$date)) {
        foo <- subset(intensityData, date==d)
        bar <- matrix(foo$mean, ncol=19, nrow=19)
        
        tables[[paste0(d)]] <- bar
      }
      
      lapply(unique(intensityData$date), function(name){
        output[[paste0("date-",name)]] <- renderDT({
          table1 <- data.frame(tables[[paste0(name)]])
          colnames(table1) <- LETTERS[1:ncol(table1)]
          datatable(data=table1, options=list(pageLength=19),
                    selection = list(target = 'cell'))
        })
      })
      
      generatePlotLines <- function(table_list, coor) {
        p_lines <- list()
        for(n in names(table_list)){
          for(i in 1:nrow(coor)) {
            row <- coor[i,][1]
            col <- coor[i,][2]
            p_lines[[paste0(LETTERS[col], row)]] <- c(p_lines[[paste0(LETTERS[col], row)]],table_list[[n]][row,col])
          }
        }
        return(p_lines)
      }
      
      observe({reactivePlotting()})
      
      reactivePlotting <- eventReactive(input$plotme, {
        isolate({
          updateTabsetPanel(session, "tabs", selected = "plotly")
          
          output$plotly <- plotly::renderPlotly({
            coor <- input[[paste0("date-", unique(intensityData$date)[1], "_cells_selected")]]
            validate(need(!is.na(coor[1]), "Please select a cell on previous tab."))
            validate(need(!any(coor==0), "ERROR: Invalid cell selected."))  
            p_lines <- generatePlotLines(tables, coor) 
            data <- list()
            
            for(n in names(p_lines)) {
              data[[paste0(n)]] <- p_lines[[n]]
            }
            x <- factor(unique(intensityData$date))
            data <- data.frame(data)
            data <- cbind(x,data)
            
            
            fig <- plot_ly(type="scatter")
            
            for(i in names(p_lines)) {
              # fig <- fig %>% plotly::add_trace(data=data, x = ~x, y = as.formula(paste0("~`", i, "`")), name = i, mode="scatter")
              fig <- fig %>% plotly::add_trace(data=data, x = ~x, y = data[[i]], name = i, mode="scatter")
            }
            
            a <- list(
              title = "Timeline",
              showticklabels = TRUE,
              tickangle = 45,
              exponentformat = "E"
            )
            
            y <- list(
              title = "Intensities"
            )
            
            fig <- fig %>% layout(xaxis = a, yaxis = y)
            fig
          })
        })
      })
    })
  })
  
  
}
