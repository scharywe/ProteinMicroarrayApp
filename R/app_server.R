#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny 
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
    options(shiny.maxRequestSize=50*1024^2) 
    shinyImageFile <- reactiveValues(shiny_img_origin = NULL, Threshold = NULL)

    ##INPUT##
    
    #checks radio for file input
    observe({
      if(input$radio == 1)
      {
        shinyjs::reset('file1') #allows plot1 to be null when radio is clicked
        # creates a warning to upload correct file 
        # otherwise outputs image
        output$plot1 <- renderPlot({ 
          validate(need(!is.null(input$file1), "Must upload a valid jpg, png, or tiff"))
          if (is.null(input$file1))
            return(NULL)
        })
      }
      
      if(input$radio == 2){
        # using sample image
        img <- readImage(system.file("images", "Sample.jpeg", package="ProteinMicroarrayApp"))
        shinyImageFile$shiny_img_origin <- img
        
        shinyImageFile$filename <- "Sample.jpeg"
        #outputs image to plot1 -- main plot
        output$plot1 <- renderPlot({ EBImage::display(img, method = "raster") })
      }
    }) # end of observe
    
    # second observe
    # looks at its first 3 lines and observes those inputs
    # resets the sliders
    observe({
      #if user clicks a new radio button, uploads new file, or url
      #the sliders will change
      #and the brush will default 
      input$file1
      input$radio
      
      
      # update sliders to initial values 
      # and reset plot brush
      
      #updateRadioButtons(session, "color", selected = 1)
      session$resetBrush("plot_brush")
    })
    
    
    ##RADIO BUTTONS##
    
    #when user uploads file
    #the datapath is different from the one needed to properly recognize photo
    #so this function renames the file 
    renameUpload <- function(inFile) {
      if(is.null(inFile))
        return(NULL)
      
      oldNames = inFile$datapath
      newNames = file.path(dirname(inFile$datapath), inFile$name)
      file.rename(from = oldNames, to = newNames)
      inFile$datapath <- newNames
      
      return(inFile$datapath)
    }
    
    #if they enter a new file, their file will become the new imageFile
    observeEvent(input$file1, {
      # reseting plots and text messages
      output$plot3 <- renderPlot({})
      output$plot4 <- renderPlot({})
      if(!is.null(shinyImageFile$Threshold))
        shinyImageFile$Threshold <- NULL
      if(!is.null(shinyImageFile$Mean_Intensities))
        shinyImageFile$Mean_Intensities <- NULL
      if(!is.null(shinyImageFile$Median_Intensities))
        shinyImageFile$Median_Intensities <- NULL
      
      shinyImageFile$filename <- input$file1$name
      img <- readImage(renameUpload(input$file1))
      shinyImageFile$shiny_img_origin <- img
      output$plot1 <- renderPlot({EBImage::display(img, method = "raster")})
    })
    
    
    
    ##CROPPING AND PLOTS##
    
    #prompts shiny to look at recursive crop
    observe({
      recursiveCrop()
    })
    
    #only executes when cropping is clicked 
    recursiveCrop <- eventReactive(input$crop,{
      isolate({
        p <- input$plot_brush
        img <- shinyImageFile$shiny_img_origin
        if(length(dim(img)) == 2)
          shinyImageFile$shiny_img_origin <- img[p$xmin:p$xmax, p$ymin:p$ymax, drop = FALSE]
        if(length(dim(img)) == 3)
          shinyImageFile$shiny_img_origin <- img[p$xmin:p$xmax, p$ymin:p$ymax, , drop = FALSE]
        output$plot1 <- renderPlot({
          EBImage::display(shinyImageFile$shiny_img_origin, method = "raster")
          
          MAX <- dim(shinyImageFile$shiny_img_origin)[1:2]
          colcuts <- seq(1, MAX[1], length.out = input$hor+1)
          rowcuts <- seq(1, MAX[2], length.out = input$ver+1)
          
          for (x in colcuts) {
            lines(x = rep(x, 2), y = c(1, MAX[2]), col="blue")
          }
          for (y in rowcuts) {
            lines(x = c(1, MAX[1]), y = rep(y, 2), col="blue")
          }
        })
      })
      session$resetBrush("plot_brush")
    })
    
    
    #hides the cropping button in the instance in which the user highlighted the plot
    #then clicks on the side so that the brush plot disappears
    #if user clicks cropping without a valid brush, there will be errors
    #so we need to hide the button
    observeEvent(is.null(input$plot_brush), {
      shinyjs::hide("crop")
    })
    
    #creates a clone of the image in the main image viewer
    #shows the user a preview of the cropped image 
    #since shinyimg saves every image that is edited, we use a clone
    #so that we aren't saving any of the previews
    #till the user clicks crop
    croppedShiny <- function(image)
    {
      p <- input$plot_brush
      validate(need(p != 'NULL', 
                    "Highlight a portion of the photo to see a cropped version!"))
      validate(need(p$xmax <= dim(shinyImageFile$shiny_img_origin)[1], 
                    "Highlighted portion is out of bounds on the x-axis of your image 1"))
      validate(need(p$ymax <= dim(shinyImageFile$shiny_img_origin)[2], 
                    "Highlighted portion is out of bounds on the x-axis of your image 1"))
      validate(need(p$xmin >= 0, 
                    "Highlighted portion is out of bounds on the x-axis of your image 2"))
      validate(need(p$ymin >= 0, 
                    "Highlighted portion is out of bounds on the y-axis of your image 2"))
      
      preview <- shinyImageFile$shiny_img_origin
      if(length(dim(preview)) == 2)
        preview <- preview[p$xmin:p$xmax, p$ymin:p$ymax, drop = FALSE]
      if(length(dim(preview)) == 3)
        preview <- preview[p$xmin:p$xmax, p$ymin:p$ymax, , drop = FALSE]
      EBImage::display(preview, method = "raster")
      
      MAX <- dim(preview)[1:2]
      colcuts <- seq(1, MAX[1], length.out = input$hor+1)
      rowcuts <- seq(1, MAX[2], length.out = input$ver+1)
      
      for (x in colcuts) {
        lines(x = rep(x, 2), y = c(1, MAX[2]), col="blue")
      }
      for (y in rowcuts) {
        lines(x = c(1, MAX[1]), y = rep(y, 2), col="blue")
      }
    }
    
    
    #shows a preview of the cropped function
    #shows the cropping button (originally hiding) 
    output$plot2 <- renderPlot({
      p <- input$plot_brush
      validate(need(p != 'NULL', 
                    "Highlight a portion of the photo to see a cropped version!"))
      validate(need(p$xmax <= dim(shinyImageFile$shiny_img_origin)[1], 
                    "Highlighted portion is out of bounds on the x-axis of your image 1"))
      validate(need(p$ymax <= dim(shinyImageFile$shiny_img_origin)[2], 
                    "Highlighted portion is out of bounds on the y-axis of your image 1"))
      validate(need(p$xmin >= 0, 
                    "Highlighted portion is out of bounds on the x-axis of your image 2"))
      validate(need(p$ymin >= 0, 
                    "Highlighted portion is out of bounds on the y-axis of your image 2"))
      
      croppedShiny(shinyImageFile$shiny_img_origin)
      
      shinyjs::show("crop")
      shinyjs::show("segmentation")
      
    })
    
    
    ##SEGMENTATION AND THRESHOLDING##
    
    observe({recursiveSegmentation()})
    
    
    #only executes when Segmentation is clicked
    recursiveSegmentation <- eventReactive(input$segmentation,{
      isolate({
        MAX <- dim(shinyImageFile$shiny_img_origin)[1:2]
        colcuts <- seq(1, MAX[1], length.out = input$hor+1)
        rowcuts <- seq(1, MAX[2], length.out = input$ver+1)
        
        segmentation.list <- vector("list", length = input$hor)  
        count <- 0
        for(i in 1:input$hor){
          tmp.list <- vector("list", length = input$ver)
          for(j in 1:input$ver){
            img <- shinyImageFile$shiny_img_origin
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
                  if(input$channel == 1)
                    img <- 1-channel(img, "luminance")
                  if(input$channel == 2)
                    img <- 1-channel(img, "gray")
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
                  if(input$channel == 1)
                    img <- 1-channel(img, "luminance")
                  if(input$channel == 2)
                    img <- 1-channel(img, "gray")
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
                if(input$channel == 1)
                  img <- 1-channel(img, "luminance")
                if(input$channel == 2)
                  img <- 1-channel(img, "gray")
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
                  if(input$channel == 1)
                    img <- 1-channel(img, "luminance")
                  if(input$channel == 2)
                    img <- 1-channel(img, "gray")
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
                  if(input$channel == 1)
                    img <- 1-channel(img, "luminance")
                  if(input$channel == 2)
                    img <- 1-channel(img, "gray")
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
        DF <- data.frame("File" = shinyImageFile$filename,
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
        
        letters <- "ABCDEFGFIJKLMNOPQRSTUVWXYZ"
        letters <- substr(letters, 1, input$ver)
        numbers <- 1:input$hor
        
        pos <- NULL
        for (i in unlist(strsplit(letters, split=""))) {
          for (j in numbers) {
            pos <- c(pos, paste(i,j, sep=""))
          }
        }
        
        DF <- cbind(pos, DF)
        
        if(inherits(try(IntensData, silent = TRUE), "try-error"))
          IntensData <<- stack(DF)
        else
          IntensData <<- cbind(IntensData, DF)
        
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
          if(input$channel == 1) MODE <- "luminance"
          if(input$channel == 2) MODE <- "gray"
          #if(input$channel == 3) MODE <- "red"
          #if(input$channel == 4) MODE <- "green"
          #if(input$channel == 5) MODE <- "blue"
          DF <- data.frame("File" = shinyImageFile$filename,
                           "Mode" = MODE,
                           #"Strip" = input$selectStrip,
                           BG.method, AM, Med, 
                           check.names = FALSE)
        }else{
          DF <- data.frame("File" = shinyImageFile$filename,
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
        
        DF <- cbind(pos, DF)
        
        if(inherits(try(IntensData, silent = TRUE), "try-error"))
          IntensData <<- stack(DF)
        else
          IntensData <<- cbind(IntensData, DF)
        
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
        suppressWarnings(rm(IntensData, pos = 1))
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
    
    
}
