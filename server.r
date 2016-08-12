# ==========================================================================
# Phylosophy: the 'htm' data.frame is placed in the global enviromnent, where it can be accessed and updated by reactive functions
#
# These are the columns being added to the 'htm' data.frame:
# 
# - HTM_batch_treatment
# - HTM_QC
# - HTM_heatmapX
# - HTM_heatmapY
#
# - HTM_norm__z_score__Median_cell_final_Math_ratio
#
# ==========================================================================


source("dependencies.r")
source("functions.r")

# Adjust maximum upload size to 2 Gb
options(shiny.maxRequestSize=2*1024^3)



shinyServer(function(input, output){
    
    # File Input
    sourcefile <- reactive(input$file1$datapath)
    htm <- reactive(read.csv(sourcefile(), stringsAsFactors = FALSE))

    
    # Plot variables
    output$selectX <- renderUI({
        switch(input$plotType,
               "Scatter plot" = return(selectInput("Xaxis", "X axis:", as.list(names(htm())))),
               "Boxplot"      = return(selectInput("Xaxis", "Categories:", as.list(names(htm())))),
               "Heatmap"      = return(NULL)
        )
    })
    output$selectY <- renderUI({
        switch(input$plotType,
               "Scatter plot" = return(selectInput("Yaxis", "Y axis:", as.list(names(htm())))),
               "Boxplot"      = return(selectInput("Yaxis", "Values:", as.list(names(htm())))),
               "Heatmap" =      return(selectInput("Yaxis", "Values:", as.list(names(htm()))))
        )
    })
    output$selectBatch <- renderUI({
        selectInput("batch", "Show this batch:", as.list(c("All batches",unique(htm()[[input$colBatch]]))))
    })
    
    
    # Settings
    output$colNameTreatment <- renderUI({
        selectInput("colTreatment", "Treatment:", as.list(names(htm())))
    })
    output$colBatchName     <- renderUI({
        selectInput("colBatch", "Batch:", as.list(names(htm())))
    })
    output$colNameWell      <- renderUI({
        selectInput("colWell", "Well coordinate:", as.list(names(htm())))
    })
    output$colNamePos       <- renderUI({
        selectInput("colPos", "Sub-position coordinate:", as.list(names(htm())))
    })
    output$fiji_path        <- renderUI({
        if (Sys.info()['sysname'] == "Windows"){
            return(textInput("fiji_binary", "Path to Fiji (only necessary for Windows)", value = "C:/Fiji.app/ImageJ-win64.exe"))
        } else {
                return("/Applications/Fiji.app/Contents/MacOS/ImageJ-macosx")
        }
    })
    output$availableimages  <- renderUI({
        img_names <- gsub(paste0("^", input$prefixPath, "(.*)"), "\\1", names(htm())[grep(paste0("^", input$prefixPath), names(htm()))])
        checkboxGroupInput("images2display", "Select images to be viewed upon clicking within a plot", as.list(img_names))
    })
    

    # Data table
    output$valuestable <- renderDataTable(htm())
    
    
    # Heatmap-specific settings
    hm1 <<- reactive({hmcoords <- heatmapCoord(WellX = input$wells_X,
                                                 WellY = input$wells_Y,
                                                 PosX = input$npos_X,
                                                 PosY = input$npos_Y,
                                                 subposjitter = input$squaredodge)
            })

    hm2 <<- reactive({addHeatmapCoordinates(htm(), hm1(), input$colBatch, input$batch, input$colWell, input$colPos)})
    
    # Plot
    output$plot <- renderPlotly({
        switch(input$plotType,
               "Scatter plot" = pointPlot(htm(), input$colBatch, input$batch, input$Xaxis, input$Yaxis),
               "Boxplot"      = boxPlot(htm(), input$colBatch, input$batch, input$Xaxis, input$Yaxis),
               "Heatmap"      = heatmapPlot(hm2(), input$Yaxis, input$batch, input$wells_Y, input$wells_X, input$squaresize)
        )
    })
    
    
    # Plot-Fiji interaction
    output$selection <- renderPrint({
        s <- event_data("plotly_click")
        if (length(s) == 0) {
            "Click on a data point to open images!"
        } else {
            print("You selected:")
            print(s)
            i = s[["pointNumber"]]
            
            openTheseImgChannels <- input$images2display
            
            tempPathInTable    <- gsub("\\\\", "/", input$pathInTable)
            tempPathInComputer <- gsub("\\\\", "/", input$pathInComputer)
            tempFullPathName <- paste0(htm()[i, paste0(input$prefixPath, input$images2display)], "/", htm()[i, paste0(input$prefixFile, input$images2display)])
            tempFullPathName <- gsub("\\\\", "/", tempFullPathName)
            FullPathFile <- sub(tempPathInTable, tempPathInComputer, tempFullPathName, ignore.case = TRUE)

            print("Launching Fiji now....")
            print(FullPathFile)
            OpenInFiji(FullPathFile, input$fiji_binary)
            
        }
    })
    
})