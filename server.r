# ==========================================================================
# Phylosophy: the 'htm' data.frame is placed in the global enviromnent, where it can be accessed and updated by reactive functions
#
# Heatmaps are built from a reactive data.frame 'htmHM()'
#
# These are the columns being added to the 'htm' data.frame:
# 
# - HTM_QC
# - HTM_norm__z_score__Median_cell_final_Math_ratio
#
# ==========================================================================


source("dependencies.r")
source("functions.r")

# Adjust maximum upload size to 2 Gb
options(shiny.maxRequestSize=2*1024^3)

# Initialize variables
QCsettings <<- data.frame(type = character(), 
                         measurement = character(), 
                         minimum = character(), 
                         maximum = character(),
                         stringsAsFactors = FALSE)


shinyServer(function(input, output){
    
    # Read input file
    # Initialize the 'master' htm data frame as a global variable
    observeEvent(input$file1, {
        htm <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
        htm$HTM_QC <- TRUE
        htm <<- htm
    })
    
    
    # Plot variables
    output$UIselectXaxis <- renderUI({
        input$file1
        switch(input$plotType,
               "Scatter plot" = return(selectInput("Xaxis", "X axis:", as.list(names(htm)))),
               "Boxplot"      = return(selectInput("Xaxis", "Categories:", as.list(names(htm)))),
               "Heatmap"      = return(NULL)
        )
    })
    output$UIselectYaxis <- renderUI({
        input$file1
        switch(input$plotType,
               "Scatter plot" = return(selectInput("Yaxis", "Y axis:", as.list(names(htm)))),
               "Boxplot"      = return(selectInput("Yaxis", "Values:", as.list(names(htm)))),
               "Heatmap" =      return(selectInput("Yaxis", "Values:", as.list(names(htm))))
        )
    })
    output$UIselectBatch <- renderUI({
        input$file1
        selectInput("batch", "Show this batch:", as.list(c("All batches",unique(htm[[input$colBatch]]))))
    })
    
    
    # Settings
    output$UIcolNameTreatment <- renderUI({
        input$file1
        selectInput("colTreatment", "Treatment:", as.list(names(htm)))
    })
    output$UIcolNameBatch     <- renderUI({
        input$file1
        selectInput("colBatch", "Batch:", as.list(names(htm)))
    })
    output$UIcolNameWell      <- renderUI({
        input$file1
        selectInput("colWell", "Well coordinate:", as.list(names(htm)))
    })
    output$UIcolNamePos       <- renderUI({
        input$file1
        selectInput("colPos", "Sub-position coordinate:", as.list(names(htm)))
    })
    output$UIfiji_path        <- renderUI({
        if (Sys.info()['sysname'] == "Windows"){
            return(textInput("fiji_binary", "Path to Fiji (only necessary for Windows)", value = "C:/Fiji.app/ImageJ-win64.exe"))
        } else {
                return("/Applications/Fiji.app/Contents/MacOS/ImageJ-macosx")
        }
    })
    output$UIavailableimages  <- renderUI({
        input$file1
        img_names <- gsub(paste0("^", input$prefixPath, "(.*)"), "\\1", names(htm)[grep(paste0("^", input$prefixPath), names(htm))])
        checkboxGroupInput("images2display", "Select images to be viewed upon clicking within a plot", as.list(img_names))
    })
    

    # Data table
    observeEvent(input$file1, {
        output$valuestable <- renderDataTable(htm)
    })

    
    # Heatmap-specific settings
    htmHM <<- reactive({
        if(input$plotType != "Heatmap") return(NULL)
        if(input$batch == "All batches") return(NULL)
        
        makeHeatmapDataFrame(df  = htm, 
                             WellX        = input$wells_X,
                             WellY        = input$wells_Y, 
                             PosX         = input$npos_X, 
                             PosY         = input$npos_Y, 
                             subposjitter = input$squaredodge, 
                             batch_col    = input$colBatch, 
                             batch        = input$batch, 
                             col_Well     = input$colWell, 
                             col_Pos      = input$colPos, 
                             col_QC       = "HTM_QC")
        })
    

    # QC-specific settings
    output$UIQCfailedExperiments <- renderUI({
        input$file1
        fluidRow(
            column(2,
                strong("Failed experiments")
            ),
            column(3,
                selectInput("QCfailedExperiment", "Failed experiments:", htm[[input$colBatch]])
            ),
            column(2,
                NULL
            ),
            column(2,
                   NULL
            ),
            column(1,
                   actionButton("QCAddfailedExperiments", "Add QC", icon = icon("plus-square"), width = "100px")
            ),
            tags$style(type='text/css', "#QCAddfailedExperiments { width:100%; margin-top: 25px;}")
        )
    })
    output$UIQCnumeric           <- renderUI({
        input$file1
        fluidRow(
            column(2,
                strong("Number-based QC")
            ),
            column(3,
                selectInput("QCnumMeasurement", "Measurement:", as.list(names(htm)))
            ),
            column(2,
                numericInput("QCnumMin", "Minimum:", value=1)
            ),
            column(2,
                numericInput("QCnumMax", "Maximum:", value=100)
            ),
            column(1,
                actionButton("QCAddnumeric", "Add QC", icon = icon("plus-square"), width = "100px")
            ),
            tags$style(type='text/css', "#QCAddnumeric { width:100%; margin-top: 25px;}")
        )
    })
    output$UIQCtext              <- renderUI({
        input$file1
        fluidRow(
            column(2,
                   strong("Text-based QC")
            ),
            column(3,
                   selectInput("QCtxtMeasurement", "Measurement:", as.list(names(htm)))
            ),
            column(2,
                   textInput("QCtxtBad", "Value:")
            ),
            column(2,
                   NULL
            ),
            column(1,
                   actionButton("QCAddtxt", "Add QC", icon = icon("plus-square"), width = "100px")
            ),
            tags$style(type='text/css', "#QCAddtxt { width:100%; margin-top: 25px;}")
        )
    })

    observeEvent(input$QCAddfailedExperiments,{
        temp <- data.frame(
            type        = "Failed experiment",
            measurement = isolate(input$colBatch),
            minimum     = isolate(input$QCfailedExperiment),
            maximum     = isolate(input$QCfailedExperiment)
        )
        QCsettings <<- rbind(QCsettings, temp)
        
        # Update show/remove QC
        output$QCtable    <- renderTable(QCsettings)
        output$UIQCactive <- renderUI({
            checkboxGroupInput("QCcheckGroup",
                               label = strong("Disable this QC"), 
                               choices = as.list(row.names(QCsettings))
            )
        })
    })
    observeEvent(input$QCAddnumeric,{
        temp <- data.frame(
                    type        = "Numeric QC",
                    measurement = isolate(input$QCnumMeasurement),
                    minimum     = as.character(isolate(input$QCnumMin)),
                    maximum     = as.character(isolate(input$QCnumMax))
                )
        QCsettings <<- rbind(QCsettings, temp)
        
        # Update show/remove QC
        output$QCtable    <- renderTable(QCsettings)
        output$UIQCactive <- renderUI({
            checkboxGroupInput("QCcheckGroup",
                               label = strong("Disable this QC"), 
                               choices = as.list(row.names(QCsettings))
            )
        })
    })
    observeEvent(input$QCAddtxt,{
        temp <- data.frame(
            type        = "Text QC",
            measurement = isolate(input$QCtxtMeasurement),
            minimum     = isolate(input$QCtxtBad),
            maximum     = isolate(input$QCtxtBad)
        )
        QCsettings <<- rbind(QCsettings, temp)
        
        # Update show/remove QC
        output$QCtable    <- renderTable(QCsettings)
        output$UIQCactive <- renderUI({
            checkboxGroupInput("QCcheckGroup",
                               label = strong("Disable this QC"), 
                               choices = as.list(row.names(QCsettings))
            )
        })
    })    
    observeEvent(input$QCcheckGroup, {
        QCsettings <<- QCsettings[row.names(QCsettings) != input$QCcheckGroup,]
        
        # Update show/remove QC
        output$QCtable    <- renderTable(QCsettings)
        output$UIQCactive <- renderUI({
            checkboxGroupInput("QCcheckGroup",
                               label = strong("Disable this QC"), 
                               choices = as.list(row.names(QCsettings))
            )
        })
    })
    observeEvent(input$applyQC,{
        
        if(nrow(QCsettings) != 0){
            temp <- applyQC(htm, QCsettings)
            htm$HTM_QC <- temp
            htm <<- htm   
        }
    })
    
    
    # Plot
    output$plot <- renderPlotly({
        switch(input$plotType,
               "Scatter plot" = pointPlot(htm, input$colBatch, input$batch, input$Xaxis, input$Yaxis),
               "Boxplot"      = boxPlot(htm, input$colBatch, input$batch, input$Xaxis, input$Yaxis),
               "Heatmap"      = heatmapPlot(htmHM(), input$Yaxis, input$batch, input$wells_Y, input$wells_X, input$squaresize)
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
            tempFullPathName <- paste0(htm[i, paste0(input$prefixPath, input$images2display)], "/", htm[i, paste0(input$prefixFile, input$images2display)])
            tempFullPathName <- gsub("\\\\", "/", tempFullPathName)
            FullPathFile <- sub(tempPathInTable, tempPathInComputer, tempFullPathName, ignore.case = TRUE)

            print("Launching Fiji now....")
            print(FullPathFile)
            OpenInFiji(FullPathFile, input$fiji_binary)
            
        }
    })
    
    
    # Normalization settings
    output$UINormFeatures      <- renderUI({
        input$file1
        input$applySummary
        selectInput("NormFeatures", "Data features to be analyzed", choices = as.list(names(htm)), multiple = TRUE)
    })
    output$UINormDataTransform <- renderUI({
        input$file1
        selectInput("NormDataTransform", "Data transformation", choices = list("None selected", "log2"))
    })
    output$UINormGradientCorr  <- renderUI({
        input$file1
        selectInput("NormGradientCorr", "Batch-wise spatial gradient correction", choices = list("None selected", "median_polish", "median_7x7", "median_5x5", "median_3x3", "z_score_5x5"))
    })
    output$UINormMethod        <- renderUI({
        input$file1
        selectInput("NormMethod", "Batch-wise normalisation against negative control", choices = list("None selected", "z_score", "robust_z_score", "subtract_mean_ctrl", "divide_by_mean_ctrl", "subtract_median_ctrl", "divide_by_median_ctrl"))
    })
    output$UINormNegCtrl       <- renderUI({
        input$file1
        selectInput("NormNegCtrl", "Negative control", choices = as.list(sort(htm[[input$colTreatment]])))
    })
    
    
    # Treatment summary
    output$UISummaryMeasurements <- renderUI({
        input$file1
        input$applyNorm
        selectInput("SummaryMeasurements", "Measurements to be analyzed", choices = as.list(names(htm)), multiple = TRUE)
    })
    output$UISummaryNegCtrl      <- renderUI({
        input$file1
        selectInput("SummaryNegCtrl", "Negative control", choices = as.list(sort(htm[[input$colTreatment]])))
    })
    output$UISummaryPosCtrl      <- renderUI({
        input$file1
        selectInput("SummaryPosCtrl", "Positive control", choices = as.list(sort(htm[[input$colTreatment]])))
    })
    output$UISummaryNumObjects   <- renderUI({
        input$file1
        input$applyNorm
        selectInput("SummaryNumObjects", "Number of objects per image", choices = as.list(names(htm)))
    })
})