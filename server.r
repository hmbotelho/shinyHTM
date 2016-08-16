# ==========================================================================
# Phylosophy: the 'htm' data.frame is placed in the global enviromnent, where it can be accessed and updated by reactive functions
#
# Heatmaps are built from a reactive data.frame 'htmHM()'
#
# ==========================================================================


source("dependencies.r")
source("functions.r")

# Adjust maximum upload size to 2 Gb
options(shiny.maxRequestSize=2*1024^3)

# Initialize variables
rm(htm, htmHM, QCsettings)

QCsettings <<- data.frame(type = character(), 
                         measurement = character(), 
                         minimum = character(), 
                         maximum = character(),
                         stringsAsFactors = FALSE)

col_QC <- "HTM_QC"


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
        input$applyNorm
        
        switch(input$plotType,
               "Scatter plot" = return(selectInput("Xaxis", "X axis:", choices = as.list(names(htm)), width = "200%")),
               "Boxplot"      = return(selectInput("Xaxis", "Categories:", choices = as.list(names(htm)), width = "200%")),
               "Heatmap"      = return(NULL)
        )
    })
    output$UIselectYaxis <- renderUI({
        input$file1
        input$applyNorm
        
        switch(input$plotType,
               "Scatter plot" = return(selectInput("Yaxis", "Y axis:", choices = as.list(names(htm)), width = "200%")),
               "Boxplot"      = return(selectInput("Yaxis", "Values:", choices = as.list(names(htm)), width = "200%")),
               "Heatmap" =      return(selectInput("Yaxis", "Values:", choices = as.list(names(htm)), width = "200%"))
        )
    })
    output$UIselectBatch <- renderUI({
        input$file1
        
        selectInput("batch", "Show this batch:", as.list(c("All batches",unique(htm[[input$colBatch]]))))
    })
    
    
    # Settings
    output$UIcolNameTreatment <- renderUI({
        input$file1
        input$applyNorm
        
        selectInput("colTreatment", "Treatment:", as.list(names(htm)))
    })
    output$UIcolNameBatch     <- renderUI({
        input$file1
        input$applyNorm
        
        selectInput("colBatch", "Batch:", as.list(names(htm)))
    })
    output$UIcolNameWell      <- renderUI({
        input$file1
        input$applyNorm
        
        selectInput("colWell", "Well coordinate:", as.list(names(htm)))
    })
    output$UIcolNamePos       <- renderUI({
        input$file1
        input$applyNorm
        
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
        input$applyNorm
        
        fluidRow(
            column(6,
                selectInput("QCfailedExperiment", "Failed experiments:", htm[[input$colBatch]], width = "200%")
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
        input$applyNorm
        
        fluidRow(
            column(6,
                selectInput("QCnumMeasurement", "Number-based QC:", as.list(names(htm)), width = "200%")
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
        input$applyNorm
        
        fluidRow(
            column(6,
                   selectInput("QCtxtMeasurement", "Failed images (text-based):", as.list(names(htm)), width = "200%")
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
        
        selectInput("NormGradientCorr", "Batch-wise spatial gradient correction", choices = list("None selected", "median polish", "median 7x7", "median 5x5", "median 3x3", "z-score 5x5"))
    })
    output$UINormMethod        <- renderUI({
        input$file1
        
        selectInput("NormMethod", "Batch-wise normalisation against negative control", choices = list("None selected", "z-score", "robust z-score", "subtract mean ctrl", "divide by mean ctrl", "subtract median ctrl", "divide by median ctrl"))
    })
    output$UINormNegCtrl       <- renderUI({
        input$file1
        
        selectInput("NormNegCtrl", "Negative control", choices = as.list(sort(htm[[input$colTreatment]])))
    })
    
    observeEvent(input$applyNorm,{
        htm <<- htmNormalization(data                = htm,
                                 measurements        = input$NormFeatures,
                                 col_Experiment      = input$colBatch,
                                 transformation      = input$NormDataTransform,
                                 gradient_correction = input$NormGradientCorr,
                                 normalisation       = input$NormMethod,
                                 negcontrol          = input$NormNegCtrl,
                                 col_QC              = col_QC,
                                 col_Well            = input$colWell,
                                 col_Treatment       = input$colTreatment,
                                 num_WellX           = input$wells_X,
                                 num_WellY           = input$wells_Y)
    })

    
    # Treatment summary
    output$UISummaryMeasurements <- renderUI({
        input$file1
        input$applyNorm
        
        selectInput("SummaryMeasurements", "Measurements to be analyzed", choices = as.list(names(htm)), multiple = TRUE)
    })
    output$UISummaryNegCtrl      <- renderUI({
        input$file1
        input$applyNorm
        
        selectInput("SummaryNegCtrl", "Negative control", choices = as.list(sort(htm[[input$colTreatment]])))
    })
    output$UISummaryPosCtrl      <- renderUI({
        input$file1
        input$applyNorm
        
        selectInput("SummaryPosCtrl", "Positive control", choices = as.list(sort(htm[[input$colTreatment]])))
    })
    output$UISummaryNumObjects   <- renderUI({
        input$file1
        input$applyNorm
        
        selectInput("SummaryNumObjects", "Number of objects per image", choices = as.list(names(htm)))
    })
    
    
    # Data table
    observeEvent(input$file1, {
        input$applyNorm
        output$valuestable <- renderDataTable(htm)
    })
})