# ==========================================================================
# Phylosophy: the 'htm' data.frame is placed in the global enviromnent, where it can be accessed and updated by reactive functions
#
# Heatmaps are built from a reactive data.frame 'htmHM()'
#
# ==========================================================================

# To do:
# Sync negative control across Normalize&Summarize
# Update plot symbols each time QC is applied
# Use a separate function to generate each 'selectInput' widget. This facilitates saving and loading states.
enableBookmarking(store = "url")
source("./functions.r")
loadpackage("shiny")
loadpackage("plotly")
loadpackage("ggplot2")
loadpackage("tcltk")
# loadpackage("xlsx")
loadpackage("shinyjs")
loadpackage("raster")


# Adjust maximum upload size to 2 Gb
options(shiny.maxRequestSize=2*1024^3)

# Initialize variables
if(exists("htm")) rm(htm)
if(exists("QCsettings")) rm(QCsettings)

QCsettings <<- data.frame(type       = character(), 
                         measurement = character(), 
                         minimum     = character(), 
                         maximum     = character(),
                         failed      = integer(),
                         stringsAsFactors = FALSE)

col_QC <- "HTM_QC"


shinyServer(function(input, output){

    # File Input
    observeEvent(input$file1, {
        htm <- read.HTMtable(input$file1$datapath)
        htm[[col_QC]] <- TRUE
        htm <<- htm
    })

    # Settings
    output$UIcolNameTreatment <- renderUI({
        input$file1
        input$applyNorm
        
        selectInput("colTreatment", "Treatment:", as.list(names(htm)), width = "100%")
    })
    output$UIcolNameBatch     <- renderUI({
        input$file1
        input$applyNorm
        
        selectInput("colBatch", "Batch:", as.list(names(htm)), width = "100%")
    })
    output$UIcolNameWell      <- renderUI({
        input$file1
        input$applyNorm
        
        selectInput("colWell", "Well coordinate:", as.list(names(htm)), width = "100%")
    })
    output$UIcolNamePos       <- renderUI({
        input$file1
        input$applyNorm
        
        selectInput("colPos", "Sub-position coordinate:", as.list(names(htm)), width = "100%")
    })
    
    output$UIcolNameObjectPosX       <- renderUI({
      input$file1
      input$applyNorm
      
      selectInput("colObjectPosX", "Object's x-position:", as.list( c("NA", names(htm))), width = "100%")
    })
    
    output$UIcolNameObjectPosY       <- renderUI({
      input$file1
      input$applyNorm
      
      selectInput("colObjectPosY", "Object's y-position:", as.list( c("NA", names(htm))), width = "100%")
    })
    
    output$UIcolNameObjectPosZ       <- renderUI({
      input$file1
      input$applyNorm
      
      selectInput("colObjectPosZ", "Object's z-position:", as.list( c("NA", names(htm))), width = "100%")
    })
    
    
    output$UIfiji_path        <- renderUI({
        
        if (Sys.info()['sysname'] == "Windows")
        {
            fiji_binary_path = "C:/Fiji.app/ImageJ-win64.exe"
        } 
        else 
        {
            fiji_binary_path = "/Applications/Fiji.app/Contents/MacOS/ImageJ-macosx"
        }

        textInput("fiji_binary", "Path to Fiji ", value = fiji_binary_path, width = "100%")
    
    })
    
    output$UIavailableimages  <- renderUI({
        input$file1
        img_names <- gsub(paste0("^", input$prefixPath, "(.*)"), "\\1", names(htm)[grep(paste0("^", input$prefixPath), names(htm))])
        checkboxGroupInput("images2display", "Select images to be viewed upon clicking within a plot", as.list(img_names))
    })
    
    
    
    # Plot settings
    output$UIselectBatch <- renderUI({
        input$file1
        input$plotType
        
        if( is.null( input$colBatch ) )
        {
          selectInput("batch", "Show this batch:", as.list( c( "All batches" ) ) )
        }
        else if( input$plotType == "Heatmap" )
        {
          selectInput("batch", "Show this batch:", as.list( c( unique( htm[[input$colBatch]] ) ) ) ) 
        }
        else
        {
          selectInput("batch", "Show this batch:", as.list( c( "All batches", unique( htm[[input$colBatch]] ) ) ) )
        }
    })
    
    observeEvent(input$plotType,{

        # Display plot control widgets depending on which plot type is selected
        switch(input$plotType,
            "Scatter plot" = {
                output$UIselectXaxis <- renderUI(selectInput("Xaxis", "X axis:", choices = as.list(names(htm)), width = "200%"))
                output$UIselectYaxis <- renderUI(selectInput("Yaxis", "Y axis:", choices = as.list(names(htm)), width = "200%"))
                
                output$UIhighlightQCfailed     <- renderUI(selectInput("highlightQCfailed", "Display data points that failed QC", choices = c("Don't show","Show with cross")))
                
                output$UIPointplotsplitBy      <- renderUI(selectInput("PointplotsplitBy", "Split plot by", choices = as.list(c("None", names(htm)))))
                output$UIPointplotfilterColumn <- renderUI(selectInput("PointplotfilterColumn", "Only show images where column:", choices = as.list(c("None", names(htm))), width = "100%"))
                output$UIPointplotfilterValues <- renderUI(
                  if ( is.null( input$PointplotfilterColumn ) )
                  {
                    selectInput("PointplotfilterValues", "Matches:", choices = as.list(c("All")) , width = "100%", multiple = TRUE)
                  }
                  else
                  {
                    selectInput("PointplotfilterValues", "Matches:", choices = as.list(c("All", htm[[input$PointplotfilterColumn]])), width = "100%", multiple = TRUE)
                  }
                  )
                output$UIBoxplothighlightCenter <- renderUI(NULL)
                output$UIBoxplotsplitBy <- renderUI(NULL)
                
                output$UILUTminmax <- renderUI(NULL)
                
                output$UILUTcolors <- renderUI(NULL)
            },
            "Boxplot"      = {
                output$UIselectXaxis <- renderUI(selectInput("Xaxis", "Categories:", choices = as.list(names(htm)), width = "200%"))
                output$UIselectYaxis <- renderUI(selectInput("Yaxis", "Values:", choices = as.list(names(htm)), width = "200%"))
                
                output$UIhighlightQCfailed     <- renderUI(selectInput("highlightQCfailed", "Display data points that failed QC", choices = c("Don't show","Show as cross")))
                
                output$UIPointplotsplitBy      <- renderUI(NULL)
                output$UIPointplotfilterColumn <- renderUI(NULL)
                output$UIPointplotfilterValues <- renderUI(NULL)
                
                output$UIBoxplothighlightCenter <- renderUI(selectInput("BoxplothighlightCenter", "Highlight box center?", choices = list("No", "Mean", "Median")))
                output$UIBoxplotsplitBy <- renderUI(selectInput("BoxplotsplitBy", "Split plot by", choices = as.list(c("None", names(htm)))))
                
                output$UILUTminmax <- renderUI(NULL)
                output$UILUTcolors <- renderUI(NULL)
            },
            "Heatmap"      = {
                output$UIselectXaxis <- renderUI(NULL)
                output$UIselectYaxis <- renderUI(selectInput("Yaxis", "Values:", choices = as.list(names(htm)), width = "200%"))
                
                output$UIhighlightQCfailed     <- renderUI(selectInput("highlightQCfailed", "Display data points that failed QC", choices = c("Don't show","Show with cross")))
                
                output$UIPointplotsplitBy       <- renderUI(NULL)
                output$UIPointplotfilterColumn  <- renderUI(NULL)
                output$UIPointplotfilterValues  <- renderUI(NULL)
                
                output$UIBoxplothighlightCenter <- renderUI(NULL)
                output$UIBoxplotsplitBy         <- renderUI(NULL)
                
                output$UILUTcolors <- renderUI( selectInput("LUTcolors", "LUT colors", choices = c("Red-White-Green", "Blue-White-Red"), width = "100%"))
         
                output$UILUTminmax <- renderUI({
                    Ymin <- min(htm[[input$Yaxis]], na.rm = TRUE)
                    Ymax <- max(htm[[input$Yaxis]], na.rm = TRUE)
                    sliderInput("LUTminmax", "LUT adjustment", min = Ymin, max = Ymax, value = c(Ymin, Ymax), width = "100%")
                })
                
            }
        )
    })
    
    htmHM <- reactive({

        if( input$batch == "All batches" ) return( NULL )

        makeHeatmapDataFrame(df           = htm, 
                             WellX        = input$wells_X,
                             WellY        = input$wells_Y,
                             PosX         = input$npos_X,
                             PosY         = input$npos_Y,
                             subposjitter = input$squaredodge,
                             batch_col    = input$colBatch,
                             batch        = input$batch,
                             col_Well     = input$colWell,
                             col_Pos      = input$colPos,
                             col_QC       = col_QC)
    })



    # Plot
    output$plot <- renderPlotly({
        
        # Take dependency on clicking the "Update plot" button
        input$plotScatterBoxOrHeatmap
        
        isolate(
            if( ! is.null( input$batch ) )
            {
                switch(input$plotType,
                   "Scatter plot" = pointPlot(htm, input$colBatch, input$batch, input$Xaxis, input$Yaxis, col_QC, input$highlightQCfailed, input$PointplotsplitBy, filterByColumn = input$PointplotfilterColumn, whichValues = input$PointplotfilterValues, input$colTreatment, input$colBatch ),
                   "Boxplot"      = boxPlot(htm, input$colBatch, input$batch, input$Xaxis, input$Yaxis, col_QC, input$highlightQCfailed, input$BoxplothighlightCenter, input$BoxplotsplitBy, input$colTreatment, input$colBatch ),
                   "Heatmap"      = heatmapPlot(htmHM(), input$Yaxis, input$batch, input$wells_Y, input$wells_X, input$squaresize, col_QC, input$highlightQCfailed, colorMin = input$LUTminmax[1], colorMax = input$LUTminmax[2], lutColors = input$LUTcolors, input$colTreatment, input$colBatch )
                )
            }
            else
            {
              ggplotly( ggplot() ) 
            }
         )
      
    })

    
    
    # QC-specific settings
    approvedExperiments          <- reactive({
        input$QCAddfailedExperiments
        input$QCcheckGroup
        unique(as.character(htm[[input$colBatch]]))[!(unique(as.character(htm[[input$colBatch]])) %in% as.character(QCsettings[QCsettings$type == "Failed experiment","minimum"]))]
    })
    
    output$UIQCfailedExperiments <- renderUI({
        input$file1
        input$applyNorm
        
        fluidRow(
            column(6,
                selectInput("QCfailedExperiment", "Failed experiments:", approvedExperiments(), width = "200%")
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
            maximum     = isolate(input$QCfailedExperiment),
            failed      = sum(htm[[isolate(input$colBatch)]] == isolate(input$QCfailedExperiment))
        )
        QCsettings <<- rbind(QCsettings, temp)
        
        # Update show/remove QC
        output$QCtable    <- renderTable(QCsettings[,1:4])
        output$UIQCactive <- renderUI({
            checkboxGroupInput("QCcheckGroup",
                               label = strong("Disable this QC"), 
                               choices = as.list(row.names(QCsettings))
            )
        })
        
        # Reset QC report
        output$QCreport <- renderPrint("")
    })
    observeEvent(input$QCAddnumeric,{
        temp <- data.frame(
                    type        = "Numeric QC",
                    measurement = isolate(input$QCnumMeasurement),
                    minimum     = as.character(isolate(input$QCnumMin)),
                    maximum     = as.character(isolate(input$QCnumMax)),
                    failed      = sum((htm[[isolate(input$QCnumMeasurement)]] < isolate(input$QCnumMin) | htm[[isolate(input$QCnumMeasurement)]] > isolate(input$QCnumMax)) & !is.na(htm[[isolate(input$QCnumMeasurement)]]))
                )
        QCsettings <<- rbind(QCsettings, temp)
        
        # Update show/remove QC
        output$QCtable    <- renderTable(QCsettings[,1:4])
        output$UIQCactive <- renderUI({
            checkboxGroupInput("QCcheckGroup",
                               label = strong("Disable this QC"), 
                               choices = as.list(row.names(QCsettings))
            )
        })
        
        # Reset QC report
        output$QCreport <- renderPrint("")
    })
    observeEvent(input$QCAddtxt,{
        temp <- data.frame(
            type        = "Text QC",
            measurement = isolate(input$QCtxtMeasurement),
            minimum     = isolate(input$QCtxtBad),
            maximum     = isolate(input$QCtxtBad),
            failed      = sum(htm[[isolate(input$QCtxtMeasurement)]] == isolate(input$QCtxtBad))
        )
        QCsettings <<- rbind(QCsettings, temp)
        
        # Update show/remove QC
        output$QCtable    <- renderTable(QCsettings[,1:4])
        output$UIQCactive <- renderUI({
            checkboxGroupInput("QCcheckGroup",
                               label = strong("Disable this QC"), 
                               choices = as.list(row.names(QCsettings))
            )
        })
        
        # Reset QC report
        output$QCreport <- renderPrint("")
    })    
    observeEvent(input$QCcheckGroup, {
        QCsettings <<- QCsettings[row.names(QCsettings) != input$QCcheckGroup,]
        
        # Update show/remove QC
        output$QCtable    <- renderTable(QCsettings[,1:4])
        output$UIQCactive <- renderUI({
            checkboxGroupInput("QCcheckGroup",
                               label = strong("Disable this QC"), 
                               choices = as.list(row.names(QCsettings))
            )
        })
        
        # Reset QC report
        output$QCreport <- renderPrint("")
    })
    observeEvent(input$applyQC,{
        
        withCallingHandlers({
            html("echo_QC", "", add = FALSE)
            
            echo("Performing QCs:")
            echo("")
            
            if(nrow(QCsettings) == 0){
                
                # If no QC setting is selected, approve all images
                htm[[col_QC]] <- TRUE
                htm <<- htm
                
                echo("  No QCs selected. Setting all data to valid.")
                echo("  Total measurements: ", nrow(htm))
                echo("")
                echo("The column ", col_QC, " has been updated.")
                
            } else{
                
                # If QC parameters have been selected, label the htm data.frame accordingly
                temp <- applyQC(htm, QCsettings)
                htm[[col_QC]] <- temp
                htm <<- htm
                
                echo("QCs:")
                for(i in 1:nrow(QCsettings)){
                    switch(as.character(QCsettings[i, "type"]),
                        "Failed experiment" = {
                            echo("Failed experiment:")
                            echo("  Batch: ", QCsettings[i, "minimum"])
                        },
                        "Numeric QC" = {
                            echo("Measurement: ", QCsettings[i, "measurement"])
                            echo("  Allowed range: ", QCsettings[i, "minimum"], " ... ", QCsettings[i, "maximum"], " and not NA.")
                        },
                        "Text QC" = {
                            echo("Measurement: ", QCsettings[i, "measurement"])
                            echo("  Reject text: ", QCsettings[i, "minimum"])
                        }
                    )
                    
                    echo("  Total: ", nrow(htm))
                    echo("  Failed: ", QCsettings[i, "failed"])
                    echo("")
                }
                
                echo("Summary of all QCs:")
                echo("  Total (all QCs): ", nrow(htm))
                echo("     Approved (all Qcs): ", sum(htm[[col_QC]]))
                echo("     Failed (all Qcs): ", sum(!htm[[col_QC]]))
                echo("")
                echo("The column ", col_QC, " has been updated.")
            }
            
        },
            message = function(m) html("echo_QC", m$message, add = TRUE)
        )
    })
    

    
    # Plot-Fiji interaction
    output$selection <- renderPrint({
        
        s <- event_data("plotly_click")
        
        if (length(s) == 0) 
        {
          "Click on a data point to open images!"
        } 
        else 
        {
          
          if ( length( input$images2display ) == 0 )
          {
            return("There are no images for viewing selected.");
          }
          
          print("You selected:")
          print(s)
          i = s[["pointNumber"]] + 1
          
          tempPathInTable    <- gsub("\\\\", "/", input$pathInTable)
          tempPathInComputer <- gsub("\\\\", "/", input$pathInComputer)
          
          directories <- htm[i, paste0(input$prefixPath, input$images2display)]
          directories <- gsub("\\\\", "/", directories)
          directories <- sub( tempPathInTable, tempPathInComputer, directories, ignore.case = TRUE)
          
          filenames <- htm[i, paste0( input$prefixFile, input$images2display )]
          
          print( paste0( "Launching Fiji: ", input$fiji_binary ) )
          print( directories )
          print( filenames )
          
          x <- "NA"; y <- "NA"; z <- "NA";
          if ( input$colObjectPosX != "NA ") { x <- htm[i, input$colObjectPosX] }
          if ( input$colObjectPosY != "NA ") { y <- htm[i, input$colObjectPosY] }
          if ( input$colObjectPosZ != "NA ") { z <- htm[i, input$colObjectPosZ] }
          
          print( paste( "Object position: ", x, y, z ) )
          
          OpenInFiji( directories, filenames, input$fiji_binary, x, y, z )
        }
    })

    
    
    # Normalization settings
    output$UINormFeatures      <- renderUI({
        input$file1
        input$applySummary
        
        selectInput("NormFeatures", "Data features to be analyzed", choices = as.list(names(htm)), width = "100%", multiple = FALSE)
    })
    output$UINormDataTransform <- renderUI({
        input$file1
        
        selectInput("NormDataTransform", "Data transformation", choices = list("None selected", "log2"), width = "100%")
    })
    output$UINormGradientCorr  <- renderUI({
        input$file1
        
        selectInput("NormGradientCorr", "Batch-wise spatial gradient correction", choices = list("None selected", "median polish", "median 7x7", "median 5x5", "median 3x3", "z-score 5x5"), width = "100%")
    })
    output$UINormMethod        <- renderUI({
        input$file1
        
        selectInput("NormMethod", "Batch-wise normalisation against negative control", choices = list("None selected", "z-score", "z-score (median subtraction)", "robust z-score", "subtract mean ctrl", "divide by mean ctrl", "subtract median ctrl", "divide by median ctrl"), width = "100%")
    })
    output$UINormNegCtrl       <- renderUI({
        input$file1
        
        selectInput("NormNegCtrl", "Negative control", choices = as.list(c("None selected", "All treatments", sort(htm[[input$colTreatment]]))), width = "100%")
    })
    
    observeEvent(input$applyNorm,{
        
        withCallingHandlers({
            html("echo_Normalization", "", add = FALSE)
            
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
        },
            message = function(m) html("echo_Normalization", m$message, add = TRUE)
        )
        
    })


    
    # Treatment summary
    output$UISummaryMeasurements <- renderUI({
        input$file1
        input$applyNorm
        
        selectInput("SummaryMeasurements", "Measurements to be analyzed", choices = as.list(names(htm)), width = "100%", multiple = TRUE)
    })
    output$UISummaryNegCtrl      <- renderUI({
        input$file1
        input$applyNorm
        input$NormNegCtrl
        
        selectInput("SummaryNegCtrl", "Negative control", choices = as.list(c("None selected", "All treatments", sort(htm[[input$colTreatment]]))), width = "100%")
    })
    output$UISummaryPosCtrl      <- renderUI({
        input$file1
        input$applyNorm
        
        selectInput("SummaryPosCtrl", "Positive control", choices = as.list(c("None selected", sort(htm[[input$colTreatment]]))), width = "100%")
    })
    output$UISummaryNumObjects   <- renderUI({
        input$file1
        input$applyNorm
        
        selectInput("SummaryNumObjects", "Number of objects per image", choices = as.list(names(htm)), width = "100%")
    })
    
    observeEvent(input$SummaryMeasurements,{
        output$TreatmentSummaryTable <- renderDataTable(data.frame())
        output$SummaryReport <- renderPrint("")
    })
    observeEvent(input$SummaryNegCtrl,{
        output$TreatmentSummaryTable <- renderDataTable(data.frame())
        output$SummaryReport <- renderPrint("")
    })
    observeEvent(input$SummaryPosCtrl,{
        output$TreatmentSummaryTable <- renderDataTable(data.frame())
        output$SummaryReport <- renderPrint("")
    })
    observeEvent(input$SummaryNumObjects,{
        output$TreatmentSummaryTable <- renderDataTable(data.frame())
        output$SummaryReport <- renderPrint("")
    })
    observeEvent(input$applySummary,{
        
        withCallingHandlers({
            html("echo_TreatmentSummary", "", add = FALSE)
            
            temp <- htmTreatmentSummary(data                 = htm,
                                        measurements         = input$SummaryMeasurements,
                                        col_Experiment       = input$colBatch,
                                        col_Treatment        = input$colTreatment,
                                        col_ObjectCount      = input$SummaryNumObjects,
                                        col_QC               = col_QC,
                                        negative_ctrl        = input$SummaryNegCtrl,
                                        positive_ctrl        = input$SummaryPosCtrl,
                                        excluded_Experiments = "")
            
            # Display the summary table in the html page
            output$TreatmentSummaryTable <- renderDataTable(temp[,c("treatment", "median__means", "t_test__p_value", "t_test__signCode", "numObjectsOK", "numImagesOK", "numReplicatesOK")])
            
            # Save summary table
            echo("")
            echo("Please save the summary table using the popup window.")
            path <- tclvalue(tkgetSaveFile(initialfile = paste0("TreatmentSummary--", input$SummaryMeasurements, ".csv")))
            write.csv(temp, path, row.names = FALSE)
            
            if(path == ""){
                echo("Did not save treatment summary table!")
            } else{
                echo("Saved summary table to ", path)
            }
            
        },
            message = function(m) html("echo_TreatmentSummary", m$message, add = TRUE)
        )
        
    })

    
    
    # R Console
    runcodeServer()
    
    
    
    # Save & Load session
    observeEvent(input$buttonSessionSave, {
        
        withCallingHandlers({
            html("echo_SaveLoadSession", "", add = FALSE)
            
            echo("Dialog box: Where is the image table file?")
            echo("")
            pathImageTable <- choose.files( caption = "Select the image table file",
                                            multi = FALSE, filters = Filters["All",] )
            echo("Saving analysis session of image table '", pathImageTable, "'")
            echo("")
            
            echo("Dialog box: Where shall the session file be saved?")
            echo("")
            targetpath <- tclvalue( tkgetSaveFile(initialfile = "settings.r") )
            
            echo("Saving all shinyHTM settings.")
            echo("Please keep the original image table at the same location.")
            echo("Saving shinyHTM tables and settings to ", targetpath)
            
            
            # Subset the columns generated by shinyHTM
            HTMcols         <- grepl("^HTM_.*$", names(htm))
            HTMdf           <- data.frame(htm[,HTMcols])
            colnames(HTMdf) <- names(htm)[HTMcols]
            
            
            # Extract the selected settings
            HTMsettings <- list(HTMdf               = HTMdf,
                                
                                pathImageTable      = pathImageTable,
                                
                                colTreatment        = isolate(input$colTreatment), 
                                colBatch            = isolate(input$colBatch), 
                                colWell             = isolate(input$colWell), 
                                colPos              = isolate(input$colPos), 
                                
                                colObjectPosX       = isolate(input$colObjectPosX), 
                                colObjectPosY       = isolate(input$colObjectPosY), 
                                colObjectPosZ       = isolate(input$colObjectPosZ), 
                                
                                
                                fiji_binary         = isolate(input$fiji_binary), 
                                images2display      = isolate(input$images2display), 
                                
                                NormFeatures        = isolate(input$NormFeatures), 
                                NormDataTransform   = isolate(input$NormDataTransform), 
                                NormGradientCorr    = isolate(input$NormGradientCorr), 
                                NormMethod          = isolate(input$NormMethod), 
                                NormNegCtrl         = isolate(input$NormNegCtrl), 
                                
                                SummaryMeasurements = isolate(input$SummaryMeasurements), 
                                SummaryNegCtrl      = isolate(input$SummaryNegCtrl), 
                                SummaryPosCtrl      = isolate(input$SummaryPosCtrl), 
                                SummaryNumObjects   = isolate(input$SummaryNumObjects))
            
            dput(HTMsettings, targetpath)
            echo("Done!")
            
        },
        message = function(m) html("echo_SaveLoadSession", m$message, add = TRUE)
        )
    })
    observeEvent(input$buttonSessionLoad, {
        
        withCallingHandlers({
            html("echo_SaveLoadSession", "", add = FALSE)

            echo("Loading shinyHTM settings.")
            echo("")
            settingsFilePath <-  tclvalue(tkgetOpenFile(title = "Open shinyHTM session file.")) 

            # Load the settings file
            echo("* Loading settings file")
            HTMsettings <- dget(settingsFilePath)
            echo("     done")
            
            
            # Reconstitute the htm data frame
            echo("* Loading image table")
            htm <- read.HTMtable(HTMsettings[["pathImageTable"]])
            htm <- cbind(htm, HTMsettings[["HTMdf"]])
            htm <<- htm
            output$valuestable <- renderDataTable(htm)
            echo("     done")
            
            
            # Reconstitute all widget values
            
            echo("* Loading experiment settings")
            output$UIcolNameTreatment <- renderUI({
                selectInput("colTreatment", "Treatment:", as.list(names(htm)), width = "100%", selected = HTMsettings[["colTreatment"]])
            })
            output$UIcolNameBatch     <- renderUI({
                selectInput("colBatch", "Batch:", as.list(names(htm)), width = "100%", selected = HTMsettings[["colBatch"]])
            })
            output$UIcolNameWell      <- renderUI({
                selectInput("colWell", "Well coordinate:", as.list(names(htm)), width = "100%", selected = HTMsettings[["colWell"]])
            })
            output$UIcolNamePos       <- renderUI({
                selectInput("colPos", "Sub-position coordinate:", as.list(names(htm)), width = "100%", selected = HTMsettings[["colPos"]])
            })
            
            output$UIcolNameObjectPosX     <- renderUI({
              selectInput("colObjectPosX", "Object's x-position:", as.list( c("NA", names(htm))), width = "100%", selected = HTMsettings[["colObjectPosX"]])
            })
            output$UIcolNameObjectPosX     <- renderUI({
              selectInput("colObjectPosY", "Object's y-position:", as.list( c("NA", names(htm))), width = "100%", selected = HTMsettings[["colObjectPosY"]])
            })
            output$UIcolNameObjectPosX     <- renderUI({
              selectInput("colObjectPosZ", "Object's z-position:", as.list( c("NA", names(htm))), width = "100%", selected = HTMsettings[["colObjectPosZ"]])
            })
            
            echo("     done")
            
            
            echo("* Loading Fji Settings")
            output$UIfiji_path        <- renderUI({
                textInput("fiji_binary", "Path to Fiji ", value = HTMsettings[["fiji_binary"]], width = "100%")
            })
            output$UIavailableimages  <- renderUI({
                checkboxGroupInput("images2display", "Select images to be viewed upon clicking within a plot", as.list(img_names), selected = HTMsettings[["images2display"]])
            })
            echo("     done")
            
            
            echo("* Loading Normalization Settings")
            output$UINormFeatures      <- renderUI({
                selectInput("NormFeatures", "Data features to be analyzed", choices = as.list(names(htm)), width = "100%", multiple = FALSE, selected = HTMsettings[["NormFeatures"]])
            })
            output$UINormDataTransform <- renderUI({
                selectInput("NormDataTransform", "Data transformation", choices = list("None selected", "log2"), width = "100%", selected = HTMsettings[["NormDataTransform"]])
            })
            output$UINormGradientCorr  <- renderUI({
                selectInput("NormGradientCorr", "Batch-wise spatial gradient correction", choices = list("None selected", "median polish", "median 7x7", "median 5x5", "median 3x3", "z-score 5x5"), width = "100%", selected = HTMsettings[["NormGradientCorr"]])
            })
            output$UINormMethod        <- renderUI({
                selectInput("NormMethod", "Batch-wise normalisation against negative control", choices = list("None selected", "z-score", "robust z-score", "subtract mean ctrl", "divide by mean ctrl", "subtract median ctrl", "divide by median ctrl"), width = "100%", selected = HTMsettings[["NormMethod"]])
            })
            output$UINormNegCtrl       <- renderUI({
                selectInput("NormNegCtrl", "Negative control", choices = as.list(c("None selected", sort(htm[[input$colTreatment]]))), width = "100%", selected = HTMsettings[["NormNegCtrl"]])
            })
            echo("     done")
            
            
            echo("* Loading Treatment Summary settings")
            output$UISummaryMeasurements <- renderUI({
                selectInput("SummaryMeasurements", "Measurements to be analyzed", choices = as.list(names(htm)), width = "100%", multiple = TRUE, selected = HTMsettings[["SummaryMeasurements"]])
            })
            output$UISummaryNegCtrl      <- renderUI({
                selectInput("SummaryNegCtrl", "Negative control", choices = as.list(c("None selected", "All treatments", sort(htm[[input$colTreatment]]))), width = "100%", selected = HTMsettings[["SummaryNegCtrl"]])
            })
            output$UISummaryPosCtrl      <- renderUI({
                selectInput("SummaryPosCtrl", "Positive control", choices = as.list(c("None selected", sort(htm[[input$colTreatment]]))), width = "100%", selected = HTMsettings[["SummaryPosCtrl"]])
            })
            output$UISummaryNumObjects   <- renderUI({
                selectInput("SummaryNumObjects", "Number of objects per image", choices = as.list(names(htm)), width = "100%", selected = HTMsettings[["SummaryNumObjects"]])
            })
            echo("     done")
            
            echo("")
            echo("Loaded all settings!")

        },
        message = function(m) html("echo_SaveLoadSession", m$message, add = TRUE)
        )
    })
    
    
    # Data table
    observeEvent(input$file1, {
        input$applyNorm
        output$valuestable <- renderDataTable(htm)
    })
})
