# ==========================================================================
# Phylosophy: the 'htm' data.frame is placed in the global enviromnent, where it can be accessed and updated by reactive functions
#
# Heatmaps are built from a reactive data.frame 'htmHM()'
#
# ==========================================================================

# To do:
# Sync negative control across Normalize&Summarize
# Update plot symbols each time QC is applied

source("./functions.r")
loadpackage("shiny")
loadpackage("plotly")
loadpackage("ggplot2")
loadpackage("tcltk")
loadpackage("xlsx")
loadpackage("shinyjs")




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
    
    
    
    # Plot settings
    output$UIselectBatch <- renderUI({
        input$file1
        selectInput("batch", "Show this batch:", as.list(c("All batches",unique(htm[[input$colBatch]]))))
    })
    
    observeEvent(input$plotType,{

        # Display plot control widgets depending on which plot type is selected
        switch(input$plotType,
            "Scatter plot" = {
                output$UIselectXaxis <- renderUI(selectInput("Xaxis", "X axis:", choices = as.list(names(htm)), width = "200%"))
                output$UIselectYaxis <- renderUI(selectInput("Yaxis", "Y axis:", choices = as.list(names(htm)), width = "200%"))
                
                output$UIhighlightQCfailed     <- renderUI(checkboxInput("highlightQCfailed", "Show data points that did not pass QC", value = FALSE))
                
                output$UIPointplotsplitBy      <- renderUI(selectInput("PointplotsplitBy", "Split plot by", choices = as.list(c("None", names(htm)))))
                output$UIPointplotfilterColumn <- renderUI(selectInput("PointplotfilterColumn", "Only show images where column:", choices = as.list(c("None", names(htm))), width = "100%"))
                output$UIPointplotfilterValues <- renderUI(selectInput("PointplotfilterValues", "Matches:", choices = as.list(c("All", htm[[input$PointplotfilterColumn]])), width = "100%", multiple = TRUE))
                
                output$UIBoxplothighlightCenter <- renderUI(NULL)
                output$UIBoxplotsplitBy <- renderUI(NULL)
            },
            "Boxplot"      = {
                output$UIselectXaxis <- renderUI(selectInput("Xaxis", "Categories:", choices = as.list(names(htm)), width = "200%"))
                output$UIselectYaxis <- renderUI(selectInput("Yaxis", "Values:", choices = as.list(names(htm)), width = "200%"))
                
                output$UIhighlightQCfailed <- renderUI(checkboxInput("highlightQCfailed", "Hide data points that did not pass QC", value = FALSE))
                
                output$UIPointplotsplitBy      <- renderUI(NULL)
                output$UIPointplotfilterColumn <- renderUI(NULL)
                output$UIPointplotfilterValues <- renderUI(NULL)
                
                output$UIBoxplothighlightCenter <- renderUI(selectInput("BoxplothighlightCenter", "Highlight box center?", choices = list("No", "Mean", "Median")))
                output$UIBoxplotsplitBy <- renderUI(selectInput("BoxplotsplitBy", "Split plot by", choices = as.list(c("None", names(htm)))))
            },
            "Heatmap"      = {
                output$UIselectXaxis <- renderUI(NULL)
                output$UIselectYaxis <- renderUI(selectInput("Yaxis", "Values:", choices = as.list(names(htm)), width = "200%"))
                
                output$UIhighlightQCfailed <- renderUI(checkboxInput("highlightQCfailed", "Show data points that did not pass QC", value = FALSE))
                
                output$UIPointplotsplitBy       <- renderUI(NULL)
                output$UIPointplotfilterColumn  <- renderUI(NULL)
                output$UIPointplotfilterValues  <- renderUI(NULL)
                
                output$UIBoxplothighlightCenter <- renderUI(NULL)
                output$UIBoxplotsplitBy         <- renderUI(NULL)
            }
        )
    })
    
    htmHM <- reactive({

        if(input$batch == "All batches") return(NULL)
        
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
        switch(input$plotType,
               "Scatter plot" = pointPlot(htm, input$colBatch, input$batch, input$Xaxis, input$Yaxis, col_QC, input$highlightQCfailed, input$PointplotsplitBy, filterByColumn = input$PointplotfilterColumn, whichValues = input$PointplotfilterValues),
               "Boxplot"      = boxPlot(htm, input$colBatch, input$batch, input$Xaxis, input$Yaxis, col_QC, input$highlightQCfailed, input$BoxplothighlightCenter, input$BoxplotsplitBy),
               "Heatmap"      = heatmapPlot(htmHM(), input$Yaxis, input$batch, input$wells_Y, input$wells_X, input$squaresize, col_QC, input$highlightQCfailed)
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
        if (length(s) == 0) {
            "Click on a data point to open images!"
        } else {
            print("You selected:")
            print(s)
            i = s[["pointNumber"]] + 1
            
            openTheseImgChannels <- input$images2display
            
            tempPathInTable    <- gsub("\\\\", "/", input$pathInTable)
            tempPathInComputer <- gsub("\\\\", "/", input$pathInComputer)
            tempFullPathName <- paste0(htm[i, paste0(input$prefixPath, input$images2display)], "/", htm[i, paste0(input$prefixFile, input$images2display)])
            tempFullPathName <- gsub("\\\\", "/", tempFullPathName)
            FullPathFile <- sub(tempPathInTable, tempPathInComputer, tempFullPathName, ignore.case = TRUE)

            print("Launching Fiji: "+input$fiji_binary)
            print(FullPathFile)
            OpenInFiji(FullPathFile, input$fiji_binary)
            
        }
    })

    
    
    # Normalization settings
    output$UINormFeatures      <- renderUI({
        input$file1
        input$applySummary
        
        selectInput("NormFeatures", "Data features to be analyzed", choices = as.list(names(htm)), multiple = FALSE)
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
        
        selectInput("NormNegCtrl", "Negative control", choices = as.list(c("None selected", sort(htm[[input$colTreatment]]))))
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
        
        selectInput("SummaryMeasurements", "Measurements to be analyzed", choices = as.list(names(htm)), multiple = TRUE)
    })
    output$UISummaryNegCtrl      <- renderUI({
        input$file1
        input$applyNorm
        input$NormNegCtrl
        
        selectInput("SummaryNegCtrl", "Negative control", choices = as.list(c("None selected", "All treatments", sort(htm[[input$colTreatment]]))))
    })
    output$UISummaryPosCtrl      <- renderUI({
        input$file1
        input$applyNorm
        
        selectInput("SummaryPosCtrl", "Positive control", choices = as.list(c("None selected", sort(htm[[input$colTreatment]]))))
    })
    output$UISummaryNumObjects   <- renderUI({
        input$file1
        input$applyNorm
        
        selectInput("SummaryNumObjects", "Number of objects per image", choices = as.list(names(htm)))
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

    
    
    # Data table
    observeEvent(input$file1, {
        input$applyNorm
        output$valuestable <- renderDataTable(htm)
    })
})
