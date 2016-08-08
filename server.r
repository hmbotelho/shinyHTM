source("dependencies.r")
source("functions.r")

shinyServer(function(input, output){
    
    # File Input
    sourcefile <- reactive(input$file1$datapath)
    data <- reactive(read.csv(sourcefile(), stringsAsFactors = FALSE))
    
    
    # Plot variables
    output$selectX <- renderUI({
        selectInput("Xaxis", "X axis:", as.list(names(data())))
    })
    output$selectY <- renderUI({
        selectInput("Yaxis", "Y axis:", as.list(names(data())))
    })
    
    
    # Settings
    output$selectTreatment <- renderUI({
        selectInput("TreatmentCol", "Treatment:", as.list(names(data())))
    })
    output$selectBatch     <- renderUI({
        selectInput("BatchCol", "Batch:", as.list(names(data())))
    })
    output$selectWellCoord <- renderUI({
        selectInput("WellCol", "Well coordinate:", as.list(names(data())))
    })
    output$selectPosCoord  <- renderUI({
        selectInput("PosCol", "Sub-position coordinate:", as.list(names(data())))
    })
    output$fiji_path       <- renderUI({
        if (Sys.info()['sysname'] == "Windows"){
            return(textInput("fiji_binary", "Path to Fiji (only necessary for Windows)", value = "C:/Fiji.app/ImageJ-win64.exe"))
        } else {
                return("/Applications/Fiji.app/Contents/MacOS/ImageJ-macosx")
        }
    })
    output$availableimages <- renderUI({
        img_names <- gsub(paste0("^", input$prefixPath, "(.*)"), "\\1", names(data())[grep(paste0("^", input$prefixPath), names(data()))])
        checkboxGroupInput("images2display", "Select images to be viewed upon clicking within a plot", as.list(img_names))
    })
    

    # Data table
    output$valuestable <- renderDataTable(data())
    
    
    # Plot
    output$plot <- renderPlotly({
                        if (input$plotType == "Scatter plot"){
                            scatterPlot(data(), input$Xaxis, input$Yaxis)
                        } else if (input$plotType == "Jitter plot"){
                            jitterPlot(data(), input$Xaxis, input$Yaxis)
                        } else{
                            heatmapPlot()
                        }
                    })
    
    
    # Plot-Fiji interaction
    output$selection <- renderPrint({
        s <- event_data("plotly_click")
        if (length(s) == 0) {
            "Click on a data point!"
        } else {
            print("You selected:")
            print(s)
            i = s[["pointNumber"]]
            
            openTheseImgChannels <- input$images2display
            
            tempPathInTable    <- gsub("\\\\", "/", input$pathInTable)
            tempPathInComputer <- gsub("\\\\", "/", input$pathInComputer)
            tempFullPathName <- paste0(data()[i, paste0(input$prefixPath, input$images2display)], "/", data()[i, paste0(input$prefixFile, input$images2display)])
            tempFullPathName <- gsub("\\\\", "/", tempFullPathName)
            FullPathFile <- sub(tempPathInTable, tempPathInComputer, tempFullPathName, ignore.case = TRUE)

            print("Launching Fiji now....")
            print(FullPathFile)
            OpenInFiji(FullPathFile, input$fiji_binary)
            
        }
    })
    
})