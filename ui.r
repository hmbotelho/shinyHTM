shinyUI(pageWithSidebar(
    
    
    headerPanel("Data visualization"),
    
    
    sidebarPanel(
        
        helpText("Upload an image table."),
        fileInput('file1', 'Choose File', accept=c('.csv')),
        hr(),
        
        selectInput("plotType", label = h5("Plot Type"), choices = c("Scatter plot", "Jitter plot", "Heatmap [NOT WORKING]")),
        hr(),
        

        uiOutput("selectX"),
        uiOutput("selectY"),
        hr()
        
    ),
    
    mainPanel(
        
        tabsetPanel(
            tabPanel("Plot",
                    verbatimTextOutput("selection"),
                    plotlyOutput("plot")
            ),
            
            tabPanel("Settings",
                     h2("Settings"),
                     
                     h3("Select column names containing..."),
                     uiOutput("selectTreatment"),
                     uiOutput("selectBatch"),
                     uiOutput("selectWellCoord"),
                     uiOutput("selectPosCoord"),
                     hr(),
                     
                     h3("Click & view settings"),
                     uiOutput("fiji_path"),
                     hr(),
                     uiOutput("availableimages"),
                     hr(),
                     textInput("pathInTable", "Image root folder name in table", "c:\\tutorial\\myplate_01"),
                     textInput("pathInComputer", "Image root folder name in this computer", "c:\\myplate_01"),
                     textInput("prefixPath", "Prefix: column with folder name", "PathName_"),
                     textInput("prefixFile", "Prefix: column with file name", "FileName_"),
                     hr(),
                     
                     h3("Heatmap settings"),
                     numericInput("wells_X", "Number positions X", 24),
                     numericInput("wells_Y", "Number positions Y", 16),
                     numericInput("npos_X", "Number subpositions X", 2),
                     numericInput("npos_Y", "Number subpositions Y", 2)
            ),
            
            tabPanel("Data table",
                     dataTableOutput("valuestable")
            )
        )
    )
))
