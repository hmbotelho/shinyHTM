shinyUI(pageWithSidebar(
    
    
    headerPanel("shinyHTM"),
    
    
    sidebarPanel(
        
        helpText("Upload an image table."),
        fileInput('file1', 'Choose File', accept=c('.csv')),
        hr(),
        
        selectInput("plotType", label = h5("Plot Type"), choices = c("Scatter plot", "Jitter plot", "Heatmap")),
        hr(),
        
        uiOutput("selectX"),
        uiOutput("selectY"),
        uiOutput("selectBatch"),
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
                     uiOutput("colNameTreatment"),
                     uiOutput("colBatchName"),
                     uiOutput("colNameWell"),
                     uiOutput("colNamePos"),
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
                     numericInput("wells_Y", "Number of Rows", 16),
                     numericInput("wells_X", "Number of Columns", 24),
                     numericInput("npos_Y", "Number of subposition Rows", 2),
                     numericInput("npos_X", "Number of subposition Columns", 2),
                     sliderInput("squaredodge", "Separation between positions", min=0, max=0.5, value=0.2, step=0.1),
                     sliderInput("squaresize", "Square size", min=0.5, max=5, value=1, step=0.5)
            ),
            
            tabPanel("Data table",
                     dataTableOutput("valuestable")
            )
        )
    )
))
