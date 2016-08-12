shinyUI(navbarPage("shinyHTM",




    tabPanel("1. Upload data",
        helpText("Upload an image table."),
        fileInput('file1', 'Choose File', accept=c('.csv'))
    ),




    tabPanel("2. Configure settings",
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




    tabPanel("3. Plot",
        verbatimTextOutput("selection"),
        plotlyOutput("plot"),
        hr(),
        
        
        fluidRow(
            
            column(3,
                selectInput("plotType", label = h5("Plot Type"), choices = c("Scatter plot", "Boxplot", "Heatmap"))
            ),
            
            column(3,
                uiOutput("selectX"),
                uiOutput("selectY")
            ),
            
            column(3,
                uiOutput("selectBatch")
            )
        )
        
    ),




    tabPanel("4. Quality Control",
        p("To be added...")
    ),




    tabPanel("5. Normalization",
        p("To be added...")
    ),




    tabPanel("6. Treatment summary",
        p("To be added...")
    ),




    navbarMenu("More",
               
        tabPanel("View Image Table",
            dataTableOutput("valuestable")
        ),
        
        tabPanel("About",
            
            p("By Hugo Botelho, Aug 2016"),
            a(href="mailto:hugobotelho@gmail.com", "hugobotelho@gmail.com"),
            hr(),
            p("Check for updates on GitHub"),
            a(href="https://github.com/hmbotelho/shinyHTM", "https://github.com/hmbotelho/shinyHTM")
        )
    )




))