shinyUI(navbarPage("shinyHTM",




    tabPanel("1. Upload data",
        helpText("Upload an image table."),
        fileInput('file1', 'Choose File', accept=c('.csv'))
    ),




    tabPanel("2. Configure settings",
        h2("Settings"),
        h3("Select column names containing..."),
        uiOutput("UIcolNameTreatment"),
        uiOutput("UIcolNameBatch"),
        uiOutput("UIcolNameWell"),
        uiOutput("UIcolNamePos"),
        hr(),
        
        h3("Click & view settings"),
        uiOutput("UIfiji_path"),
        hr(),
        
        uiOutput("UIavailableimages"),
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
            
            column(2,
                selectInput("plotType", label = "Plot Type", choices = c("Scatter plot", "Boxplot", "Heatmap"))
            ),
            
            column(7,
                uiOutput("UIselectXaxis"),
                uiOutput("UIselectYaxis"),
                
                # Scatter plot customization options
                br(),br(),
                uiOutput("UIPointplotsplitBy"),
                
                # Boxplot customziation options
                br(),br(),
                uiOutput("UIBoxplothighlightCenter"),
                uiOutput("UIBoxplotsplitBy")
            ),
            
            column(3,
                uiOutput("UIselectBatch")
            )
        )
        
    ),




    tabPanel("4. Quality Control",
        uiOutput("UIQCfailedExperiments"),
        uiOutput("UIQCnumeric"),
        uiOutput("UIQCtext"),
        hr(),
        
        fluidRow(
            column(5,
                uiOutput("QCtable")
            ),
            column(2,
                uiOutput("UIQCactive")
            )
        ),
        
        hr(),
        actionButton("applyQC", "Apply QCs now", icon = icon("paper-plane-o")),
        
        p(""),
        verbatimTextOutput("QCreport")
    ),




    tabPanel("5. Normalization",
        uiOutput("UINormFeatures"),
        uiOutput("UINormDataTransform"),
        uiOutput("UINormGradientCorr"),
        uiOutput("UINormMethod"),
        uiOutput("UINormNegCtrl"),
        checkboxInput("NormCombinedVecror", "[DISABLED] Compute combined vector for all selected measurements?"),
        selectInput("NormMultiply", "[DISABLED] Multiply with cos(tetha)^N along average treatment effect; N=", choices = list(0,1,2,4,8,16,32)),
        hr(),
        actionButton("applyNorm", "Normalize", icon = icon("paper-plane-o"))
    ),




    tabPanel("6. Treatment summary",
        uiOutput("UISummaryMeasurements"),
        uiOutput("UISummaryNegCtrl"),
        uiOutput("UISummaryPosCtrl"),
        uiOutput("UISummaryNumObjects"),
        actionButton("applySummary", "Analyze", icon = icon("paper-plane-o")),
        
        br(),
        br(),
        verbatimTextOutput("SummaryReport"),
        
        br(),
        br(),
        dataTableOutput("TreatmentSummaryTable")
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