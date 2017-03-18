library(shiny)
library(plotly)
library(shinyjs)
library(ggplot2)
library(xlsx)


shinyUI(navbarPage("shinyHTM",

    # Initialize shinyjs
    shinyjs::useShinyjs(),
                   
                   
    tabPanel("1. Upload data",
        h2("Upload data"),
        helpText("Upload an image table (must be a comma-separated, tab-sepatated or Excel file)."),
        fileInput('file1', 'Choose File', accept=c('.csv', '.txt', '.xlsx', '.xls'))
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
        
        textInput("pathInTable", "Image root folder name in table", "c:\\tutorial\\myplate_01", width = "100%"),
        textInput("pathInComputer", "Image root folder name in this computer", "c:\\myplate_01", width = "100%"),
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
        h2("Plotting"),
        p("Use this plotting tool to explore your data and inspect the images which produced each data point."),
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
                uiOutput("UILUTminmax"),
                br(),br(),
                
                uiOutput("UIhighlightQCfailed"),
                
                # Scatter plot customization options
                uiOutput("UIPointplotsplitBy"),
                br(),
                fluidRow(
                    column(6, uiOutput("UIPointplotfilterColumn")),
                    column(6, uiOutput("UIPointplotfilterValues"))
                ),
                
                # Boxplot customziation options
                uiOutput("UIBoxplothighlightCenter"),
                uiOutput("UIBoxplotsplitBy")
                
                # Heatmap customziation options
                
            ),
            
            column(3,
                uiOutput("UIselectBatch")
            )
        )
        
    ),




    tabPanel("4. Quality Control",
        h2("Quality Control"),
        p("Use this tool to exclude some images from analysis."),
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
        verbatimTextOutput("echo_QC")
    ),




    tabPanel("5. Normalization",
        h2("Normalization"),
        p("Normalize your data, on a plate-by-plate basis."),
        uiOutput("UINormFeatures"),
        uiOutput("UINormDataTransform"),
        uiOutput("UINormGradientCorr"),
        uiOutput("UINormMethod"),
        uiOutput("UINormNegCtrl"),
        checkboxInput("NormCombinedVecror", "[DISABLED] Compute combined vector for all selected measurements?"),
        selectInput("NormMultiply", "[DISABLED] Multiply with cos(tetha)^N along average treatment effect; N=", choices = list(0,1,2,4,8,16,32)),
        hr(),
        actionButton("applyNorm", "Normalize", icon = icon("paper-plane-o")),
        hr(),
        verbatimTextOutput("echo_Normalization")
    ),




    tabPanel("6. Treatment summary",
        h2("Treatment Summary"),
        p("Average all measurements coming from the same treatment, regardless of the number of measurements (i.e. images)"),
        uiOutput("UISummaryMeasurements"),
        uiOutput("UISummaryNegCtrl"),
        uiOutput("UISummaryPosCtrl"),
        uiOutput("UISummaryNumObjects"),
        actionButton("applySummary", "Analyze", icon = icon("paper-plane-o")),
        
        br(),
        br(),
        verbatimTextOutput("echo_TreatmentSummary"),
        
        br(),
        br(),
        dataTableOutput("TreatmentSummaryTable")
    ),


    
    
    tabPanel("R Console",
        h2("R Console"),
        p("Use this text area to write R commands. Resuts will be shown in the R console, outside this website."),
        p("Click the \"run\" button to run the code"),
        p("You may need to use the print() function for your results to show up."),
        br(),
        runcodeUI(code = "print(\"Inside shinyHTM your image table is called 'htm'\")\nprint(\"This is a summary of of it:\")\nprint(\"==============================================================\")\nstr(htm)", 
                  type = "textarea", 
                  width = "800",
                  height = "100")
    ),
    


    navbarMenu("More",
               
        tabPanel("View Image Table",
            dataTableOutput("valuestable")
        ),
        
        tabPanel("About",
            
            p("By Hugo Botelho, March 2017"),
            a(href="mailto:hugobotelho@gmail.com", "hugobotelho@gmail.com"),
            a(href="http://webpages.fc.ul.pt/~hmbotelho", "http://webpages.fc.ul.pt/~hmbotelho"),
            hr(),
            p("Check for updates on GitHub"),
            a(href="https://github.com/hmbotelho/shinyHTM", "https://github.com/hmbotelho/shinyHTM")
        )
    )




))