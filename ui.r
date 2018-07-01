library(shiny)
library(plotly)
library(shinyjs)
library(ggplot2)



shinyUI(navbarPage("shinyHTM",

    # Initialize shinyjs
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),
                   
                   
    tabPanel("1. Upload data",
        h2("Upload data"),
        helpText("Upload an image table (must be a comma-separated, tab-sepatated or Excel file)."),
        fluidRow(
            column(8,
                fileInput('file1', 'Choose File', accept=c('.csv', '.txt', 'tsv', '.xlsx'), width = "90%")
            ),
            column(4,
                radioButtons("loadFileType", label = h4("File format"),
                             choices = list("Auto detect" = "Auto", "Comma separated (csv)" = "csv", "Tab separated (txt)" = "tsv", "Excel (xlsx)" = "xlsx"), 
                             selected = "Auto"),
                br(),
                radioButtons("loadDecimalSeparator", label = h4("File format"),
                             choices = list("Auto detect" = "Auto", "Dot (.)" = ".", "Comma (,)" = ","), 
                             selected = "Auto")
            )
        )
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
        
        uiOutput("UIpathInTable"),
        uiOutput("UIpathInComputer"),
        uiOutput("UIprefixPath"),
        uiOutput("UIprefixFile"),
        hr(),
        
        uiOutput("UIcolNameObjectPosX"),
        uiOutput("UIcolNameObjectPosY"),
        uiOutput("UIcolNameObjectPosZ"),
        hr(),
        
        h3("Heatmap settings"),
        uiOutput("UIwells_Y"),
        uiOutput("UIwells_X"),
        uiOutput("UInpos_Y"),
        uiOutput("UInpos_X"),
        uiOutput("UIsquaredodge"),
        uiOutput("UIsquaresize")
    ),




    tabPanel("3. Plot",
        h2("Plotting"),
        p("Use this plotting tool to explore your data and inspect the images which produced each data point."),
        verbatimTextOutput("selection"),
        plotlyOutput("plot"),
        br(),
        actionButton("plotScatterBoxOrHeatmap", "Update plot", icon = icon("refresh"), width = "100%", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        hr(),
        
        
        fluidRow(
            
            column(2,
                selectInput("plotType", label = "Plot Type", choices = c("Scatter plot", "Boxplot", "Heatmap"))
            ),
            
            column(7,
                uiOutput("UIselectXaxis"),
                uiOutput("UIselectYaxis"),
                uiOutput("UILUTminmax"),
                uiOutput("UILUTcolors"),
                br(),br(),
                
                uiOutput("UIhighlightQCfailed"),
                
                # Scatter plot customization options
                uiOutput("UIPointplotsBeeswarm"),
                uiOutput("UIPointplotsplitBy"),
                br(),
                uiOutput("UIPointplotSubsample"),
                uiOutput("UIPointplotSubsampleN"),
                uiOutput("UIPointplotSubsampleM"),
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
        uiOutput("UINormFormula"),
        uiOutput("UINormNegCtrl"),
        # checkboxInput("NormCombinedVecror", "[DISABLED] Compute combined vector for all selected measurements?"),
        # selectInput("NormMultiply", "[DISABLED] Multiply with cos(tetha)^N along average treatment effect; N=", choices = list(0,1,2,4,8,16,32)),
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
               
        tabPanel("View table",
            #actionButton("update_dataTable", "Update table", icon = icon("refresh"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            #br(),
            #dataTableOutput("ValuesTable")
            uiOutput("UIValuesTable")
        ),
        
        tabPanel("Save & Load settings",
            h2("Save & Load settings"),
            br(),
            actionButton("buttonSessionSave", "Save session...", icon = icon("save")),
            br(),br(),br(),
            actionButton("buttonSessionLoad", "Restore previous session...", icon = icon("refresh")),
            br(),br(),
            verbatimTextOutput("echo_SaveLoadSession")
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