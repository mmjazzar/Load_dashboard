library(shiny)

ui <- shinyUI(fluidPage(
  titlePanel("Load Forecasting Dashboard"),
  tabsetPanel(
    tabPanel("Upload File",
             titlePanel("Uploading Files"),
             sidebarLayout(
               sidebarPanel(
                 
                 tags$form(
                   fileInput('file1', 'Choose CSV File',
                             accept=c('text/csv', 
                                      'text/comma-separated-values,text/plain','.csv')),
                   
                   radioButtons('sep', 'Separator',c(Comma=','
                                                     ,Semicolon=';',Tab='\t'),','),
                   actionButton("upload", "Upload")
                 )
                 ) 
                 
                 
           ,
               
               mainPanel(
                 dataTableOutput('contents')
               )
             )#sidebar layout
    ),
#####################################################################
#####################################################################
    tabPanel("Plot",
             pageWithSidebar(
               headerPanel('Data'),
               sidebarPanel(
                 
                 tags$form(
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('xcol', 'X axis', "",selected = 1),
                 selectInput('ycol', 'Y axis', "", selected = 1),
                 downloadButton('downloadPlot', 'Download Plot'),
                 actionButton("drawPlot","Plot")
               )),mainPanel(
                  dygraphOutput("dygraph")
               )
             )
),# end of tabpanel 2
#####################################################################
####################################################################
    tabPanel("Model",
             pageWithSidebar(
               headerPanel('Model'),
               # 1. upload button for regressors. 
               # 2. apply a specific model.
               # 3. divide the data.
                sidebarPanel(
                  tags$form(
                   selectInput("model", "please specifiy the model",
                               c("ANN" = "ANN",
                               "ARIMA" = "ARIMA",
                               "HoltWinters" = "HoltWinters",
                               "HybridModel" = "HybridModel",
                               "STLF" = "STLF",
                               "TBATS" = "TBATS"
                    )),
                   selectInput("freq", "please specifiy the data frequency",
                                c("24" = 24,
                                  "168" = 168,
                                  "365" = 365,
                                  "7866" = 7866
                   ),selected = 24),
                   sliderInput(inputId   = "train",
                               label     ="Training",
                               min       = 1,
                               max       = 70, 
                               value     = 70),
                   sliderInput(inputId   = "test",
                               label     = "Testing",
                               min       = 1,  
                               max       = 30,  
                               value     = 15),
                   sliderInput(inputId   = "Validte",
                               label     ="Validation",
                               min       = 1,
                               max       = 30, 
                               value     = 15),
                   actionButton("applymodel","Apply")
               )),
               #main panel of tabpanel 3
               mainPanel(
                 
                tags$article(
                #textOutput("modelData")
                verbatimTextOutput("modelData")
                 ),
                 tags$hr(),
                 tableOutput("modelData2")

                 )
             )
    ),
#end of main panel 3 ####################################
##########################################################
    
    tabPanel("Forecast Plot",
             titlePanel("Forecasting Polt"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("FP", "please specifiy the Forecasting period",
                             c("24" = 24,
                               "168" = 168,
                               "365" = 365,
                               "7866" = 7866
                             ),selected = 24),
                 downloadButton('forecastData', 'Download Data')
              ),
               
               mainPanel(
                 plotOutput("forecastPlot"),
                 tags$hr(),
                 dataTableOutput('forecastTables')
                
               )
             )#sidebar layout
    ),
#end of main panel 3 ####################################
##########################################################
    tabPanel("Residuals",
             titlePanel("Residuals"),
             sidebarLayout(
               sidebarPanel(
                 tags$p('if you want to download the data'),
                 downloadButton('downloadRes', 'Download Data')
               ),
               mainPanel(
                 dygraphOutput('residualsPlot'),
                tags$hr(),
                 dataTableOutput('residualsTables')
               )
             )#sidebar layout
    ),
#end of main panel 5 ####################################
##################About###############################
    tabPanel("About",
             titlePanel("About"),
             mainPanel(
               
               tags$hr()
               
               
             )
    )
  )#end tabset panel
  
)#end fluidpanel
)
