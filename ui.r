library(shiny)
library(datasets)

ui <- shinyUI(fluidPage(
  titlePanel("Load Forecasting Dashboard"),
  tabsetPanel(
    tabPanel("Upload File",
             titlePanel("Uploading Files"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Choose CSV File',
                    accept=c('text/csv', 
                      'text/comma-separated-values,text/plain','.csv')),
                 
                radioButtons('sep', 'Separator',c(Comma=','
                    ,Semicolon=';',Tab='\t'),',')
              ),
               
               mainPanel(
                 tableOutput('contents')
               )
             )#sidebar layout
    ),
    ########################################
    #########################################
    tabPanel("plot",
             pageWithSidebar(
               headerPanel('Data'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('xcol', 'X axis', ""),
                 selectInput('ycol', 'Y axis', "", selected = ""),
                 downloadButton('downloadPlot', 'Download Plot')
               ),
               mainPanel(
                 plotOutput('MyPlot')
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
                selectInput("model", "please specifiy the model",
                             c("ARIMA" = "ARIMA",
                               "ANN" = "ANN"
                                  
               )),  selectInput("freq", "please specifiy the data frequency",
                                c("24" = "24",
                                  "168" = "168",
                                  "365" = "365",
                                  "7866" = "7866"
                                )),
               sliderInput(    inputId   = "train",
                               label     ="Training",
                               min       = 1,
                               max       = 70, 
                               value     = 70),
               sliderInput(    inputId   = "test",
                               label     = "Testing",
                               min       = 1,  
                               max       = 30,  
                               value     = 15),
               sliderInput(    inputId   = "Validte",
                               label     ="Validation",
                               min       = 1,
                               max       = 30, 
                               value     = 15)
               
               ),
               #main panel of tabpanel 3
               mainPanel( 
                
                 textOutput('modelData')
               )
             )
    ),
    #end of main panel 3 ####################################
    ##########################################################
    
    tabPanel("Forecast Plot",
             titlePanel("Forecasting Polt"),
             sidebarLayout(
               sidebarPanel(
                 
                 
               ),
               
               mainPanel(
                 
               )
             )#sidebar layout
    ),
    #end of main panel 4 ####################################
    ##########################################################
    tabPanel("Forecasting Data",
             titlePanel("Forecasting Data"),
             sidebarLayout(
               sidebarPanel(
                 
                  ),
               
               mainPanel(
                
               )
             )#sidebar layout
    ),
    
    #end of main panel 3 ####################################
    ##########################################################
    tabPanel("Residuals",
             titlePanel("Residuals"),
             sidebarLayout(
               sidebarPanel(
                 
                
               ),
               
               mainPanel(
                 plotOutput('forecastPlot')
                 
               )
             )#sidebar layout
    )
    
    #end of main panel 3 ####################################
    ##########################################################
    
  )#end tabset panel
)#end fluidpanel
)
