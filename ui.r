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
    tabPanel("Model",
             pageWithSidebar(
               headerPanel('Model'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
               
               ),#main panel of tabpanel 2
               mainPanel( 
               
               )
             )
    )
    
  )#end tabset panel
)#end fluidpanel
)
