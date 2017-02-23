library(shiny)
library(forecast)
library(tseries)

#library(forecast)
function(input, output, session) {
  # added "session" because updateSelectInput requires it
  
  data <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1 
    
    df <- read.csv(inFile$datapath, header = TRUE, sep = input$sep)
    
    # Update inputs (you could create an observer with both updateSel...)
    # You can also constraint your choices. If you wanted select only numeric
    # variables you could set "choices = sapply(df, is.numeric)"
    # It depends on what do you want to do later on.
    
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = names(df), selected = names(df)[2])
    
    return(df)
  })
  
  output$contents <- renderTable({
    data()
  })
  
  # I Since you have two inputs I decided to make a scatterplot
    output$MyPlot <- renderPlot({
      x <- data()[, c(input$xcol, input$ycol)]
    plot(x)
      
    })
    #download plot downloadPlot
    output$downloadPlot <- downloadHandler(
      filename <- function() {
        paste('plot1', 'png', sep = ".")
      },
      content <- function(file) {
        png(file)
        x <- data()[, c(input$xcol, input$ycol)]
        plot(x)
      dev.off()
      },
      contentType = "image/png"
    )#end download plot handler
    
####################chossing the model
    ###########################################################
    #######################################################
    #######################################################
    ##applying model
    modeldata <- reactive({ 
      x <- data()
      mydata<-ts(x[,input$ycol])
      #choose between ANN, ARIMA
      if(input$model =="ARIMA"){
        #do ARIMA 
      
        results <- auto.arima(mydata)
        
      #  create a time series from data
     #   mydata<-ts(data()[,2],frequency= input$freq)
      #  results <- auto.arima(mydata, frequency= input$freq)
        
      } else if (input$model =="ANN") {
        #do ANN
        results <- nnetar(mydata)
      }#end if  
      
      
      #  modelSummery <- summary(y)
      return(results)
    })
    
    #presenting output###########################################
    output$modelData <- renderPrint({
      modeldata()  
    })
    ######################################################
    # I Since you have two inputs I decided to make a scatterplot
    output$forecastPlot <- renderPlot({
      x <- modeldata()[, c(input$xcol, input$ycol)]
      plot(x)
      
    })
    

    
}
