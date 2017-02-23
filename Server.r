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
                      choices = names(df), selected = names(df)[1])
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

      # Converting frequecny from character into int.
      #mydata<-ts(x[,input$ycol],frequency= as.numeric(input$freq))
      
      mydata<-ts(data()[,input$ycol])
      #choose between ANN, ARIMA
      if(input$model =="ARIMA"){
        #do ARIMA 
      
        results <- auto.arima(mydata)
        
      #  create a time series from data
     #   mydata<-ts(data()[,2],frequency= input$freq)
      #  results <- auto.arima(mydata, frequency= input$freq)
        
      } else if (input$model =="ANN") {
        #do ANN
       # results <- nnetar(mydata,frequency= as.numeric(input$freq))
      }#end if  
      
      
      #  modelSummery <- summary(y)
      return(results)
    })
    
    #presenting output###########################################
    output$modelData <- renderPrint({
  
        modeldata()  
      
      summary(modeldata())
      
    })
    ######################################################
    output$forecastPlot <- renderPlot({
      x <- modeldata()[, c(input$xcol, input$ycol)]
      plot(x)
      
    })
    
############################################################
    ##############Reiduals 
    output$residualsTables <- renderTable({
      residuals(modeldata())
    # accuracy(fit12, mydata)
    # add accuracy section
      })
    
    output$residualsPlot <- renderPlot({
      plot(ts(data()[,input$ycol]),xlab= input$xcol,ylab=input$ycol)
     lines(fitted(modeldata()), col="red")
     legend("topleft", lty=1,col=c(4,2),
            legend=c("Real Data","fitted Data"))
     
 #    plot(residuals(modeldata()), col="red"))
      
    })
    
    
    
    
}
