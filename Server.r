library(shiny)
library(forecast)
library(tseries)
library(DT) 
#source("common.R")

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
  ############################################################
  output$contents <- renderDataTable({
    data()
  }, options = list(lengthMenu = c(10, 30, 50), pageLength = 10,searching=TRUE)
  
  )
  
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
      mydata<-ts(data()[,input$ycol],frequency= as.numeric(input$freq))
      
      #mydata<-ts(data()[,input$ycol])
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
        
       # results <- nnetar(mydata,frequency= as.numeric(input$freq))
      } else if (input$model == "TBATS"){
        
        results <- tbats(mydata)
      } else if (input$model =="HoltWinters")
      {
        results <- HoltWinters(mydata)
        
      } else if (input$model == "LR")
      {
        results <- tslm(mydata ~ trend)
        
      }
      
      
      #  modelSummery <- summary(y)
      return(results)
    })
    
    #presenting output###########################################
    output$modelData <- renderPrint({
    modeldata()
     })
    output$modelData2 <- renderTable({
      mydaya <- ts(data()[,input$ycol])
      # to be edited 
   #   accuracy(fitted(modeldata()))
      
      # if ARIMA
      if(input$model =="ARIMA"){
        summary(modeldata())
      }#if TBATS
      else if (input$model == "TBATS"){
        accuracy(modeldata()$fitted.values,mydaya)
      }#if ANN
      else if (input$model == "ANN"){
        accuracy(modeldata()$fitted,mydaya)
        
      }
      
    })
    
######################################################
    output$forecastPlot <- renderPlot({
   
      if (input$model == "ARIMA" || input$model == "ANN")
      plot(forecast(modeldata(), h = input$FP))
      else {
        plot(forecast(modeldata()))
        
      }
      
    })
    
############################################################
    ##############Reiduals 
    output$residualsTables <- renderDataTable({
    # global(i=1)
    }, options = list(lengthMenu = c(10, 30, 50), pageLength = 10,searching=TRUE))
    
    
    output$residualsPlot <- renderPlot({
      plot(ts(data()[,input$ycol]),xlab= input$xcol,ylab=input$ycol)
     lines(fitted(modeldata()), col="red")
     legend("topleft", lty=1,col=c(4,2),
            legend=c("Real Data","fitted Data"))
     
 #    plot(residuals(modeldata()), col="red"))
     #more plots
     # plot(density(resid(data12)),main="Residuals Distribution") #A density plot

    })
    
    
    
    
}
