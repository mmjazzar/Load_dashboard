library(shiny)
library(forecast)
library(tseries)
library(DT) 
library(ggplot2)
library(tidyr)
library(xts)
library(dygraphs)
library(forecastHybrid)

#webshot::install_phantomjs()
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

 
#ggplot(data = data(),aes(x = as.numeric(data()[,input$xcol]), y = as.numeric(data()[,input$ycol]))) + 
#geom_line( colour='purple') + 
#ggtitle("Temperature by month") +
#labs(x = input$xcol, y = input$ycol) 
#x <- data()[, c(input$xcol, input$ycol)]
#plot(mydata)
      
})

# Plot the data
output$dygraph <- renderDygraph({

dygraph( xts(x = data(), order.by = as.Date(data()[,input$xcol] , format='%m/%d/%Y'))
, main = "Predicted Deaths/Month" ) %>%
dyOptions(drawGrid = input$xcol) %>%
dyRangeSelector() 
      
})
    
##################### subset of the data.
subsetdata  <- reactive({

df <- data()
if (is.null(df)) return(NULL)
if(!is.numeric( input$Xrange[1]) ) {
        df 
}
      
if(!is.numeric( input$Yrange[1]) ) {
        df 
}
      
      
if(is.numeric( input$Xrange[1]) & is.numeric(df[,input$xcol])) {
df<- df [!is.na(df[,input$xcol]),]
        
df <-  df [df[,input$xcol] >= input$Xrange[1]&df[,input$xcol] <= input$Xrange[2],]
        
}
      
if(is.numeric( input$Yrange[1])& is.numeric(df[,input$ycol]) ) {
df<- df [!is.na(df[,input$ycol]),]
        
df <-  df [df[,input$ycol] >= input$Yrange[1]&df[,input$ycol] <= input$Yrange[2],]
        
}
      
df
})
    
#############################download plot downloadPlot
output$downloadPlot <- downloadHandler(
  filename <- function() {
  paste('plot1', 'png', sep = ".")
},
content <- function(file) {
  png(file)
  
  #x <- xts(x = data(), order.by = as.Date(data()[,input$xcol] , format='%m/%d/%Y'))
  #dygraph( x)
  x <- data()[, c(input$xcol, input$ycol)]
  plot(x)
  dev.off()
},
contentType = "image/png"
)#end download plot handler
    
#################### chossing the model ####################################
##applying model
modeldata <- reactive({ 

#Converting frequecny from character into int.
#mydata<-ts(data()[,input$ycol])
mydata<-ts(data()[,input$ycol],frequency= as.numeric(input$freq))

#choose between models
if(input$model =="ARIMA")
  {
    results <- auto.arima(mydata)
      
#  create a time series from data
#  mydata<-ts(data()[,2],frequency= input$freq)
#  results <- auto.arima(mydata, frequency= input$freq)
  }# end ARIMA 

# 2. ANN model
else if (input$model =="ANN") 
  {
    results <- nnetar(mydata)
# results <- nnetar(mydata,frequency= as.numeric(input$freq))
  }
# TBATS model
else if (input$model == "TBATS")
  {
    results <- tbats(mydata)
  }
# Exponential smoothing model
else if (input$model =="HoltWinters")
  {
    results <- HoltWinters(mydata)
  }
else if (input$model == "STLF")
  {
    results <-  stlf(mydata,s.window="periodic", method=c("ets","arima","naive","rwdrift"))
  }
else if (input$model == "HybridModel")
  {
    results <-  hybridModel(mydata)
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
# accuracy(fitted(modeldata()))
      
# if ARIMA
if(input$model =="ARIMA")
  {
    summary(modeldata())
  }#if TBATS
else if (input$model == "TBATS")
  {
    accuracy(modeldata()$fitted.values,mydaya)
  }#if ANN
else if (input$model == "ANN")
  {
    accuracy(modeldata()$fitted,mydaya)
  }
else if (input$model == "STLF")
  {
    accuracy(modeldata()$fitted,mydaya)
  }
else if (input$model == "HoltWinters")
  {
    accuracy(modeldata()$fitted,mydaya)
  }
else if (input$model == "HybridModel")
  {
    accuracy(modeldata()$fitted,mydaya)
  }

      
})
    
######################################################
# plotting forecasting period.########################
output$forecastPlot <- renderPlot({
   
if (input$model == "ARIMA" || input$model == "ANN")
  {
    autoplot(forecast(modeldata(), h = input$FP))
  }
else if (input$model == "TBATS")
  {
    p <- predict(modeldata())
    autoplot(p)
  }
else if (input$model == "HybridModel")
  {
  autoplot(forecast(modeldata(), h = as.numeric(input$FP)))
  
  }
  
else if(input$model =="HoltWinters")
  {
    y <- modeldata()
    p <- predict(y, input$FP)
  
    X <- cbind(  fitted = y$fitted[,1] , mean = p)
    df <- cbind(y$x, X)
    colnames(df) <- c("Data", "Fitted", "Forecasting")
  
    autoplot(df)

  }
else if(input$model == "STLF")
  {
    autoplot(modeldata())
  }
      
})
############################################################
########oresent forecast data

output$forecastTables <- renderDataTable({
  # global(i=1) 
  
  if (input$model == "ARIMA")
  {
    p <- forecast(modeldata(),h = input$FP)
    a <- cbind(p$mean,p$lower,p$upper)
    
  }
  else if (input$model == "ANN")
  {
    p <- forecast(modeldata(),h = input$FP)
    a <- cbind( seq(1:length(p$mean)),p$mean)
  }
  else if (input$model == "TBATS")
  {
    p <- (predict(modeldata()))
    a <- cbind(p$mean,p$lower,p$upper)
    
  }
  else if(input$model == "STLF" || input$model == "HoltWinters" || input$model == "HybridModel")
  {
    p <- forecast(modeldata(),h = input$FP)
    a <- cbind( seq(1:length(p$mean)),p$mean)
  }

#  colnames(a) <- c("Point Forecast","LO 95","HI 95","Lo 80","Hi 80")
}, options = list(lengthMenu = c(10, 30, 50), pageLength = 10,searching=TRUE))

############################################################
# Download forecasted data and figure as pdf
output$downloadfor <- downloadHandler(
  a <-  data.frame( residuals(modeldata())),
  
  filename = function() {paste("a",'.csv',sep='')},
  content = function(file){
    write.csv(a,file)
  }
  
)#end function


    
############################################################
##############Reiduals######################################

output$residualsTables <- renderDataTable({
# global(i=1) 
a <-  cbind( seq(1:nrow(data())),residuals(modeldata()))

}, options = list(lengthMenu = c(10, 30, 50), pageLength = 10,searching=TRUE))
    

###########################################################
# plot  residuals figure.
# adjusting time series by date.
output$residualsPlot <- renderDygraph({

  x <- xts(x = residuals(modeldata()), order.by = as.Date(data()[,input$xcol] , format='%m/%d/%Y'))
  
  dygraph(x)
})
############################################################
# Download Residuals data
  output$downloadRes <- downloadHandler(
    a <-  data.frame( residuals(modeldata())),
    
    filename = function() {paste("a",'.csv',sep='')},
    content = function(file){
    write.csv(a,file)
    }
    
  )#end function
    
}# end session


