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
# adding action button  

  
  data <- reactive({
    input$upload
    isolate(
    req(input$file1) ## ?req #  require that the input is available
    )
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
    updateSelectInput(session, inputId = 'xreg', label = 'xreg',
                      choices = names(df), selected = names(df)[3])
    
    
    
    return(df)
    
})

###################################################################
#1. presenting data
  
  output$contents <- renderDataTable(
    {
      data()
    },
    
    options = list(lengthMenu = c(10, 30, 50), pageLength = 10,searching=TRUE)
  
  )#end function
  
###################################################################
#2. plotting the function using plot methods.
# the function is replaced with dygraph
  
  output$MyPlot <- renderPlot(
    {
    #x <- data()[, c(input$xcol, input$ycol)]
    #plot(mydata)
      
    }
  )# end function

###################################################################
# Plot the data using dygraph
  
  output$dygraph <- renderDygraph(
    {
      input$drawPlot
      isolate(
      dygraph( xts(x = data(), order.by = as.Date(data()[,input$xcol] , format='%m/%d/%Y'))
      , main = "Predicted Deaths/Month" ) %>%
      dyOptions(drawGrid = input$xcol) %>%
      dyRangeSelector() 
      )
    })

##########################################################################      
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
    
############################################################################
#############################download plot downloadPlot based on plot function
  output$downloadPlot <- downloadHandler(
    
    filename = function () {paste('plot', '.png',sep='') },
    content = function(file) {
      cat(output$dygraph,file = 'temp.html')
      
    saveWidget(x, "temp.html", selfcontained = FALSE)
     webshot("temp.html", file = file)
    }
    
    

    )#end download plot handler
    
#################### chossing the model ####################################
##applying model
  modeldata <- reactive(
    { 
      #Converting frequecny from character into int.
      #adding regressors
      xreg <- (data()[ ,input$xreg])
      mydata<-ts(data()[ ,input$ycol],frequency= as.numeric(input$freq))
      
      #choose between models
      if(input$model =="ARIMA")
      {
        #results <- auto.arima(mydata)
        results <- auto.arima(mydata,xreg = xreg ,frequency= input$freq)
      }# end ARIMA 
      
      # 2. ANN model
      else if (input$model =="ANN") 
      {
        #results <- nnetar(mydata)
         results <- nnetar(mydata,xreg = xreg,frequency= as.numeric(input$freq))
      }
      # TBATS model, note TBATS do not accept regressors
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
        results <- hybridModel(mydata,
                               models = "aenst",
                               a.args = list(xreg = xreg),
                               n.args = list(xreg = xreg),
                               s.args = list(xreg = xreg, method = "arima"))
      }
      
      input$applymodel
      isolate(        return(results))

    })#end function
    
#########################################################################
#presenting output
    output$modelData <- renderPrint(
     {
       input$applymodel
       isolate(
        modeldata()
        )
     })

#presenting summary    
    output$modelData2 <- renderTable(
     {
       input$applymodel
       isolate({
       mydaya <- ts(data()[,input$ycol])
       
#comparing models
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
      })#end function
    
###############################################################
# plotting forecasting period.##############################
  output$forecastPlot <- renderPlot(
    {
      xreg <- data.frame(data()[ ,input$xreg])
      input$forecastAction
    isolate(    {
      if (input$model == "ARIMA" || input$model == "ANN")
      {
        autoplot(forecast(modeldata(),xreg = xreg, h = input$FP))
      }
      else if (input$model == "TBATS")
      {
        p <- predict(modeldata())
        autoplot(p)
      }
      else if (input$model == "HybridModel")
      {
      
        autoplot(forecast(modeldata(),xreg = xreg, h = as.numeric(input$FP)))
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

    }
  )#end function
############################################################
######## present forecast data

    output$forecastTables <- renderDataTable({
      xreg <- data.frame(data()[ ,input$xreg])
      input$forecastAction
      isolate({
        
        if (input$model == "ARIMA")
        {
          p <- forecast(modeldata(), xreg = xreg, h = input$FP)
          a <- cbind(p$mean,p$lower,p$upper)
          colnames(a) <- c("Point Forecast","LO 95","HI 95","Lo 80","Hi 80")
        }
        else if (input$model == "ANN")
        {
          p <- forecast(modeldata(), xreg = xreg, h = input$FP)
          a <- cbind( seq(1:length(p$mean)),p$mean)
          colnames(a) <- c("Point Forecast","mean")
          
        }
        else if (input$model == "TBATS")
        {
          p <- (predict(modeldata()))
          a <- cbind(p$mean,p$lower,p$upper)
          colnames(a) <- c("Point Forecast","LO 95","HI 95","Lo 80","Hi 80")
        }
        else if(input$model == "STLF" || input$model == "HoltWinters")
        {
          p <- forecast(modeldata(),h = input$FP)
          a <- cbind( seq(1:length(p$mean)),p$mean)
          colnames(a) <- c("Point Forecast","mean")
          
        }
        else if (input$model ==  "HybridModel")
        {
          
          
          p <- forecast(modeldata(), xreg = xreg, h = as.numeric(input$FP)
                        , FUN = hybridModel)
          a <- cbind( seq(1:length(p$mean)),p$mean)
          colnames(a) <- c("Point Forecast","mean")
          
      
        }
        # format the data
        a <- formatC(a)  
      })
      
#  colnames(a) <- c("Point Forecast","LO 95","HI 95","Lo 80","Hi 80")
    }, options = list(lengthMenu = c(10, 30, 50), pageLength = 10,searching=TRUE))

############################################################
# Download forecasted data and figure as pdf
    output$forecastData <- downloadHandler(
      filename = function() {
        
        paste("forecasting-",input$model,'.csv',sep='')},
      content = function(file){
        xreg <- data.frame(data()[ ,input$xreg])
        
        
        if (input$model == "ARIMA")
        {
          p <- forecast(modeldata(),xreg= xreg,h = input$FP)
          dataFor <- data.frame(p$mean,p$lower,p$upper)
          colnames(dataFor) <- c("Point Forecast","LO 95","HI 95","Lo 80","Hi 80")
        }
        else if (input$model == "ANN")
        {
          p <- forecast(modeldata(),xreg = xreg,h = input$FP)
          dataFor <- data.frame(p$mean)
          colnames(dataFor) <- c("Forecasting")
          
        }
        else if (input$model == "TBATS")
        {
          p <- (predict(modeldata()))
          dataFor <- data.frame(p$mean,p$lower,p$upper)
          colnames(dataFor) <- c("Point Forecast","LO 95","HI 95","Lo 80","Hi 80")
        }
        else if(input$model == "STLF" || input$model == "HoltWinters")
        {
          p <- forecast(modeldata(),h = input$FP)
          dataFor <- data.frame( p$mean)
          colnames(dataFor) <- c("Forecasting")
          
        }
        else if (input$model == "HybridModel")
        {
          p <- forecast(modeldata(), xreg = xreg, h = as.numeric(input$FP)
                        , FUN = hybridModel)
          dataFor <- data.frame( p$mean)
          colnames(dataFor) <- c("Forecasting")
        }
        # format the data
       # dataFor <- formatC(dataFor)  
        write.csv(dataFor,file)
      }
      
    )#end function
    

    
############################################################
##############Reiduals######################################

  output$residualsTables <- renderDataTable(
    {

      if(input$model == "HoltWinters" || input$model == "HybridModel")
      {
        a <-  cbind( seq(1:length(residuals(modeldata()))),residuals(modeldata()))
        
        colnames(a) <- c("Sequence","Residuals")
        
      }else 
      {
        a <-  cbind( seq(1:nrow(data())),residuals(modeldata()))
        colnames(a) <- c("Sequence","Residuals")
      }
      # removing NULL values, adjusting data format
      a <- na.omit(a)
      a <- formatC(a)


    },
    options = list(lengthMenu = c(10, 30, 50), pageLength = 10,searching=TRUE))
    

########################################################### Residuals plot
# plot  residuals figure.
# adjusting time series by date.
    output$residualsPlot <- renderPlot(
      {
        autoplot(residuals(modeldata()))
      }
    )
############################################################
# Download Residuals data
  output$downloadRes <- downloadHandler(
    filename = function() {

      paste("res-",input$model,'.csv',sep='')},
    content = function(file){
      
      DataRes <-  data.frame(residuals(modeldata()))
      # removing NULL values, adjusting data format
      DataRes <- na.omit(DataRes)
      colnames(DataRes) <- c("Residuals")
      write.csv(DataRes,file)
    }
    
    )#end function
    
}# end session


