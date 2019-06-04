
#Library




server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  #Will save the id hist element to output
  # {RCODE}#
  #Access the slider via $input$ID
  # data<- reactive( {rnorm(input$num)}) #Changes value as soon as you slide
  #  data2<-eventReactive(input$clicks, {rnorm(input$num)}) #Changes Value upon clicks
  #reactive values
  rv<-reactiveValues(data=rnorm(100))
  #We create some Data values inside rv
  
  
  # observeEvent(input$uniform, {rv$data<-runif(100)})
  # observeEvent(input$clicks, {rv$data<-rnorm(input$num)})
  # 
  titolo<- eventReactive(c(input$clicks),#,input$normal,input$uniform),
                         {input$title})
  stocks<-eventReactive(input$clicks,
                        {
                          strsplit(input$stocks,split=";")[[1]]
                        })
  start_date<-eventReactive(input$clicks,
                            {
                              input$dates[1]
                            })
  end_date<-eventReactive(input$clicks,
                          {
                            input$dates[2]
                          })
  
  
  valueToPlot<-eventReactive(input$clicks,{
    input$radio3
  }
  )
  compression<-eventReactive(input$clicks,{
    input$radio2
  })
  #renderHist
  output$plot<-renderPlot(
    {hist(rv$data,#data2(),
          main =titolo()
    )}
  )
  
  ##Various Observers
  ##Plot Type
  observeEvent(input$clicks, {if(input$radio==1)
    output$plot<-renderPlot(
      {validate(need(input$radio==1,message=FALSE))
        hist(rv$data,main=titolo(),breaks=20)}
    )})
  observeEvent(input$clicks, {if(input$radio==2)
    output$plot<-renderPlot(
      {plot(density(rv$data),main=titolo())}
    )})
  observeEvent(input$clicks, {if(input$radio==3)
    output$plot<-renderPlot(
      {qqnorm(rv$data,main=titolo());
        qqline(rv$data)}
    )})
  observeEvent(input$clicks, {if(input$radio==4)
    output$plot<-renderPlot(
      chart.Boxplot(rv$data,main=titolo(), outlier.symbol = "X")
    )})
  # observeEvent(input$clicks, {if(input$radio==5)
  #   output$plot2<-renderDygraph(
  #     {dygraph(rv$data, main=titolo())}
  #   )})
  ## UPDATE STOCKS OBSERVER
  observeEvent(input$clicks, {
    rv$data<-as.xts(na.omit(
      get.hist.quote(instrument =stocks(),
                     start =start_date(),
                     end =end_date(),
                     quote =valueToPlot(),
                     provider ="yahoo",
                     compression =compression(),
                     retclass ="zoo")))
  }
  )
  #observeEvent(input$clicks,{print("Current Val");print(valueToPlot());print(head(rv$data))
  #})
  
  # observeEvent(input$clicks, {if(input$radio3==5){
  #   #DoStuff
  #   
  # }
  #   
  # })
  # 
  
  #renderPrint Stats
  output$stats<-renderPrint(
    summary(rv$data)
  )
  output$quantile<-renderPrint(quantile(rv$data))
  output$kurtosis<-renderPrint(kurtosis(rv$data))
  output$skewness<-renderPrint(skewness(rv$data))
  
  
  #############
  #FORECASTING#
  #############
  
  start_dateForecasting<-eventReactive(
    input$clicksForecast,
    {
     input$datesForecasting[1]
    }
  )
  
  end_dateForecasting<-eventReactive(
    input$clicksForecast,
    {
      input$datesForecasting[2]
    }
  )
  
  stocksForecasting<-eventReactive(
    input$clicksForecast,
    {
      input$stocksForecasting
    }
  )
  
  methodForecasting<-eventReactive(
    input$clicksForecast,
    {
      input$arimaRadio
    }
  )
  
  trainingTime<-eventReactive(
    input$clicksForecast,
    {
      input$sliderForecasting
    }
  )
  
  
  observeEvent(input$clicksForecast,
               {
                 #
                 #will return a vector of decomposition and forecasting
                 #the arimaMethod is specified in order to solve some edge cases
                 #start,end are dates
                 #training time is a percentage indicating how much of the allotted time is give for training
                 # returns fitVal and arma.forecast (minimum)
               
                   
                   #Convert to percentage
                   trainingTimeStart<-trainingTime()[1]/100
                   trainingTimeEnd<-trainingTime()[2]/100    
                   dataset <- get.hist.quote( instrument=stocksForecasting(),
                                              start=start_dateForecasting(),
                                              end=end_dateForecasting()
                                              ,quote="AdjClose", 
                                              provider="yahoo", origin="1970-01-01", compression="month")
                   
                   index(dataset)<- as.yearmon(index(dataset)) #The format date index neeeds to change format
                   colnames(dataset)<- stocksForecasting()
                   dataset<-aggregate(dataset,index(dataset),tail,1)
                   #We now decompose the ts in its 4 components
                   fitVal <- stl(dataset[,1], s.window = "periodic")
                   mainName<-paste("Decomposition of",stocksForecasting())
                   output$Decomposition<-renderPlot(plot(fitVal,main=mainName)) 
                   #Compounded Returns
                   returns <- diff(log(dataset[,1]))
                   #Dataset Training for Forecasting
                   returnsTrain <- returns[1:(trainingTimeStart*length(returns))]#60% of returns is used for training
                   returnsTest <- returns[(trainingTimeStart*length(returns)+1):(trainingTimeEnd*length(returns))]#The remainder 60-80% is used for testing
                   returnsValidation<-returns[(trainingTimeEnd*length(returns)+1):length(returns)]#Then 80%-100% is used for validating
                   #ar,ma are the first values we attemp to use in our arima function
                   #max is the threshold for mistakes
                   ar=0;
                   ma=0;
                   max=100;
                   for (p in 1:6){
                     for(f in 1:6){
                       fit <- arima(returnsTrain, order = c(p, 0, f),method=methodForecasting()) #without ML the UVV stock didn't work
                       arma.predictions <- predict(fit, n.ahead = (length(returns) - (trainingTimeStart * length(returns))))$pred
                       s=accuracy(arma.predictions, returnsTest)[2]
                       if(s<max){
                         max=s;
                         ar=p;
                         ma=f;
                       }
                     }
                   }
                   #chosen values
                   ar
                   ma
                   
                   fit <- arima(returnsTrain, order = c(ar, 0, ma))
                   arma.predictions <- predict(fit, n.ahead = (length(returns) - (trainingTimeStart * length(returns))))$pred
                   arma.forecast <- forecast(fit, h = (length(returns)-length(returnsTrain)),level = c(95,80))
                   #arma.forecast
                   mainName<- paste("ARMA forecast for returns of", stocksForecasting())
                   output$Forecast<-renderPlot({ plot(arma.forecast, main = mainName)
                     lines(returns)
                     lines(returnsValidation,col="red")})
                   #We don't want all the accuracy values
                   
                   #Accuracy compared to start-end%
                   
                   output$statsForecast<-renderPrint({
                     cat("Accuracy: Test Period (RMSE) \n",
                     accuracy(arma.predictions, returnsTest)[2],
                     "\n Accuracy: Validation  (RMSE)\n",
                     accuracy(arma.predictions, returnsValidation)[2]
                     )})
                   #Accuracy compared to last end%
                   #accuracy(arma.predictions, returnsValidation)[2]
                   #lines(returns)
                   #lines(returnsValidation, col = "red")
               }
               )
  
  ##############
  #MULTIVARIATE#
  ##############
  
  
  

  start_date2<-eventReactive(input$clicksMultivariate,
                            {
                              input$datesMulti[1]
                            })
  end_date2<-eventReactive(input$clicksMultivariate,
                          {
                            input$datesMulti[2]
                          })
  
  stocks2<-eventReactive(input$clicksMultivariate,
                        {
                          (x=c(input$stocksMulti1,input$stocksMulti2,input$stocksMulti3,input$stocksMulti4));
                          x=unique(x[x!=""])
                          
                          x=returnsCalculator(x,start_date2(),end_date2())
                          
                        })
  
  observeEvent(input$clicksMultivariate,{
    
    if(input$radioMulti==1){
      output$dygraphPlot<-renderDygraph({dygraph(stocks2())})
    }
    if(input$radioMulti==2){
      pairsSet<-c()
      for (i in 1:ncol(stocks2())) {
        
        x<-as.numeric(stocks2()[,i])
        pairsSet<-cbind(pairsSet,x)
        #print("____________________")
        #print(x)
      }
      
      #print(pairsSet)
      ##FIXTHIS
      
      output$correlationMatrix<-renderPlot({pairs(pairsSet,main="Correlation Matrix",lower.panel=panel.cor,upper.panel=upper,labels = colnames(stocks2()))})
    }
    
  })
  
  
  

##
  ##FUNCTIONS
  returnsCalculator<-function(x,start,end){
    dataset<-as.data.frame(NULL,NULL)
    for (i in x) {
      dataset<-cbind(dataset,
                     as.xts(na.omit(get.hist.quote(
        instrument=i, 
        start=start,
        end=end,
        quote ="AdjClose",
        provider ="yahoo",
        compression ="m", 
        retclass ="zoo"))))
        
      
    }
    colnames(dataset)<-(x[1:length(x)])
    
    dataset<-Return.calculate(dataset,"discrete")
 
    
    #if one of the series begins/ends later/earlier it WILL cut the other stocks!
    return(na.omit(dataset))
  }
  
  panel.cor <- function(x, y){
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0,1,0,1))
    r <- round(cor(x, y), digits=2)
    txt <- paste0("R = ", r)
    cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r+0.5)
  }
  #We distinguish between the values of the two stocks
  upper<-function(x,y){
    my_cols <- c("red","green3") 
    points(x,y,pch=19, col=my_cols)
  }
  
  
 
##


  
}
