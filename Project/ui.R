library(shinydashboard)
library(curl)
library(zoo)
library(tseries)
library(PerformanceAnalytics)
library(quantmod)
library(dygraphs)
library(forecast)





##HEADER
header<- dashboardHeader(title = "820651-WebApp")
##SIDEBAR
sidebar<-dashboardSidebar(
  sidebarMenu(
    menuItem("Descriptive Statistics", tabName="Stats", icon=icon("bar-chart-o")),
    menuItem("Forecasting", tabName="Forecasting", icon=icon("list-alt")),
    menuItem("Multivariate", tabName="Multivariate", icon=icon("list"))
  )
)

##MENU TAB1
tab1<-tabItem( tabName="Stats",
               fluidRow(
                 box(
                   title="Plots",
                   status="primary",
                   solidHeader=TRUE,
                   collapsible=TRUE,
                   # plotOutput("plot"),
                   # dygraphOutput("plot2")
                   conditionalPanel('input.radio<=4', plotOutput("plot")),
                   conditionalPanel('input.radio==5', dygraphOutput("plot2"))
                   ##There has to be a better solution
                 ),
                 
                 box(
                   title = "Controls",
                   solidHeader = TRUE,
                   collapsible=TRUE,
                   width=4,
                   
                   
                   dateRangeInput(
                     "dates", 
                     label = h3("Date range"),
                     start='2009-01-01',
                     end= '2019-01-01',
                     max=Sys.Date()
                   ),
                   # sliderInput
                   # (
                   #   inputId="num",
                   #   label="Choose a number",
                   #   value=40,min=1,max=100
                   # ),
                   textInput
                   (
                     inputId="title",
                     label="Insert a Title",
                     value="Default Title"
                   ),
                   textInput
                   (
                     inputId="stocks",
                     label="Insert the stock(s) you want to plot",
                     value="AMZN"
                   ),
                   
                   radioButtons(
                     "radio", h3("Plot type"),
                     choices = list("Histogram" = 1, "Line Plot" = 2,
                                    "QQ Plot" = 3, "Box Plot" = 4),selected = 1),
                   #"DyGraph [ONLY FOR TS]" = 5
                   
                   radioButtons(
                     "radio2", h3("Compression"),
                     choices = list("Monthly" = "m", "Weekly" = "w",
                                    "Daily" = "d"),selected = "m"),
                   radioButtons(
                     "radio3", h3("Value to plot"),
                     choices = list("Open" = "Open", "High" = "High",
                                    "Low" = "Low", "Close" = "Close"),selected = "High"),
                   
                   
                   
                   actionButton(inputId="clicks",
                                label="UpdatePlot")
                 )
               ),
               fluidRow(
                 box(title=h3("Summary"),verbatimTextOutput(outputId = "stats"))
                 
               ),
               fluidRow(
                 box(
                   title=h3("Quantile,Kurtosis & Skewness"),
                   verbatimTextOutput(outputId="quantile"),    
                   verbatimTextOutput(outputId = "kurtosis"),
                   verbatimTextOutput(outputId="skewness"))
               )
)


##MENU TAB2
tab2<- tabItem(tabName="Forecasting",
               fluidRow(
                 
                 box(title="Controls Forecast",
                     solidHeader=TRUE,
                     collapsible=TRUE,
                     
                     dateRangeInput(
                       "datesForecasting", 
                       label = h3("Date range Forecasting"),
                       start='2009-01-01',
                       end= '2019-01-01',
                       max=Sys.Date()
                     ),
                     #Percentages
                     sliderInput("sliderForecasting", label = h3("Slider Range"), min = 0, 
                                 max = 100, value = c(70, 90)),
                     textInput
                     (
                       inputId="stocksForecasting",
                       label="Insert the stock(s) you want to plot",
                       value="AMZN"
                     ),
                     radioButtons(
                       inputId="arimaRadio",
                       label=h3("Method:"),
                       #ML=>Missing Values
                       #CSS=>Conditional sum-of-squares
                       choices=list("ML"="ML","CSS"="CSS", "CSS-ML"="CSS-ML")
                       
                       
                     ),
                     actionButton(inputId="clicksForecast",
                                  label="Update")
                     
                     
                    ),
                 box(title="Statistics",
                     solidHeader = TRUE,
                     collapsible = TRUE),
                 verbatimTextOutput(outputId = "statsForecast")
               ),
               fluidRow(
                 box(title="PlotsForecasting",
                     status="primary",
                     width=12,
                     solidHeader=TRUE,
                     collapsible=TRUE,
                     plotOutput("Decomposition"),
                     plotOutput("Forecast"))
               )
)



tab3<- tabItem(tabName="Multivariate",
               fluidRow(
                 box(
                   title="Plots",
                   status="primary",
                   solidHeader=TRUE,
                   collpasible=TRUE,
                   # plotOutput("plot"),
                   # dygraphOutput("plot2")
                   conditionalPanel('input.radioMulti==1', dygraphOutput("dygraphPlot")),
                   conditionalPanel('input.radioMulti==2', plotOutput("correlationMatrix"))
                                   
                 ),
                 box(
                   title="Controls",
                   solidHeader = TRUE,
                   collapsible=TRUE,
                   width=4,
                   dateRangeInput(
                     "datesMulti", 
                     label = h3("Date range"),
                     start='2009-01-01',
                     end= '2019-01-01',
                     max=Sys.Date()
                   ),
                   textInput
                   (
                     inputId="stocksMulti1",
                     label="Insert the stock you want to plot",
                     value="AMZN"
                   ),
                   textInput
                   (
                     inputId="stocksMulti2",
                     label="Insert the stock you want to plot",
                     value="GOOG"
                   ),
                   textInput
                   (
                     inputId="stocksMulti3",
                     label="Insert the stock you want to plot",
                     value="FB"
                   ),
                   textInput
                   (
                     inputId="stocksMulti4",
                     label="Insert the stock you want to plot",
                     value="ATVI"
                   ),
                   radioButtons(
                     "radioMulti", h3("Plot type"),
                     choices = list("DyGraph" = 1, "CorrelationMatrix [ATLEAST 2 STOCKS]" = 2)
                                    ,selected = 1),
                   actionButton(inputId="clicksMultivariate",
                                label="Update")
                   
                   
                 )
               )
)


##BODY == TAB1+TAB2+TAB3
body<- dashboardBody(
  # Boxes need to be put in a row (or column)
  tabItems(
    tab1,
    tab2,
    tab3
  )
)

##UI == EVERYTHING TOGETHER


ui <- dashboardPage(
  skin="purple",
  header,
  sidebar,
  body
)