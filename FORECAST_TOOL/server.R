
rm(list = ls())

#install.packages("devtools")
#install.packages("Rcpp")
#install.packages("RCurl")
#install.packages("RJSONIO")
#library(devtools)
#library(Rcpp)
#library(RCurl)
#library(RJSONIO)
#install.packages('devtools') 
#library(devtools) 
#install_github("ramnathv/rCharts")
#require(devtools)
#install_github('rCharts', 'ramnathv')
library(shiny)
library(ggplot2)
library(sqldf)
library(plotly)
library(ggthemes)
library(rCharts)
library(DT)
library(shinyBS)
library(shinythemes)
library(timeDate)
options(scipen=999)


Logged = FALSE;

credentials <- data.frame(
  my_username = c("admin","mmarshal","kwildt","nporbandarwalla", "intelenet"), 
  my_password = c("forec@st","@pril2020","@pril2021","@pril2022", "forecast")
)

setwd("C:/Users/p.avasthi/Documents/FORECAST_TOOL/")
#setwd("C:\\Users\\ii00061002\\Desktop\\DJO TOOL\\DJO_Ver1.16- Versionwith Login - New Data - Weekly - Progress Bar Without Quarterly")
####Import Data####
forecastData<- read.csv("Forecast_New.csv",check.names = FALSE,header=TRUE)
fittedData<-read.csv("Fitted_New.csv",check.names = FALSE,header=TRUE)
SummaryData=read.csv("volume1_New1.csv",check.names = FALSE,header=TRUE)
#SummaryDataQtr=read.csv("volume1_Quarter1.csv",check.names = FALSE,header=TRUE)
#exception<-read.csv("Exception.csv",check.names = FALSE,header=TRUE)
#template<-read.csv("Template.csv",check.names = FALSE,header=TRUE)
#weeklyData<-read.csv("Weekly_Data.csv",check.names = FALSE,header=TRUE)
#weeklyforecast<-read.csv("Forecast_weekly.csv",check.names = FALSE,header=TRUE)
#weeklyfitted<-read.csv("Fitted_weekly.csv",check.names = FALSE,header=TRUE)

#stock<-read.csv("InventoryForecast.csv",check.names = FALSE,header=TRUE)

Vmonth<-0
Vyear<-0

####Forecasting Screen####
ui <- function(){navbarPage(img(src='accenture-logo.bmp', align = "left"),
                             fluid = TRUE, responsive = NULL,windowTitle = "iForecaster-DRC",
                             theme = "bootstrap/css/bootstrap.css",
  
  ########################################## Data Summary Tab ####################################################                  
  
  tabPanel("Data Summary", icon = icon("signal", lib="glyphicon"),

           sidebarLayout(
             
             sidebarPanel(
               tags$h4("Exploratory Data Analysis"),
               tags$hr(),
               radioButtons("orders_exp", "Model Category:",
                            c("All    " = "All",
                              "NPI    " = "<40",
                              "Regular    " = "40+"), inline=T),
               selectizeInput("gender_exp", 
                              label = "Select Model",
                              choices = c(levels(unique(SummaryData$MainCategory)),"---All Items---"),
                              selected = levels(unique(SummaryData$MainCategory))[1]
               ),
               
               selectizeInput("category_exp", 
                              label = "Select PHC_NAME",
                              #choices = levels(unique(SummaryData$SubCategory[SummaryData$MainCategory==levels(SummaryData$MainCategory)[1]]))
                              choices = c("---All Items---")
                              
                              ,selected="---All Items---"
                              
               ),
               
               selectizeInput("brand_exp", 
                              label = "Select Region",
                              #choices = levels(unique(SummaryData$Brands[SummaryData$SubCategory==levels(SummaryData$SubCategory)[1]]))
                              choices = c("---All Items---")
                              ,selected="---All Items---"
               ),
               
               selectizeInput("color_exp", 
                              label = "Select Country",
                              #choices = levels(unique(SummaryData$Color[SummaryData$Brands==levels(SummaryData$Brands)[1]]))
                              choices = c("---All Items---")
                              ,selected="---All Items---"
                              
               ),
               selectizeInput("size_exp", 
                              label = "Select Category",
                              #choices = unique(SummaryData$Size[SummaryData$Color==levels(SummaryData$Color)[1]])
                              choices = c("---All Items---")
                              ,selected="---All Items---"
                              
               ),
               selectizeInput("description_exp", 
                              label = "Select Service Code",
                              #choices = levels(unique(SummaryData$Description[SummaryData$Size==levels(SummaryData$Size)[1]]))
                              choices = c("---All Items---")
                              ,selected="---All Items---"
                              
               ),
               width=3
               
             ),
             
             mainPanel(
               #"Hi, ", USER$name, 
               actionLink("logout", "Logout",class='pull-right',icon = icon("log-out", lib = "glyphicon")),
               tags$br(),
               tabsetPanel(
                 tabPanel("Trend", 
                          tags$br(),
                          tags$br(),
                          dygraphOutput("clinePlot1")),
                 
                 
                 tabPanel("Boxplot",
                          tags$br(),
                          tags$br(),
                          plotOutput("boxPlot"),
                          #plotlyOutput("boxPlot")
                          radioButtons("orders_bplot", "Plot Base:",
                                       c("Static    " = "Static_bplot",
                                         "Dynamic    " = "Dynamic_bplot"), inline=T)
                 ),
                 
                 tabPanel("Histogram",
                          tags$br(),
                          tags$br(),
                          plotOutput("histPlot")),
                 
                 tabPanel("Decomposition",
                          tags$br(),
                          tags$br(),
                          plotOutput("DecompositMul") 
                 ),
                 
                 tabPanel("Auto-Correlation",
                          tags$br(),
                          tags$br(),
                          plotOutput("ACF"),
                          plotOutput("PACF")
                 ),
                 
                 tabPanel("Summary",
                          tags$br(),
                          tags$br(),
                          textOutput("summaryCaption1"),
                          #tags$small("(All values are in seconds.)"),
                          tags$br(),
                          tags$br(),
                          textOutput("summaryCaption2"),
                          textOutput("summaryCaption3"),
                          textOutput("summaryCaption4"),
                          textOutput("summaryCaption5"),
                          textOutput("summaryCaption6"),
                          textOutput("summaryCaption7"),
                          textOutput("summaryCaption8"),
                          textOutput("summaryCaption9"),
                          textOutput("summaryCaption10"),
                          textOutput("summaryCaption11"),
                          textOutput("summaryCaption12"),
                          textOutput("summaryCaption13"))
                 
               ),
               width=9
             )
           )
  ),                  
  
  ########################################## Monthly Forecasting Tab ####################################################                  
  
  tabPanel("Monthly Forecast",icon = icon("stats", lib="glyphicon"),
           
           # tags$head(includeScript("./js/ga-denkwerk.js")),
           
           sidebarLayout(
             sidebarPanel(
               helpText(h4("Monthly Order Quantity")),
               tags$hr(),
               # radioButtons("orders", "Order Category:",
               #              c("All    " = "All",
               #                "<40    " = "<40",
               #                "40+    " = "40+"), inline=T),
               selectizeInput("gender", 
                              label = "Select Model",
                              choices = c(levels(unique(forecastData$MainCategory)),"---All Items---"),
                              selected = levels(unique(forecastData$MainCategory))[1]
               ),
               
               selectizeInput("category", 
                              label = "Select PHC Name",
                              #choices = levels(unique(SummaryData$SubCategory[SummaryData$MainCategory==levels(SummaryData$MainCategory)[1]]))
                              choices = c("---All Items---")
                              ,selected="---All Items---"
               ),
               
               selectizeInput("brand", 
                              label = "Select Region",
                              #choices = levels(unique(SummaryData$Brands[SummaryData$SubCategory==levels(SummaryData$SubCategory)[1]]))
                              choices = c("---All Items---")
                              ,selected="---All Items---"
               ),
               
               
               selectizeInput("color", 
                              label = "Select Country",
                              #choices = levels(unique(SummaryData$Color[SummaryData$Brands==levels(SummaryData$Brands)[1]]))
                              choices = c("---All Items---")
                              ,selected="---All Items---" 
               ),
               selectizeInput("size", 
                              label = "Select Category",
                              #choices = unique(SummaryData$Size[SummaryData$Color==levels(SummaryData$Color)[1]])
                              choices = c("---All Items---")
                              ,selected="---All Items---"
               ),
               selectizeInput("description", 
                              label = "Select Service Code",
                              #choices = levels(unique(SummaryData$Description[SummaryData$Size==levels(SummaryData$Size)[1]]))
                              choices = c("---All Items---")
                              ,selected="---All Items---"
               ),
               
               width=3
               
             ),
             
             mainPanel(
               #"Hi, ", USER$name,
               actionLink("logout1", "Logout",class='pull-right',icon = icon("log-out", lib = "glyphicon")),
               tags$br(),
               # fluidRow(
               #   column(8,
               # fileInput('file1', 'Upload Exception File (CSV)',
               #           accept=c('.csv'))),
               # column(3,
               # actionButton("go", "View Exceptions"))
               # ),
               tags$br(),
               showOutput("monthly2","highcharts"),
               downloadButton('downloadData9', 'Download'),
               tags$hr(),
               dataTableOutput("ratio"),
               #bsModal("modalExample", "Exception File", "go", size = "large",dataTableOutput("distTable")),
               width=9
             )
           )
  ),                    
  
  ########################################## Statistical Monthly Forecasting Tab ####################################################                  
  
  tabPanel("Statistical Monthly",icon = icon("stats", lib="glyphicon"),
           
           #tags$head(includeScript("./js/ga-denkwerk.js")),
           
           sidebarLayout(
             sidebarPanel(
               helpText(h4("Monthly Forecasting")),
               tags$hr(),
               radioButtons("orders_ifor", "Order Category:",
                            c("All    " = "All",
                              "NPI    " = "<40",
                              "Regular    " = "40+"), inline=T),
               selectizeInput("gender_ifor", 
                              label = "Select Model",
                              choices = c(levels(unique(SummaryData$MainCategory)),"---All Items---"),
                              selected = levels(unique(SummaryData$MainCategory))[1]
               ),
               
               selectizeInput("category_ifor", 
                              label = "Select PHC Name",
                              #choices = levels(unique(SummaryData$SubCategory[SummaryData$MainCategory==levels(SummaryData$MainCategory)[1]]))
                              choices = c("---All Items---")
                              ,selected="---All Items---"
               ),
               
               selectizeInput("brand_ifor", 
                              label = "Select Region",
                              #choices = levels(unique(SummaryData$Brands[SummaryData$SubCategory==levels(SummaryData$SubCategory)[1]]))
                              choices = c("---All Items---")
                              ,selected="---All Items---"
               ),
               
               
               selectizeInput("color_ifor", 
                              label = "Select Country",
                              #choices = levels(unique(SummaryData$Color[SummaryData$Brands==levels(SummaryData$Brands)[1]]))
                              choices = c("---All Items---")
                              ,selected="---All Items---"
               ),
                selectizeInput("size_ifor", 
                            label = "Select Category",
                            #choices = unique(SummaryData$Size[SummaryData$Color==levels(SummaryData$Color)[1]])
                            choices = c("---All Items---")
                            ,selected="---All Items---"
                ),
                selectizeInput("description_ifor", 
                            label = "Select Service Code",
                            #choices = levels(unique(SummaryData$Description[SummaryData$Size==levels(SummaryData$Size)[1]]))
                            choices = c("---All Items---")
                            ,selected="---All Items---"
                ),
               width=3
               
               
               
             ),
             
             
             mainPanel(
               #"Hi, ", USER$name,
               actionLink("logout2", "Logout",class='pull-right',icon = icon("log-out", lib = "glyphicon")),
               tags$br(),
               tabsetPanel(
                 tabPanel("ETS", 
                          tags$br(),
                          #highchartOutput("forecastPlotETS"),
                          plotOutput("forecastPlotETS"),
                          #downloadButton('downloadData', 'Download'),
                          tags$hr(),
                          dataTableOutput("ETStab") 
                 ),
                 
                 tabPanel("ARIMA",
                          tags$br(),
                          plotOutput("forecastPlotARIMA"),
                          #('downloadData1', 'Download'),
                          tags$hr(),
                          dataTableOutput("ARtab")
                 ),
                 
                 # tabPanel("StructTS",
                 #          plotOutput("forecastPlotStructTS"),
                 #          downloadButton('downloadData2', 'Download'),
                 #          tags$hr(),
                 #          dataTableOutput("STtab")
                 #),
                 tabPanel("HoltWinters",
                          tags$br(),
                          plotOutput("forecastPlotHoltWinters"),
                          #downloadButton('downloadData3', 'Download'),
                          tags$hr(),
                          dataTableOutput("HWtab")
                 ),
                 tabPanel("Theta",
                          tags$br(),
                          plotOutput("forecastPlotthetaf"),
                          #downloadButton('downloadData4', 'Download'),
                          tags$hr(),
                          dataTableOutput("TFtab")
                 ),
                 tabPanel("RandomWalk",
                          tags$br(),
                          plotOutput("forecastPlotrwf"),
                          
                          #downloadButton('downloadData5', 'Download'),
                          tags$hr(),
                          dataTableOutput("RFtab")
                 ),
                 tabPanel("Naive",
                          tags$br(),
                          plotOutput("forecastPlotnaive"),
                          
                          #downloadButton('downloadData6', 'Download'),
                          tags$hr(),
                          dataTableOutput("NAtab")
                 ),
                 tabPanel("TBATS",
                          tags$br(),
                          plotOutput("forecastPlottbats"),
                          
                          #downloadButton('downloadData7', 'Download'),
                          tags$hr(),
                          dataTableOutput("TBtab")
                 ),
                 
                 # tabPanel("CubicSpline",
                 #          tags$br(),
                 #          plotOutput("forecastPlotsplinef"),
                 #         
                 #          downloadButton('downloadData8', 'Download'),
                 #          tags$hr(),
                 #          dataTableOutput("SFtab")
                 # ),
                 tabPanel("Summary",
                          tags$br(),
                          tags$br(),
                          DT::dataTableOutput("ACC")
                          
                 )
                 
               )
               
             )
           )
  ),                    

  ########################################## Footer Note ####################################################                  
  
  tags$hr(),
  tags$br(),
  tags$span(style="color:grey", 
            tags$footer(("© 2019 - "), 
                        tags$a(
                          href="http://www.accenture.com",
                          target="_blank",
                          "Knowledge Services"), 
                        tags$br(),
                        align = "center")
  )
  ########################################## End ############################################################
)}

####Global Variable####
USER <- reactiveValues(Logged = Logged)

#### Server script####
server = (function(input, output,session){

#### Forecast Screen Script####     
  observe({
    if (USER$Logged == FALSE)
      {

      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,
                                  c("",ui())))
      })
    }
    if (USER$Logged == TRUE)
    {
      output$page <- renderUI({
        #div(class="outer",do.call(navbarPage,c(inverse=TRUE,title = "Dr. Comfort",ui2())))

        div(
          tagList(
            tags$head(
              tags$link(rel="stylesheet", type="text/css",href="style.css"),
              tags$script(type="text/javascript", src = "busy.js")
            )
          ),
          div(class = "busy",
              # p("Calculation in progress.."),
              img(src='loader.gif')
          ),
          ui2()
            )
      })
      htmlOutput("page")

    }

##################################################################################################################################
##################################################### Data Summary Tab  ##########################################################
##################################################################################################################################
    
    ####Observe Coding#####
    observe({    
      orders_exp1 <- paste("'",input$orders_exp,"'",sep="")
      if(!is.null(input$orders_exp) ){
      if (input$orders_exp=="All")
      {
        query<-paste("select distinct MainCategory from SummaryData order by MainCategory",sep="")
      }
      else
      {
        query<-paste("select distinct MainCategory from SummaryData where Bucket=",orders_exp1," order by MainCategory",sep="")
      }
      
      df<-sqldf(query)
      
      if (nrow(df)>0){
        df1 <- data.frame(MainCategory = c("---All Items---"))
        df<-rbind(df,df1)
        items=c(df)
      }
      else
      {
        items=c("---All Items---")
      }
      
      updateSelectInput(session, "gender_exp", choices =items,selected = "---All Items---")
      }
      })
    
    observe({    
      orders_exp1 <- paste("'",input$orders_exp,"'",sep="")
      gender_exp1 <- paste("'",input$gender_exp,"'",sep="")
      if(!is.null(input$orders_exp) ){
        
      if (input$orders_exp=="All")
      {
        query<-paste("select distinct SubCategory from SummaryData where MainCategory=",gender_exp1," order by SubCategory",sep="")
      }
      else
      {
        query<-paste("select distinct SubCategory from SummaryData where MainCategory=",gender_exp1," and Bucket=", orders_exp1," order by SubCategory",sep="")
      }
      
      df<-sqldf(query)
      
      if (nrow(df)>0){
        df1 <- data.frame(SubCategory = c("---All Items---"))
        df<-rbind(df,df1)
        items=c(df)
      }
      else
      {
        items=c("---All Items---")
      }
      updateSelectInput(session, "category_exp", choices =items,selected = "---All Items---")
      }
    })
    
    observe({   
      orders_exp1 <- paste("'",input$orders_exp,"'",sep="")
      gender_exp1 <- paste("'",input$gender_exp,"'",sep="")
      category_exp1 <- paste("'",input$category_exp,"'",sep="")
      if(!is.null(input$orders_exp) ){
        
      if (input$orders_exp=="All")
      {
        
        query<-paste("select distinct Brands from SummaryData where MainCategory=",gender_exp1," and SubCategory=", category_exp1," order by Brands",sep="")
      }
      else
      {
        
        query<-paste("select distinct Brands from SummaryData where MainCategory=",gender_exp1," and SubCategory=", category_exp1," and Bucket=", orders_exp1," order by Brands",sep="")
      }
      
      df<-sqldf(query)
      
      if (nrow(df)>0)
      {
        df1 <- data.frame(Brands = c("---All Items---"))
        df<-rbind(df,df1)
        items=c(df)
      }
      else
      {
        items=c("---All Items---")
      }
      
      updateSelectInput(session, "brand_exp", choices = items,selected = "---All Items---")
      }
    })
    
    observe({
      orders_exp1 <- paste("'",input$orders_exp,"'",sep="")
      gender_exp1 <- paste("'",input$gender_exp,"'",sep="")
      category_exp1 <- paste("'",input$category_exp,"'",sep="")
      color_exp1 <- paste("'",input$brand_exp,"'",sep="")
      
      if(!is.null(input$orders_exp) ){
      if (input$orders_exp=="All")
      {
        query<-paste("select distinct Color from SummaryData where MainCategory=",gender_exp1," and SubCategory=", category_exp1," and Brands=", color_exp1," order by Color",sep="")
      }
      else
      {
        query<-paste("select distinct Color from SummaryData where MainCategory=",gender_exp1," and SubCategory=", category_exp1," and Brands=", color_exp1," and Bucket=", orders_exp1," order by Color",sep="")
      }
      
      df<-sqldf(query)
      if (nrow(df)>0){
        df1 <- data.frame(Color = c("---All Items---"))
        df<-rbind(df,df1)
        items=c(df)
      }
      else
      {
        items=c("---All Items---")
      }
      
      updateSelectInput(session, "color_exp", choices =items,selected = "---All Items---" )
      }
    })
    
    observe({
      orders_exp1 <- paste("'",input$orders_exp,"'",sep="")
      gender_exp1 <- paste("'",input$gender_exp,"'",sep="")
      category_exp1 <- paste("'",input$category_exp,"'",sep="")
      color_exp1 <- paste("'",input$brand_exp,"'",sep="")
      size_exp1 <- paste("'",input$color_exp,"'",sep="")
      
      if(!is.null(input$orders_exp) ){
        
      if (input$orders_exp=="All")
      {
        query<-paste("select distinct Size from SummaryData where MainCategory=",gender_exp1," and SubCategory=", category_exp1," and Brands=", color_exp1," and Color=", size_exp1," order by Size",sep="")
      }
      else
      {
        query<-paste("select distinct Size from SummaryData where MainCategory=",gender_exp1," and SubCategory=", category_exp1," and Brands=", color_exp1," and Color=", size_exp1," and Bucket=", orders_exp1," order by Size",sep="")
      }
      
      df<-sqldf(query)
      if (nrow(df)>0){
        df1 <- data.frame(Size = c("---All Items---"))
        df<-rbind(df,df1)
        items=c(df)
      }
      else
      {
        items=c("---All Items---")
      } 
      
      updateSelectInput(session, "size_exp", choices =items,selected = "---All Items---")
      }
    })
    
    observe({
      orders_exp1 <- paste("'",input$orders_exp,"'",sep="")
      gender_exp1 <- paste("'",input$gender_exp,"'",sep="")
      category_exp1 <- paste("'",input$category_exp,"'",sep="")
      color_exp1 <- paste("'",input$brand_exp,"'",sep="")
      size_exp1 <- paste("'",input$color_exp,"'",sep="")
      description_exp1 <- paste("'",input$size_exp,"'",sep="")
      
      if(!is.null(input$orders_exp) ){
        
      if (input$orders_exp=="All")
      {
        query<-paste("select distinct Description from SummaryData where MainCategory=",gender_exp1," and SubCategory=", category_exp1," and Brands=", color_exp1," and Color=", size_exp1," and Size=", description_exp1," order by Description",sep="")
      }
      else
      {
        query<-paste("select distinct Description from SummaryData where MainCategory=",gender_exp1," and SubCategory=", category_exp1," and Brands=", color_exp1," and Color=", size_exp1," and Size=", description_exp1," and Bucket=", orders_exp1," order by Description",sep="")
      }
      
      df<-sqldf(query)
      if (nrow(df)>0){
        df1 <- data.frame(Description = c("---All Items---"))
        df<-rbind(df,df1)
        items=c(df)
      }
      else
      {
        items=c("---All Items---")
      }
      
      updateSelectInput(session, "description_exp", choices = items,selected = "---All Items---")
      }
    })
    
    
    
    ####Extract Data####
    getDataset1 <- reactive({
      #data1=read.csv("volume1_New.csv",check.names = FALSE,header=TRUE)
      data1=SummaryData
      #print(input$orders_exp)
      orders1 <- paste("'",input$orders_exp,"'",sep="")
      MainCategory1=paste("'",input$gender_exp,"'",sep="")
      SubCategory1=paste("'",input$category_exp,"'",sep="")
      Brands1=paste("'",input$brand_exp,"'",sep="")
      Color1=paste("'",input$color_exp,"'",sep="")
      size1=paste("'",input$size_exp,"'",sep="")
      description1=paste("'",input$description_exp,"'",sep="")
      
      if (input$orders_exp=="All"){
        if (input$gender_exp=='---All Items---'){
          query<-paste("select * from data1",sep="")
        }
        else if (input$category_exp=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,sep="")
        }
        else if(input$brand_exp=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,sep="")
        }  
        else if(input$color_exp=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,sep="")
        }
        else if(input$size_exp=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,sep="")
        }
        else if(input$description_exp=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,sep="")
        }
        else{
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,"and Description=",description1,sep="")
        }
      }
      else{
        if (input$gender_exp=='---All Items---'){
          query<-paste("select * from data1 where Bucket=",orders1,sep="")
        }
        else if (input$category_exp=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1," and Bucket=", orders1,sep="")
        }
        else if(input$brand_exp=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1," and Bucket=", orders1,sep="")
        }  
        else if(input$color_exp=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1," and Bucket=", orders1,sep="")
        }
        else if(input$size_exp=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1," and Bucket=", orders1,sep="")
        }
        else if(input$description_exp=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1," and Bucket=", orders1,sep="")
        }
        else{
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,"and Description=",description1," and Bucket=", orders1,sep="")
        }
      }
      
      tosa1<-sqldf(query)
      n <- colnames(tosa1[,-1:-8])
      
      tosa=data.frame(colSums(tosa1[,-1:-8],na.rm = T,1))
      names(tosa)[1]='Quantity by Ordered Month'
      tosa$dates <- n
      tosa <- tosa[c(2,1)]
      
    })
    
    getDataset_new <- reactive({
      #data1=read.csv("volume1_New.csv",check.names = FALSE,header=TRUE)
      data1=SummaryData
      orders1 <- paste("'",input$orders_exp,"'",sep="")
      MainCategory1=paste("'",input$gender_exp,"'",sep="")
      SubCategory1=paste("'",input$category_exp,"'",sep="")
      Brands1=paste("'",input$brand_exp,"'",sep="")
      Color1=paste("'",input$color_exp,"'",sep="")
      size1=paste("'",input$size_exp,"'",sep="")
      description1=paste("'",input$description_exp,"'",sep="")
      
      if (input$orders_exp=="All"){
        if (input$gender_exp=='---All Items---'){
          query<-paste("select * from data1",sep="")
        }
        else if (input$category_exp=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,sep="")
        }
        else if(input$brand_exp=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,sep="")
        }  
        else if(input$color_exp=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,sep="")
        }
        else if(input$size_exp=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,sep="")
        }
        else if(input$description_exp=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,sep="")
        }
        else{
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,"and Description=",description1,sep="")
        }
      }
      else{
        if (input$gender_exp=='---All Items---'){
          query<-paste("select * from data1 where Bucket=",orders1,sep="")
        }
        else if (input$category_exp=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1," and Bucket=", orders1,sep="")
        }
        else if(input$brand_exp=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1," and Bucket=", orders1,sep="")
        }  
        else if(input$color_exp=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1," and Bucket=", orders1,sep="")
        }
        else if(input$size_exp=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1," and Bucket=", orders1,sep="")
        }
        else if(input$description_exp=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1," and Bucket=", orders1,sep="")
        }
        else{
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,"and Description=",description1," and Bucket=", orders1,sep="")
        }
      }
      
      tosa1<-sqldf(query)
      
      n <- colnames(tosa1[,-1:-8])
      
      tosa=data.frame(colSums(tosa1[,-1:-8],na.rm = T,1))
      names(tosa)[1]='Quantity by Ordered Month'
      tosa$dates <- n
      tosa <- tosa[c(2,1)]
      tosa=tosa[,2]
      
    })
    
    ####Line chart function####
    output$clinePlot1 <-renderDygraph({
      
      tosa<-getDataset1()
      tosa$dates=as.Date(tosa$dates,format='%d/%m/%Y')
      
      dygA <- xts(tosa[2:2], as.Date(tosa[,1], format="%m/%d/%Y"))
      dygraph(dygA, main = colnames(tosa)[2]) %>% 
        dyRangeSelector() %>%
        dyAxis("x", drawGrid = FALSE) %>%
        #dyAxis("y", valueRange = c(0,max(tosa[,2])+5),label = "Quantity Orders", drawGrid = FALSE) %>%
        dyAxis("y", valueRange = c(0,max(tosa[,2])+5), drawGrid = FALSE) %>%
        
        dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),highlightCircleSize = 5)
      
    })
    
    ####Boxplot plot function####
    output$boxPlot <- renderPlot({
      tosa<-getDataset1()
      minv <- switch(input$orders_bplot,
                     Static_bplot = 0,
                     Dynamic_bplot = min(tosa[,2]),
                     0)
      
      boxplot(tosa[2:2],
              main = colnames(tosa)[2],
              xlab = "Orders Quantity",
              horizontal = TRUE,
              ylim=c(minv,max(tosa[,2])),
              staplewex = 1,
              
              axes = TRUE,
              col="lightblue")
      #col = brewer.pal(n = 5, name = "Dark2"))
      text(x=fivenum(tosa[,2]), labels =fivenum(tosa[,2]), y=1.25)
      
      stripchart(tosa[2:2],vertical = FALSE,
                 method = "jitter", add = TRUE, pch = 20, col = 'red')
    })
    
    ####Histogram function####
    histPlotInput <- function() {
      
      tosa<-getDataset1()
      histogramPlot <- hist(tosa[,2])
      multiplier <- histogramPlot$counts / histogramPlot$density
      mydensity <- density(tosa[,2])
      mydensity$y <- mydensity$y * multiplier[1]
      
      plot(histogramPlot,
           main = colnames(tosa)[2],
           xlab = "Orders Quantity",
           ylab = "Monthly Frequency",
           col = "mintcream")
      box()
      lines(mydensity)
      
      myx <- seq(min(tosa[,2]), max(tosa[,2]), 
                 length.out = 100)
      mymean <- mean(tosa[,2])
      mysd <- sd(tosa[,2])
      
      normal <- dnorm(x = myx, mean = mymean, sd = mysd)
      lines(myx, normal * multiplier[1], 
            col = "lightblue", lwd = 2)
      
      sd_x <- seq(mymean - 3 * mysd, mymean + 3 * mysd, by = mysd)
      sd_y <- dnorm(x = sd_x, mean = mymean, 
                    sd = mysd) * multiplier[1]
      
      segments(x0 = sd_x, y0= 0, x1 = sd_x, y1 = sd_y, 
               col = "firebrick4", lwd = 2)
    }
    
    ## Caption function
    output$histPlotCaption <- renderText({
      paste("The time on site histogram for the", 
            input$tabOne, "website.",
            "The mean for this at", 
            round(mean(getDataset_new()), digits = 2),
            "seconds and the standard deviation is",
            round(sd(getDataset_new()), digits = 2),".")
    })
    
    output$histPlot <- renderPlot({
      histPlotInput()
    })
    
    
    
    ####Decomposition##################################        
    
    DecompositInput <- function() {
      
      tosa1<-getDataset1()
      tosa1[,1]=as.Date(tosa1[,1], format = "%m/%d/%Y")
      x1=as.numeric(format(min(tosa1[,1]),'%Y'))
      x2=as.numeric(format(min(tosa1[,1]),'%m'))
      
      y1=as.numeric(format(max(tosa1[,1]),'%Y'))
      y2=as.numeric(format(max(tosa1[,1]),'%m'))
      
      tosa <- ts(tosa1[,2], start=c(x1,x2), end=c(y1, y2), frequency=12) 
      f <- decompose(tosa,type="additive")
      plot(f)
      
      
    }
    
    DecompositInputMul <- function() {
      
      tosa1<-getDataset1()
      tosa1[,1]=as.Date(tosa1[,1], format = "%m/%d/%Y")
      x1=as.numeric(format(min(tosa1[,1]),'%Y'))
      x2=as.numeric(format(min(tosa1[,1]),'%m'))
      
      y1=as.numeric(format(max(tosa1[,1]),'%Y'))
      y2=as.numeric(format(max(tosa1[,1]),'%m'))
      
      tosa <- ts(tosa1[,2], start=c(x1,x2), end=c(y1, y2), frequency=12) 
      f <- decompose(tosa,type="multiplicative")
      plot(f)
    }
    output$Decomposit<- renderPlot({
      DecompositInput()
    })
    output$DecompositMul <- renderPlot({
      DecompositInputMul()
    })
    
    output$Decomposit<- renderPlot({
      DecompositInput()
    })
    
    output$DecompositMul <- renderPlot({
      DecompositInputMul()
    })
    
    ####ACF##################################        
    
    ACFInput <- function() {
      
      tosa1<-getDataset1()
      tosa <- ts(tosa1[,2]) 
      f <- acf(tosa)
      plot(f,main="Auto Correlation for AR")
    }        
    output$ACF <- renderPlot({
      ACFInput()
    }) 
    
    ####PACF##################################        
    PACFInput <- function() {
      tosa1<-getDataset1()
      tosa <- ts(tosa1[,2]) 
      f <- pacf(tosa)
      plot(f,main="Partial Auto Correlation for MA")
    }        
    output$PACF <- renderPlot({
      PACFInput()
    }) 
    
    
    ####Data Summary functions####
    Mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    
    std <- function(x) {
      sd(x)/sqrt(length(x))
    }
    
    output$summaryCaption1 <- renderText({
      paste("Descriptive statistics:")
    })
    
    output$summaryCaption2 <- renderText({
      paste("Minimum:", round(min(getDataset_new()), digits = 2))
    })
    
    output$summaryCaption3 <- renderText({
      paste("1st Quartile:", round(quantile(getDataset_new(), probs=0.25), digits = 2))
    })
    
    output$summaryCaption4 <- renderText({
      paste("3rd Quartile:", round(quantile(getDataset_new(), probs=0.75), digits = 2))
    })
    
    output$summaryCaption5 <- renderText({
      paste("Maximum:", round(max(getDataset_new()), digits = 2))
    })
    
    output$summaryCaption6 <- renderText({
      paste("Median:", round(median(getDataset_new()), digits = 2))
    })
    
    output$summaryCaption7 <- renderText({
      paste("Mean:", round(mean(getDataset_new()), digits = 2))
    })
    
    output$summaryCaption8 <- renderText({
      paste("Mode:", round(Mode(getDataset_new()), digits = 2))
    })
    
    output$summaryCaption9 <- renderText({
      paste("Standard Deviation:", round(sd(getDataset_new()), digits = 2))
    })
    
    output$summaryCaption10 <- renderText({
      paste("Skewness:", round(skewness(getDataset_new()), digits = 2))
    })
    
    output$summaryCaption11 <- renderText({
      paste("Kurtosis:", round(kurtosis(getDataset_new()), digits = 2))
    })
    
    output$summaryCaption12 <- renderText({
      paste("Median Absolute Deviation:", round(mad(getDataset_new()), digits = 2))
    })
    
    output$summaryCaption13 <- renderText({
      paste("Standard Error:", round(std(getDataset_new()), digits = 2))
    })
    
    
    
##################################################################################################################################
##################################################### Monthly Forecasting Tab  #######################################################
##################################################################################################################################
   
    #### Oberve Coding######
      observe({
        if(!is.null(input$gender) ){
        gender_exp1 <- paste("'",input$gender,"'",sep="")
        query<-paste("select distinct SubCategory from forecastData where MainCategory=",gender_exp1," order by SubCategory",sep="")
        df<-sqldf(query)
        
        if (nrow(df)>0){
          df1 <- data.frame(SubCategory = c("---All Items---"))
          df<-rbind(df,df1)
          items=c(df)
        }
        else
        {
          items=c("---All Items---")
        }
        
        updateSelectInput(session, "category", choices =items,selected = "---All Items---")
        }
      })
      
      observe({
        if(!is.null(input$gender) ){
          
        gender_exp1 <- paste("'",input$gender,"'",sep="")
        category_exp1 <- paste("'",input$category,"'",sep="")
        
        query<-paste("select distinct Brands from forecastData where MainCategory=",gender_exp1," and SubCategory=", category_exp1," order by Brands",sep="")
        df<-sqldf(query)
        if (nrow(df)>0){
          df1 <- data.frame(Brands = c("---All Items---"))
          df<-rbind(df,df1)
          items=c(df)
        }
        else
        {
          items=c("---All Items---")
        }
        
        updateSelectInput(session, "brand", choices = items,selected = "---All Items---")
        }
      })
      
      observe({
        
        if(!is.null(input$gender) ){
          
        gender_exp1 <- paste("'",input$gender,"'",sep="")
        category_exp1 <- paste("'",input$category,"'",sep="")
        color_exp1 <- paste("'",input$brand,"'",sep="")
        
        query<-paste("select distinct Color from forecastData where MainCategory=",gender_exp1," and SubCategory=", category_exp1," and Brands=", color_exp1," order by Color",sep="")
        df<-sqldf(query)
        
        if (nrow(df)>0){
          df1 <- data.frame(Color = c("---All Items---"))
          df<-rbind(df,df1)
          items=c(df)
        }
        else
        {
          items=c("---All Items---")
        } 
        
        updateSelectInput(session, "color", choices =items,selected = "---All Items---" )
        }
      })
      
      observe({
        if(!is.null(input$gender) ){
          
        gender_exp1 <- paste("'",input$gender,"'",sep="")
        category_exp1 <- paste("'",input$category,"'",sep="")
        color_exp1 <- paste("'",input$brand,"'",sep="")
        size_exp1 <- paste("'",input$color,"'",sep="")
        
        query<-paste("select distinct Size from forecastData where MainCategory=",gender_exp1," and SubCategory=", category_exp1," and Brands=", color_exp1," and Color=", size_exp1," order by Size",sep="")
        df<-sqldf(query)
        
        if (nrow(df)>0){
          df1 <- data.frame(Size = c("---All Items---"))
          df<-rbind(df,df1)
          items=c(df)
        }
        else
        {
          items=c("---All Items---")
        } 
        
        updateSelectInput(session, "size", choices =items,selected = "---All Items---")
        }
        })
      
      observe({
        if(!is.null(input$gender) ){
          
        gender_exp1 <- paste("'",input$gender,"'",sep="")
        category_exp1 <- paste("'",input$category,"'",sep="")
        color_exp1 <- paste("'",input$brand,"'",sep="")
        size_exp1 <- paste("'",input$color,"'",sep="")
        description_exp1 <- paste("'",input$size,"'",sep="")
        
        query<-paste("select distinct Description from forecastData where MainCategory=",gender_exp1," and SubCategory=", category_exp1," and Brands=", color_exp1," and Color=", size_exp1," and Size=", description_exp1," order by Description",sep="")
        df<-sqldf(query)
        
        if (nrow(df)>0){
          df1 <- data.frame(Description = c("---All Items---"))
          df<-rbind(df,df1)
          items=c(df)
        }
        else
        {
          items=c("---All Items---")
        } 
        
        updateSelectInput(session, "description", choices = items,selected = "---All Items---")
        }
      })
      
        
    #### Main Panel Code ##################    
    output$monthly2 <- renderChart2({
      data1=forecastData 
      data_exp=fittedData
      #data_ratio=ratio
      
      MainCategory1=paste("'",input$gender,"'",sep="")
      SubCategory1=paste("'",input$category,"'",sep="")
      Brands1=paste("'",input$brand,"'",sep="")
      Color1=paste("'",input$color,"'",sep="")
      size1=paste("'",input$size,"'",sep="")
      description1=paste("'",input$description,"'",sep="")
      
      if (input$gender=='---All Items---'){
        query<-paste("select * from data1",sep="")
        query_exp<-paste("select * from data_exp",sep="")
        #query_ratio<-paste("select * from data_ratio",sep="")
      }
      else if (input$category=='---All Items---'){
        query<-paste("select * from data1 where MainCategory=",MainCategory1,sep="")
        query_exp<-paste("select * from data_exp where MainCategory=",MainCategory1,sep="")
        #query_ratio<-paste("select * from data_ratio where MainCategory=",MainCategory1,sep="")
      }
      else if(input$brand=='---All Items---'){
        query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,sep="")
        query_exp<-paste("select * from data_exp where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,sep="")
        #query_ratio<-paste("select * from data_ratio where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,sep="")
      }  
      else if(input$color=='---All Items---'){
        query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,sep="")
        query_exp<-paste("select * from data_exp where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,sep="")
        #query_ratio<-paste("select * from data_ratio where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,sep="")
      }
      else if(input$size=='---All Items---'){
        query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,sep="")
        query_exp<-paste("select * from data_exp where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,sep="")
        #query_ratio<-paste("select * from data_ratio where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,sep="")    
      }
      else if(input$description=='---All Items---'){
        query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,sep="")
        query_exp<-paste("select * from data_exp where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,sep="")
        #query_ratio<-paste("select * from data_ratio where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,sep="")
      }
      else{
        query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,"and Description=",description1,sep="")
        query_exp<-paste("select * from data_exp where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,"and Description=",description1,sep="")
        #query_ratio<-paste("select * from data_ratio where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,"and Description=",description1,sep="")
      }
      
      #print(query)
      #query<-paste("select * from data1 where MainCategory=","'Men Footwear'","and SubCategory=","'Casual'"," and Brands=","'Scott'","and Color=","'Black'","and Size=","'11'"," and Description=","'Wide'",sep="")
      #query_exp<-paste("select * from data_exp where MainCategory=","'Men Footwear'","and SubCategory=","'Casual'"," and Brands=","'Scott'","and Color=","'Black'","and Size=","'11'"," and Description=","'Wide'",sep="")
      #query_ratio<-paste("select * from data_ratio where MainCategory=","'Men Footwear'","and SubCategory=","'Casual'"," and Brands=","'Scott'","and Color=","'Black'","and Size=","'11'"," and Description=","'Wide'",sep="")
      
      Forecast_data<-sqldf(query)
      Fitted_data<-sqldf(query_exp)
      #ratio_data<-sqldf(query_ratio)
      
      n <- colnames(Forecast_data[,-1:-7])
      Forecast_data1=data.frame(colSums(Forecast_data[,-1:-7],na.rm = T,1))
      names(Forecast_data1)[1]='Quantity'
      Forecast_data1$ID <- n
      Forecast_data1$Status<-"Forecast"
      Forecast_data1$Status[1:(nrow(Forecast_data1)-12)]<-"Actual"
      Forecast_data1$ID=as.Date(Forecast_data1$ID,format='%d/%m/%Y')
      
      n1 <- colnames(Fitted_data[,-1:-7])
      Fitted_data1=data.frame(colSums(Fitted_data[,-1:-7],na.rm = T,1))
      names(Fitted_data1)[1]='Quantity'
      Fitted_data1$ID <- n1
      Fitted_data1$Status<-"Fitted"
      Fitted_data1$ID=as.Date(Fitted_data1$ID,format='%d/%m/%Y')
      
      finaldf<-rbind(Forecast_data1,Fitted_data1)
      
      # Ploting Output
      finaldata = transform(finaldf, dat2 = as.numeric(as.POSIXct(ID))*1000)
      h1 <- Highcharts$new()
      h1 <- hPlot(Quantity ~ dat2, data = finaldata, 
                  group = 'Status', 
                  type = "line",
                  radius=6
      )
      h1$xAxis(type = 'datetime', labels = list(
        format = '{value:%b-%Y}'  
      ))
      h1$yAxis(title = list(text = "Quantity"),
        min = 0 
      )
      h1$colors('rgba(236, 58, 62, 1)', 'rgba(0, 154, 217, 1)', 'rgba(88, 189, 66, 1)')
      h1$plotOptions(line = list(marker = list(symbol = 'circle')))
      
      
      s="Forecasting-"
      if (input$gender != "---All Items---")
      {
        s=paste(s,input$gender,sep = " ")
      }
      if (input$category != "---All Items---")
      {
        s=paste(s,input$category,sep = ", ")
      }
      if (input$brand != "---All Items---")
      {
        s=paste(s,input$brand,sep = ", ")
      }
      if (input$color != "---All Items---")
      {
        s=paste(s,input$color,sep = ", ")
      }
      if (input$size != "---All Items---")
      {
        s=paste(s,input$size,sep = ", ")
      }
      if (input$description != "---All Items---")
      {
        s=paste(s,input$description,sep = ", ")
      }
      
      h1$title(text = s)
      h1
    })
    
    #### download the filtered data ####
    output$downloadData9 = downloadHandler('ForecastData.csv', content = function(file) {
          # data1=forecastData      
          # MainCategory1=paste("'",input$gender,"'",sep="")
          # SubCategory1=paste("'",input$category,"'",sep="")
          # Brands1=paste("'",input$brand,"'",sep="")
          # Color1=paste("'",input$color,"'",sep="")
          # size1=paste("'",input$size,"'",sep="")
          # description1=paste("'",input$description,"'",sep="")
          # 
          # if (input$gender=='---All Items---'){
          #   query<-paste("select * from data1",sep="")
          # }
          # else if (input$category=='---All Items---'){
          #   query<-paste("select * from data1 where MainCategory=",MainCategory1,sep="")
          # }
          # else if(input$brand=='---All Items---'){
          #   query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,sep="")
          # }  
          # else if(input$color=='---All Items---'){
          #   query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,sep="")
          # }
          # else if(input$size=='---All Items---'){
          #   query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,sep="")
          # }
          # else if(input$description=='---All Items---'){
          #   query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,sep="")
          # }
          # else{
          #   query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,"and Description=",description1,sep="")
          #   
          # }
          # 
          # 
          # 
          # data_color<-sqldf(query)
          # 
          # n <- colnames(data_color[,-1:-6])
          # #n<-n[1:(length(n)-2)]
          # 
          # finaldf=data.frame(colSums(data_color[,-1:-6],na.rm = T,1))
          # #finaldf=data.frame(finaldf[1:(nrow(finaldf)-2),])
          # names(finaldf)[1]='TotalOrders'
          # #finaldf$ID <- seq.int(nrow(finaldf))
          # finaldf$ID <- n
          # myet<-tail(finaldf,6)
      myet<-forecastData
      ActualNames=names(myet[,-c(1:7)])
      
      newcolumn<-c(names(myet[,c(1:7)]),format(as.Date(as.Date(ActualNames,format = "%d/%m/%Y")), "%b-%Y"))
      names(myet)<-newcolumn

      write.csv(myet, row.names = FALSE,file)
    })
    
    output$downloadExp = downloadHandler('Exception.csv', content = function(file) {
      
      write.csv(template,row.names = FALSE, file)
    })
    
    #### Data Table screen####
    output$ratio <- renderDataTable({
      
      data1=forecastData 
      MainCategory1=paste("'",input$gender,"'",sep="")
      SubCategory1=paste("'",input$category,"'",sep="")
      Brands1=paste("'",input$brand,"'",sep="")
      Color1=paste("'",input$color,"'",sep="")
      size1=paste("'",input$size,"'",sep="")
      description1=paste("'",input$description,"'",sep="")
      
      if (input$gender=='---All Items---'){
        query<-paste("select * from data1",sep="")
      }
      else if (input$category=='---All Items---'){
        query<-paste("select * from data1 where MainCategory=",MainCategory1,sep="")
      }
      else if(input$brand=='---All Items---'){
        query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,sep="")
      }  
      else if(input$color=='---All Items---'){
        query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,sep="")
      }
      else if(input$size=='---All Items---'){
        query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,sep="")
      }
      else if(input$description=='---All Items---'){
        query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,sep="")
      }
      else{
        query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,"and Description=",description1,sep="")
      }
      
      #query<-paste("select * from data1 where MainCategory=","'Men Footwear'","and SubCategory=","'Casual'"," and Brands=","'Scott'","and Color=","'Black'","and Size=","'11'"," and Description=","'Wide'",sep="")
      
      Forecast_data<-sqldf(query)
      ActualNames=names(Forecast_data)
      newcolumn<-format(as.Date(as.Date(ActualNames,format = "%d/%m/%Y")), "%b-%Y")
      names(Forecast_data)<-newcolumn
      
      Forecast_data<-Forecast_data[,c((ncol(Forecast_data)-11):ncol(Forecast_data))]
      nameofcolumn<-names(Forecast_data)
      Forecast_data1=(data.frame(colSums(Forecast_data)))
      Forecast_data1$Month<-nameofcolumn
      Forecast_data1<-Forecast_data1[c(2,1)]
      #names(Forecast_data1)[1]='Total Orders'
      names(Forecast_data1)<-c('Month','Quantity')
      row.names(Forecast_data1) <- NULL 
      Forecast_data1=Forecast_data1
      #
      
      #newcolumn<-format(as.Date(as.Date(ActualNames,format = "%d/%m/%Y")), "%b-%Y")
      #names(Forecast_data1)<-newcolumn

    }, options = list(pageLength = 6) )  
    
    
    
    
    
##################################################################################################################################
##################################################### iForecaster Monthly  #######################################################
##################################################################################################################################
    
    #### OBSERVE CODES####
      observe({   
        if(!is.null(input$orders_ifor) ){
        orders_iform <- paste("'",input$orders_ifor,"'",sep="")
        if (input$orders_ifor=="All")
        {
          query<-paste("select distinct MainCategory from SummaryData order by MainCategory",sep="")
        }
        else
        {
          query<-paste("select distinct MainCategory from SummaryData where Bucket=",orders_iform," order by MainCategory",sep="")
        }
        
        df<-sqldf(query)
        
        if (nrow(df)>0){
          df1 <- data.frame(MainCategory = c("---All Items---"))
          df<-rbind(df,df1)
          items=c(df)
        }
        else
        {
          items=c("---All Items---")
        }
        
        updateSelectInput(session, "gender_ifor", choices =items,selected = "---All Items---")
        }
      })
      
      observe({
        if(!is.null(input$orders_ifor) ){
        orders_iform <- paste("'",input$orders_ifor,"'",sep="")
        gender_iform <- paste("'",input$gender_ifor,"'",sep="")
        
        if (input$orders_ifor=="All")
        {
          query<-paste("select distinct SubCategory from SummaryData where MainCategory=",gender_iform," order by SubCategory",sep="")
        }
        else
        {
          query<-paste("select distinct SubCategory from SummaryData where MainCategory=",gender_iform," and Bucket=", orders_iform," order by SubCategory",sep="")
        }
        
        df<-sqldf(query)
        
        if (nrow(df)>0){
          df1 <- data.frame(SubCategory = c("---All Items---"))
          df<-rbind(df,df1)
          items=c(df)
        }
        else
        {
          items=c("---All Items---")
        }
        updateSelectInput(session, "category_ifor", choices =items,selected = "---All Items---")
        }
      })
      
      observe({   
        if(!is.null(input$orders_ifor) ){
        orders_iform <- paste("'",input$orders_ifor,"'",sep="")
        gender_iform <- paste("'",input$gender_ifor,"'",sep="")
        category_iform <- paste("'",input$category_ifor,"'",sep="")
        
        if (input$orders_ifor=="All")
        {
          
          query<-paste("select distinct Brands from SummaryData where MainCategory=",gender_iform," and SubCategory=", category_iform," order by Brands",sep="")
        }
        else
        {
          
          query<-paste("select distinct Brands from SummaryData where MainCategory=",gender_iform," and SubCategory=", category_iform," and Bucket=", orders_iform," order by Brands",sep="")
        }
        
        df<-sqldf(query)
        
        if (nrow(df)>0)
        {
          df1 <- data.frame(Brands = c("---All Items---"))
          df<-rbind(df,df1)
          items=c(df)
        }
        else
        {
          items=c("---All Items---")
        }
        
        updateSelectInput(session, "brand_ifor", choices = items,selected = "---All Items---")
        }
      })
      
      observe({
        if(!is.null(input$orders_ifor) ){
        orders_iform <- paste("'",input$orders_ifor,"'",sep="")
        gender_iform <- paste("'",input$gender_ifor,"'",sep="")
        category_iform <- paste("'",input$category_ifor,"'",sep="")
        color_iform <- paste("'",input$brand_ifor,"'",sep="")
        
        if (input$orders_ifor=="All")
        {
          query<-paste("select distinct Color from SummaryData where MainCategory=",gender_iform," and SubCategory=", category_iform," and Brands=", color_iform," order by Color",sep="")
        }
        else
        {
          query<-paste("select distinct Color from SummaryData where MainCategory=",gender_iform," and SubCategory=", category_iform," and Brands=", color_iform," and Bucket=", orders_iform," order by Color",sep="")
        }
        
        df<-sqldf(query)
        if (nrow(df)>0){
          df1 <- data.frame(Color = c("---All Items---"))
          df<-rbind(df,df1)
          items=c(df)
        }
        else
        {
          items=c("---All Items---")
        }
        
        updateSelectInput(session, "color_ifor", choices =items,selected = "---All Items---" )
        }
        })
      
      observe({
        if(!is.null(input$orders_ifor) ){
        orders_iform <- paste("'",input$orders_ifor,"'",sep="")
        gender_iform <- paste("'",input$gender_ifor,"'",sep="")
        category_iform <- paste("'",input$category_ifor,"'",sep="")
        color_iform <- paste("'",input$brand_ifor,"'",sep="")
        size_iform <- paste("'",input$color_ifor,"'",sep="")
        
        if (input$orders_ifor=="All")
        {
          query<-paste("select distinct Size from SummaryData where MainCategory=",gender_iform," and SubCategory=", category_iform," and Brands=", color_iform," and Color=", size_iform," order by Size",sep="")
        }
        else
        {
          query<-paste("select distinct Size from SummaryData where MainCategory=",gender_iform," and SubCategory=", category_iform," and Brands=", color_iform," and Color=", size_iform," and Bucket=", orders_iform," order by Size",sep="")
        }
        
        df<-sqldf(query)
        if (nrow(df)>0){
          df1 <- data.frame(Size = c("---All Items---"))
          df<-rbind(df,df1)
          items=c(df)
        }
        else
        {
          items=c("---All Items---")
        } 
        
        updateSelectInput(session, "size_ifor", choices =items,selected = "---All Items---")
        }
      })
      
      observe({
        if(!is.null(input$orders_ifor) ){
        orders_iform <- paste("'",input$orders_ifor,"'",sep="")
        gender_iform <- paste("'",input$gender_ifor,"'",sep="")
        category_iform <- paste("'",input$category_ifor,"'",sep="")
        color_iform <- paste("'",input$brand_ifor,"'",sep="")
        size_iform <- paste("'",input$color_ifor,"'",sep="")
        description_iform <- paste("'",input$size_ifor,"'",sep="")
        
        if (input$orders_ifor=="All")
        {
          query<-paste("select distinct Description from SummaryData where MainCategory=",gender_iform," and SubCategory=", category_iform," and Brands=", color_iform," and Color=", size_iform," and Size=", description_iform," order by Description",sep="")
        }
        else
        {
          query<-paste("select distinct Description from SummaryData where MainCategory=",gender_iform," and SubCategory=", category_iform," and Brands=", color_iform," and Color=", size_iform," and Size=", description_iform," and Bucket=", orders_iform," order by Description",sep="")
        }
        
        df<-sqldf(query)
        if (nrow(df)>0){
          df1 <- data.frame(Description = c("---All Items---"))
          df<-rbind(df,df1)
          items=c(df)
        }
        else
        {
          items=c("---All Items---")
        }
        
        updateSelectInput(session, "description_ifor", choices = items,selected = "---All Items---")
        }
      })
      
      
    #### Data extrcation codes ##############################
    
    mydata <- reactive({
      data1=SummaryData
      orders1 <- paste("'",input$orders_ifor,"'",sep="")
      MainCategory1=paste("'",input$gender_ifor,"'",sep="")
      SubCategory1=paste("'",input$category_ifor,"'",sep="")
      Brands1=paste("'",input$brand_ifor,"'",sep="")
      Color1=paste("'",input$color_ifor,"'",sep="")
      size1=paste("'",input$size_ifor,"'",sep="")
      description1=paste("'",input$description_ifor,"'",sep="")
      
      if (input$orders_ifor=="All"){
        if (input$gender_ifor=='---All Items---'){
          query<-paste("select * from data1",sep="")
        }
        else if (input$category_ifor=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,sep="")
        }
        else if(input$brand_ifor=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,sep="")
        }  
        else if(input$color_ifor=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,sep="")
        }
         else if(input$size_ifor=='---All Items---'){
           query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,sep="")
         }
         else if(input$description_ifor=='---All Items---'){
           query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,sep="")
         }
        else{
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,sep="")
        }
      }
      else{
        if (input$gender_ifor=='---All Items---'){
          query<-paste("select * from data1 where Bucket=",orders1,sep="")
        }
        else if (input$category_ifor=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1," and Bucket=", orders1,sep="")
        }
        else if(input$brand_ifor=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1," and Bucket=", orders1,sep="")
        }  
        else if(input$color_ifor=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1," and Bucket=", orders1,sep="")
        }
         else if(input$size_ifor=='---All Items---'){
           query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1," and Bucket=", orders1,sep="")
         }
         else if(input$description_ifor=='---All Items---'){
           query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1," and Bucket=", orders1,sep="")
         }
        else{
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1," and Bucket=", orders1,sep="")
        }
      }
      
      data11<-sqldf(query)
      
      n <- colnames(data11[,-1:-8])
      
      data111=data.frame(colSums(data11[,-1:-8],na.rm = T,1))
      names(data111)[1]='Months'
      data111$dates <- n
      data111 <- data111[c(2,1)]
    })
    
    getDataset_ifor <- reactive({
      
      data1=SummaryData
      orders1 <- paste("'",input$orders_ifor,"'",sep="")
      MainCategory1=paste("'",input$gender_ifor,"'",sep="")
      SubCategory1=paste("'",input$category_ifor,"'",sep="")
      Brands1=paste("'",input$brand_ifor,"'",sep="")
      Color1=paste("'",input$color_ifor,"'",sep="")
      size1=paste("'",input$size_ifor,"'",sep="")
      description1=paste("'",input$description_ifor,"'",sep="")
      
      if (input$orders_ifor=="All"){
        if (input$gender_ifor=='---All Items---'){
          query<-paste("select * from data1",sep="")
        }
        else if (input$category_ifor=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,sep="")
        }
        else if(input$brand_ifor=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,sep="")
        }  
        else if(input$color_ifor=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,sep="")
        }
         else if(input$size_ifor=='---All Items---'){
           query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,sep="")
         }
         else if(input$description_ifor=='---All Items---'){
           query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,sep="")
         }
        else{
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,sep="")
        }
      }
      else{
        if (input$gender_ifor=='---All Items---'){
          query<-paste("select * from data1 where Bucket=",orders1,sep="")
        }
        else if (input$category_ifor=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1," and Bucket=", orders1,sep="")
        }
        else if(input$brand_ifor=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1," and Bucket=", orders1,sep="")
        }  
        else if(input$color_ifor=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1," and Bucket=", orders1,sep="")
        }
         else if(input$size_ifor=='---All Items---'){
           query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1," and Bucket=", orders1,sep="")
         }
         else if(input$description_ifor=='---All Items---'){
           query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1," and Bucket=", orders1,sep="")
         }
        else{
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1," and Bucket=", orders1,sep="")
        }
      }
      
      data11<-sqldf(query)
      
      n <- colnames(data11[,-1:-8])
      
      data111=data.frame(colSums(data11[,-1:-8],na.rm = T,1))
      names(data111)[1]='Months'
      data111$dates <- n
      data111 <- data111[c(2,1)]
      
      tosa1=data111
      tosa1[,1]=as.Date(tosa1[,1], format = "%d/%m/%Y")
      x1=as.numeric(format(min(tosa1[,1]),'%Y'))
      x2=as.numeric(format(min(tosa1[,1]),'%m'))
      
      y1=as.numeric(format(max(tosa1[,1]),'%Y'))
      y2=as.numeric(format(max(tosa1[,1]),'%m'))
      
      tosa <- ts(tosa1, start=c(x1,x2), end=c(y1, y2), frequency=12) 
      tosa=tosa[,2]
    })
    
    
    ################## Forecasting models
    ####1. ETS Model Coding####
    forecastPlotInputETS <- function() {
      #x <- forecast(ets(getDataset_ifor()), h=6)
      x <- forecast(stlf(getDataset_ifor(), method = "ets",level=c(70,80)), h = 6)
      #hchart(x)%>% 
      #  hc_add_series(data = x$fitted,name = paste("Fitted"))
      
      plot(x, flty = 3, ylim=c(0,max(x$x)),axes=TRUE)
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
      
    }
    
    output$forecastPlotETS <- renderPlot({
      
      forecastPlotInputETS()
      
    })
    
    output$ETStab <- renderDataTable({
      #myet <- data.frame(forecast(ets(getDataset_ifor()), h=6))
      myet <- data.frame(forecast(stlf(getDataset_ifor(), method = "ets",level=c(70,80)), h=6))
      myet <-round(myet,0)
    })
    
    ####2. ARIMA model#### 
    forecastPlotInputARIMA<- function() {
      #x <- forecast(auto.arima(getDataset_ifor()), h=6)
      x <- forecast(stlf(getDataset_ifor(), method = "arima",level=c(70,80)), h = 6)
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
      
    }
    
    output$forecastPlotARIMA<- renderPlot({
      
      forecastPlotInputARIMA()
      
    })
    
    output$ARtab <- renderDataTable({
      #myar <- data.frame(forecast(auto.arima(getDataset_ifor()), h=6))
      myar <- data.frame(forecast(stlf(getDataset_ifor(), method = "arima",level=c(70,80)), h=6))
      myar <-round(myar,0)
    })
    
    ####3. StructTS model#### 
    forecastPlotInputStructTS<- function() {
      x <- forecast(StructTS(getDataset_ifor(), "level",level=c(70,80)), h=6)
      
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
    }
    
    output$forecastPlotStructTS<- renderPlot({
      
      forecastPlotInputStructTS()        
      
    })
    
    output$STtab <- renderDataTable({
      myts <- data.frame(forecast(StructTS(getDataset_ifor(), "level",level=c(70,80)), h=6))
      myts <-round(myts,0)
    })
    
    ####4. HoltWinters model####
    forecastPlotInputHoltWinters<- function() {
      x <- forecast(HoltWinters(getDataset_ifor(), gamma=FALSE), h=6,level=c(70,80))
      
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
    }
    
    output$forecastPlotHoltWinters<- renderPlot({
      
      forecastPlotInputHoltWinters()
      
    })
    
    output$HWtab <- renderDataTable({
      myHW <- data.frame(forecast(HoltWinters(getDataset_ifor(), gamma=FALSE), h=6,level=c(70,80)))
      myHW <-round(myHW,0)
    })
    
    ####5. Thetaf Model####
    forecastPlotInputthetaf<- function() {
      x <- forecast(thetaf(getDataset_ifor(),level=c(70,80)), h=6)
      
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
    }
    
    output$forecastPlotthetaf<- renderPlot({
      
      forecastPlotInputthetaf()
      
    })
    
    output$TFtab <- renderDataTable({
      myTF <- data.frame(forecast(thetaf(getDataset_ifor(),level=c(70,80)), h=6))
      myTF <-round(myTF,0)
    })
    
    ####6. RWF Model####
    forecastPlotInputrwf<- function() {
      x <- forecast(stlf(getDataset_ifor(), method = "rwdrift",level=c(70,80)), h=6)
      
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
    }
    
    output$forecastPlotrwf<- renderPlot({
      forecastPlotInputrwf()
    })
    
    output$RFtab <- renderDataTable({
      myRF <- data.frame(forecast(stlf(getDataset_ifor(), method = "rwdrift",level=c(70,80)), h=6))
      myRF <-round(myRF,0)
    })
    
    ####7. Naive model####
    forecastPlotInputnaive<- function() {
      x <- forecast(stlf(getDataset_ifor(), method = "naive",level=c(70,80)), h=6)
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
    }
    
    output$forecastPlotnaive<- renderPlot({
      forecastPlotInputnaive()
    })
    
    output$NAtab <- renderDataTable({
      myNA <- data.frame(forecast(stlf(getDataset_ifor(), method = "naive",level=c(70,80)), h=6))
      myNA <-round(myNA,0)
    })
    
    ####8. Tbats model####
    forecastPlotInputtbats<- function() {
      x <- forecast(tbats(getDataset_ifor(), use.parallel=TRUE), h=6,level=c(70,80))
      
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
    }
    
    output$forecastPlottbats<- renderPlot({
      
      forecastPlotInputtbats()
    })
    
    output$TBtab <- renderDataTable({
      myTB <- data.frame(forecast(tbats(getDataset_ifor(), use.parallel=TRUE), h=6,level=c(70,80)))
      myTB <-round(myTB,0)
    })
    
    ####9. Meanf model ####
    forecastPlotInputmeanf<- function() {
      x <- forecast(meanf(getDataset_ifor(),level=c(70,80)), h=6)
      
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
    }
    
    output$forecastPlotmeanf<- renderPlot({
      forecastPlotInputmeanf()
    })
    
    output$MFtab <- renderDataTable({
      myMF <- data.frame(forecast(meanf(getDataset_ifor(),level=c(70,80)), h=6))
      myMF <-round(myMF,0)
    })
    
    ####10. Splinef model####
    forecastPlotInputsplinef<- function() {
      x <- forecast(splinef(getDataset_ifor(),level=c(70,80)), h=6)
      
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
    }
    output$forecastPlotsplinef<- renderPlot({
      
      forecastPlotInputsplinef()
      
    })
    output$SFtab <- renderDataTable({
      mySF <- data.frame(forecast(splinef(getDataset_ifor(),level=c(70,80)), h=6))
      mySF <-round(mySF,0)
      
    })
    
    ####Each Models Accuracy Table####
    output$ACC <- renderDataTable({
      tosa=mydata()
      acets <- round(accuracy(forecast(ets(tosa[,2]), h=6)),2)
      acarm <- round(accuracy(forecast(auto.arima(tosa[2:2]), h=6)),2)
      #acts <- round(accuracy(forecast(StructTS(tosa[2:2],"level"), h=6)),2)
      achw <- round(accuracy(forecast(HoltWinters(tosa[2:2], gamma=FALSE), h=6)),2)
      acTH <- round(accuracy(forecast(thetaf(tosa[,2]), h=6)),2)
      acRW <- round(accuracy(forecast(rwf(tosa[2:2]), h=6)),2)
      acNV <- round(accuracy(forecast(naive(tosa[2:2]), h=6)),2)
      acTT <- round(accuracy(forecast(tbats(tosa[,2],use.parallel=TRUE), h=6)),2)
      #acMF <- accuracy(forecast(meanf(tosa[2:2]), h=6))
      #acCS <- round(accuracy(forecast(splinef(tosa[2:2]), h=6)),2)
      #fin=rbind(acets,acarm,acts,achw,acTH,acRW,acNV,acTT,acCS)
      fin=rbind(acets,acarm,achw,acTH,acRW,acNV,acTT)
      finn=data.frame(fin)
      # finn$Model=c("ETS","ARIMA","StructTS","HoltWinters","ThetaF","RandomWalk","Naive","Tbats","CubicSpline")
      finn$Model=c("ETS","ARIMA","HoltWinters","ThetaF","RandomWalk","Naive","Tbats")
      finn=finn[c("Model","ME","RMSE","MAE","MPE","MAPE","MASE","ACF1")]
      DT::datatable(finn,
                    options = list(rowCallback = JS(
                      paste0('function(row, data) {
                             // Bold cells for the max in the first column
                             if (parseFloat(data[6]) == ',min(finn[,6]),')
                             
                             $("td:eq(6)", row).css("background-color", "#9BF59B");
    }')
    )))
  })
    
    ####Code to download the filtered data####
    output$downloadData = downloadHandler('ETS.csv', content = function(file) {
      tosa=mydata()
      myet <- forecast(ets(tosa[,2],level=c(70,80)), h=6)
      write.csv(myet, file)
    })
    output$downloadData1 = downloadHandler('Arima.csv', content = function(file) {
      tosa=mydata()
      myet <- forecast(auto.arima(tosa[2:2],level=c(70,80)), h=6)
      write.csv(myet, file)
    })
    output$downloadData2 = downloadHandler('StructTS.csv', content = function(file) {
      tosa=mydata()
      myet <- forecast(StructTS(tosa[2:2], "level",level=c(70,80)), h=6)
      write.csv(myet, file)
    })
    output$downloadData3 = downloadHandler('HoltWinters.csv', content = function(file) {
      tosa=mydata()
      myet <- forecast(HoltWinters(tosa[2:2], gamma=FALSE), h=6,level=c(70,80))
      write.csv(myet, file)
    })
    output$downloadData4 = downloadHandler('thetaf.csv', content = function(file) {
      tosa=mydata()
      myet <- forecast(thetaf(tosa[,2],level=c(70,80)), h=6)
      write.csv(myet, file)
    })
    output$downloadData5 = downloadHandler('RandomWalk.csv', content = function(file) {
      tosa=mydata()
      myet <- forecast(rwf(tosa[2:2],level=c(70,80)), h=6)
      write.csv(myet, file)
    })
    output$downloadData6 = downloadHandler('naive.csv', content = function(file) {
      tosa=mydata()
      myet <- forecast(naive(tosa[2:2],level=c(70,80)), h=6)
      write.csv(myet, file)
    })
    output$downloadData7 = downloadHandler('tbats.csv', content = function(file) {
      tosa=mydata()
      myet <- forecast(tbats(tosa[,2], use.parallel=TRUE), h=6,level=c(70,80))
      write.csv(myet, file)
    })
    output$downloadData8 = downloadHandler('splinef.csv', content = function(file) {
      tosa=mydata()
      myet <- forecast(splinef(tosa[2:2],level=c(70,80)), h=6)
      write.csv(myet, file)
    })    
    
    
    
    
    
##################################################################################################################################
##################################################### iForecaster Quarterly ######################################################
##################################################################################################################################
    
    ####oBSERVE CODES####
    observe({    
      if(!is.null(input$orders_ifor1) ){
        
      orders_ifor1 <- paste("'",input$orders_ifor1,"'",sep="")
      if (input$orders_ifor1=="All")
      {
        query<-paste("select distinct MainCategory from SummaryDataQtr order by MainCategory",sep="")
      }
      else
      {
        query<-paste("select distinct MainCategory from SummaryDataQtr where Bucket=",orders_ifor1," order by MainCategory",sep="")
      }
      
      df<-sqldf(query)
      
      if (nrow(df)>0){
        df1 <- data.frame(MainCategory = c("---All Items---"))
        df<-rbind(df,df1)
        items=c(df)
      }
      else
      {
        items=c("---All Items---")
      }
      
      updateSelectInput(session, "gender_ifor1", choices =items,selected = "---All Items---")
      }
      })
    
    observe({    
      if(!is.null(input$orders_ifor1) ){
      orders_ifor1 <- paste("'",input$orders_ifor1,"'",sep="")
      gender_ifor1 <- paste("'",input$gender_ifor1,"'",sep="")
      
      if (input$orders_ifor1=="All")
      {
        query<-paste("select distinct SubCategory from SummaryDataQtr where MainCategory=",gender_ifor1," order by SubCategory",sep="")
      }
      else
      {
        query<-paste("select distinct SubCategory from SummaryDataQtr where MainCategory=",gender_ifor1," and Bucket=", orders_ifor1," order by SubCategory",sep="")
      }
      
      df<-sqldf(query)
      
      if (nrow(df)>0){
        df1 <- data.frame(SubCategory = c("---All Items---"))
        df<-rbind(df,df1)
        items=c(df)
      }
      else
      {
        items=c("---All Items---")
      }
      updateSelectInput(session, "category_ifor1", choices =items,selected = "---All Items---")
      }
      })
    
    observe({   
      if(!is.null(input$orders_ifor1) ){
      orders_ifor1 <- paste("'",input$orders_ifor1,"'",sep="")
      gender_ifor1 <- paste("'",input$gender_ifor1,"'",sep="")
      category_ifor1 <- paste("'",input$category_ifor1,"'",sep="")
      
      if (input$orders_ifor1=="All")
      {
        
        query<-paste("select distinct Brands from SummaryDataQtr where MainCategory=",gender_ifor1," and SubCategory=", category_ifor1," order by Brands",sep="")
      }
      else
      {
        
        query<-paste("select distinct Brands from SummaryDataQtr where MainCategory=",gender_ifor1," and SubCategory=", category_ifor1," and Bucket=", orders_ifor1," order by Brands",sep="")
      }
      
      df<-sqldf(query)
      
      if (nrow(df)>0)
      {
        df1 <- data.frame(Brands = c("---All Items---"))
        df<-rbind(df,df1)
        items=c(df)
      }
      else
      {
        items=c("---All Items---")
      }
      
      updateSelectInput(session, "brand_ifor1", choices = items,selected = "---All Items---")
      }
    })
    
    observe({
      if(!is.null(input$orders_ifor1) ){
      orders_ifor1 <- paste("'",input$orders_ifor1,"'",sep="")
      gender_ifor1 <- paste("'",input$gender_ifor1,"'",sep="")
      category_ifor1 <- paste("'",input$category_ifor1,"'",sep="")
      color_ifor1 <- paste("'",input$brand_ifor1,"'",sep="")
      
      if (input$orders_ifor1=="All")
      {
        query<-paste("select distinct Color from SummaryDataQtr where MainCategory=",gender_ifor1," and SubCategory=", category_ifor1," and Brands=", color_ifor1," order by Color",sep="")
      }
      else
      {
        query<-paste("select distinct Color from SummaryDataQtr where MainCategory=",gender_ifor1," and SubCategory=", category_ifor1," and Brands=", color_ifor1," and Bucket=", orders_ifor1," order by Color",sep="")
      }
      
      df<-sqldf(query)
      if (nrow(df)>0){
        df1 <- data.frame(Color = c("---All Items---"))
        df<-rbind(df,df1)
        items=c(df)
      }
      else
      {
        items=c("---All Items---")
      }
      
      updateSelectInput(session, "color_ifor1", choices =items,selected = "---All Items---" )
      }
      })
    
    observe({
      if(!is.null(input$orders_ifor1) ){
      orders_ifor1 <- paste("'",input$orders_ifor1,"'",sep="")
      gender_ifor1 <- paste("'",input$gender_ifor1,"'",sep="")
      category_ifor1 <- paste("'",input$category_ifor1,"'",sep="")
      color_ifor1 <- paste("'",input$brand_ifor1,"'",sep="")
      size_ifor1 <- paste("'",input$color_ifor1,"'",sep="")
      
      if (input$orders_ifor1=="All")
      {
        query<-paste("select distinct Size from SummaryDataQtr where MainCategory=",gender_ifor1," and SubCategory=", category_ifor1," and Brands=", color_ifor1," and Color=", size_ifor1," order by Size",sep="")
      }
      else
      {
        query<-paste("select distinct Size from SummaryDataQtr where MainCategory=",gender_ifor1," and SubCategory=", category_ifor1," and Brands=", color_ifor1," and Color=", size_ifor1," and Bucket=", orders_ifor1," order by Size",sep="")
      }
      
      df<-sqldf(query)
      if (nrow(df)>0){
        df1 <- data.frame(Size = c("---All Items---"))
        df<-rbind(df,df1)
        items=c(df)
      }
      else
      {
        items=c("---All Items---")
      } 
      
      updateSelectInput(session, "size_ifor1", choices =items,selected = "---All Items---")
      }
      })
    
    observe({
      if(!is.null(input$orders_ifor1) ){
      orders_ifor1 <- paste("'",input$orders_ifor1,"'",sep="")
      gender_ifor1 <- paste("'",input$gender_ifor1,"'",sep="")
      category_ifor1 <- paste("'",input$category_ifor1,"'",sep="")
      color_ifor1 <- paste("'",input$brand_ifor1,"'",sep="")
      size_ifor1 <- paste("'",input$color_ifor1,"'",sep="")
      description_ifor1 <- paste("'",input$size_ifor1,"'",sep="")
      
      if (input$orders_ifor1=="All")
      {
        query<-paste("select distinct Description from SummaryDataQtr where MainCategory=",gender_ifor1," and SubCategory=", category_ifor1," and Brands=", color_ifor1," and Color=", size_ifor1," and Size=", description_ifor1," order by Description",sep="")
      }
      else
      {
        query<-paste("select distinct Description from SummaryDataQtr where MainCategory=",gender_ifor1," and SubCategory=", category_ifor1," and Brands=", color_ifor1," and Color=", size_ifor1," and Size=", description_ifor1," and Bucket=", orders_ifor1," order by Description",sep="")
      }
      
      df<-sqldf(query)
      if (nrow(df)>0){
        df1 <- data.frame(Description = c("---All Items---"))
        df<-rbind(df,df1)
        items=c(df)
      }
      else
      {
        items=c("---All Items---")
      }
      
      updateSelectInput(session, "description_ifor1", choices = items,selected = "---All Items---")
      }
    })
    
    
    
    ####Data Extraction code####
    mydataQtr <- reactive({
      
      data1=SummaryDataQtr
      
      orders1 <- paste("'",input$orders_ifor1,"'",sep="")
      MainCategory1=paste("'",input$gender_ifor1,"'",sep="")
      SubCategory1=paste("'",input$category_ifor1,"'",sep="")
      Brands1=paste("'",input$brand_ifor1,"'",sep="")
      Color1=paste("'",input$color_ifor1,"'",sep="")
      size1=paste("'",input$size_ifor1,"'",sep="")
      description1=paste("'",input$description_ifor1,"'",sep="")
      
      if (input$orders_ifor1=="All"){
        if (input$gender_ifor1=='---All Items---'){
          query<-paste("select * from data1",sep="")
        }
        else if (input$category_ifor1=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,sep="")
        }
        else if(input$brand_ifor1=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,sep="")
        }  
        else if(input$color_ifor1=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,sep="")
        }
        else if(input$size_ifor1=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,sep="")
        }
        else if(input$description_ifor1=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,sep="")
        }
        else{
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,"and Description=",description1,sep="")
        }
      }
      else{
        if (input$gender_ifor1=='---All Items---'){
          query<-paste("select * from data1 where Bucket=",orders1,sep="")
        }
        else if (input$category_ifor1=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1," and Bucket=", orders1,sep="")
        }
        else if(input$brand_ifor1=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1," and Bucket=", orders1,sep="")
        }  
        else if(input$color_ifor1=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1," and Bucket=", orders1,sep="")
        }
        else if(input$size_ifor1=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1," and Bucket=", orders1,sep="")
        }
        else if(input$description_ifor1=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1," and Bucket=", orders1,sep="")
        }
        else{
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,"and Description=",description1," and Bucket=", orders1,sep="")
        }
      }
      
      data11<-sqldf(query)
      
      n <- colnames(data11[,-1:-8])
      
      data111=data.frame(colSums(data11[,-1:-8],na.rm = T,1))
      names(data111)[1]='Months'
      data111$dates <- n
      data111 <- data111[c(2,1)]
      
    })
    
    getDataset_ifor1 <- reactive({
      
      data1=SummaryDataQtr
      
      orders1 <- paste("'",input$orders_ifor1,"'",sep="")
      MainCategory1=paste("'",input$gender_ifor1,"'",sep="")
      SubCategory1=paste("'",input$category_ifor1,"'",sep="")
      Brands1=paste("'",input$brand_ifor1,"'",sep="")
      Color1=paste("'",input$color_ifor1,"'",sep="")
      size1=paste("'",input$size_ifor1,"'",sep="")
      description1=paste("'",input$description_ifor1,"'",sep="")
      
      if (input$orders_ifor1=="All"){
        if (input$gender_ifor1=='---All Items---'){
          query<-paste("select * from data1",sep="")
        }
        else if (input$category_ifor1=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,sep="")
        }
        else if(input$brand_ifor1=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,sep="")
        }  
        else if(input$color_ifor1=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,sep="")
        }
        else if(input$size_ifor1=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,sep="")
        }
        else if(input$description_ifor1=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,sep="")
        }
        else{
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,"and Description=",description1,sep="")
        }
      }
      else{
        if (input$gender_ifor1=='---All Items---'){
          query<-paste("select * from data1 where Bucket=",orders1,sep="")
        }
        else if (input$category_ifor1=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1," and Bucket=", orders1,sep="")
        }
        else if(input$brand_ifor1=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1," and Bucket=", orders1,sep="")
        }  
        else if(input$color_ifor1=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1," and Bucket=", orders1,sep="")
        }
        else if(input$size_ifor1=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1," and Bucket=", orders1,sep="")
        }
        else if(input$description_ifor1=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1," and Bucket=", orders1,sep="")
        }
        else{
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,"and Description=",description1," and Bucket=", orders1,sep="")
        }
      }
      
      df1<-sqldf(query)
      n <- colnames(df1[,-1:-8])
      
      df2=data.frame(colSums(df1[,-1:-8],na.rm = T,1))
      names(df2)[1]='Months'
      df2$dates <- n
      df2 <- df2[c(2,1)]
      
      tosa1=df2
      
      tosa1[,1]=as.Date(tosa1[,1], format = "%d/%m/%Y")
      x1=as.numeric(format(min(tosa1[,1]),'%Y'))
      x2=as.numeric(format(min(tosa1[,1]),'%m'))
      
      y1=as.numeric(format(max(tosa1[,1]),'%Y'))
      y2=as.numeric(format(max(tosa1[,1]),'%m'))
      
      tosa <- ts(tosa1, start=c(x1,x2), end=c(y1, y2), frequency=4) 
      tosa=tosa[,2]
      
    })
    
    
    
    ####Forecasting Models####
    ####Forecast model plot function for ETS#######################
    forecastPlotInputETSQtr <- function() {
      
      x <- forecast(stlf(getDataset_ifor1(), method = "ets"), h=3)
      
      plot.new()
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
      
    }
    
    output$forecastPlotETSQtr <- renderPlot({
      
      forecastPlotInputETSQtr()
      
    })
    
    output$ETStabQtr <- renderDataTable({
       myet <- data.frame(forecast(stlf(getDataset_ifor1(), method = "ets"), h=3))
      myet <-round(myet,0)
    })
    
    
    ####Forecast model plot function for ARIMA###############################
    forecastPlotInputARIMAQtr<- function() {
      
      x <- forecast(auto.arima(getDataset_ifor1()), h=3)
      
      plot.new()
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
      
    }
    
    output$forecastPlotARIMAQtr<- renderPlot({
      
      forecastPlotInputARIMAQtr()
      
    })
    
    output$ARtabQtr <- renderDataTable({
      myar <- data.frame(forecast(auto.arima(getDataset_ifor1()), h=3))
      myar <-round(myar,0)
    })
    
    ####Forecast model plot function for StructTS###############################
    forecastPlotInputStructTSQtr<- function() {
      x <- forecast(StructTS(getDataset_ifor1(), "level"), h=3)
      
      plot.new()
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
    }
    
    output$forecastPlotStructTSQtr<- renderPlot({
      
      forecastPlotInputStructTSQtr()        
      
    })
    
    output$STtabQtr <- renderDataTable({
      tosa=mydataQtr()
      myts <- data.frame(forecast(StructTS(getDataset_ifor1(), "level"), h=3))
      myts <-round(myts,0)
    })
    
    
    ####Forecast model plot function for HoltWinters#####################
    forecastPlotInputHoltWintersQtr<- function() {
      x <- forecast(HoltWinters(getDataset_ifor1(), gamma=FALSE), h=3)
      
      plot.new()
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
    }
    
    output$forecastPlotHoltWintersQtr<- renderPlot({
      
      forecastPlotInputHoltWintersQtr()
      
    })
    
    
    output$HWtabQtr <- renderDataTable({
      tosa=mydataQtr()
      myHW <- data.frame(forecast(HoltWinters(getDataset_ifor1(), gamma=FALSE), h=3))
      myHW <-round(myHW,0)
    })
    
    ####Forecast model plot function for thetaf###########################
    forecastPlotInputthetafQtr<- function() {
      x <- forecast(thetaf(getDataset_ifor1()), h=3)
      
      plot.new()
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
    }
    
    output$forecastPlotthetafQtr<- renderPlot({
      
      forecastPlotInputthetafQtr()
      
    })
    
    output$TFtabQtr <- renderDataTable({
      myTF <- data.frame(forecast(thetaf(getDataset_ifor1()), h=3))
      myTF <-round(myTF,0)
    })
    
    ####Forecast model plot function for rwf#######################
    forecastPlotInputrwfQtr<- function() {
      x <- forecast(rwf(getDataset_ifor1()), h=3)
      plot.new()
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
    }
    
    output$forecastPlotrwfQtr<- renderPlot({
      forecastPlotInputrwfQtr()
    })
    
    output$RFtabQtr <- renderDataTable({
      tosa=mydataQtr()
      myRF <- data.frame(forecast(rwf(getDataset_ifor1()), h=3))
      myRF <-round(myRF,0)
    })
    
    ####Forecast model plot function for naive#####################
    forecastPlotInputnaiveQtr<- function() {
      x <- forecast(naive(getDataset_ifor1()), h=3)
      
      plot.new()
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
    }
    
    output$forecastPlotnaiveQtr<- renderPlot({
      forecastPlotInputnaiveQtr()
    })
    
    output$NAtabQtr <- renderDataTable({
      tosa=mydataQtr()
      myNA <- data.frame(forecast(naive(getDataset_ifor1()), h=3))
      myNA <-round(myNA,0)
    })    
    
    ####Forecast model plot function for tbats#######################
    forecastPlotInputtbatsQtr<- function() {
      x <- forecast(tbats(getDataset_ifor1(), use.parallel=TRUE), h=3)
      plot.new()
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
    }
    
    output$forecastPlottbatsQtr<- renderPlot({
      
      forecastPlotInputtbatsQtr()
    })
    
    output$TBtabQtr <- renderDataTable({
      myTB <- data.frame(forecast(tbats(getDataset_ifor1(), use.parallel=TRUE), h=3))
      myTB <-round(myTB,0)
    })  
    
    
    ####Forecast model plot function for meanf#######################
    forecastPlotInputmeanfQtr<- function() {
      x <- forecast(meanf(getDataset_ifor1()), h=3)
      
      plot.new()
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
    }
    
    output$forecastPlotmeanfQtr<- renderPlot({
      forecastPlotInputmeanfQtr()
    })
    
    output$MFtabQtr <- renderDataTable({
      myMF <- data.frame(forecast(meanf(getDataset_ifor1()), h=3))
      myMF <-round(myMF,0)
    })
    
    
    ####Forecast model plot function for splinef#####################
    forecastPlotInputsplinefQtr<- function() {
      x <- forecast(splinef(getDataset_ifor1()), h=3)
      
      plot.new()
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
    }
    
    output$forecastPlotsplinefQtr<- renderPlot({
      
      forecastPlotInputsplinefQtr()
      
    })
    output$SFtabQtr <- renderDataTable({
      mySF <- data.frame(forecast(splinef(getDataset_ifor1()), h=3))
      mySF <-round(mySF,0)
      
    })
    
    ####Each Forecasting Models Accuracy###############################################
    output$ACCQtr <- renderDataTable({
      tosa=mydataQtr()
      acets <- round(accuracy(forecast(ets(tosa[,2]), h=3)),2)
      acarm <- round(accuracy(forecast(auto.arima(tosa[2:2]), h=3)),2)
      #acts <- round(accuracy(forecast(StructTS(tosa[2:2],"level"), h=3)),2)
      achw <- round(accuracy(forecast(HoltWinters(tosa[2:2], gamma=FALSE), h=3)),2)
      acTH <- round(accuracy(forecast(thetaf(tosa[,2]), h=3)),2)
      acRW <- round(accuracy(forecast(rwf(tosa[2:2]), h=3)),2)
      acNV <- round(accuracy(forecast(naive(tosa[2:2]), h=3)),2)
      acTT <- round(accuracy(forecast(tbats(tosa[,2],use.parallel=TRUE), h=3)),2)
      #acMF <- accuracy(forecast(meanf(tosa[2:2]), h=6))
      #acCS <- round(accuracy(forecast(splinef(tosa[2:2]), h=3)),2)
      #fin=rbind(acets,acarm,acts,achw,acTH,acRW,acNV,acTT,acCS)
      fin=rbind(acets,acarm,achw,acTH,acRW,acNV,acTT)
      finn=data.frame(fin)
      #finn$Model=c("ETS","ARIMA","StructTS","HoltWinters","ThetaF","RandomWalk","Naive","Tbats","CubicSpline")
      finn$Model=c("ETS","ARIMA","HoltWinters","ThetaF","RandomWalk","Naive","Tbats")
      finn=finn[c("Model","ME","RMSE","MAE","MPE","MAPE","MASE","ACF1")]
      DT::datatable(finn,
                    options = list(rowCallback = JS(
                      paste0('function(row, data) {
                             // Bold cells for the max in the first column
                             if (parseFloat(data[6]) == ',min(finn[,6]),')
                             
                             $("td:eq(6)", row).css("background-color", "#9BF59B");
    }')
    )))
    })
    
    
    ####Download data codes #### 
    output$downloadDataQtr = downloadHandler('ETS.csv', content = function(file) {
      tosa=mydataQtr()
      myet <- forecast(ets(tosa[,2]), h=3)
      write.csv(myet, file)
    })
    output$downloadData1Qtr = downloadHandler('Arima.csv', content = function(file) {
      tosa=mydataQtr()
      myet <- forecast(auto.arima(tosa[2:2]), h=3)
      write.csv(myet, file)
    })
    output$downloadData2Qtr = downloadHandler('StructTS.csv', content = function(file) {
      tosa=mydataQtr()
      myet <- forecast(StructTS(tosa[2:2], "level"), h=3)
      write.csv(myet, file)
    })
    output$downloadData3Qtr = downloadHandler('HoltWinters.csv', content = function(file) {
      tosa=mydataQtr()
      myet <- forecast(HoltWinters(tosa[2:2], gamma=FALSE), h=3)
      write.csv(myet, file)
    })
    output$downloadData4Qtr = downloadHandler('thetaf.csv', content = function(file) {
      tosa=mydataQtr()
      myet <- forecast(thetaf(tosa[,2]), h=3)
      write.csv(myet, file)
    })
    output$downloadData5Qtr = downloadHandler('RandomWalk.csv', content = function(file) {
      tosa=mydataQtr()
      myet <- forecast(rwf(tosa[2:2]), h=3)
      write.csv(myet, file)
    })
    output$downloadData6Qtr = downloadHandler('naive.csv', content = function(file) {
      tosa=mydataQtr()
      myet <- forecast(naive(tosa[2:2]), h=3)
      write.csv(myet, file)
    })
    output$downloadData7Qtr = downloadHandler('tbats.csv', content = function(file) {
      tosa=mydataQtr()
      myet <- forecast(tbats(tosa[,2], use.parallel=TRUE), h=3)
      write.csv(myet, file)
    })
    output$downloadData8Qtr = downloadHandler('splinef.csv', content = function(file) {
      tosa=mydataQtr()
      myet <- forecast(splinef(tosa[2:2]), h=3)
      write.csv(myet, file)
    })
    
    
    
    
    
##################################################################################################################################
##################################################### iForecaster Weekly Tab #######################################################
##################################################################################################################################
    
    ####Observe Codes####
    
    observe({   
      if(!is.null(input$orders_week) ){
        orders_iform <- paste("'",input$orders_week,"'",sep="")
        if (input$orders_week=="All")
        {
          query<-paste("select distinct MainCategory from weeklyData order by MainCategory",sep="")
        }
        else
        {
          query<-paste("select distinct MainCategory from weeklyData where Bucket=",orders_iform," order by MainCategory",sep="")
        }
        
        df<-sqldf(query)
        
        if (nrow(df)>0){
          df1 <- data.frame(MainCategory = c("---All Items---"))
          df<-rbind(df,df1)
          items=c(df)
        }
        else
        {
          items=c("---All Items---")
        }
        
        updateSelectInput(session, "gender_week", choices =items,selected = "---All Items---")
      }
    })
    
    observe({
      if(!is.null(input$orders_week) ){
        orders_iform <- paste("'",input$orders_week,"'",sep="")
        gender_iform <- paste("'",input$gender_week,"'",sep="")
        
        if (input$orders_week=="All")
        {
          query<-paste("select distinct SubCategory from weeklyData where MainCategory=",gender_iform," order by SubCategory",sep="")
        }
        else
        {
          query<-paste("select distinct SubCategory from weeklyData where MainCategory=",gender_iform," and Bucket=", orders_iform," order by SubCategory",sep="")
        }
        
        df<-sqldf(query)
        
        if (nrow(df)>0){
          df1 <- data.frame(SubCategory = c("---All Items---"))
          df<-rbind(df,df1)
          items=c(df)
        }
        else
        {
          items=c("---All Items---")
        }
        updateSelectInput(session, "category_week", choices =items,selected = "---All Items---")
      }
    })
    
    observe({   
      if(!is.null(input$orders_week) ){
        orders_iform <- paste("'",input$orders_week,"'",sep="")
        gender_iform <- paste("'",input$gender_week,"'",sep="")
        category_iform <- paste("'",input$category_week,"'",sep="")
        
        if (input$orders_week=="All")
        {
          
          query<-paste("select distinct Brands from weeklyData where MainCategory=",gender_iform," and SubCategory=", category_iform," order by Brands",sep="")
        }
        else
        {
          
          query<-paste("select distinct Brands from weeklyData where MainCategory=",gender_iform," and SubCategory=", category_iform," and Bucket=", orders_iform," order by Brands",sep="")
        }
        
        df<-sqldf(query)
        
        if (nrow(df)>0)
        {
          df1 <- data.frame(Brands = c("---All Items---"))
          df<-rbind(df,df1)
          items=c(df)
        }
        else
        {
          items=c("---All Items---")
        }
        
        updateSelectInput(session, "brand_week", choices = items,selected = "---All Items---")
      }
    })
    
    observe({
      if(!is.null(input$orders_week) ){
        orders_iform <- paste("'",input$orders_week,"'",sep="")
        gender_iform <- paste("'",input$gender_week,"'",sep="")
        category_iform <- paste("'",input$category_week,"'",sep="")
        color_iform <- paste("'",input$brand_week,"'",sep="")
        
        if (input$orders_week=="All")
        {
          query<-paste("select distinct Color from weeklyData where MainCategory=",gender_iform," and SubCategory=", category_iform," and Brands=", color_iform," order by Color",sep="")
        }
        else
        {
          query<-paste("select distinct Color from weeklyData where MainCategory=",gender_iform," and SubCategory=", category_iform," and Brands=", color_iform," and Bucket=", orders_iform," order by Color",sep="")
        }
        
        df<-sqldf(query)
        if (nrow(df)>0){
          df1 <- data.frame(Color = c("---All Items---"))
          df<-rbind(df,df1)
          items=c(df)
        }
        else
        {
          items=c("---All Items---")
        }
        
        updateSelectInput(session, "color_week", choices =items,selected = "---All Items---" )
      }
    })
    
    observe({
      if(!is.null(input$orders_week) ){
        orders_iform <- paste("'",input$orders_week,"'",sep="")
        gender_iform <- paste("'",input$gender_week,"'",sep="")
        category_iform <- paste("'",input$category_week,"'",sep="")
        color_iform <- paste("'",input$brand_week,"'",sep="")
        size_iform <- paste("'",input$color_week,"'",sep="")
        
        if (input$orders_week=="All")
        {
          query<-paste("select distinct Size from weeklyData where MainCategory=",gender_iform," and SubCategory=", category_iform," and Brands=", color_iform," and Color=", size_iform," order by Size",sep="")
        }
        else
        {
          query<-paste("select distinct Size from weeklyData where MainCategory=",gender_iform," and SubCategory=", category_iform," and Brands=", color_iform," and Color=", size_iform," and Bucket=", orders_iform," order by Size",sep="")
        }
        
        df<-sqldf(query)
        if (nrow(df)>0){
          df1 <- data.frame(Size = c("---All Items---"))
          df<-rbind(df,df1)
          items=c(df)
        }
        else
        {
          items=c("---All Items---")
        } 
        
        updateSelectInput(session, "size_week", choices =items,selected = "---All Items---")
      }
    })
    
    observe({
      if(!is.null(input$orders_week) ){
        orders_iform <- paste("'",input$orders_week,"'",sep="")
        gender_iform <- paste("'",input$gender_week,"'",sep="")
        category_iform <- paste("'",input$category_week,"'",sep="")
        color_iform <- paste("'",input$brand_week,"'",sep="")
        size_iform <- paste("'",input$color_week,"'",sep="")
        description_iform <- paste("'",input$size_week,"'",sep="")
        
        if (input$orders_week=="All")
        {
          query<-paste("select distinct Description from weeklyData where MainCategory=",gender_iform," and SubCategory=", category_iform," and Brands=", color_iform," and Color=", size_iform," and Size=", description_iform," order by Description",sep="")
        }
        else
        {
          query<-paste("select distinct Description from weeklyData where MainCategory=",gender_iform," and SubCategory=", category_iform," and Brands=", color_iform," and Color=", size_iform," and Size=", description_iform," and Bucket=", orders_iform," order by Description",sep="")
        }
        
        df<-sqldf(query)
        if (nrow(df)>0){
          df1 <- data.frame(Description = c("---All Items---"))
          df<-rbind(df,df1)
          items=c(df)
        }
        else
        {
          items=c("---All Items---")
        }
        
        updateSelectInput(session, "description_week", choices = items,selected = "---All Items---")
      }
    })
    
    ####Data Extraction code####
    mydataweek <- reactive({
      
      data1=weeklyData
      
      orders1 <- paste("'",input$orders_week,"'",sep="")
      MainCategory1=paste("'",input$gender_week,"'",sep="")
      SubCategory1=paste("'",input$category_week,"'",sep="")
      Brands1=paste("'",input$brand_week,"'",sep="")
      Color1=paste("'",input$color_week,"'",sep="")
      size1=paste("'",input$size_week,"'",sep="")
      description1=paste("'",input$description_week,"'",sep="")
      
      if (input$orders_week=="All"){
        if (input$gender_week=='---All Items---'){
          query<-paste("select * from data1",sep="")
        }
        else if (input$category_week=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,sep="")
        }
        else if(input$brand_week=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,sep="")
        }  
        else if(input$color_week=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,sep="")
        }
        else if(input$size_week=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,sep="")
        }
        else if(input$description_week=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,sep="")
        }
        else{
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,"and Description=",description1,sep="")
        }
      }
      else{
        if (input$gender_week=='---All Items---'){
          query<-paste("select * from data1 where Bucket=",orders1,sep="")
        }
        else if (input$category_week=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1," and Bucket=", orders1,sep="")
        }
        else if(input$brand_week=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1," and Bucket=", orders1,sep="")
        }  
        else if(input$color_week=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1," and Bucket=", orders1,sep="")
        }
        else if(input$size_week=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1," and Bucket=", orders1,sep="")
        }
        else if(input$description_week=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1," and Bucket=", orders1,sep="")
        }
        else{
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,"and Description=",description1," and Bucket=", orders1,sep="")
        }
      }
      
      data11<-sqldf(query)
      
      n <- colnames(data11[,-1:-8])
      
      data111=data.frame(colSums(data11[,-1:-8],na.rm = T,1))
      names(data111)[1]='Months'
      data111$dates <- n
      data111 <- data111[c(2,1)]
      
    })
    
    getDataset_week <- reactive({
      
      data1=weeklyData
      
      orders1 <- paste("'",input$orders_week,"'",sep="")
      MainCategory1=paste("'",input$gender_week,"'",sep="")
      SubCategory1=paste("'",input$category_week,"'",sep="")
      Brands1=paste("'",input$brand_week,"'",sep="")
      Color1=paste("'",input$color_week,"'",sep="")
      size1=paste("'",input$size_week,"'",sep="")
      description1=paste("'",input$description_week,"'",sep="")
      
      if (input$orders_week=="All"){
        if (input$gender_week=='---All Items---'){
          query<-paste("select * from data1",sep="")
        }
        else if (input$category_week=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,sep="")
        }
        else if(input$brand_week=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,sep="")
        }  
        else if(input$color_week=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,sep="")
        }
        else if(input$size_week=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,sep="")
        }
        else if(input$description_week=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,sep="")
        }
        else{
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,"and Description=",description1,sep="")
        }
      }
      else{
        if (input$gender_week=='---All Items---'){
          query<-paste("select * from data1 where Bucket=",orders1,sep="")
        }
        else if (input$category_week=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1," and Bucket=", orders1,sep="")
        }
        else if(input$brand_week=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1," and Bucket=", orders1,sep="")
        }  
        else if(input$color_week=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1," and Bucket=", orders1,sep="")
        }
        else if(input$size_week=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1," and Bucket=", orders1,sep="")
        }
        else if(input$description_week=='---All Items---'){
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1," and Bucket=", orders1,sep="")
        }
        else{
          query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,"and Description=",description1," and Bucket=", orders1,sep="")
        }
      }
      #query<-paste("select * from weeklyData where MainCategory='Men'",sep="")
      
      df1<-sqldf(query)
      n <- colnames(df1[,-1:-8])
      
      df2=data.frame(colSums(df1[,-1:-8],na.rm = T,1))
      names(df2)[1]='Months'
      df2$dates <- n
      df2 <- df2[c(2,1)]
      
      tosa1=df2
      
      tosa1[,1]=as.Date(tosa1[,1], format = "%d/%m/%Y")
      x1=as.numeric(format(min(tosa1[,1]),'%Y'))
      x2=as.numeric(format(min(tosa1[,1]),'%m'))
      
      y1=as.numeric(format(max(tosa1[,1]),'%Y'))
      y2=as.numeric(format(max(tosa1[,1]),'%m'))
      Vmonth<<-y2
      Vyear<<-y1
      tosa <- ts(tosa1, start=c(x1,x2), end=c(y1, y2), frequency=52) 
      #tosa <- ts(tosa1, start=x1+31/365.25, frequency=365.25/7) 
      
      tosa=tosa[,2]
      
    })
    
    ####Forecast model plot function for ETS#######################
    forecastPlotInputETSweek <- function() {
      
      x <- forecast(stlf(getDataset_week(), method = "ets",level=c(70,80)), h=6)
      
      plot.new()
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
      
    }
    
    output$forecastPlotETSweek <- renderPlot({
      
      forecastPlotInputETSweek()
      
    })
    
    output$ETStabweek <- renderDataTable({
      myet <- data.frame(forecast(stlf(getDataset_week(), method = "ets",level=c(70,80)), h=6))
      #myet <- data.frame(forecast(stlf(tosa, method = "ets",level=c(70,80)), h=6))
      dateV<-paste(Vyear,Vmonth+1,sep = "-")
      xx<-zoo::as.Date(zoo::as.yearmon(dateV, "%Y-%m"))
      week1<-format(as.Date(as.Date(xx,format = "%d/%m/%Y")), "%U")
      year1<-format(as.Date(as.Date(xx,format = "%d/%m/%Y")), "%Y")
      week1<-as.numeric(week1)
      year1<-as.numeric(year1)
      
      names<-c(paste(week1,year1,sep = "-"),paste(week1+1,year1,sep = "-"),paste(week1+2,year1,sep = "-"),paste(week1+3,year1,sep = "-"),paste(week1+4,year1,sep = "-"),paste(week1+5,year1,sep = "-"))
      myet <-round(myet,0)
      myet$week<-names
      myet<-myet[c(6,1,2,3,4,5)]
      #row.names(myet) <- NULL
      
    }, rownames = FALSE )
    
    

    ####Forecast model plot function for ARIMA###############################
    forecastPlotInputARIMAweek<- function() {
      
      x <- forecast(stlf(getDataset_week(), method = "arima",level=c(70,80)), h=6)
      
      plot.new()
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
      
    }
    
    output$forecastPlotARIMAweek<- renderPlot({
      
      forecastPlotInputARIMAweek()
      
    })
    
    output$ARtabweek <- renderDataTable({
      myet <- data.frame(forecast(stlf(getDataset_week(), method = "arima",level=c(70,80)), h=6))
      dateV<-paste(Vyear,Vmonth+1,sep = "-")
      xx<-zoo::as.Date(zoo::as.yearmon(dateV, "%Y-%m"))
      week1<-format(as.Date(as.Date(xx,format = "%d/%m/%Y")), "%U")
      year1<-format(as.Date(as.Date(xx,format = "%d/%m/%Y")), "%Y")
      week1<-as.numeric(week1)
      year1<-as.numeric(year1)
      
      names<-c(paste(week1,year1,sep = "-"),paste(week1+1,year1,sep = "-"),paste(week1+2,year1,sep = "-"),paste(week1+3,year1,sep = "-"),paste(week1+4,year1,sep = "-"),paste(week1+5,year1,sep = "-"))
      myet <-round(myet,0)
      myet$week<-names
      myet<-myet[c(6,1,2,3,4,5)]
      #row.names(myet) <- NULL
      
    }, rownames = FALSE )
    
    
    ####Forecast model plot function for StructTS###############################
    forecastPlotInputStructTSweek<- function() {
      x <- forecast(StructTS(getDataset_week(), "level",level=c(70,80)), h=6)
      
      plot.new()
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
    }
    
    output$forecastPlotStructTSweek<- renderPlot({
      
      forecastPlotInputStructTSweek()        
      
    })
    
    output$STtabweek <- renderDataTable({
      myet <- data.frame(forecast(StructTS(getDataset_week(), "level",level=c(70,80)), h=6))
      dateV<-paste(Vyear,Vmonth+1,sep = "-")
      xx<-zoo::as.Date(zoo::as.yearmon(dateV, "%Y-%m"))
      week1<-format(as.Date(as.Date(xx,format = "%d/%m/%Y")), "%U")
      year1<-format(as.Date(as.Date(xx,format = "%d/%m/%Y")), "%Y")
      week1<-as.numeric(week1)
      year1<-as.numeric(year1)
      
      names<-c(paste(week1,year1,sep = "-"),paste(week1+1,year1,sep = "-"),paste(week1+2,year1,sep = "-"),paste(week1+3,year1,sep = "-"),paste(week1+4,year1,sep = "-"),paste(week1+5,year1,sep = "-"))
      myet <-round(myet,0)
      myet$week<-names
      myet<-myet[c(6,1,2,3,4,5)]
      #row.names(myet) <- NULL
      
    }, rownames = FALSE )
    
    
    
    ####Forecast model plot function for HoltWinters#####################
    forecastPlotInputHoltWintersweek<- function() {
      x <- forecast(HoltWinters(getDataset_week(), gamma=FALSE), h=6,level=c(70,80))
      
      plot.new()
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
    }
    
    output$forecastPlotHoltWintersweek<- renderPlot({
      
      forecastPlotInputHoltWintersweek()
      
    })
    
    
    output$HWtabweek <- renderDataTable({
      myet <- data.frame(forecast(HoltWinters(getDataset_week(), gamma=FALSE), h=6,level=c(70,80)))
      dateV<-paste(Vyear,Vmonth+1,sep = "-")
      xx<-zoo::as.Date(zoo::as.yearmon(dateV, "%Y-%m"))
      week1<-format(as.Date(as.Date(xx,format = "%d/%m/%Y")), "%U")
      year1<-format(as.Date(as.Date(xx,format = "%d/%m/%Y")), "%Y")
      week1<-as.numeric(week1)
      year1<-as.numeric(year1)
      
      names<-c(paste(week1,year1,sep = "-"),paste(week1+1,year1,sep = "-"),paste(week1+2,year1,sep = "-"),paste(week1+3,year1,sep = "-"),paste(week1+4,year1,sep = "-"),paste(week1+5,year1,sep = "-"))
      myet <-round(myet,0)
      myet$week<-names
      myet<-myet[c(6,1,2,3,4,5)]
      #row.names(myet) <- NULL
      
    }, rownames = FALSE )
    
    
    ####Forecast model plot function for thetaf###########################
    forecastPlotInputthetafweek<- function() {
      x <- forecast(thetaf(getDataset_week(),level=c(70,80)), h=6)
      
      plot.new()
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
    }
    
    output$forecastPlotthetafweek<- renderPlot({
      
      forecastPlotInputthetafweek()
      
    })
    
    output$TFtabweek <- renderDataTable({
      myet <- data.frame(forecast(thetaf(getDataset_week(),level=c(70,80)), h=6))
      dateV<-paste(Vyear,Vmonth+1,sep = "-")
      xx<-zoo::as.Date(zoo::as.yearmon(dateV, "%Y-%m"))
      week1<-format(as.Date(as.Date(xx,format = "%d/%m/%Y")), "%U")
      year1<-format(as.Date(as.Date(xx,format = "%d/%m/%Y")), "%Y")
      week1<-as.numeric(week1)
      year1<-as.numeric(year1)
      
      names<-c(paste(week1,year1,sep = "-"),paste(week1+1,year1,sep = "-"),paste(week1+2,year1,sep = "-"),paste(week1+3,year1,sep = "-"),paste(week1+4,year1,sep = "-"),paste(week1+5,year1,sep = "-"))
      myet <-round(myet,0)
      myet$week<-names
      myet<-myet[c(6,1,2,3,4,5)]
      #row.names(myet) <- NULL
      
    }, rownames = FALSE )
    
    
    ####Forecast model plot function for rwf#######################
    forecastPlotInputrwfweek<- function() {
      x <- forecast(stlf(getDataset_week(), method = "rwdrift",level=c(70,80)), h=6)
      plot.new()
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
    }
    
    output$forecastPlotrwfweek<- renderPlot({
      forecastPlotInputrwfweek()
    })
    
    output$RFtabweek <- renderDataTable({
      myet <- data.frame(forecast(stlf(getDataset_week(), method = "rwdrift",level=c(70,80)), h=6))
      dateV<-paste(Vyear,Vmonth+1,sep = "-")
      xx<-zoo::as.Date(zoo::as.yearmon(dateV, "%Y-%m"))
      week1<-format(as.Date(as.Date(xx,format = "%d/%m/%Y")), "%U")
      year1<-format(as.Date(as.Date(xx,format = "%d/%m/%Y")), "%Y")
      week1<-as.numeric(week1)
      year1<-as.numeric(year1)
      
      names<-c(paste(week1,year1,sep = "-"),paste(week1+1,year1,sep = "-"),paste(week1+2,year1,sep = "-"),paste(week1+3,year1,sep = "-"),paste(week1+4,year1,sep = "-"),paste(week1+5,year1,sep = "-"))
      myet <-round(myet,0)
      myet$week<-names
      myet<-myet[c(6,1,2,3,4,5)]
      #row.names(myet) <- NULL
      
    }, rownames = FALSE )
    
    
    ####Forecast model plot function for naive#####################
    forecastPlotInputnaiveweek<- function() {
      x <- forecast(stlf(getDataset_week(), method = "naive",level=c(70,80)), h=6)
      
      plot.new()
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
    }
    
    output$forecastPlotnaiveweek<- renderPlot({
      forecastPlotInputnaiveweek()
    })
    
    output$NAtabweek <- renderDataTable({
      myet <- data.frame(forecast(stlf(getDataset_week(), method = "naive",level=c(70,80)), h=6))
      dateV<-paste(Vyear,Vmonth+1,sep = "-")
      xx<-zoo::as.Date(zoo::as.yearmon(dateV, "%Y-%m"))
      week1<-format(as.Date(as.Date(xx,format = "%d/%m/%Y")), "%U")
      year1<-format(as.Date(as.Date(xx,format = "%d/%m/%Y")), "%Y")
      week1<-as.numeric(week1)
      year1<-as.numeric(year1)
      
      names<-c(paste(week1,year1,sep = "-"),paste(week1+1,year1,sep = "-"),paste(week1+2,year1,sep = "-"),paste(week1+3,year1,sep = "-"),paste(week1+4,year1,sep = "-"),paste(week1+5,year1,sep = "-"))
      myet <-round(myet,0)
      myet$week<-names
      myet<-myet[c(6,1,2,3,4,5)]
      #row.names(myet) <- NULL
      
    }, rownames = FALSE )
    
    
    ####Forecast model plot function for tbats#######################
    forecastPlotInputtbatsweek<- function() {
      x <- forecast(tbats(getDataset_week(), use.parallel=TRUE), h=6,level=c(70,80))
      plot.new()
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
    }
    
    output$forecastPlottbatsweek<- renderPlot({
      
      forecastPlotInputtbatsweek()
    })
    
    output$TBtabweek <- renderDataTable({
      myet <- data.frame(forecast(tbats(getDataset_week(), use.parallel=TRUE), h=6,level=c(70,80)))
      dateV<-paste(Vyear,Vmonth+1,sep = "-")
      xx<-zoo::as.Date(zoo::as.yearmon(dateV, "%Y-%m"))
      week1<-format(as.Date(as.Date(xx,format = "%d/%m/%Y")), "%U")
      year1<-format(as.Date(as.Date(xx,format = "%d/%m/%Y")), "%Y")
      week1<-as.numeric(week1)
      year1<-as.numeric(year1)
      
      names<-c(paste(week1,year1,sep = "-"),paste(week1+1,year1,sep = "-"),paste(week1+2,year1,sep = "-"),paste(week1+3,year1,sep = "-"),paste(week1+4,year1,sep = "-"),paste(week1+5,year1,sep = "-"))
      myet <-round(myet,0)
      myet$week<-names
      myet<-myet[c(6,1,2,3,4,5)]
      #row.names(myet) <- NULL
      
    }, rownames = FALSE )
    
    
    
    ####Forecast model plot function for meanf#######################
    forecastPlotInputmeanfweek<- function() {
      x <- forecast(meanf(getDataset_week(),level=c(70,80)), h=6)
      
      plot.new()
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
    }
    
    output$forecastPlotmeanfweek<- renderPlot({
      forecastPlotInputmeanfweek()
    })
    
    output$MFtabweek <- renderDataTable({
      myet <- data.frame(forecast(meanf(getDataset_week(),level=c(70,80)), h=6))
      dateV<-paste(Vyear,Vmonth+1,sep = "-")
      xx<-zoo::as.Date(zoo::as.yearmon(dateV, "%Y-%m"))
      week1<-format(as.Date(as.Date(xx,format = "%d/%m/%Y")), "%U")
      year1<-format(as.Date(as.Date(xx,format = "%d/%m/%Y")), "%Y")
      week1<-as.numeric(week1)
      year1<-as.numeric(year1)
      
      names<-c(paste(week1,year1,sep = "-"),paste(week1+1,year1,sep = "-"),paste(week1+2,year1,sep = "-"),paste(week1+3,year1,sep = "-"),paste(week1+4,year1,sep = "-"),paste(week1+5,year1,sep = "-"))
      myet <-round(myet,0)
      myet$week<-names
      myet<-myet[c(6,1,2,3,4,5)]
      #row.names(myet) <- NULL
      
    }, rownames = FALSE )
    
    
    
    ####Forecast model plot function for splinef#####################
    forecastPlotInputsplinefweek<- function() {
      x <- forecast(splinef(getDataset_week(),level=c(70,80)), h=6)
      
      plot.new()
      plot(x, flty = 3, axes = TRUE, ylim=c(0,max(x$x)))
      lines(fitted(x),col='blue')
      tosa=mydata()
      a <- seq(as.Date(tosa[,1], format = "%d/%m/%y")[1] - 0, 
               by = "months", length = length(date) + 11)
      axis(1, at = as.numeric(a)/12 + 1970, 
           labels = format(a, format = "%d/%m/%Y"), 
           cex.axis = 0.9)
      axis(2, cex.axis = 0.1, las = 0)
      
      
      box()
      legend('topright', c("Actual","Forecasted"),col=c('black','blue'), lty=1, cex=.65)
    }
    
    output$forecastPlotsplinefweek<- renderPlot({
      
      forecastPlotInputsplinefweek()
      
    })
    output$SFtabweek <- renderDataTable({
      myet <- data.frame(forecast(splinef(getDataset_week(),level=c(70,80)), h=6))
      dateV<-paste(Vyear,Vmonth+1,sep = "-")
      xx<-zoo::as.Date(zoo::as.yearmon(dateV, "%Y-%m"))
      week1<-format(as.Date(as.Date(xx,format = "%d/%m/%Y")), "%U")
      year1<-format(as.Date(as.Date(xx,format = "%d/%m/%Y")), "%Y")
      week1<-as.numeric(week1)
      year1<-as.numeric(year1)
      
      names<-c(paste(week1,year1,sep = "-"),paste(week1+1,year1,sep = "-"),paste(week1+2,year1,sep = "-"),paste(week1+3,year1,sep = "-"),paste(week1+4,year1,sep = "-"),paste(week1+5,year1,sep = "-"))
      myet <-round(myet,0)
      myet$week<-names
      myet<-myet[c(6,1,2,3,4,5)]
      #row.names(myet) <- NULL
      
    }, rownames = FALSE )
    
    
    ####Each Forecasting Models Accuracy###############################################
    output$ACCweek <- renderDataTable({
      tosa=mydataweek()
      acets <- round(accuracy(forecast((tosa[,2]), h=6)),2)
      acarm <- round(accuracy(forecast(auto.arima(tosa[2:2]), h=6)),2)
      #acts <- round(accuracy(forecast(StructTS(tosa[2:2],"level"), h=6)),2)
      achw <- round(accuracy(forecast(HoltWinters(tosa[2:2], gamma=FALSE), h=6)),2)
      acTH <- round(accuracy(forecast(thetaf(tosa[,2]), h=6)),2)
      acRW <- round(accuracy(forecast(rwf(tosa[2:2]), h=6)),2)
      acNV <- round(accuracy(forecast(naive(tosa[2:2]), h=6)),2)
      acTT <- round(accuracy(forecast(tbats(tosa[,2],use.parallel=TRUE), h=6)),2)
      #acMF <- accuracy(forecast(meanf(tosa[2:2]), h=6))
      #acCS <- round(accuracy(forecast(splinef(tosa[2:2]), h=6)),2)
      #fin=rbind(acets,acarm,acts,achw,acTH,acRW,acNV,acTT,acCS)
      fin=rbind(acets,acarm,achw,acTH,acRW,acNV,acTT)
      finn=data.frame(fin)
      #finn$Model=c("ETS","ARIMA","StructTS","HoltWinters","ThetaF","RandomWalk","Naive","Tbats","CubicSpline")
      finn$Model=c("ETS","ARIMA","HoltWinters","ThetaF","RandomWalk","Naive","Tbats")
      finn=finn[c("Model","ME","RMSE","MAE","MPE","MAPE","MASE","ACF1")]
      DT::datatable(finn,
                    options = list(rowCallback = JS(
                      paste0('function(row, data) {
                             // Bold cells for the max in the first column
                             if (parseFloat(data[6]) == ',min(finn[,6]),')
                             
                             $("td:eq(6)", row).css("background-color", "#9BF59B");
    }')
    )))
  })
    
    
    ####Download data codes #### 
    output$downloadDataweek = downloadHandler('ETS.csv', content = function(file) {
      tosa=mydataweek()
      myet <- forecast(ets(tosa[,2]), h=6)
      write.csv(myet, file)
    })
    output$downloadData1week = downloadHandler('Arima.csv', content = function(file) {
      tosa=mydataweek()
      myet <- forecast(auto.arima(tosa[2:2]), h=6)
      write.csv(myet, file)
    })
    output$downloadData2week = downloadHandler('StructTS.csv', content = function(file) {
      tosa=mydataweek()
      myet <- forecast(StructTS(tosa[2:2], "level"), h=6)
      write.csv(myet, file)
    })
    output$downloadData3week = downloadHandler('HoltWinters.csv', content = function(file) {
      tosa=mydataweek()
      myet <- forecast(HoltWinters(tosa[2:2], gamma=FALSE), h=6)
      write.csv(myet, file)
    })
    output$downloadData4week = downloadHandler('thetaf.csv', content = function(file) {
      tosa=mydataweek()
      myet <- forecast(thetaf(tosa[,2]), h=6)
      write.csv(myet, file)
    })
    output$downloadData5week = downloadHandler('RandomWalk.csv', content = function(file) {
      tosa=mydataweek()
      myet <- forecast(rwf(tosa[2:2]), h=6)
      write.csv(myet, file)
    })
    output$downloadData6week = downloadHandler('naive.csv', content = function(file) {
      tosa=mydataweek()
      myet <- forecast(naive(tosa[2:2]), h=6)
      write.csv(myet, file)
    })
    output$downloadData7week = downloadHandler('tbats.csv', content = function(file) {
      tosa=mydataweek()
      myet <- forecast(tbats(tosa[,2], use.parallel=TRUE), h=6)
      write.csv(myet, file)
    })
    output$downloadData8week = downloadHandler('splinef.csv', content = function(file) {
      tosa=mydataweek()
      myet <- forecast(splinef(tosa[2:2]), h=6)
      write.csv(myet, file)
    })
    
    
    
    
##################################################################################################################################
##################################################### Weekly Forecasting Tab  #######################################################
##################################################################################################################################
    
    #### Oberve Coding######
    observe({
      if(!is.null(input$gender_week1) ){
        gender_exp1 <- paste("'",input$gender_week1,"'",sep="")
        query<-paste("select distinct SubCategory from weeklyforecast where MainCategory=",gender_exp1," order by SubCategory",sep="")
        df<-sqldf(query)
        
        if (nrow(df)>0){
          df1 <- data.frame(SubCategory = c("---All Items---"))
          df<-rbind(df,df1)
          items=c(df)
        }
        else
        {
          items=c("---All Items---")
        }
        
        updateSelectInput(session, "category_week1", choices =items,selected = "---All Items---")
      }
    })
    
    observe({
      if(!is.null(input$gender_week1) ){
        
        gender_exp1 <- paste("'",input$gender_week1,"'",sep="")
        category_exp1 <- paste("'",input$category_week1,"'",sep="")
        
        query<-paste("select distinct Brands from weeklyforecast where MainCategory=",gender_exp1," and SubCategory=", category_exp1," order by Brands",sep="")
        df<-sqldf(query)
        if (nrow(df)>0){
          df1 <- data.frame(Brands = c("---All Items---"))
          df<-rbind(df,df1)
          items=c(df)
        }
        else
        {
          items=c("---All Items---")
        }
        
        updateSelectInput(session, "brand_week1", choices = items,selected = "---All Items---")
      }
    })
    
    observe({
      
      if(!is.null(input$gender_week1) ){
        
        gender_exp1 <- paste("'",input$gender_week1,"'",sep="")
        category_exp1 <- paste("'",input$category_week1,"'",sep="")
        color_exp1 <- paste("'",input$brand_week1,"'",sep="")
        
        query<-paste("select distinct Color from weeklyforecast where MainCategory=",gender_exp1," and SubCategory=", category_exp1," and Brands=", color_exp1," order by Color",sep="")
        df<-sqldf(query)
        
        if (nrow(df)>0){
          df1 <- data.frame(Color = c("---All Items---"))
          df<-rbind(df,df1)
          items=c(df)
        }
        else
        {
          items=c("---All Items---")
        } 
        
        updateSelectInput(session, "color_week1", choices =items,selected = "---All Items---" )
      }
    })
    
    observe({
      if(!is.null(input$gender_week1) ){
        
        gender_exp1 <- paste("'",input$gender_week1,"'",sep="")
        category_exp1 <- paste("'",input$category_week1,"'",sep="")
        color_exp1 <- paste("'",input$brand_week1,"'",sep="")
        size_exp1 <- paste("'",input$color_week1,"'",sep="")
        
        query<-paste("select distinct Size from weeklyforecast where MainCategory=",gender_exp1," and SubCategory=", category_exp1," and Brands=", color_exp1," and Color=", size_exp1," order by Size",sep="")
        df<-sqldf(query)
        
        if (nrow(df)>0){
          df1 <- data.frame(Size = c("---All Items---"))
          df<-rbind(df,df1)
          items=c(df)
        }
        else
        {
          items=c("---All Items---")
        } 
        
        updateSelectInput(session, "size_week1", choices =items,selected = "---All Items---")
      }
    })
    
    observe({
      if(!is.null(input$gender_week1) ){
        
        gender_exp1 <- paste("'",input$gender_week1,"'",sep="")
        category_exp1 <- paste("'",input$category_week1,"'",sep="")
        color_exp1 <- paste("'",input$brand_week1,"'",sep="")
        size_exp1 <- paste("'",input$color_week1,"'",sep="")
        description_exp1 <- paste("'",input$size_week1,"'",sep="")
        
        query<-paste("select distinct Description from weeklyforecast where MainCategory=",gender_exp1," and SubCategory=", category_exp1," and Brands=", color_exp1," and Color=", size_exp1," and Size=", description_exp1," order by Description",sep="")
        df<-sqldf(query)
        
        if (nrow(df)>0){
          df1 <- data.frame(Description = c("---All Items---"))
          df<-rbind(df,df1)
          items=c(df)
        }
        else
        {
          items=c("---All Items---")
        } 
        
        updateSelectInput(session, "description_week1", choices = items,selected = "---All Items---")
      }
    })
    
    
    #### Main Panel Code ##################    
    output$weekly2 <- renderChart2({
      data1=weeklyforecast 
      data_exp=weeklyfitted
      #data_ratio=ratio
      
      MainCategory1=paste("'",input$gender_week1,"'",sep="")
      SubCategory1=paste("'",input$category_week1,"'",sep="")
      Brands1=paste("'",input$brand_week1,"'",sep="")
      Color1=paste("'",input$color_week1,"'",sep="")
      size1=paste("'",input$size_week1,"'",sep="")
      description1=paste("'",input$description_week1,"'",sep="")
      
      if (input$gender_week1=='---All Items---'){
        query<-paste("select * from data1",sep="")
        query_exp<-paste("select * from data_exp",sep="")
        #query_ratio<-paste("select * from data_ratio",sep="")
      }
      else if (input$category_week1=='---All Items---'){
        query<-paste("select * from data1 where MainCategory=",MainCategory1,sep="")
        query_exp<-paste("select * from data_exp where MainCategory=",MainCategory1,sep="")
        #query_ratio<-paste("select * from data_ratio where MainCategory=",MainCategory1,sep="")
      }
      else if(input$brand_week1=='---All Items---'){
        query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,sep="")
        query_exp<-paste("select * from data_exp where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,sep="")
        #query_ratio<-paste("select * from data_ratio where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,sep="")
      }  
      else if(input$color_week1=='---All Items---'){
        query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,sep="")
        query_exp<-paste("select * from data_exp where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,sep="")
        #query_ratio<-paste("select * from data_ratio where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,sep="")
      }
      else if(input$size_week1=='---All Items---'){
        query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,sep="")
        query_exp<-paste("select * from data_exp where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,sep="")
        #query_ratio<-paste("select * from data_ratio where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,sep="")    
      }
      else if(input$description_week1=='---All Items---'){
        query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,sep="")
        query_exp<-paste("select * from data_exp where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,sep="")
        #query_ratio<-paste("select * from data_ratio where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,sep="")
      }
      else{
        query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,"and Description=",description1,sep="")
        query_exp<-paste("select * from data_exp where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,"and Description=",description1,sep="")
        #query_ratio<-paste("select * from data_ratio where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,"and Description=",description1,sep="")
      }
      
      #print(query)
      #query<-paste("select * from data1 where MainCategory=","'Men'",sep="")
      #query_exp<-paste("select * from data_exp where MainCategory=","'Men'",sep="")
      #query_ratio<-paste("select * from data_ratio where MainCategory=","'Men Footwear'","and SubCategory=","'Casual'"," and Brands=","'Scott'","and Color=","'Black'","and Size=","'11'"," and Description=","'Wide'",sep="")
      
      Forecast_data<-sqldf(query)
      Fitted_data<-sqldf(query_exp)
      #ratio_data<-sqldf(query_ratio)
      
      n <- colnames(Forecast_data[,-1:-7])
      Forecast_data1=data.frame(colSums(Forecast_data[,-1:-7],na.rm = T,1))
      names(Forecast_data1)[1]='Quantity'
      Forecast_data1$ID <- n
      Forecast_data1$Status<-"Forecast"
      Forecast_data1$Status[1:(nrow(Forecast_data1)-6)]<-"Actual"
      Forecast_data1$ID=as.Date(Forecast_data1$ID,format='%d/%m/%Y')
      
      n1 <- colnames(Fitted_data[,-1:-7])
      Fitted_data1=data.frame(colSums(Fitted_data[,-1:-7],na.rm = T,1))
      names(Fitted_data1)[1]='Quantity'
      Fitted_data1$ID <- n1
      Fitted_data1$Status<-"Fitted"
      Fitted_data1$ID=as.Date(Fitted_data1$ID,format='%d/%m/%Y')
      
      finaldf<-rbind(Forecast_data1,Fitted_data1)
      
      # Ploting Output
      finaldata = transform(finaldf, dat2 = as.numeric(as.POSIXct(ID))*1000)
      h1 <- Highcharts$new()
      h1 <- hPlot(Quantity ~ dat2, data = finaldata, 
                  group = 'Status', 
                  type = "line", 
                  radius=6
      )
      h1$xAxis(type = 'datetime', tickInterval= 7 * 24 * 365, labels = list(
        format = '{value:%b-%Y}'  
      ))
      h1$yAxis(title = list(text = "Quantity"),
        min = 0 
      )
      
      h1$colors('rgba(236, 58, 62, 1)', 'rgba(0, 154, 217, 1)', 'rgba(88, 189, 66, 1)')
      h1$plotOptions(line = list(marker = list(symbol = 'circle')))
      
      
      s="Forecasting- "
      if (input$gender_week1 != "---All Items---")
      {
        s=paste(s,input$gender_week1,sep = " ")
      }
      if (input$category_week1 != "---All Items---")
      {
        s=paste(s,input$category_week1,sep = ", ")
      }
      if (input$brand_week1 != "---All Items---")
      {
        s=paste(s,input$brand_week1,sep = ", ")
      }
      if (input$color_week1 != "---All Items---")
      {
        s=paste(s,input$color_week1,sep = ", ")
      }
      if (input$size_week1 != "---All Items---")
      {
        s=paste(s,input$size_week1,sep = ", ")
      }
      if (input$description_week1 != "---All Items---")
      {
        s=paste(s,input$description_week1,sep = ", ")
      }
      
      h1$title(text = s)
      h1
    })
    
    #### download the filtered data ####
    output$downloadData9_week1 = downloadHandler('weeklyforecastData.csv', content = function(file) {
      # data1=forecastData      
      # MainCategory1=paste("'",input$gender,"'",sep="")
      # SubCategory1=paste("'",input$category,"'",sep="")
      # Brands1=paste("'",input$brand,"'",sep="")
      # Color1=paste("'",input$color,"'",sep="")
      # size1=paste("'",input$size,"'",sep="")
      # description1=paste("'",input$description,"'",sep="")
      # 
      # if (input$gender=='---All Items---'){
      #   query<-paste("select * from data1",sep="")
      # }
      # else if (input$category=='---All Items---'){
      #   query<-paste("select * from data1 where MainCategory=",MainCategory1,sep="")
      # }
      # else if(input$brand=='---All Items---'){
      #   query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,sep="")
      # }  
      # else if(input$color=='---All Items---'){
      #   query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,sep="")
      # }
      # else if(input$size=='---All Items---'){
      #   query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,sep="")
      # }
      # else if(input$description=='---All Items---'){
      #   query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,sep="")
      # }
      # else{
      #   query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,"and Description=",description1,sep="")
      #   
      # }
      # 
      # 
      # 
      # data_color<-sqldf(query)
      # 
      # n <- colnames(data_color[,-1:-6])
      # #n<-n[1:(length(n)-2)]
      # 
      # finaldf=data.frame(colSums(data_color[,-1:-6],na.rm = T,1))
      # #finaldf=data.frame(finaldf[1:(nrow(finaldf)-2),])
      # names(finaldf)[1]='TotalOrders'
      # #finaldf$ID <- seq.int(nrow(finaldf))
      # finaldf$ID <- n
      # myet<-tail(finaldf,6)
      myet<-weeklyforecast
      ActualNames=names(myet[,-c(1:7)])
      newcolumn<-c(names(myet[,c(1:7)]),as.character(format(as.Date(as.Date(ActualNames,format = "%d/%m/%Y")), "%U-%Y")))
      newcolumn <- paste(" ",newcolumn, sep="")
      names(myet)<-newcolumn
      
      write.csv(myet, row.names = FALSE,file)
    })
    
    
    
    #### Data Table screen####
    output$ratio_week1 <- renderDataTable({
      
      data1=weeklyforecast 
      MainCategory1=paste("'",input$gender_week1,"'",sep="")
      SubCategory1=paste("'",input$category_week1,"'",sep="")
      Brands1=paste("'",input$brand_week1,"'",sep="")
      Color1=paste("'",input$color_week1,"'",sep="")
      size1=paste("'",input$size_week1,"'",sep="")
      description1=paste("'",input$description_week1,"'",sep="")
      
      if (input$gender_week1=='---All Items---'){
        query<-paste("select * from data1",sep="")
      }
      else if (input$category_week1=='---All Items---'){
        query<-paste("select * from data1 where MainCategory=",MainCategory1,sep="")
      }
      else if(input$brand_week1=='---All Items---'){
        query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,sep="")
      }  
      else if(input$color_week1=='---All Items---'){
        query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,sep="")
      }
      else if(input$size_week1=='---All Items---'){
        query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,sep="")
      }
      else if(input$description_week1=='---All Items---'){
        query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,sep="")
      }
      else{
        query<-paste("select * from data1 where MainCategory=",MainCategory1,"and SubCategory=",SubCategory1,"and Brands=",Brands1,"and Color=",Color1,"and Size=",size1,"and Description=",description1,sep="")
      }
      
      #query<-paste("select * from data1 where MainCategory=","'Men'","and SubCategory=","'Casual'"," and Brands=","'Scott'","and Color=","'Black'","and Size=","'11'"," and Description=","'Wide'",sep="")
      
      Forecast_data<-sqldf(query)
      ActualNames=names(Forecast_data)
      newcolumn<-format(as.Date(as.Date(ActualNames,format = "%d/%m/%Y")), "%U-%Y")
      names(Forecast_data)<-newcolumn
      
      Forecast_data<-Forecast_data[,c((ncol(Forecast_data)-5):ncol(Forecast_data))]
      nameofcolumn<-names(Forecast_data)
      Forecast_data1=(data.frame(colSums(Forecast_data)))
      Forecast_data1$week<-nameofcolumn
      Forecast_data1<-Forecast_data1[c(2,1)]
      #Forecast_data1=(data.frame(colSums(Forecast_data)))
      names(Forecast_data1)<-c('Week','Quantity')
      #names(Forecast_data1)[1]='Total Orders'
      row.names(Forecast_data1) <- NULL 
      Forecast_data1=Forecast_data1
      #
      
      #newcolumn<-format(as.Date(as.Date(ActualNames,format = "%d/%m/%Y")), "%b-%Y")
      #names(Forecast_data1)<-newcolumn
      
    }, options = list(pageLength = 6) )
    
    
    
  
    
##################################################################################################################################
########################################################## Exception Tab #########################################################
##################################################################################################################################
    
    #### Main Coding Section ####        
    exceptions <- reactive({
      
      inFile <- input$file1
      
      if (!is.null(inFile)){
        read.csv(inFile$datapath, check.names = FALSE,header=TRUE)
      }
      else {
        exception
      }
    })
    
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          
          th(colspan  = 6, '----------------------------------------- List of SKUs ----------------------------------------- '),
          th(colspan = 6, '--- Forecasted 12 Months Exceptions ---')
        ),
        tr(
          lapply(rep(c('SKUs', 'MainCategory', 'SubCategory','Brands','Color','Width','Size','1','2','3','4','5','6','7','8','9','10','11','12')), th)
        )
      )
    ))
    
    
    output$contents <- renderDataTable({
      # colnames(mydata()) <- c("Date", "Var1")
      #datatable(exceptions(),caption = ('Column 1 to 6 represents Forecasted 6 Months'), rownames = FALSE)
      #datatable(exceptions(),container = sketch, rownames = FALSE)
      datatable(exceptions(),
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: right;',
        '', htmltools::em('----Forecasted 12 Months Exceptions----')
      )
      )
      
    })
    
    
    
    
    observeEvent(input$runif, {
      inFile <- input$file1
      if (!is.null(inFile)){
        
        showModal(modalDialog(
          title = "Warning message",
          "Processing has been started ! Please wait till next prompt"
        ))

        #### Forecated File ####
        outputfile=read.csv("Forecast_Without_Exception.csv",check.names = FALSE,header=TRUE)
        #outputfile$SKUs=NULL
        #### Exception Calculation ####
        inFile <- input$file1
        
        if (!is.null(inFile)){
          exception<<-read.csv(inFile$datapath, check.names = FALSE,header=TRUE)
        }
        else {
          exception<<-read.csv("Exception.csv")
        }
        
        #Merge exception with Forecast data
        mergefile<-merge(outputfile, exception,by=c("SKUs","MainCategory","SubCategory","Brands","Color","Description","Size"),all.x = TRUE)
        # mergefile$X1<-as.character(mergefile$X1)  
        # mergefile$X2<-as.character(mergefile$X2)
        # mergefile$X3<-as.character(mergefile$X3)
        # mergefile$X4<-as.character(mergefile$X4)
        # mergefile$X5<-as.character(mergefile$X5)
        # mergefile$X6<-as.character(mergefile$X6)
        # mergefile$X7<-as.character(mergefile$X7)
        # mergefile$X8<-as.character(mergefile$X8)
        # mergefile$X9<-as.character(mergefile$X9)
        # mergefile$X10<-as.character(mergefile$X10)
        # mergefile$X11<-as.character(mergefile$X11)
        # mergefile$X12<-as.character(mergefile$X12)
        
        mergefile[is.na(mergefile)]<-0   #Replacing Na with 0
        
        #df1 <- mergefile[,-c(1:33,40:45)]
        df1 <- mergefile[,-c(1:(ncol(mergefile)-24),(ncol(mergefile)-11):ncol(mergefile))]
        
        #df2 <- mergefile[,-c(1:39)]
        df2 <- mergefile[,-c(1:(ncol(mergefile)-12))]
        
        df3<-data.frame(
          Map(function(x,y) if(all(is.numeric(x),is.numeric(y))) x +(x*y) else x, df1, df2)
        )
        
        df3<-round(df3,digits = 0)
        mergefile<-mergefile[,c(1:(ncol(mergefile)-24))]
        mergefile<-cbind(mergefile,df3)                
        
        names(mergefile)=colnames(outputfile)
        
        forecastData <<-  mergefile
        
        write.csv(forecastData,"Forecast_New.csv",row.names = FALSE)
        write.csv(exception,"Exception.csv",row.names = FALSE)
        
        showModal(modalDialog(
          title = "Important message",
          "Data processed sucessfully"
        ))
      }
      else {
        showModal(modalDialog(
          title = "Aleart message",
          "No file selected"
        ))
        return()
      }
    })
    
    

    
##################################################################################################################################
########################################################## Inventory Tab #########################################################
##################################################################################################################################
    
    #### Main Coding Section ####        
    output$downloadstock = downloadHandler('Inventory.csv', content = function(file) {
      
      write.csv(stock,row.names = FALSE, file)
    })
    output$contentsstock <- renderDataTable({
      # colnames(mydata()) <- c("Date", "Var1")
      #datatable(exceptions(),caption = ('Column 1 to 6 represents Forecasted 6 Months'), rownames = FALSE)
      #datatable(exceptions(),container = sketch, rownames = FALSE)
      DT::datatable(stock,
                    options = list(rowCallback = JS(
                      paste0('function(row, data) {
                             // Bold cells for the max in the first column
                             if (parseFloat(data[2]) <  ',0,')
                             $("td:eq(2)", row).css("background-color", "#FF0000");
                             if (parseFloat(data[3]) <  ',0,')
                             $("td:eq(3)", row).css("background-color", "#FF0000");
                             if (parseFloat(data[4]) <  ',0,')
                             $("td:eq(4)", row).css("background-color", "#FF0000");
                             if (parseFloat(data[5]) <  ',0,')
                             $("td:eq(5)", row).css("background-color", "#FF0000");
                             if (parseFloat(data[6]) <  ',0,')
                             $("td:eq(6)", row).css("background-color", "#FF0000");
                             if (parseFloat(data[7]) <  ',0,')
                             $("td:eq(7)", row).css("background-color", "#FF0000");
                             
    }')
    )))
    })
  
  
  })    
})