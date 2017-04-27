## APP ##

#### Install Libraries ####
packages.used <- c("shiny","shinydashboard","plotly","plyr","dplyr","lazyeval","ggplot2","reshape2")

packages.needed <- setdiff(packages.used, 
                           intersect(installed.packages()[,1], 
                                     packages.used))

if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE,
                   repos='http://cran.us.r-project.org')
}

#### Library ####
library(shiny)
library(shinydashboard)
library(plotly)
library(plyr)
library(dplyr)
library(lazyeval)
library(ggplot2)
library(reshape2)

#### Source helper functions ####
load("../output/cleaned_data.Rdata")
source("../lib/graph_helper.R")
source("../lib/ds_helper.R")

#### Header of the Dashboard ####
header <- dashboardHeader(
  title = c("Survive or Not","a")
  
)

#### Sidebar ####
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("APP Overview",tabName = "Overview"),
    menuItem("Map",tabName = "Map"),
    menuItem("Graph",tabName = "Graph"),
    menuItem("DS",tabName = "DS")
  )
)

#### Body of the Dashboard ####
body <- dashboardBody(
  
  tabItems(
    tabItem(tabName = "Overview",
            mainPanel(
              "In this app, we are going to ...."
            )),
    
    tabItem(tabName = "Map",
            mainPanel(
              "zuotu"
            )),
    
    tabItem(tabName = "Graph",
            
            
            fluidRow(
              tabBox(
                title = "",
                width = 12,
                height = 550,
                
                ## First Graph
                tabPanel(
                  title = "H1B Trend",
                  column(width=9,
                         plotlyOutput("g1")),
                  
                  column(width = 3,
                         selectInput("state1",
                                     label = h3("State"),
                                     choices= c("ALL",sort(as.character(unique(h1b$STATE)))),
                                     selected = "NEW YORK"
                         )
                         
                  )
                ),
                
                ## Second Graph
                tabPanel(
                  title = "Top Employor",
                  column(width=10,
                         plotlyOutput("g2")),
                  
                  column(width = 2,
                         radioButtons("type2", label = h3("Stats Type"),
                                      choices = list("Total Number"="Total Number",
                                                     "Percentage Share"="Percentage Share"),
                                      inline = F),
                         selectInput("year2",
                                     label = h3("Year"),
                                     choices= list("ALL"="ALL",
                                                   "2011"=2011,
                                                   "2012"=2012,
                                                   "2013"=2013,
                                                   "2014"=2014,
                                                   "2015"=2015,
                                                   "2016"=2016)
                         ),
                         selectInput("state2",
                                     label = h3("State"),
                                     choices= c("ALL",sort(as.character(unique(h1b$STATE)))),
                                     selected = "NEW YORK"
                         )
                         
                         
                  )
                ),
                
                ## Third Graph
                tabPanel(
                  title = "Top Job Title",
                  column(width=10,
                         plotlyOutput("g3")),
                  
                  column(width = 2,
                         radioButtons("type3", label = h3("Stats Type"),
                                      choices = list("Total Number"="Total Number",
                                                     "Percentage Share"="Percentage Share"),
                                      inline = F),
                         selectInput("year3",
                                     label = h3("Year"),
                                     choices= list("ALL"="ALL",
                                                   "2011"=2011,
                                                   "2012"=2012,
                                                   "2013"=2013,
                                                   "2014"=2014,
                                                   "2015"=2015,
                                                   "2016"=2016)
                         ),
                         selectInput("state3",
                                     label = h3("State"),
                                     choices= c("ALL",sort(as.character(unique(h1b$STATE)))),
                                     selected = "NEW YORK"
                         )
                         
                         
                  )
                ),
                
                ## Fourth Graph
                tabPanel(
                  title = "Wage",
                  column(width = 9,
                         plotlyOutput("g4")),
                  
                  column(width = 3,
                         selectInput("case4",
                                     label = h3("Case Status"),
                                     choices= list("ALL"="ALL",
                                                   "CERTIFIED"="CERTIFIED",
                                                   "DENIED"="DENIED",
                                                   "CERTIFIED-WITHDRAWN"="CERTIFIED-WITHDRAWN",
                                                   "WITHDRAWN"="WITHDRAWN"),
                                     selected = "CERTIFIED"
                         ),
                         selectInput("state4",
                                     label = h3("State"),
                                     choices= c("ALL",sort(as.character(unique(h1b$STATE)))),
                                     selected = "NEW YORK"
                         )
                         
                         
                  )
                )
                
              )
              
              
            )),
    tabItem(tabName = "DS",

    fluidRow(
      column(width = 12,
      tabBox(
        title = "",
        width = 12,
        
        ## First Graph
        tabPanel(
          title = "Application Trend",
          box(width=NULL,
                 plotlyOutput("ds1")),
          
          infoBox(title = "Observations",
                  "1.Data Scientist and Data Engineer positions have observed an exponential growth in the last 6 years.\n
                  2.Job Titles with Machine Learning explicitly in them are still few in number (< 75 in any year).\n
                  3.In 2016, Data Scientist position broke the 1000 barrier on the number of H-1B Visa applications.",
                  icon = icon("list"),color = "blue",width = NULL)
        ),
        
        ## Second Graph
        tabPanel(
          title = "Wage boxplot",
          box(width=NULL,
                 plotlyOutput("ds2")),
          
          infoBox(title = "Observations",
                  "1.Machine Learning jobs have the highest median wage although the number of Job Titles with Machine Learning explicitly in them are less than 75 in any year.\t
                  2.Median wage for Data Engineer jobs is consistently increasing.\t
                 3. Median wage for Data Scientist positions is negligibly decreasing since 2012 although this is the position that has seen the most growth in the last 6 years.",
                  icon = icon("list"),color = "purple",width = NULL)
          ),
        tabPanel(
          title = "App trend of industry",
          box(width=NULL,
              plotlyOutput("ds3")),
          
          infoBox(title = "Top three categories",
                  "1.Statisticians
                   2.Software Developers
                   3.Operation Research Analysts",
                  icon = icon("list"),color = "lime",width = NULL)
        ),
        tabPanel(
          title = "Wage trend of industry",
          box(width=NULL,
              plotlyOutput("ds4")),
          
          infoBox(title = "Highest salaries belong to",
                  "1.Mathematicians
                   2.Computer and Information Research Scientists
                   3.Software Developers, Systems Software",
                  icon = icon("list"),color = "black",width = NULL))        
        )
      )
      
      
    ))))




#### UI ####
ui <- dashboardPage(
  header,
  sidebar,
  body
)

#### Server ####
server <- function(input,output) {
  
  ## First graph
  output$g1 <- renderPlotly(
    { g1_generator(input.state =input$state1, h1b) }
  )
  
  ## Second Graph
  output$g2 <- renderPlotly(
    { g2_generator(input.type = input$type2,input.year = input$year2, input.state =input$state2, h1b) }
  )
  
  ## Third Graph 
  output$g3 <- renderPlotly(
    { g3_generator(input.type = input$type3,input.year = input$year3, input.state =input$state3, h1b) }
  )
  
  ## Fourth Graph 
  output$g4 <- renderPlotly(
    { g4_generator(input.case = input$case4, input.state =input$state4, h1b) }
  )
  
  
  output$ds1 <- renderPlotly(
    { (ds1) }
  )
  output$ds2 <- renderPlotly(
    { (ds2)}
  )
  output$ds3 <-renderPlotly(
    {(ds3)}
  )
  output$ds4 <-renderPlotly(
    {(ds4)}
  )
}

#### APP ####
shinyApp(ui,server)



