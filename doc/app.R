## APP ##

#### Install Libraries ####
packages.used <- c("shiny","shinydashboard","plotly","plyr","dplyr","lazyeval","ggplot2")

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
                         selectInput("case1",
                                     label = h3("Case Status"),
                                     choices=list("ALL" = "ALL",
                                                  "Certified" = "CERTIFIED",
                                                  "Denied" = "DENIED",
                                                  "Certified-Withdrawn" = "CERTIFIED-WITHDRAWN",
                                                  "Withdrawn" = "WITHDRAWN")),
                         selectInput("state1",
                                     label = h3("State"),
                                     choices= c("ALL",sort(as.character(unique(h1b$STATE))))
                         )
                         
                  )
                )
                
                ## Second Graph
                #tabPanel(
                #  title = "H1B Trend",
                #  column(width=9,
                #         plotlyOutput("g1")),
                #  
                #  column(width = 3,
                #         selectInput("case",
                #                     label = h3("Case Status"),
                #                     choices=list("All"= "ALL",
                #                                  "Certified" = "CERTIFIED",
                #                                  "Denied" = "DENIED",
                #                                  "Certified-Withdrawn" = "CERTIFIED-WITHDRAWN",
                #                                  "Withdrawn" = "WITHDRAWN"))
                
                
                #  )
                #)
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
    { g1_generator(input.case = input$case1, input.state =input$state1, h1b) }
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



