## APP ##

#### Install Libraries ####
packages.used <- c("shiny","shinydashboard","plotly","plyr","dplyr")

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

#### Source helper functions ####
load("../output/cleaned_data.Rdata")
source("../lib/graph_helper.R")

#### Header of the Dashboard ####
header <- dashboardHeader(
  title = c("Survive or Not","a")
 
)

#### Sidebar ####
sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("APP Overview",tabName = "Overview"),
      menuItem("Map",tabName = "Map"),
      menuItem("Graph",tabName = "Graph")
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
}

#### APP ####
shinyApp(ui,server)











