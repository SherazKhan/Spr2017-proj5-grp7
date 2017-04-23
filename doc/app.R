## APP ##

#### Install Libraries ####
packages.used <- c("shiny","shinydashboard","plotly","plyr")

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

#### Source helper functions ####
load("../output/cleaned_data.Rdata")

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
            mainPanel(
              
              ## First Graph
              fluidRow(
                headerPanel("H1B Trend"),
                box(
                  height = 500, 
                  width = 9,
                  plotlyOutput("g1")
                ),
                box(
                  height = 500,
                  width = 3,
                  selectInput("case",
                              label = h3("Case Status"),
                              choices=list("Certified" = "CERTIFIED",
                                           "Denied" = "DENIED",
                                           "Certified-Withdrawn" = "CERTIFIED-WITHDRAWN",
                                           "Withdrawn" = "WITHDRAWN"),
                              selected = 1)
                )
                
              )
              
            ))
  )

  
)


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
    {plot_ly(x=sort(unique(h1b$YEAR)),y =daply(h1b[h1b$CASE_STATUS==input$case,],.(YEAR),nrow),type = "scatter" ,mode = "lines")}
  )
}

#### APP ####
shinyApp(ui,server)











