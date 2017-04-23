## APP ##

#### Install Libraries ####
packages.used <- c("shiny","shinydashboard")

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

#### Source helper functions ####


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
              "HEHE"
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
  
}

#### APP ####
shinyApp(ui,server)











