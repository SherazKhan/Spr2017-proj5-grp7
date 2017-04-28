## APP ##

#### Install Libraries ####
packages.used <- c("shiny","shinydashboard","plotly","plyr","dplyr","lazyeval","ggplot2","reshape2","cellranger","hashmap","readxl","rematch")

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
load("../output/cleaned_data.Rdata",.GlobalEnv)
source("../lib/graph_helper.R")
source("../lib/ds_helper.R")
h1b$STATE<-as.character(h1b$STATE)
h1b <- h1b[h1b$STATE!="NA",]

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
              h2(strong("Project Overview")),
              p("In this project, we will utilize the H1B visa petition dataset and perform various statistical analysis aiming to find interesting trend. The key components of this app are the following: "),
              p("--", em("Interactive Map ")),
              p("--", em("Overall H1B Visa Petition Trend")),
              p("--", em("High-Applicant Employers and Job Title")),
              p("--", em("Data Science Related Job Analysis")),
              hr(),
              h2("H1B"),
              p("H-1B is an employment-based, non-immigrant visa category for temporary foreign workers in the United States. For a foreign national to apply for H1-B visa, an US employer must offer a job and petition for H-1B visa with the US immigration department."),
              hr(),
              h2("H1B Process"),
              p(strong("Step 1:"), "The first step of the H1B application process is for the U.S. employer to file the H1B petition on behalf of the foreign worker. Be sure the employment letter includes the position's exact duties, dates of employment,  detailed description, salary offered, position requirements, contact information, etc."),
              p(strong("Step 2:"), "The prevailing and actual wages should be confirmed by the State Employment Security Agency. If the prevailing wage exceeds the offer made by the prospective employer then a wage determination will be sought. It's important to ensure that the foreign worker will not be working below the min. prevailing wage in the specific location. The Foreign Labor Certification Data Center maintains the prevailing wage determinations by location."),
              p(strong("Step 3:"), "The third step of the H1B application process is to file the Labor Condition Application (ETA-9035)."),
              p(strong("Step 4:"), "The next step is to prepare the petition and file it at the proper USCIS office"),
              p(strong("Step 5:"), "Processing times for H1B application petitions are subject to vary from location to location. If you would like your petition expedited you may elect for premium processing. There is an additional charge for this service and it does not necessarily guarantee an approval."),
              p(strong("Step 6:"), "The final step of the H1B application process is to check the status of your H1B visa petition by entering your receipt number. Once USCIS has your application on file, they will update your status on their system.")
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
                         ),
                         div("Note: Data only contains 0 - 90 percentile of wage")
                         
                         
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
                  "1.There has been a exponential increase in H1b certified positions in Data Scientist and Data Engineer.
                  2.As an emerging insustry, jobs titled machine learning are still in a small number.
                  3.Data Scientist in H1b applicants grow up above 1000 in 2016, with a positive trend on increase",
                  icon = icon("list"),color = "blue",width = NULL)
        ),
        
        ## Second Graph
        tabPanel(
          title = "Wage boxplot",
          box(width=NULL,
                 plotlyOutput("ds2")),
          
         infoBox(title = "Observations",
                  "1.Although small in total number, jobs titled in Machine learning gets the highest median wage among three jobs
                   2.As for Data Scientist job, increasing in number, the wage range is decreasing starting from 2012.
                   3.Data Engineering job salaries are growing in the last 6 years",
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
          
          infoBox(title = "Highest salary industries",
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



