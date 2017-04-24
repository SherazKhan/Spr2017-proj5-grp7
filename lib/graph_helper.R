#### Graph Helper Functions ####

#install.packages("dplyr","plotly")
library(dplyr)
library(plotly)

## First Graph: H1B Trending 
g1_generator <- function(input.case, input.state, dataset) {
  
  df <- dataset
  
  ## CASE_STATUS
  if (input.case != "ALL") {
    df <- filter(df, CASE_STATUS == input.case)
  }
  
  ## STATE
  if (input.state != "ALL") {
    df <- filter(df, STATE == input.state)
  }
  
  ## Plot
  plot_ly(x=sort(unique(df$YEAR)),y=daply(df,.(YEAR),nrow),type = "scatter" ,mode = "lines")
  
}