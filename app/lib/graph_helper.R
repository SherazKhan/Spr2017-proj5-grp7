#### Graph Helper Functions ####

#install.packages("dplyr","plotly")
library(dplyr)
library(plotly)


## First Graph: H1B Trending 
g1_generator <- function(input.state, df) {

  df$CASE_STATUS <- as.character(df$CASE_STATUS)
  df$STATE <- as.character(df$STATE)

  
  ## STATE
  if (input.state != "ALL") {
    df <- filter(df, STATE == input.state)
  }
  
  ## Plot
  plot_ly(x=sort(unique(df$YEAR)),y=as.numeric(table(df$YEAR)),type = "scatter" ,mode = "lines",name="ALL") %>% 
    add_lines(x=sort(unique(df$YEAR)),y=as.numeric(table(df[df$CASE_STATUS=="CERTIFIED",]$YEAR)),name="CERTIFIED") %>% 
    add_lines(x=sort(unique(df$YEAR)),y=as.numeric(table(df[df$CASE_STATUS=="DENIED",]$YEAR)),name="DENIED") %>% 
    add_lines(x=sort(unique(df$YEAR)),y=as.numeric(table(df[df$CASE_STATUS=="CERTIFIED-WITHDRAWN",]$YEAR)),name="CERTIFIED-WITHDRAWN") %>% 
    add_lines(x=sort(unique(df$YEAR)),y=as.numeric(table(df[df$CASE_STATUS=="WITHDRAWN",]$YEAR)),name="WITHDRAWN") %>%
    layout(xaxis=list(title="Year"),
           yaxis=list(title="Number of People"))
  
}

## Second Graph: Top Employor
g2_generator <- function(input.type, input.year, input.state, df) {
  

  df$EMPLOYER_NAME<- as.character(df$EMPLOYER_NAME)
  df$STATE <- as.character(df$STATE)

  ## input.year
  if (input.year != "ALL") {
    df <- filter(df, YEAR==input.year)
  }
  
  ## input.state
  if (input.state != "ALL") {
    df <- filter(df, STATE == input.state)
  }
  
  ##Computation
  total_num <- nrow(df)
  top_em <- names(head(sort(table(df$EMPLOYER_NAME), decreasing = T),15))
  
  hehe<- matrix(nrow = 15,ncol = 4)
  for (i in 1:15) {
    hehe[i,] <-c(nrow(df[df$EMPLOYER_NAME==top_em[i] & df$CASE_STATUS=="CERTIFIED",]),
                 nrow(df[df$EMPLOYER_NAME==top_em[i] & df$CASE_STATUS=="DENIED",]),
                 nrow(df[df$EMPLOYER_NAME==top_em[i] & df$CASE_STATUS=="CERTIFIED-WITHDRAWN",]),
                 nrow(df[df$EMPLOYER_NAME==top_em[i] & df$CASE_STATUS=="WITHDRAWN",]))
  }
  
  ## input.type
  if (input.type=="Percentage Share"){
    hehe <- hehe/total_num*100 
    hehe <- data.frame(hehe)
    names(hehe) <- unique(df$CASE_STATUS)
    hehe$Title <- top_em
    hehe <- melt(hehe, id.var="Title")
    ggplotly(ggplot(hehe,aes(x=reorder(Title,value),y=value ,fill=variable)) + xlab("Top Employer Name") + ylab("Percentage Share (%)")+ geom_bar(stat = "identity") + coord_flip())
  
  } else {
    hehe <- data.frame(hehe)
    names(hehe) <- unique(df$CASE_STATUS)
    hehe$Title <- top_em
    hehe <- melt(hehe, id.var="Title")
    ggplotly(ggplot(hehe,aes(x=reorder(Title,value),y=value ,fill=variable)) + xlab("Top Employer Name") + ylab("Number of Applicants")+ geom_bar(stat = "identity") + coord_flip())
  }
  
 
}

## Third Graph: Top Job Titles
g3_generator <- function(input.type, input.year, input.state, df) {
  
  
  df$JOB_TITLE<- as.character(df$JOB_TITLE)
  df$STATE <- as.character(df$STATE)

  
  ## input.year
  if (input.year != "ALL") {
    df <- filter(df, YEAR==input.year)
  }
  
  ## input.state
  
  if (input.state != "ALL") {
    df <- filter(df, STATE == input.state)
  }
  
  ##Computation
  total_num <- nrow(df)
  top_job <- names(head(sort(table(df$JOB_TITLE), decreasing = T),15))
  
  hehe<- matrix(nrow = 15,ncol = 4)
  for (i in 1:15) {
    hehe[i,] <-c(nrow(df[df$JOB_TITLE==top_job[i] & df$CASE_STATUS=="CERTIFIED",]),
                 nrow(df[df$JOB_TITLE==top_job[i] & df$CASE_STATUS=="DENIED",]),
                 nrow(df[df$JOB_TITLE==top_job[i] & df$CASE_STATUS=="CERTIFIED-WITHDRAWN",]),
                 nrow(df[df$JOB_TITLE==top_job[i] & df$CASE_STATUS=="WITHDRAWN",]))
  }
  
  ## input.type
  if (input.type=="Percentage Share"){
    hehe <- hehe/total_num*100
    hehe <- data.frame(hehe)
    names(hehe) <- unique(df$CASE_STATUS)
    hehe$Title <- top_job
    hehe <- melt(hehe, id.var="Title")
    ggplotly(ggplot(hehe,aes(x=reorder(Title,value),y=value ,fill=variable)) + xlab("Top Job Title") + ylab("Percentage Share (%)") + geom_bar(stat = "identity") + coord_flip())
  } else {
    hehe <- data.frame(hehe)
    names(hehe) <- unique(df$CASE_STATUS)
    hehe$Title <- top_job
    hehe <- melt(hehe, id.var="Title")
    ggplotly(ggplot(hehe,aes(x=reorder(Title,value),y=value ,fill=variable)) + xlab("Top Job Title") + ylab("Number of Applicants") + geom_bar(stat = "identity") + coord_flip())
  }
  
  
  
}

## Fourth Graph: Wage
g4_generator <- function(input.case, input.state, df) {
  
  df$YEAR <- as.character(df$YEAR)
  df$PREVAILING_WAGE <- as.numeric(df$PREVAILING_WAGE)
  df$STATE <- as.character(df$STATE)
  
  ## input.case
  if (input.case != "ALL") {
    df <- df[df$CASE_STATUS==input.case,]
  }
  
  ## input.state
  if (input.state != "ALL") {
    df <- filter(df, STATE == input.state)
  }
  
  ggplotly(ggplot(df,aes(x=df$YEAR,y=df$PREVAILING_WAGE)) + xlab("Year") + ylab("Wage in USD") + geom_boxplot()+ scale_y_continuous(limits = quantile(df$PREVAILING_WAGE, c(0, 0.9),na.rm = TRUE)))
  
}


## Map
map_generator <- function(input.year,input.job,df) {
  
  df$YEAR <- as.numeric(df$YEAR)
  
  ## input.year
  if (input.year != "ALL") {
    df <- filter(df, YEAR==input.year)
  }
  
  ## input job
  if (input.job !="ALL") {
    df<- df[df$JOB_TITLE==input.job,]
  }
  
  
  
  
  ## Draw
  leaflet(df) %>% addTiles() %>% addCircleMarkers(lng=df$lon,lat=df$lat,radius=1) %>%setView(lng=-97,lat=40,zoom=4)
  
}