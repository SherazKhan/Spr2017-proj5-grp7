##############################Source for DS panel####################

##packages
library(lazyeval)
library(ggplot2)
library(readr) 
library(dplyr)
library(ggrepel)
library(plotly)

# Function to filter only the rows from dataframe 

job_filter <- function(data,input_vector) {

  if(length(input_vector) == 0) {
    return(df %>%
             mutate(JOB_INPUT_CLASS = JOB_TITLE))
  }
  
  new_data <- data.frame()
  
  for(i in input_vector){
    new_data <- rbind(new_data, data %>% 
                      filter(regexpr(i,JOB_TITLE,ignore.case=TRUE) != -1) %>%
                      mutate(JOB_INPUT_CLASS = toupper(i)))
  }
  return(unique(new_data))
}



  # Function to find the top values in x_feature based on metric value
top_value <- function(data,x_feature,criterion, Ntop = 3) {

  arrange_criteria <- interp(~ desc(x), x = as.name(criterion))
  
  data %>% 
    group_by_(x_feature) %>% 
    mutate(certified =ifelse(CASE_STATUS == "CERTIFIED",1,0)) %>%
    # Metrics to be used in data analysis
    summarise(TotalApps = n(),
              Wage = median(PREVAILING_WAGE), 
              CertiApps = sum(certified),
              Share = CertiApps/850) %>%
    arrange_(arrange_criteria) -> top_data
  
  top_length <- min(dim(top_value)[1],Ntop)
  
  return(top_data[1:top_length,1])
}


inputplot <- function(data, x_feature, fill_feature,criterion,filter = FALSE, ...) {
  # Function to transform the filtered dataframe to one with computed metrics

  #Finding out the top across the entire range independent of the fill_feature
  top_x <- unlist(top_value(data,x_feature,criterion, ...))
  

  
  filter_criteria <- interp(~x %in% y, .values = list(x = as.name(x_feature), y = top_x))
  arrange_criteria <- interp(~ desc(x), x = as.name(criterion))
  
  if(filter == TRUE) {
    data %>%
      filter_(filter_criteria) -> data
  }

  return(data %>% 
           group_by_(.dots=c(x_feature,fill_feature)) %>% 
           mutate(certified =ifelse(CASE_STATUS == "CERTIFIED",1,0)) %>%
           # metrics I will be using in my data analysis   
           summarise(TotalApps = n(),
                     CertiApps = sum(certified), 
                     Wage = median(PREVAILING_WAGE),
                     Share = CertiApps/850))
}



theme_for_plot <- function() {
  # Function for ggplot2 graphics parameters
  return(
    theme(axis.title = element_text(size = rel(1.5)),
          legend.position = "right",
          legend.key = element_rect(fill = "white", colour = "black"),
          legend.text = element_text(size = rel(1.5)),
          legend.title = element_text(size=rel(1.5)))
  )
}


    # Function to plot output
outputplot <- function(data, x_feature,fill_feature,criterion, xlab_var,ylab_var) {  


    options(scipen = 999)
    
    return(ggplot(data, aes_string(x=x_feature,y=criterion)) +
      geom_bar(stat = "identity", aes_string(fill = fill_feature), position = "dodge") + 
      coord_flip() + xlab(xlab_var) + ylab(ylab_var) + theme_for_plot()+scale_fill_hue(c=45, l=80))

  }



h1b %>% 
  mutate(YEAR = as.character(YEAR)) -> h1b

job_list <- c("Data Engineer","Data Scientist","Machine Learning")

data_science <- inputplot(job_filter(h1b,job_list),
                              "JOB_INPUT_CLASS",
                              "YEAR",
                              "TotalApps")
ds1<-outputplot(data_science, "JOB_INPUT_CLASS","YEAR", "TotalApps", "JOB CLASS", "NO. OF APPLICATIONS")

ds2 <-plot_ly(data = job_filter(h1b,job_list),x=~JOB_INPUT_CLASS,y= ~PREVAILING_WAGE,color = ~YEAR,type = "box") %>%
  layout(boxmode="group",
         yaxis = list(title="WAGE ($)", range = c(25000,200000)), xaxis = list(title="Job Title"))


h1b%>%
  mutate(SOC_NAME = toupper(SOC_NAME)) -> h1b




data_science_1 <- inputplot(job_filter(h1b,job_list), "SOC_NAME", "YEAR", "TotalApps",
                                  filter = TRUE,
                                  Ntop = 10)
ds3<-outputplot(data_science_1, "SOC_NAME","YEAR", "TotalApps", "INDUSTRY", "NO. OF APPLICATIONS")

ds4<-outputplot(data_science_1, "SOC_NAME","YEAR", "Wage", "INDUSTRY", "WAGE(USD)")

