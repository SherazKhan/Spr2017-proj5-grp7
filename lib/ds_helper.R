

job_filter <- function(df,input_vec) {
  # Function to filter only the rows from dataframe 
  # with Job titles provided in the inputs
  # Inputs:
  # df         : H-1B dataset dataframe
  # input_vec  : vector of job types input
  # Output     : filtered dataframe
  # If no match, returns an empty data frame
  # If the inputs are all equal to "", it returns the complete dataframe
  # A new column JOB_INPUT_CLASS is created to identify the Job Type
  # If multiple job type inputs match with a single row in the dataframe df, the 
  # output contains them in different rows each with distinct JOB_INPUT_CLASS
  
  # If input_vec is empty, return without any filtering
  if(length(input_vec) == 0) {
    return(df %>%
             mutate(JOB_INPUT_CLASS = JOB_TITLE))
  }
  
  new_df <- data.frame()
  
  for(value in input_vec){
    new_df <- rbind(new_df, df %>% 
                      filter(regexpr(value,JOB_TITLE,ignore.case=TRUE) != -1) %>%
                      mutate(JOB_INPUT_CLASS = toupper(value)))
  }
  return(unique(new_df))
}


find_top <- function(df,x_feature,metric, Ntop = 3) {
  # Function to find the top values in x_feature based on metric value
  # Inputs:
  # df            : filtered dataframe from job_type, location, employer and year range inputs
  # x_feature     : the column in df against which the metric is plotted for e.g., EMPLOYER_NAME
  # metric        : metric for data comparison 
  # Output        : list of top values in x_feature based on metric
  arrange_criteria <- interp(~ desc(x), x = as.name(metric))
  
  df %>% 
    group_by_(x_feature) %>% 
    mutate(certified =ifelse(CASE_STATUS == "CERTIFIED",1,0)) %>%
    # Metrics that I will be using in my data analysis
    summarise(TotalApps = n(),
              Wage = median(PREVAILING_WAGE), 
              CertiApps = sum(certified),
              Share = CertiApps/850) %>%
    arrange_(arrange_criteria) -> top_df
  
  top_len <- min(dim(top_df)[1],Ntop)
  
  return(top_df[1:top_len,1])
}


plot_input <- function(df, x_feature, fill_feature, metric,filter = FALSE, ...) {
  # Function to transform the filtered dataframe to one with computed metrics
  # Inputs:
  # df            : filtered dataframe from job_type, location, employer and year range inputs
  # x_feature     : the column in df against which the metric is plotted for e.g., EMPLOYER_NAME
  # fill_feature  : additional level of classification; for e.g., Year
  # metric        : metric for data comparison 
  # filter        : logical operator that filters only the rows with x_feature value belonging to top_find() output
  # Output        : dataframe grouped by x_feature and fill_feature with metrics as columns
  
  #Finding out the top across the entire range independent of the fill_feature e.g. Year
  top_x <- unlist(find_top(df,x_feature,metric, ...))
  
  # lazyeval package interp () generates expression that interprets x_feature and metric arguments
  # this is fed into filter_ and arrange_ accordingly
  # Source: https://cran.r-project.org/web/packages/lazyeval/vignettes/lazyeval.html
  
  filter_criteria <- interp(~x %in% y, .values = list(x = as.name(x_feature), y = top_x))
  arrange_criteria <- interp(~ desc(x), x = as.name(metric))
  
  if(filter == TRUE) {
    df %>%
      filter_(filter_criteria) -> df
  }
  
  #Grouping by not just x_feature but also fill_feature
  return(df %>% 
           group_by_(.dots=c(x_feature,fill_feature)) %>% 
           mutate(certified =ifelse(CASE_STATUS == "CERTIFIED",1,0)) %>%
           # metrics I will be using in my data analysis   
           summarise(TotalApps = n(),
                     CertiApps = sum(certified), 
                     Wage = median(PREVAILING_WAGE),
                     Share = CertiApps/850))
}




plot_output <- function(df, x_feature,fill_feature,metric, xlabb,ylabb) {  
    # Function to plot output
    # Inputs:
    # df            : dataframe output of plot_input()
    # x_feature     : the column in df against which the metric is plotted for e.g., EMPLOYER_NAME
    # fill_feature  : additional level of classification; for e.g., Year
    # metric        : metric for data comparison 
    # xlabb         : x label
    # ylabb         : y label
    # Output        : ggplot object
    
    # Prevents numbers on plot transforming into scientific notation
    options(scipen = 999)
    
    g <- ggplot(df, aes_string(x=x_feature,y=metric)) +
      geom_bar(stat = "identity", aes_string(fill = fill_feature), position = "dodge") + 
      coord_flip() + xlab(xlabb) + ylab(ylabb) + get_theme()
    
    return(g)
  }


get_theme <- function() {
  # Function for ggplot2 graphics parameters
  return(
    theme(axis.title = element_text(size = rel(1.5)),
          legend.position = "right",
          legend.text = element_text(size = rel(1.5)),
          legend.title = element_text(size=rel(1.5)))
  )
}
h1b %>% 
  mutate(YEAR = as.character(YEAR)) -> h1b

job_list <- c("Data Scientist","Data Engineer","Machine Learning")

data_science_df <- plot_input(job_filter(h1b,job_list),
                              "JOB_INPUT_CLASS",
                              "YEAR",
                              "TotalApps")
ds1<-plot_output(data_science_df, "JOB_INPUT_CLASS","YEAR", "TotalApps", "JOB CLASS", "NO. OF APPLICATIONS")
