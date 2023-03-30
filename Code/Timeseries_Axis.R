timeseries_axis2 <- function(breaks, dates){
  
  # create breaks and labels
  
  breaks <- as.character(breaks)
  labels <- character(length(breaks))
  years <- dates %>% 
    ymd() %>% 
    year() %>% 
    unique()
  
  # intialize `first year` vector to store the indexes for the first observation from each new year
  first_year <-c()
  
  # start with first observation and first year
  index <- 1
  i = min(years)
  
  # iterate through all dates
  for(date in as.character(dates)){
    if (str_detect(date, as.character(i)) == TRUE){
      # if the date is the first occurrence of a specific year, write it to the vector `first_year`
      first_year <- c(first_year, index)
      i <- i + 1
    }
    index <- index + 1
  }
  
  # create vector of labels, add a label (with the year) for the first observations in each year, otherwise leave all labels blank
  
  for (i in 1:length(years)){
    labels[first_year[i]] <- as.character(years[i])
    
  }
  
  data.frame(breaks,labels)
  
}
