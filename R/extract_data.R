extract_data <- function(inclusion_dummy,catalogue, data_source)
{
  relevant_variables <- filter(catalogue, inclusion_dummy == 1)  #keep variables that have been chosen in the catalogue
  relevant_variables <- relevant_variables$var.id #keep names of the variables in a list
  relevant_data <- data_source[, colnames(data_source) %in% relevant_variables] %>% #keep the columns with the relevant variables in the final data file
    mutate(ISO3_code=data_source[,1], Period = data_source[,2]) %>%  #add the country and period columns
    select(ISO3_code, Period, everything())  #set country and period columns as the first of the dataframe
  return(relevant_data)
}
