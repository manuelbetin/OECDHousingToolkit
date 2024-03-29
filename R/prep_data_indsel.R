
#' @title prepare datasets for figures
#' @description provide the dataset with the main variables
#' or the substitutes if variables ranking 1 are missing
#' @param mydata a dataframe with all the policy variables
#' columns and iso3 codes in rows.
#' @param ranking a dataframe with the ranking of the policy variables
#' @param ctry the iso3 code for the selected country
#' @param var_codes a dataframe with the the code of the policy variables
#' @return a list with data a tibble with the proper selection of
#' variables, var_codes the codes of the selected vars and
#' var_names the names of the selected vars
#' @author Manuel Betin, Federica De Pace

prep_data_indsel=function(mydata,ranking,ctry,var_codes,type_var){
  # reshape the variables
  dt_non_na <- mydata %>%
    filter(Iso_code3 == ctry) %>%
    select(Iso_code3, var_codes) %>% 
    gather(variable, value, var_codes) %>% 
    filter(!is.na(value)) %>% merge(ranking, by = "variable") %>% 
    filter(!is.na(value))
  

  #select relevant variables according to availability
  dt_non_na = get_vars_indsel(dt_non_na, type_var) %>% filter(!is.na(variable))%>%distinct()

  #store final variable codes and names
  var_codes = dt_non_na$variable
  var_names = dt_non_na$variable_name
  var_names_long = dt_non_na$variable_name_long
  var_direction = dt_non_na$direction
  var_units=dt_non_na$unit
  var_date=dt_non_na$date

  # keep only relevant variables in the dataset
  vars_needed = mydata %>% select(Iso_code3, var_codes) %>% 
    data.frame()

  # transform in numeric
  vars_needed[var_codes] <- sapply(vars_needed[var_codes], as.numeric)
  
  #compute OECD average
  OECD_mean <-vars_needed %>% filter(Iso_code3!="CHN"&Iso_code3!="RUS"&Iso_code3!="ZAF") %>%
    summarise_at(.vars = var_codes,.funs=list(~mean(.,na.rm=T)))%>%
    mutate(Iso_code3="OECD")%>%
    arrange(Iso_code3, "") %>%
    select(Iso_code3, everything())

  # Aggregate OECD average to the dataset
  vars_needed<-rbind(vars_needed, OECD_mean)

  #compute relevant statistics
  vars_needed_plus<- vars_needed  %>%
    mutate_at(vars(var_codes),.funs=list(mean=~mean(.[Iso_code3!="CHN"&Iso_code3!="RUS"&Iso_code3!="ZAF"], na.rm=T ),
                                         min=~min(.,na.rm=T),
                                         max=~max(.,na.rm=T),
                                         rank=~percent_rank(.)))


  return(list(data = vars_needed_plus, var_codes = var_codes, var_names = var_names, var_direction = var_direction, var_names_long = var_names_long, var_units=var_units,var_date=var_date))
}
