

#'@title Create dynamic text
#'@description generate dynamic text
#'@param data dataset with one row per country
#'@param category name of the category: Efficiency, Affordability, Sustainability
#'@param vars a vector of four variable codes that enter the algorithm, the first
#'two are the variables of interest and the fourth one is a value [0,1] denoting
#'the quality of the data
#'@param vars_label a vector of three variable name that enter the algorithm
#'@author Manuel Betin, Federica De Pace, Naomi Cohen
#'@return a dataset containing the values of the indicator for each country
#'and the paragraph of text interpretating the data
#'@export
#'


# data=dt_effic
# ranking=ranking_eff
# ctry=params$ctry_code
# var_codes=var_codes_eff
# var_names=var_names_eff
# var_names_long=var_names_long_eff
# sec_col=sec_col_eff
# title="Efficiency Outcomes"

htk_text_generator=function(data,category,ranking,ctry,var_codes){

  category<<-category

 # prepare the data according to data availability

  vars_needed=prep_data(data,ranking,ctry,var_codes,type_var="outcomes")

  main_vars<<-vars_needed$var_codes
  main_vars_rank<<-paste0(main_vars,"_rank")
  main_vars_label<<-vars_needed$var_names_long
  main_vars_direction <<- vars_needed$var_direction

  #select the variables in the database
  dt<<-vars_needed$data %>% data.frame()
  dt=dt %>% dplyr::select(Iso_code3,main_vars,main_vars_rank)

  output <-"paragraphs.R"

   current.folder=system.file("extdata", package = "OECDHousingToolkit")
   local.folder <- "."
   file.copy(paste0(current.folder,"/",output), local.folder,overwrite = T)
  source(output)

  #include the paragraphs in the database for each country
  dt=dt %>% filter(Iso_code3==ctry)
  dt[[category]]=NA
  for(i in 1:length(dt$Iso_code3)){
    i<<-i
    ctry=dt[i,"Iso_code3"]
    current_input <- c(round(dt[i,colsnum[1]],2),round(dt[i,colsnum[2]],2),round(dt[i,colsnum[3]],2),round(dt[i,colsnum[3]],2))
    my_ldcp <- ldcp_run(ldcp=my_ldcp,input=current_input)
    text=gsub("targetcountry",countrycode(ctry,origin="iso3c",destination="country.name"),my_ldcp$report$description)
    dt[i,category]=text
  }
  #colnames(dt)=c("country",main_vars,category)
   file.remove(output)
  #rio::export(dt,paste0("htk_paragraphs_",category,".csv"),sep=";")
  dt
}
