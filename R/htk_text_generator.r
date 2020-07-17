
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
  ctry<<-ctry
 # prepare the data according to data availability

  vars_needed=prep_data(data,ranking,ctry,var_codes,type_var="outcomes")

  main_vars<<-vars_needed$var_codes
  main_vars_rank<<-paste0(main_vars,"_rank")
  main_vars_label<<-vars_needed$var_names_long
  main_vars_direction <<- vars_needed$var_direction

  if(length(main_vars)==3){
    #select the variables in the database
    mydt<<-vars_needed$data %>% data.frame()
    mydt<<-mydt %>% dplyr::select(Iso_code3,main_vars,main_vars_rank)

    output <-"paragraphs.R"

    current.folder=system.file("extdata", package = "OECDHousingToolkit")
    local.folder <- "."
    file.copy(paste0(current.folder,"/",output), local.folder,overwrite = T)
    source(output)

    #include the paragraphs in the database for each country
    mydt=mydt %>% filter(Iso_code3==ctry)
    mydt[[category]]=NA
    for(i in 1:length(mydt$Iso_code3)){
      i<<-i
      ctry=mydt[i,"Iso_code3"]
      current_input <- c(round(mydt[i,colsnum[1]],2),round(mydt[i,colsnum[2]],2),round(mydt[i,colsnum[3]],2))
      my_ldcp <- ldcp_run(ldcp=my_ldcp,input=current_input)
      text=gsub("targetcountry",countrycode(ctry,origin="iso3c",destination="country.name"),my_ldcp$report$description)
      mydt[i,category]=text
    }
    #colnames(mydt)=c("country",main_vars,category)
    file.remove(output)
    #rio::export(mydt,paste0("htk_paragraphs_",category,".csv"),sep=";")
  }else{
    mydt=data.frame(ctry,
                    paste0("The ",category, "of the housing sector in ",countrycode(ctry,origin="iso3c",destination="country.name")," cannot be assess based on available data.
                           Improvments on data collection should be undertaken"),
                    paste0("The ",category, "of the housing sector in ",countrycode(ctry,origin="iso3c",destination="country.name")," cannot be assess based on available data.
                           Improvments on data collection should be undertaken"),
                    paste0("The ",category, "of the housing sector in ",countrycode(ctry,origin="iso3c",destination="country.name")," cannot be assess based on available data.
                           Improvments on data collection should be undertaken"))
    colnames(mydt)=c("Iso_code3","efficiency","inclusiveness","sustainability")
  }

  mydt
}
