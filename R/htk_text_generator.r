

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


# mydata=dt_effic
# ranking=ranking_eff
# ctry=params$ctry_code
# var_codes=var_codes_eff
# var_names=var_names_eff
# sec_col=sec_col_eff
# title="Efficiency Outcomes"

htk_text_generator=function(data,category,ranking,ctry,var_codes,var_names){


  category<<-category
  # do here selectin of variables based on ranking
  ##################################################################################################
  this_country<-data %>%  filter(Iso_code3==ctry)  %>%  select(Iso_code3, first(var_codes):last(var_codes))
  this_country_long<-gather(this_country, variable , value,  first(var_codes):last(var_codes) )
  this_country_long<- merge(this_country_long, ranking[1:3], by="variable")

  # which of the main var is NA?
  ind_NA<-which(is.na(this_country_long$value) == TRUE, arr.ind=TRUE)

  if (length(ind_NA) ==0) {
    dt_non_na = this_country_long
  }  else if (length(ind_NA) >0) {
    dt_non_na = this_country_long[-ind_NA, ]
  }

  #n_var_nonna<-nrow(dt_non_na)
  # count Non NA vars ranked 1 or 2 or 3

  n_rank1 <- nrow(dt_non_na[with( dt_non_na,which(rank==1 )), ])
  n_rank2 <- nrow(dt_non_na[with( dt_non_na,which(rank==2 )), ])
  n_rank3 <- nrow(dt_non_na[with( dt_non_na,which(rank==3 )), ])

  vars_touse=get_vars(n_rank1, n_rank2, n_rank3, dt_non_na )
  temp_names<-merge(vars_touse, ranking , by="variable")

  main_vars<<-temp_names$variable
  main_vars_label<<-temp_names$variable_name
  main_vars_direction <<- temp_names$direction.x

  # which variables to use:
  vars_needed=data %>%  select(Iso_code3, main_vars)
  vars_needed =data.frame(vars_needed)

  if(length(main_vars)!=3){
    print("Please provide four variables, the three first for the
          indicators and the last for the overall quality of data")
    stop()
  }

  #select the variables in the database
  dt=vars_needed

  dt=dt %>% mutate_at(vars(main_vars),funs(rank=percent_rank))
  dt<<-data.frame(dt)
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
