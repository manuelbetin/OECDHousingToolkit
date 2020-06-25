

#'@title Create dynamique text from
#'@description generate dynamic text
#'@param data dataset with one row per country
#'@param category name of the category: Efficiency, Affordability, Sustainability
#'@param vars a vector of four variable names that enter the algorithm
#'@author Manuel Betin, Federica Depace, Naomi Cohen
#'@return a dataset containing the values of the indicator for each country
#'and the paragraph of text interpretating the data
#'@export
#'

htk_text_generator=function(data,category,vars,vars_label){

  if(!category %in% c("Efficiency","Sustainability","Inclusiveness")){
    print("Please select a relevant category: Efficicency, Sustainability or Inclusiveness")
    stop()
  }

  if(length(vars)!=4){
    print("Please provide three variables")
    stop()
  }

  #select the variables in the database
  dt=data %>% dplyr::select(country,vars)
  colnames(dt)=c("country","var1","var2","var3","var4")

  #export data
  # rio::export(dt,paste0("htk_paragraphs.csv"),sep=";")

  output <-paste0(category,"_paragraphs.R")

   current.folder=system.file("extdata", package = "OECDHousingToolkit")
   local.folder <- "."
   file.copy(paste0(current.folder,"/",output), local.folder,overwrite = T)
  source(output)

  #include the paragraphs in the database for each country
  dt[[category]]=NA
  for(i in 1:length(dt$country)){
    ctry=dt[i,"country"]
    current_input <- c(round(dt[i,2],2),round(dt[i,3],2),round(dt[i,4],2),round(dt[i,5],2))
    my_ldcp <- ldcp_run(ldcp=my_ldcp,input=current_input)
    text=gsub("country",ctry,my_ldcp$report$description)
    dt[i,category]=text
  }
  colnames(dt)=c("country",vars,category)
  file.remove(output)
  #rio::export(dt,paste0("htk_paragraphs_",category,".csv"),sep=";")
  dt
}
