
#'@title Create dynamique text from
#'@description generate dynamic text
#'@param category name of the category: Efficiency, Affordability, Sustainability
#'@author Manuel Betin, Federica Depace
#'@return a dataset containing the values of the indicator for each country
#'and the paragraph of text interpretating the data
#'@export
#'

htk_text_generator=function(category,vars,path=NULL){
  #import outcome variables
  data("htk_Outcomevars")
  htk_Outcomevars=htk_Outcomevars

  if(!category %in% c("Efficiency","Sustainability","Inclusiveness")){
    print("Please select a relevant category: Efficicency, Sustainability or Inclusiveness")
    stop()
  }

  if(length(vars)!=3){
    print("Please provide three variables")
    stop()
  }

  dt=htk_Outcomevars %>% dplyr::select(country,vars)
  colnames(dt)=c("country","var1","var2","var3")

  #export data
  rio::export(dt,paste0("htk_paragraphs.csv"),sep=";")

  output <-paste0(category,"_paragraphs.R")


  current.folder=system.file("extdata", package = "OECDHousingToolkit")
  local.folder <- "."
  file.copy(paste0(current.folder,"/",output), local.folder)

  source(output)

  #include the paragraphs in the database
  my_ldcp <- ldcp(data=my_data,glmp=my_glmp,report=my_report)
  dt[[category]]=NA
  for(i in 1:length(dt$country)){
    ctry=dt[i,"country"]
    current_input <- c(round(myvar1[i],2),round(myvar2[i],2),round(myvar3[i],2))
    my_ldcp <- ldcp_run(ldcp=my_ldcp,input=current_input)
    text=gsub("country",ctry,my_ldcp$report$description)
    dt[i,category]=text
  }
  colnames(dt)=c("country",vars,category)
  file.remove(output)
  file.remove("htk_paragraphs.csv")
  #rio::export(dt,paste0("htk_paragraphs_",category,".csv"),sep=";")
  dt
}
