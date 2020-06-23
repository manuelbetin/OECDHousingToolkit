
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

 # current.folder=system.file("extdata", package = "OECDHousingToolkit")
#  local.folder <- "."
#  list.of.files <- list.files(path = current.folder, full.names = TRUE)
#  file.copy(list.of.files, local.folder)

  dt=htk_Outcomevars %>% dplyr::select(country,vars)
  colnames(dt)=c("country","var1","var2")

  #export data
  rio::export(dt,paste0("htk_paragraphs.csv"),sep=";")

  #generate RlDCP files
  input <- paste0(category,"_paragraphs.xml")
  output <-paste0(category,"_paragraphs.R")

  #modify values in the xml

  myxml=xmlParse(input)
  #todo

  #generate rLDCP files

  #Efficiency
  xml2rldcp(input,output)

  #run rLDCp algorithm
  source(output)

  #include the paragraphs in the database
  my_ldcp <- ldcp(data=my_data,glmp=my_glmp,report=my_report)
  dt$Efficiency=NA
  for(i in 1:length(Res_Invest)){
    ctry=dt[i,"country"]
    current_input <- c(Res_Invest[i],Price_vol[i])
    my_ldcp <- ldcp_run(ldcp=my_ldcp,input=current_input)
    text=gsub("country",ctry,my_ldcp$report$description)
    dt[i,"Efficiency"]=text
  }

  colnames(dt)=c("country",vars,"Efficiency")
#  file.remove("htk_paragraphs.csv")
#  file.remove("Affordability_paragraphs.xml")
#  file.remove("Efficiency_paragraphs.xml")
#  file.remove("Sustainability_paragraphs.xml")
#  file.remove(output)
  rio::export(dt,paste0("htk_paragraphs_",category,".csv"),sep=";")
  dt
}
