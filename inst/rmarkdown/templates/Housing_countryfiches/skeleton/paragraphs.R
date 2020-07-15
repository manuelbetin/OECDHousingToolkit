#'@title Text generation
#'@description generate dynamic text using rLDCP algorithm: the file
#'take as input a dataset with 4 variables and produce
#'@author Manuel Betin, Naomi Cohen
#'@return a dataset containing the values of the indicator for each country
#'and the paragraph of text interpretating the data
#'@export a ldcp object containing the text generated
#'

library(rLDCP)

####################################################################
###################### input file definition #######################
####################################################################
# input1 <- read.csv(file="htk_paragraphs.csv",
# sep=";", dec=".",
# header=TRUE)

####################################################################
###################### data definition #############################
####################################################################

input1=data.frame(dt)

replace_value_na=0 #default value when missing data

non_num_vars=input1 %>% select_if(is.character)
#
colsnum=(2+length(main_vars)):(2*length(main_vars)+1)

#colsnum=2:length(main_vars)
myvars=lapply(colsnum,function(x){
  main_vars_direction[x-4]
  if(3=="decreasing"){
    myvar <- 1-c(base::t(input1[x]))
  }else{
  myvar <- c(base::t(input1[x]))
 }
  myvar=myvar%>%replace_na(replace_value_na)
  myvar
})

rm(input1)

#provide the labels to the indicators and the categories that will be
#used in the text as well as definitions for each categories

definition=c(efficiency=" is defined as the capacity of the sector to provide housing that meets demand across the country, to
             facilitate residential mobility and to deliver housing that countributes to macroeconomic stability",
             inclusiveness=" refers to the capacity of the sector to provide affordable homes across tenure modalities,
             to tackle homelessness and to coordinate housing policies across levels of government",
             sustainability=" assess the capacity of the sector to built environmentally sustainable housing and to
             design urban areas that support the environment.")


mycategory_label=tolower(category)
mycategory_definition=definition[[mycategory_label]]
myvar1_label=main_vars_label[1]

myvar2_label=main_vars_label[2]

myvar3_label=main_vars_label[3]

####################################################################
###################### Data Structure definition ##################
####################################################################

input <- c()

my_method <- function (input){
  input
}

my_data <- data_structure(input,my_method)

####################################################################
###################### CP definition ###############################
####################################################################

#3 main indicators
cp_myvar1 <- cp("cp_myvar1", c("nodata","myvar1_lowtail", "myvar1_low", "myvar1_slow", "myvar1_medium", "myvar1_shigh", "myvar1_high","myvar1_uptail"))
cp_myvar2 <- cp("cp_myvar2", c("nodata","myvar2_lowtail", "myvar2_low", "myvar2_slow", "myvar2_medium", "myvar2_shigh", "myvar2_high","myvar2_uptail"))
cp_myvar3 <- cp("cp_myvar3", c("nodata","myvar3_lowtail", "myvar3_low", "myvar3_slow", "myvar3_medium", "myvar3_shigh", "myvar3_high","myvar3_uptail"))

cp_profile <- cp("cp_profile", c("Homogeneous","Heterogeneous"))
cp_Efficiency <- cp("cp_Efficiency", c("all_low","avg","all_high"))

####################################################################
###################### PMs definition ##############################
####################################################################

#########################################
## myvar1: first main indicator
g_pm_myvar1 <- function(u,y){
  y$w <- degree_mf(fuzzy_partitions(
    triangle_mf(0, 0, 0),
    triangle_mf(0.00001 , 0.1, 0.15),
    trapezoid_mf(0.15, 0.15, 0.35, 0.35) ,
    trapezoid_mf(0.3, 0.35, 0.4, 0.45) ,
    triangle_mf(0.45, 0.5, 0.6) ,
    trapezoid_mf(0.55, 0.6, 0.65, 0.65),
    trapezoid_mf(0.65, 0.65, 0.85, 0.85),
    triangle_mf(0.85, 0.95, 1)), u)
  y }

t_pm_myvar1 <- function(y){
  templates <- c(
    paste0(" ",myvar1_label),
    paste0(" ",myvar1_label,", locates country in the bottom tail of the distribution"),
    paste0(" ",myvar1_label,", is relatively far below OECD average"),
    paste0(" ",myvar1_label,", is slightly below OECD average"),
    paste0(" ",myvar1_label,", is close to the OECD average"),
    paste0(" ",myvar1_label,", is slightly above OECD average"),
    paste0(" ",myvar1_label,", is relatively far above OECD average"),
    paste0(" ",myvar1_label,", locates country in the upper tail of the distribution")
  )
  return(templates[which.max(y$w)])
}

pm_myvar1 <- rLDCP::pm(y=cp_myvar1, g=g_pm_myvar1, t=t_pm_myvar1)

#########################################
## myvar2: second main indicator
g_pm_myvar2<- function(u,y){
  y$w <- degree_mf(fuzzy_partitions(
    triangle_mf(0, 0, 0),
    triangle_mf(0.00001 , 0.1, 0.15),
    trapezoid_mf(0.15, 0.15, 0.35, 0.35) ,
    trapezoid_mf(0.3, 0.35, 0.4, 0.45) ,
    triangle_mf(0.45, 0.5, 0.6) ,
    trapezoid_mf(0.55, 0.6, 0.65, 0.65),
    trapezoid_mf(0.65, 0.65, 0.85, 0.85),
    triangle_mf(0.85, 0.95, 1)), u)
  y }

t_pm_myvar2<- function(y){
  templates <- c(
    paste0(" ",myvar2_label,", is not available for this country"),
    paste0(" ",myvar2_label,", is in the bottom tail of the distribution"),
    paste0(" ",myvar2_label,", is in the lower third of the distribution"),
    paste0(" ",myvar2_label,", is slightly below the average"),
    paste0(" ",myvar2_label,", is close to the average"),
    paste0(" ",myvar2_label,", is slighly above the average"),
    paste0(" ",myvar2_label,", is between the 65th and 85th percentile"),
    paste0(" ",myvar2_label,", is in the upper tail of the distribution")
  )
  return(templates[which.max(y$w)])
}


pm_myvar2 <- rLDCP::pm(y=cp_myvar2, g=g_pm_myvar2, t=t_pm_myvar2)

#########################################
## myvar3: third main indicator
g_pm_myvar3<- function(u,y){
  y$w <- degree_mf(fuzzy_partitions(
    triangle_mf(0, 0, 0),
    triangle_mf(0.00001 , 0.1, 0.15),
    trapezoid_mf(0.15, 0.15, 0.35, 0.35) ,
    trapezoid_mf(0.3, 0.35, 0.4, 0.45) ,
    triangle_mf(0.45, 0.5, 0.6) ,
    trapezoid_mf(0.55, 0.6, 0.65, 0.65),
    trapezoid_mf(0.65, 0.65, 0.85, 0.85),
    triangle_mf(0.85, 0.95, 1)), u)
  y }

t_pm_myvar3<- function(y){
  templates <- c(
    paste0(" ",myvar3_label,", is not available for this country"),
    paste0(" ",myvar3_label,", is in the bottom tail"),
    paste0(" ",myvar3_label,", is among the 35 percent lower performing economies"),
    paste0(" ",myvar3_label,", is slighly below the average"),
    paste0(" ",myvar3_label,", is close to the average"),
    paste0(" ",myvar3_label,", is slighly above the average"),
    paste0(" ",myvar3_label,", is among the 35 percent better performing economies"),
    paste0(" ",myvar3_label,", is in the upper tail")
  )
  return(templates[which.max(y$w)])
}

pm_myvar3 <- rLDCP::pm(y=cp_myvar3, g=g_pm_myvar3, t=t_pm_myvar3)

#########################################
# global performance over the three indicators
g_pm_Efficiency<- function(u,y){
  operator <- operator(  min , max )
  y$w <- infer_rules(fuzzy_rules(

    fuzzy_rule( 0, 1, 1, 1, 0, 0, 0, 0 ,
                0, 1, 1, 1, 0, 0, 0, 0 ,
                0, 1, 1, 1, 0, 0, 0, 0 ,
                0, 1, 0, 0),
    fuzzy_rule( 0, 0, 0, 0, 0, 1, 1, 1,
                0, 0, 0, 0, 0, 1, 1, 1,
                0, 0, 0, 0, 0, 1, 1, 1,
                0, 0, 0, 1),
    fuzzy_rule( 0, 0, 0, 1, 1 ,1 ,0 , 0,
                0, 0, 0, 1, 1 ,1 ,0 , 0,
                0, 0, 0, 1, 1 ,1 ,0 , 0,
                0, 0, 1, 0)
  ), operator,
  list( u[[1]]$w, u[[2]]$w, u[[3]]$w ))
  y }

t_pm_Efficiency<- function(y){
  templates <- c(
    "",#" what make overall assessment on efficiency difficult",
    " Generally speaking, its performance are among the bottom OECD ones.",
    " Overall, its performance are in line with the rest of OECD countries.",
    " country is globally among the top OECD performers."
  )
  return(templates[which.max(y$w)])
}

pm_Efficiency <- rLDCP::pm(y=cp_Efficiency, g=g_pm_Efficiency, t=t_pm_Efficiency)

#########################################
# Homogeneity of performance over the three main indicators

g_pm_profile<- function(u,y){
  operator <- operator(  min , max )
  y$w <- infer_rules(fuzzy_rules(

    fuzzy_rule( 0, 1, 1, 1, 0, 0, 0, 0,
                0, 1, 1, 1, 0, 0, 0, 0,
                0, 1, 1, 1, 0, 0, 0, 0,
                0, 1),
    fuzzy_rule( 0, 0, 0, 0, 0, 1, 1, 1,
                0, 0, 0, 0, 0, 1, 1, 1,
                0, 0, 0, 0, 0, 1, 1, 1,
                0, 1),
    fuzzy_rule( 0, 0, 0, 1, 1, 1, 0, 0,
                0, 0, 0, 1, 1, 1, 0, 0,
                0, 0, 0, 1, 1, 1, 0, 0,
                0, 1)
  ), operator,
  list( u[[1]]$w, u[[2]]$w, u[[3]]$w ))
  y }

t_pm_profile<- function(y){
  templates <- c(
    paste0(" The overall performance of country is difficult to assess based on the three main indicators: ",myvar1_label, ", ", myvar2_label, " and ",myvar3_label, '. '),
    paste0(" country displays similar relative performances among all three dimensions: ",myvar1_label, ", ", myvar2_label, " and ",myvar3_label, '. ')
  )
  return(templates[which.max(y$w)])
}
pm_profile <- rLDCP::pm(y=cp_profile, g=g_pm_profile, t=t_pm_profile)

####################################################################
###################### GLMP definition ##############################
####################################################################

glmp_method <- function(pm,input){
  pm$pm_myvar1  <- pm_infer(pm$pm_myvar1, input[1])
  pm$pm_myvar2  <- pm_infer(pm$pm_myvar2, input[2])
  pm$pm_myvar3  <- pm_infer(pm$pm_myvar3, input[3])
  pm$pm_Efficiency  <- pm_infer(pm$pm_Efficiency, list(pm$pm_myvar1$y,pm$pm_myvar2$y,pm$pm_myvar3$y))
  pm$pm_profile  <- pm_infer(pm$pm_profile, list(pm$pm_myvar1$y,pm$pm_myvar2$y,pm$pm_myvar3$y))
  pm
}
my_glmp <- glmp(list(pm_myvar1 = pm_myvar1,
                     pm_myvar2 = pm_myvar2,
                     pm_myvar3 = pm_myvar3,
                     pm_Efficiency = pm_Efficiency,
                     pm_profile = pm_profile),
                glmp_method)
####################################################################
###################### Report Template definition ##################
####################################################################


report_no_data=function(var1,var2,var3){
  if((var1 == 0.00000) & (var2== 0.00000) & (var3 == 0.00000)){
    no_data= "There is no data available for this country."
  }else{
    no_data=1}
  return(no_data)
}

report_tail=function(var, nb_indicators){ #error in the function the rank is not displaying properly
  if(str_detect(pm_report(var), "tail")) {
    x = rank(myvars[[nb_indicators]])
    ctry_x=x[[i]]
    myperc_var=paste0(pm_report(var), ". Indeed, country is ranked ", ((length(myvars[[nb_indicators]])+1)-ctry_x), " over ", length(myvars[[nb_indicators]]), " (", round(var[["u"]], digits=3),") countries")
  } else {

    myperc_var=paste0(pm_report(var)," (", round(var[["u"]], digits=3),")")
  }
  return(myperc_var)
}

report_method <- function(properties,pm){

  ## specific case for no data at all ##

  no_data=report_no_data(pm$pm_myvar1$u,pm$pm_myvar2$u,pm$pm_myvar3$u)

  ## specific case for extreme values, include ranking of the country ##
  if(no_data==1){
    myperc_var1=report_tail(pm$pm_myvar1,1)
    myperc_var2=report_tail(pm$pm_myvar2,2)
    myperc_var3=report_tail(pm$pm_myvar3,3)

    paste (
      str_to_title(mycategory_label),mycategory_definition,".",
      pm_report(pm$pm_profile),
      pm_report(pm$pm_Efficiency),
      "The first dimension,",
      myperc_var1,
      " The second one,",
      myperc_var2,
      paste0(" Finally, the last selected indicator of ",mycategory_label),
      myperc_var3,
      ". Among the factors contributing to those performances we can mention [More from desks]",
      sep="")
  } #close if(no_data)
}

properties = NULL
my_report <- report_template(properties,report_method)

####################################################################
###################### LDCP definition #############################
####################################################################

my_ldcp <- ldcp(data=my_data,glmp=my_glmp,report=my_report)

