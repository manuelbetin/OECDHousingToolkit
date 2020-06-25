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
input1 <- read.csv(file="htk_paragraphs.csv",
sep=";", dec=".",
header=TRUE)

####################################################################
###################### data definition #############################
####################################################################

#input1=htk_paragraphs

replace_value_na=0 #default value when missing data

myvars=lapply(2:dim(input1)[2],function(x){
  myvar <- c(base::t(input1[x]))
  myvar=myvar%>%replace_na(replace_value_na)
  myvar
})

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
cp_myvar1 <- cp("cp_myvar1", c("cp_error1", "myvar1_lowtail", "myvar1_low", "myvar1_slow", "myvar1_medium", "myvar1_shigh", "myvar1_high","myvar1_uptail"))
cp_myvar2 <- cp("cp_myvar2", c("cp_error2", "myvar2_lowtail", "myvar2_low", "myvar2_slow", "myvar2_medium", "myvar2_shigh", "myvar2_high","myvar2_uptail"))
cp_myvar3 <- cp("cp_myvar3", c("cp_error3", "myvar3_lowtail", "myvar3_low", "myvar3_slow", "myvar3_medium", "myvar3_shigh", "myvar3_high","myvar3_uptail"))
cp_myvar4 <- cp("cp_myvar4", c("cp_error4", "myvar4_verybad", "myvar4_bad", "myvar4_low", "myvar4_good", "myvar4_verygood"))
cp_profile <- cp("cp_Efficiency", c("Homogeneous","Heterogeneous"))
cp_Efficiency <- cp("cp_Efficiency", c("all_low","avg","all_high"))

####################################################################
###################### PMs definition ##############################
####################################################################

## myvar1
g_pm_myvar1 <- function(u,y){
 y$w <- degree_mf(fuzzy_partitions(
   triangle_mf(0, 0, 0),
   triangle_mf(0.00001, 0.1, 0.15),
   trapezoid_mf(0.15, 0.15, 0.35, 0.35) ,
   trapezoid_mf(0.35, 0.35, 0.4, 0.45),
   triangle_mf(0.4, 0.5, 0.6) ,
   trapezoid_mf(0.55, 0.55, 0.6, 0.65),
   trapezoid_mf(0.65, 0.65, 0.85, 0.85),
   triangle_mf(0.85, 0.95, 1)), u)
y }

t_pm_myvar1 <- function(y){
templates <- c(
" residential investment, is not available for this country",
" residential investment, locates country in the bottom tail of the distribution",
" residential investment, is below OECD average",
" residential investment, is slightly below OECD average",
" residential investment, is close to the OECD average",
" residential investment, is slightly above OECD average",
" residential investment, is above OECD average",
" residential investment, locates country in the upper tail of the distribution"
)
return(templates[which.max(y$w)])
}

pm_myvar1 <- rLDCP::pm(y=cp_myvar1, g=g_pm_myvar1, t=t_pm_myvar1)

####################################################################

## myvar2
g_pm_myvar2<- function(u,y){
 y$w <- degree_mf(fuzzy_partitions(
   triangle_mf(0, 0, 0),
   triangle_mf(0.0001, 0.1, 0.15),
   trapezoid_mf(0.15, 0.15, 0.35, 0.35) ,
   trapezoid_mf(0.35, 0.35, 0.4, 0.45),
   triangle_mf(0.4, 0.5, 0.6) ,
   trapezoid_mf(0.55, 0.6, 0.65, 0.65),
   trapezoid_mf(0.65, 0.65, 0.85, 0.85),
   triangle_mf(0.85, 0.9, 1)), u)
y }

t_pm_myvar2<- function(y){
templates <- c(
" house price volatility, is not available for this country",
" house price volatility, is in the bottom tail of the distribution",
" house price volatility, is below the OECD average",
" house price volatility, is slightly below the OECD average",
" house price volatility, is close to the OECD average",
" house price volatility, is slighly above the OECD average",
" house price volatility, is above the OECD average",
" house price volatility, is in the upper tail of the distribution"
)
return(templates[which.max(y$w)])
}

pm_myvar2 <- rLDCP::pm(y=cp_myvar2, g=g_pm_myvar2, t=t_pm_myvar2)

####################################################################

## myvar3
g_pm_myvar3<- function(u,y){
 y$w <- degree_mf(fuzzy_partitions(
    triangle_mf(0, 0, 0),
    triangle_mf(0.0001 , 0.1, 0.15),
    trapezoid_mf(0.15, 0.15, 0.35, 0.35) ,
    triangle_mf(0.4, 0.5, 0.6) ,
    trapezoid_mf(0.55, 0.55, 0.6, 0.65),
    trapezoid_mf(0.65, 0.65, 0.85, 0.85),
    triangle_mf(0.85, 0.95, 1)), u)
 y }

t_pm_myvar3<- function(y){
templates <- c(
" affordability, is not available for this country",  
" affordability, is in the bottom tail",
" affordability, is below the OECD average",
" affordability, is slighly below the OECD average",
" affordability, is close to the OECD average",
" affordability, is slighly above the OECD average",
" affordability, is above the OECD average",
" affordability, is in the upper tail"
)
return(templates[which.max(y$w)])
}

pm_myvar3 <- rLDCP::pm(y=cp_myvar3, g=g_pm_myvar3, t=t_pm_myvar3)

####################################################################

## myvar4
g_pm_myvar4<- function(u,y){
  y$w <- degree_mf(fuzzy_partitions(
    triangle_mf(0.0001 , 0.1, 0.2),
    triangle_mf(0.2, 0.3, 0.4),
    triangle_mf(0.4, 0.5, 0.6),
    triangle_mf(0.6, 0.7, 0.8),
    triangle_mf(0.8, 0.9, 1)), u)
 y }

t_pm_myvar4<- function(y){
  templates <- c(
    "The data coverage regarding housing for country is very bad: only ",  
    "The data coverage regarding housing for country is bad: only ",
    "The data coverage regarding housing for country is low: ",
    "The data coverage regarding housing for country is good: ",
    "The data coverage regarding housing for country is very good: "
  )
  return(templates[which.max(y$w)])
}

pm_myvar4 <- rLDCP::pm(y=cp_myvar4, g=g_pm_myvar4, t=t_pm_myvar4)

####################################################################

## overall performance

g_pm_Efficiency<- function(u,y){
operator <- operator(  min , max )
 y$w <- infer_rules(fuzzy_rules(

fuzzy_rule(0, 1, 1, 1, 0, 0, 0, 0,
           0, 1, 1, 1, 0, 0, 0, 0,
           0, 1, 1, 1, 0, 0, 0, 0,
           0, 1, 0, 0),
fuzzy_rule(0, 0, 0, 0, 0, 1, 1, 1,
           0, 0, 0, 0, 0, 1, 1, 1,
           0, 0, 0, 0, 0, 1, 1, 1,
           0, 0, 0, 1),
fuzzy_rule(0, 0, 0, 1, 1, 1 ,0, 0, 
           0, 0, 0, 1, 1, 1, 0, 0,
           0, 0, 0, 1, 1, 1, 0, 0,
           0, 0, 1, 0)

), operator,
list( u[[1]]$w, u[[2]]$w, u[[3]]$w ))
y }

t_pm_Efficiency<- function(y){
templates <- c(
" ERROR",  
" Generally speaking, its performance are among the bottom OECD ones",
" Overall, its performance are in line with the rest of OECD countries",
" The efficiency of the housing sector is globally among the top OECD performers"
)
return(templates[which.max(y$w)])
}

pm_Efficiency <- rLDCP::pm(y=cp_Efficiency, g=g_pm_Efficiency, t=t_pm_Efficiency)

####################################################################

## profile
g_pm_profile<- function(u,y){
  operator <- operator(  min , max )
  y$w <- infer_rules(fuzzy_rules(

    fuzzy_rule(1, 0, 0, 0, 0, 0, 0, 0,
               1, 0, 0, 0, 0, 0, 0, 0,
               1, 0, 0, 0, 0, 0, 0, 0,
               1, 0),
    fuzzy_rule(0, 1, 1, 1, 0, 0, 0, 0,
               0, 1, 1, 1, 0, 0, 0, 0,
               0, 1, 1, 1, 0, 0, 0, 0,
               0, 1),
    fuzzy_rule(0, 0, 0, 0, 0, 1, 1, 1,
               0, 0, 0, 0, 0, 1, 1, 1,
               0, 0, 0, 0, 0, 1, 1, 1,
               0, 1),
    fuzzy_rule(0, 0, 0, 1, 1, 1, 0, 0,
               0, 0, 0, 1, 1, 1, 0, 0,
               0, 0, 0, 1, 1, 1, 0, 0,
               0, 1)
    
  ), operator,
  list( u[[1]]$w, u[[2]]$w, u[[3]]$w ))
  y }

t_pm_profile<- function(y){
  templates <- c(
    "With respect to efficiency, country displays heterogeneous performances among all three dimensions",
    "With respect to efficiency, country displays homogeneous performances among all three dimensions"
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
pm$pm_myvar4  <- pm_infer(pm$pm_myvar4, input[4])
pm$pm_Efficiency  <- pm_infer(pm$pm_Efficiency, list(pm$pm_myvar1$y,pm$pm_myvar2$y,pm$pm_myvar3$y))
pm$pm_profile  <- pm_infer(pm$pm_profile, list(pm$pm_myvar1$y,pm$pm_myvar2$y,pm$pm_myvar3$y))
pm
}
my_glmp <- glmp(list(pm_myvar1 = pm_myvar1,
                     pm_myvar2 = pm_myvar2,
                     pm_myvar3 = pm_myvar3,
                     pm_myvar4 = pm_myvar4,
                     pm_Efficiency = pm_Efficiency,
                     pm_profile=pm_profile),
                glmp_method)

####################################################################
###################### Report Template definition ##################
####################################################################

report_method <- function(properties,pm){
  
  ## specific case for no data at all ##
  if((pm$pm_myvar1$u == 0.00000) & (pm$pm_myvar2$u == 0.00000) & (pm$pm_myvar3$u == 0.00000)){
    "There is no data available for this country."
  } else {
  
  ## specific case for extreme values, include ranking of the country ##
  if(str_detect(pm_report(pm$pm_myvar1), "tail")) {
  x = rank(myvar1)
  myperc_var1=paste0(pm_report(pm$pm_myvar1), ". Indeed, country is ranked ", ((length(myvar1)+1)-x[i]), " over ", length(myvar1), " (", round(pm$pm_myvar1$u, digits=3),")")
  } else {
    myperc_var1=paste0(pm_report(pm$pm_myvar1)," (", round(pm$pm_myvar1$u, digits=3),")")
  }
  
 if(str_detect(pm_report(pm$pm_myvar2), "tail")) {
  x = rank(myvar2)
      if(str_detect(pm_report(pm$pm_myvar1), "tail")) {
      myperc_var2=paste0(str_split(pm_report(pm$pm_myvar2), "(?<=is)", 2, simplify=TRUE)[1], " also", str_split(pm_report(pm$pm_myvar2), "(?<=is)", 2, simplify=TRUE)[2], ". Indeed, country is ranked ", ((length(myvar2)+1)-x[i]), " over ", length(myvar2), " (", round(pm$pm_myvar2$u, digits=3),")")
      } else {
      myperc_var2=paste0(pm_report(pm$pm_myvar2), ". Indeed, country is ranked ", ((length(myvar2)+1)-x[i]), " over ", length(myvar2), " (", round(pm$pm_myvar2$u, digits=3),")")
  } } 
  else {
  myperc_var2=paste0(pm_report(pm$pm_myvar2)," (", round(pm$pm_myvar2$u, digits=3),")")
  } 
  
  if(str_detect(pm_report(pm$pm_myvar3), "tail")) {
  x = rank(myvar3)
    if(str_detect(pm_report(pm$pm_myvar2), "tail")) {
    myperc_var3=paste0(str_split(pm_report(pm$pm_myvar3), "(?<=is)", 2, simplify=TRUE)[1], " also", str_split(pm_report(pm$pm_myvar3), "(?<=is)", 2, simplify=TRUE)[2], ". Indeed, country is ranked ", ((length(myvar3)+1)-x[i]), " over ", length(myvar3), " (", round(pm$pm_myvar3$u, digits=3),")")
    } else {
    myperc_var3=paste0(pm_report(pm$pm_myvar3), ". Indeed, country is ranked ", ((length(myvar3)+1)-x[i]), " over ", length(myvar3), " (", round(pm$pm_myvar3$u, digits=3),")")
  } } 
  else {
  myperc_var3=paste0(pm_report(pm$pm_myvar3)," (", round(pm$pm_myvar3$u, digits=3),")")
  } 
  
  paste (
pm_report(pm$pm_myvar4), round(pm$pm_myvar4$u*100, digits=2), "% of the indicators are available. ",

pm_report(pm$pm_profile),
". ",

 ## no overall performance if heterogeneous ##
if(str_detect(pm$pm_profile, "homogeneous")) {
  pm_report(pm$pm_Efficiency)
} else {
},

"The first dimension,",
myperc_var1, 
". The second one,",
myperc_var2,
". Finally, the last indicator of efficiency,",
myperc_var3,
".",
 sep="")
} }

properties = NULL
my_report <- report_template(properties,report_method)

####################################################################
###################### LDCP definition #############################
####################################################################

my_ldcp <- ldcp(data=my_data,glmp=my_glmp,report=my_report)

