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

input1=dt

replace_value_na=0 #default value when missing data

non_num_vars=input1 %>% select_if(is.character)
x=3
myvars=lapply(2:dim(input1)[2],function(x){
  myvar <- c(base::t(input1[x]))
  myvar=myvar%>%replace_na(replace_value_na)
  myvar
})

mycategory_label=category
myvar1_label=vars_label[1]
myvar2_label=vars_label[2]
myvar3_label=vars_label[3]

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
paste0(" ",myvar1_label,", is not available for this country"),
paste0(" ",myvar1_label,", locates country in the bottom tail of the distribution"),
paste0(" ",myvar1_label,", is below OECD average"),
paste0(" ",myvar1_label,", is slightly below OECD average"),
paste0(" ",myvar1_label,", is close to the OECD average"),
paste0(" ",myvar1_label,", is slightly above OECD average"),
paste0(" ",myvar1_label,", is above OECD average"),
paste0(" ",myvar1_label,", locates country in the upper tail of the distribution")
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
  paste0(" ",myvar2_label,", is not available for this country"),
  paste0(" ",myvar2_label,", is in the bottom tail of the distribution"),
  paste0(" ",myvar2_label,", is below the OECD average"),
  paste0(" ",myvar2_label,", is slightly below the OECD average"),
  paste0(" ",myvar2_label,", is close to the OECD average"),
  paste0(" ",myvar2_label,", is slighly above the OECD average"),
  paste0(" ",myvar2_label,", is above the OECD average"),
  paste0(" ",myvar2_label,", is in the upper tail of the distribution")
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
  paste0(" ",myvar3_label,", is not available for this country"),
  paste0(" ",myvar3_label,",is in the bottom tail"),
  paste0(" ",myvar3_label,", is below the OECD average"),
  paste0(" ",myvar3_label,", is slighly below the OECD average"),
  paste0(" ",myvar3_label,", is close to the OECD average"),
  paste0(" ",myvar3_label,", is slighly above the OECD average"),
  paste0(" ",myvar3_label,", is above the OECD average"),
  paste0(" ",myvar3_label,", is in the upper tail")
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
    "The data coverage regarding housing for country is very scarce: only ",
    "The data coverage regarding housing for country is sarce: only ",
    "The data coverage regarding housing for country is scarce: ",
    "The data coverage regarding housing for country is relatively comprehensive: ",
    "The data coverage regarding housing for country is comprehensive: "
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
    paste0("With respect to ", mycategory_label," the overall performance is difficult to assess"),
    paste0("With respect to ", mycategory_label," efficiency, country displays similar relative performances among all three dimensions")
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
  x = rank(myvars[[1]])
  ctry_x=rank(myvars[[1]])[match(pm$pm_myvar1$u ,myvars[[1]])]

  myperc_var1=paste0(pm_report(pm$pm_myvar1), ". Indeed, country is ranked ", ((length(myvars[[1]])+1)-ctry_x), " over ", length(myvars[[1]]), " (", round(pm$pm_myvar1$u, digits=3),")")
  } else {
    myperc_var1=paste0(pm_report(pm$pm_myvar1)," (", round(pm$pm_myvar1$u, digits=3),")")
  }

 if(str_detect(pm_report(pm$pm_myvar2), "tail")) {
  x = rank(myvars[[2]])
  ctry_x=rank(myvars[[2]])[match(pm$pm_myvar2$u ,myvars[[2]])]

      if(str_detect(pm_report(pm$pm_myvar1), "tail")) {
      myperc_var2=paste0(str_split(pm_report(pm$pm_myvar2), "(?<=is)", 2, simplify=TRUE)[1], " also", str_split(pm_report(pm$pm_myvar2), "(?<=is)", 2, simplify=TRUE)[2], ". Indeed, country is ranked ", ((length(myvars[[2]])+1)-ctry_x), " over ", length(myvars[[2]]), " (", round(pm$pm_myvar2$u, digits=3),")")
      } else {
      myperc_var2=paste0(pm_report(pm$pm_myvar2), ". Indeed, country is ranked ", ((length(myvars[[2]])+1)-ctry_x), " over ", length(myvars[[2]]), " (", round(pm$pm_myvar2$u, digits=3),")")
  } }
  else {
  myperc_var2=paste0(pm_report(pm$pm_myvar2)," (", round(pm$pm_myvar2$u, digits=3),")")
  }

  if(str_detect(pm_report(pm$pm_myvar3), "tail")) {
  x = rank(myvars[[3]])
  ctry_x=rank(myvars[[3]])[match(pm$pm_myvar3$u ,myvars[[3]])]

    if(str_detect(pm_report(pm$pm_myvar2), "tail")) {
    myperc_var3=paste0(str_split(pm_report(pm$pm_myvar3), "(?<=is)", 2, simplify=TRUE)[1], " also", str_split(pm_report(pm$pm_myvar3), "(?<=is)", 2, simplify=TRUE)[2], ". Indeed, country is ranked ", ((length(myvars[[3]])+1)-ctry_x), " over ", length(myvars[[3]]), " (", round(pm$pm_myvar3$u, digits=3),")")
    } else {
    myperc_var3=paste0(pm_report(pm$pm_myvar3), ". Indeed, country is ranked ", ((length(myvars[[3]])+1)-ctry_x), " over ", length(myvars[[3]]), " (", round(pm$pm_myvar3$u, digits=3),")")
  } }
  else {
  myperc_var3=paste0(pm_report(pm$pm_myvar3)," (", round(pm$pm_myvar3$u, digits=3),")")
  }

  paste (
#pm_report(pm$pm_myvar4), round(pm$pm_myvar4$u*100, digits=2), " percent of the indicators are available. ",

pm_report(pm$pm_profile),
". ",

 ## no overall performance if heterogeneous ##
#if(str_detect(pm$pm_profile, "similar")) {
  pm_report(pm$pm_Efficiency),
#} else {
#},

"The first dimension,",
myperc_var1,
". The second one,",
myperc_var2,
paste0(". Finally, the last selected indicator of ",mycategory_label,","),
myperc_var3,
". Among the factors contributing to those performance we can mention [More from desks]",
 sep="")
} }

properties = NULL
my_report <- report_template(properties,report_method)

####################################################################
###################### LDCP definition #############################
####################################################################

my_ldcp <- ldcp(data=my_data,glmp=my_glmp,report=my_report)

