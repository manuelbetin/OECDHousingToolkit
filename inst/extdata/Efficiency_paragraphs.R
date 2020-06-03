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

myvar1 <- c(t(input1[2]))

 if( min(myvar1) < 0)
stop("Invalid min value in data structure: NA.")
 if( max(myvar1) > 1)
stop("Invalid max value in data structure: myvar1.")

myvar2 <- c(t(input1[3]))

 if( min(myvar2) < 0)
stop("Invalid min value in data structure: NA.")
 if( max(myvar2) > 1)
stop("Invalid max value in data structure: myvar2.")

myvar3 <- c(t(input1[4]))

 if( min(myvar3) < 0)
stop("Invalid min value in data structure: NA.")
 if( max(myvar3) > 1)
stop("Invalid max value in data structure: myvar3.")
rm(input1)

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
cp_myvar1 <- cp("cp_myvar1", c("myvar1_lowtail","myvar1_low", "myvar1_medium", "myvar1_high","myvar1_uptail"))
cp_myvar2 <- cp("cp_myvar2", c("myvar2_lowtail","myvar2_low", "myvar2_medium", "myvar2_high","myvar2_uptail"))
cp_myvar3 <- cp("cp_myvar3", c("myvar3_lowtail","myvar3_low", "myvar3_medium", "myvar3_high","myvar3_uptail"))
cp_profile <- cp("cp_Efficiency", c("Homogeneous","Heterogeneous"))
cp_Efficiency <- cp("cp_Efficiency", c("all_low","avg","all_high"))

####################################################################
###################### PMs definition ##############################
####################################################################

g_pm_myvar1<- function(u,y){
 y$w <- degree_mf(fuzzy_partitions(
triangle_mf(0, 0.1, 0.15),
trapezoid_mf(0.15, 0.15, 0.4, 0.45) ,
triangle_mf(0.4, 0.5, 0.6) ,
trapezoid_mf(0.55, 0.6, 0.85, 0.85),
triangle_mf(0.85, 0.95, 1)), u)
y }

t_pm_myvar1<- function(y){
templates <- c(
" residential investment locate country in the bottom tail of the distribution ",
" residential investment is below OECD average ",
" residential investment is close to the OECD average ",
" residential investment is above OECD average ",
" residential investment locate country in the upper tail of the distribution "
)
return(templates[which.max(y$w)])
}

pm_myvar1 <- pm(y=cp_myvar1, g=g_pm_myvar1, t=t_pm_myvar1)

g_pm_myvar2<- function(u,y){
 y$w <- degree_mf(fuzzy_partitions(
   triangle_mf(0, 0.1, 0.15),
   trapezoid_mf(0.15, 0.15, 0.4, 0.45) ,
   triangle_mf(0.4, 0.5, 0.6) ,
   trapezoid_mf(0.55, 0.6, 0.85, 0.85),
   triangle_mf(0.85, 0.95, 1)), u)
y }

t_pm_myvar2<- function(y){
templates <- c(
" house price volatility, in the bottom tail of the distribution ",
" house price volatility, is below the OECD average",
" house price volatility, is close to the OECD average",
" house price volatility, is above the OECD average",
" house price volatility, in the upper tail of the distribution "
)
return(templates[which.max(y$w)])
}

pm_myvar2 <- pm(y=cp_myvar2, g=g_pm_myvar2, t=t_pm_myvar2)

g_pm_myvar3<- function(u,y){
 y$w <- degree_mf(fuzzy_partitions(
   triangle_mf(0, 0.1, 0.15),
   trapezoid_mf(0.15, 0.15, 0.4, 0.45) ,
   triangle_mf(0.4, 0.5, 0.6) ,
   trapezoid_mf(0.55, 0.6, 0.85, 0.85),
   triangle_mf(0.85, 0.95, 1)), u)
y }

t_pm_myvar3<- function(y){
templates <- c(
" Affordability in the bottom tail ",
" Affordability is below the OECD average",
" Affordability is close to the OECD average",
" Affordability is above the OECD average",
" Affordability  in the upper tail "
)
return(templates[which.max(y$w)])
}

pm_myvar3 <- pm(y=cp_myvar3, g=g_pm_myvar3, t=t_pm_myvar3)

g_pm_Efficiency<- function(u,y){
operator <- operator(  min , max )
 y$w <- infer_rules(fuzzy_rules(


fuzzy_rule(1, 1, 0, 0, 0,
           1, 1, 0, 0, 0,
           1, 1, 0, 0, 0,
           1, 0, 0),
fuzzy_rule(0, 0, 0, 1, 1,
           0, 0, 0, 1, 1,
           0, 0, 0, 1, 1,
           0, 0, 1),
fuzzy_rule(0, 1, 1 ,1 ,0,
           0, 1, 1 ,1 ,0,
           0, 1, 1 ,1 ,0,
           0, 1, 0)
), operator,
list( u[[1]]$w, u[[2]]$w, u[[3]]$w ))
y }

t_pm_Efficiency<- function(y){
templates <- c(
"generally speaking its performance are among the bottom OECD performance ",
"overall its performance are in line with the rest of OECD countries",
"the efficiency of the housing sector is globally among the top OECD performers"
)
return(templates[which.max(y$w)])
}

pm_Efficiency <- pm(y=cp_Efficiency, g=g_pm_Efficiency, t=t_pm_Efficiency)

g_pm_profile<- function(u,y){
  operator <- operator(  min , max )
  y$w <- infer_rules(fuzzy_rules(

    fuzzy_rule(1, 1, 0, 0, 0,
               1, 1, 0, 0, 0,
               1, 1, 0, 0, 0,
               1, 0),
    fuzzy_rule(0, 0, 0, 1, 1,
               0, 0, 0, 1, 1,
               0, 0, 0, 1, 1,
               1, 0),
    fuzzy_rule(0, 1, 1 ,1 ,0,
               0, 1, 1 ,1 ,0,
               0, 1, 1 ,1 ,0,
               1, 0 ),
    fuzzy_rule(0, 1, 1 ,1 ,0,
               0, 1, 1 ,1 ,0,
               0, 1, 1 ,1 ,0,
               1, 0 ),
    fuzzy_rule(1, 0, 0 ,0 ,1,
               1, 0, 0 ,0 ,1,
               1, 0, 0 ,0 ,1,
               0, 1),
    fuzzy_rule(1, 1, 0 ,1 ,1,
               1, 1, 0 ,1 ,1,
               1, 1, 0 ,1 ,1,
               0, 1)
  ), operator,
  list( u[[1]]$w, u[[2]]$w, u[[3]]$w ))
  y }

t_pm_profile<- function(y){
  templates <- c(
    "With respect to efficiency, country displays homegeneous performances among all three dimensions",
    "With respect to efficiency, country displays homegeneous performances among all three dimensions of efficiency"
  )
  return(templates[which.max(y$w)])
}

pm_profile <- pm(y=cp_profile, g=g_pm_profile, t=t_pm_profile)

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
                     pm_profile=pm_profile),
                glmp_method)
####################################################################
###################### Report Template definition ##################
####################################################################

report_method <- function(properties,pm){
paste (
pm_report(pm$pm_profile),
",",
pm_report(pm$pm_Efficiency),
". The first dimension,",
pm_report(pm$pm_myvar1)," (", pm$pm_myvar1$u,")",
". The second indicator of effiency, ",
pm_report(pm$pm_myvar2), " (", pm$pm_myvar2$u,")",
". Finally the last dimension,",
pm_report(pm$pm_myvar3), " (", pm$pm_myvar3$u,")",
 sep="")}
properties = NULL
my_report <- report_template(properties,report_method)
####################################################################
###################### LDCP definition #############################
####################################################################

my_ldcp <- ldcp(data=my_data,glmp=my_glmp,report=my_report)
for(i in 1:length(myvar1)){
 current_input <- c(myvar1[i],myvar2[i],myvar3[i])
my_ldcp <- ldcp_run(ldcp=my_ldcp,input=current_input)
cat("Input: c(myvar1,myvar2,myvar3), c(", paste(current_input, collapse=",", sep=""), ")", sep="", fill = TRUE)
cat("Output: ",my_ldcp$report$description,fill = TRUE)
cat(fill = TRUE)
}
