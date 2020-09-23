htk_policyradar=function(mydata,ranking, ctry, var_codes,  title=NULL){

  #' @title Radar chart for policy variables
  #' @description Radar chart to produce the spider graph
  #' to display the selected policy variables
  #' @param data a dataframe with all the policy variables
  #' columns and iso3 codes in rows.
  #' @param ranking a dataframe with the ranking of the policy variables
  #' @param ctry the iso3 code for the selected country
  #' @param var_codes a dataframe with the the code of the policy variables
  #' @param title an optional title for the figure
  #' @return fmsl radarchart object
  #' @author Manuel Betin
  #' @export

  #1 prepare the data according to data availability

# mydata=dt_pol
# ranking=ranking_pol
# var_codes=var_codes_pol
# ctry=params$ctry_code
# type_var="policy"

  vars_needed_plus=prep_data_indsel(mydata,ranking,ctry,var_codes,type_var="policy")

  var_codes=vars_needed_plus$var_codes
  var_names=vars_needed_plus$var_names
  vars_needed_plus=vars_needed_plus$data

 if(length(var_codes)>2){
  # 2. create min, max, mean, value

  #find the min and max countries for each variable
  for (var in var_codes) {
    name_col_min=paste0(var, '_country_min')
    name_col_max=paste0(var, '_country_max')
    vars_needed_plus<- vars_needed_plus %>%
      mutate(!!name_col_min := vars_needed_plus$Iso_code3[which.min(get(var))],
             !!name_col_max := vars_needed_plus$Iso_code3[which.max(get(var))] )
  }

  #subsample only the target country
  temp_long<- vars_needed_plus %>%  filter(Iso_code3==ctry)%>% dplyr::select(-Iso_code3) %>%
    gather(key = "variable", value = "value")

  #clean the names of variables
  temp_long<-temp_long %>%
    mutate(main_v= ifelse(str_detect(variable, var_codes[1]), var_codes[1], NA))
  for (var in var_codes[2:length(var_codes)]){
    temp_long<-temp_long %>%
      mutate(main_v= ifelse(str_detect(variable, var), var, main_v))
  }

  #create columns to identify the type of statistic of each row
  temp_long<-temp_long %>%
    mutate(ext=ifelse( (str_detect(variable, "mean")), "mean", "value"),
           ext=ifelse( (str_detect(variable, "min")), "min", ext),
           ext=ifelse( (str_detect(variable, "max")), "max", ext),
           ext=ifelse( (str_detect(variable, "country_max")), "country_max", ext),
           ext=ifelse( (str_detect(variable, "country_min")), "country_min", ext),
           ext=ifelse( (str_detect(variable, "rank")), "rank", ext)) %>%
    dplyr::select(-variable)

  #reshape the final dataset used for the graph
  final<-reshape(temp_long, idvar = "main_v" , timevar =  "ext" , direction = "wide")
  name_vars<-c("value.value","value.mean","value.min","value.max", "value.rank")

  #find and organise values for OECD as a whole
  OECD<- vars_needed_plus %>%  filter(Iso_code3=="OECD")
  OECD<-OECD  %>%   dplyr::select( contains('rank', ignore.case = TRUE))

  OECD_final<-data.frame(t(OECD)) %>%
    rename(OECD=t.OECD.)

  OECD_final=OECD_final%>%
    mutate(main_v=rownames(OECD_final))

  OECD_final=OECD_final%>%
    mutate(main_v=gsub("_rank","",OECD_final$main_v, fixed = T)) %>%
    dplyr::select(main_v,OECD)

  #join data for the selected country and for the OECD
  final<-merge(final, OECD_final, by="main_v")
  final[name_vars] <- sapply(final[name_vars],as.numeric)

  final_t<-data.frame(t(final))
  names(final_t) <- final$main_v
  final_t <- final_t[-1,]

  rownames(final_t)[5]=ctry
  #only keep relevant lines: value of the country and rank of OECD
  names_sel<-c(ctry,  "OECD" )
  final_t1<- subset(final_t, rownames(final_t) %in% names_sel)
  data=data.frame(final_t1)
  as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
  data[var_codes] <- sapply(data[var_codes], as.numeric.factor)

  #set the upper and lower bound for the figure
  min=rep(0,5)
  max=rep(1,5)
  data=rbind(max,min,data)
  # sort columns as in var_codes
  data <- data[,match(var_codes, colnames(data))]

  rownames(data)[1:2]<-c("max", "min")

  # set colors
  colors=c(rgb(1,0,0,0.3),rgb(0,0,1,0.5))
  colors_leg=c(rgb(1,0,0,1),rgb(0,0,1,0.5))
  colnames(data)=var_names

  # plot the radar chart
  radarchart( data[,c(1, length(data):2)]  , axistype=4 ,
              title="",
              seg=5,
              #custom polygon
              pcol=colors_leg ,
              #pfcol=colors ,
              #plwd=1 , plty=1,pty=16,
              #custom the grid
              cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1.2,.2),calcex=0.5, cglwd=1,
              #custom labels : check here
              vlcex=0.4
  ) %>%
    legend(x=0.85,y=0,
           legend=rownames(data)[3:4],fill=colors_leg,col=colors_leg,
           bty = "n" ,text.col = colors_leg, cex=0.5, horiz = F,merge = F)
  # }
 } else{
   myctry=countrycode::countrycode(ctry,origin="iso3c",destination="country.name")
   ggplot()+
     geom_text(aes(x=10,y=10,label=paste0(myctry, " has no data available for this dimension")))+
     geom_point(aes(x=c(0,20),y=c(0,20)),color="white")+
     theme_void()
 }
}
