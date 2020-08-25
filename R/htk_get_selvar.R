htk_get_selvar=function(sel,catalogue,mycategory,type, title=NULL){

  #' @title Dataset for the description of indicators table
  #' @descriptionDataset dynamic dataset collecting the vars
  #' displayed in the fiche
  #' @param sel the object of the CYC figure
  if (type=="outcome"){

    if (length(sel)!=0){
      sel %>% select(main_v)
      sel<-c(sel$main_v)
    }
    dt_table_1<- catalogue %>% filter(category==mycategory) %>%
      select(variable, variable_name, variable_desc)
    dt_table_1<-dt_table_1[which(dt_table_1$variable %in% sel), ]

  }
  else if (type=="policy"){

    if (length(sel)!=0){
      sel<-sel$var_codes
    }
    dt_table_1<- catalogue %>% filter(type=="policy") %>%
      select(variable, variable_name, variable_desc)
    dt_table_1<-dt_table_1[which(dt_table_1$variable %in% sel), ]
    rownames(dt_table_1)=dt_table_1[,1]
    dt_table_1<-dt_table_1[match(sel, rownames(dt_table_1)),]

  }

  return(dt_table_1)
}
