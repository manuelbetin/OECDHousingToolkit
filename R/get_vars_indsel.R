#' @title select relevant variables based on new selection criteria
#' @description algorithm to select proper variables in
#' replacement of main variables when missing
#' @param dt_non_na a dataset containing the data non missing
#' @param type_var if policy or outcome variables 
#' @return tibble with the proper selection of
#' variables
#' @author  Federica Depace

get_vars_indsel<-function(dt_non_na,type_var) {
# for each group: A, B, C 
myvec<-c("A","B","C","J")
if(type_var=="outcomes"){
  
  for (i in myvec) {
    for (n in 1:3) {
      name=paste0('n_rank',n, i)
     # print(name)
      assign(name, nrow(dt_non_na[with( dt_non_na,which(rank==n & Indicator==i)), ]))
    }
  }
  
  for (i in myvec) {
    name1=paste0('n_rank1', i)
    name2=paste0('n_rank2', i)
    name3=paste0('n_rank3', i)
    name_dt= paste0('newdf_', i)
  
    if (get(name1)==1) {
      assign( name_dt , dt_non_na[with( dt_non_na,which(rank==1 & Indicator==i )), ])
    } else if (get(name1) ==0 & get(name2) ==1) {
      assign( name_dt, dt_non_na[with( dt_non_na,which(rank==2 & Indicator==i )), ] )
    } else if (get(name1) ==0 & get(name2) ==0  & get(name3) ==1) {
      assign(name_dt, dt_non_na[with( dt_non_na,which(rank==3  & Indicator==i )), ])
    } else if (get(name1) ==0 & get(name2) ==0  & get(name3) ==0 & n_rank2J==1)  {
      assign(name_dt, dt_non_na[with( dt_non_na,which(rank==2  & Indicator=="J" )), ])
    } else if (get(name1) ==0 & get(name2) ==0  & get(name3) ==0 & n_rank2J==0  & n_rank3J==1) {
      assign(name_dt,dt_non_na[with( dt_non_na,which(rank==3  & Indicator=="J" )), ])
    } 
  }
    
    newdf<-rbind(newdf_A, newdf_B, newdf_C)
}
  return(newdf)
}
  
  
  
  
  