#' @title select relevant variables
#' @description algorithm to select proper variables in
#' replacement of main variables when missing
#' @param n_rank1 the number of variables ranking 1
#' @param n_rank2 the number of variables ranking 2
#' @param n_rank3 the number of variables ranking 3
#' @param dt_non_na a dataset containing the data
#' @return tibble with the proper selection of
#' variables
#' @author  Federica Depace


get_vars<-function(n_rank1, n_rank2, n_rank3, dt_non_na,type_var){

  if(type_var=="outcomes"){
    newdf <- dt_non_na[with( dt_non_na,which(rank==1 )), ]
    if (n_rank1==3) { # there are 3 vars with rank 1, take the first ranked
      final<-newdf
    }else if (n_rank1==1 & n_rank2==0 & n_rank3==0) {
        temp1 <- dt_non_na[with( dt_non_na,which(rank==1)), ]
        final<-temp1 # take 3 ranked 3
    } else if (n_rank1==2 & n_rank2>=1 ) { # there are 2 vars with rank 1 and 1 or more rank 2, take the 2 first ranked +1 ranked 2
      temp <- dt_non_na[with( dt_non_na,which(rank==2 )), ]
      final<-rbind(newdf, temp[1,]) # take the first variable ranked 2 since they are indifferent
    } else if (n_rank1==2 & n_rank2==0 ) {
      temp <- dt_non_na[with( dt_non_na,which(rank==3 )), ]
      final<-rbind(newdf, temp[1,]) # take the first  variable ranked 3 since they are indifferent
    } else if (n_rank1==1 & n_rank2>1 ) {
      temp <- dt_non_na[with( dt_non_na,which(rank==2 )), ]
      final<-rbind(newdf, temp[1:2,]) # take the first 2 variable ranked 2 since they are indifferent
    } else if (n_rank1==1 & n_rank2==1 ) {
      temp <- dt_non_na[with( dt_non_na,which(rank==2 )), ]
      temp1 <- dt_non_na[with( dt_non_na,which(rank==3 )), ]
      final<-rbind(newdf, temp, temp1[1,]) # take 1 ranked 2 and 1 ranked 3
    } else if (n_rank1==1 & n_rank2==0 ) {
      temp <- dt_non_na[with( dt_non_na,which(rank==3 )), ]
      final<-rbind(newdf, temp1[1:2,]) # take 2 ranked 3
    } else if (n_rank1==0 & n_rank2==3 ) {
      temp <- dt_non_na[with( dt_non_na,which(rank==2 )), ]
      final<-temp # take 3 ranked 2
    } else if (n_rank1==0 & n_rank2==2 ) {
      temp <- dt_non_na[with( dt_non_na,which(rank==2 )), ]
      temp1 <- dt_non_na[with( dt_non_na,which(rank==3 )), ]
      final<-rbind(temp, temp1[1,]) # take 2 ranked 2 and 1 ranked 3
    } else if (n_rank1==0 & n_rank2==1 ) {
      temp <- dt_non_na[with( dt_non_na,which(rank==2 )), ]
      temp1 <- dt_non_na[with( dt_non_na,which(rank==3 )), ]
      final<-rbind(temp, temp1[1:2,]) # take 1 ranked 2 and 2 ranked 3
    } else if (n_rank1==0 & n_rank2==0 ) {
      temp1 <- dt_non_na[with( dt_non_na,which(rank==3 )), ]
      final<-temp1 # take 3 ranked 3
    }
  }else if(type_var=="policy"){
    newdf <- dt_non_na[with( dt_non_na,which(rank==1 )), ]

    if (n_rank1==5) { # there are 3 vars with rank 1, take the first ranked
      final<-newdf
    } else if (n_rank1==4 & n_rank2>=1 ) { # there are 4 vars with rank 1 and 1 or more rank 2, take the 2 first ranked +1 ranked 2
      temp <- dt_non_na[with( dt_non_na,which(rank==2 )), ]
      final<-rbind(newdf, temp[1,]) # take the first variable ranked 2 since they are indifferent
    } else if (n_rank1==4 & n_rank2==0 ) {
      temp <- dt_non_na[with( dt_non_na,which(rank==3 )), ]
      final<-rbind(newdf, temp[1,]) # take the first  variable ranked 3 since they are indifferent
    } else if (n_rank1==3 & n_rank2>=2 ) {# there are 3 vars with rank 1 and 2 or more rank 2
      temp <- dt_non_na[with( dt_non_na,which(rank==2 )), ]
      final<-rbind(newdf, temp[1:2,]) # take the first 2 variable ranked 2 since they are indifferent
    } else if (n_rank1==3 & n_rank2==1 ) {
      temp <- dt_non_na[with( dt_non_na,which(rank==2 )), ]
      temp1 <- dt_non_na[with( dt_non_na,which(rank==3 )), ]
      final<-rbind(newdf, temp, temp1[1,]) # take 1 ranked 2 and 1 ranked 3 + the 3 ranked 1 =5
    } else if (n_rank1==3 & n_rank2==0 ) {
      temp1 <- dt_non_na[with( dt_non_na,which(rank==3 )), ]
      final<-rbind(newdf, temp1[1:2,]) # take 2 ranked3 + the 3 ranked 1 =5
    } else if (n_rank1==2 & n_rank2>=3 ) {
      temp <- dt_non_na[with( dt_non_na,which(rank==2 )), ]
      final<-rbind(newdf, temp1[1:3,]) # take 3 ranked2 + 3 original ranked1 =5
    } else if (n_rank1==2 & n_rank2==2 ) {
      temp <- dt_non_na[with( dt_non_na,which(rank==2 )), ]
      temp1 <- dt_non_na[with( dt_non_na,which(rank==3 )), ]
      final<-rbind(newdf, temp, temp1[1,]) # take 2 ranked2 + 1 rank3 + 2 original ranked 1 =5
    } else if (n_rank1==2 & n_rank2==1 ) {
      temp <- dt_non_na[with( dt_non_na,which(rank==2 )), ]
      temp1 <- dt_non_na[with( dt_non_na,which(rank==3 )), ]
      final<-rbind(newdf, temp, temp1[1:2,]) # take 1 ranked2 + 2 rank3 + 2 original ranked 1 =5
    } else if (n_rank1==2 & n_rank2==0 ) {
      temp1 <- dt_non_na[with( dt_non_na,which(rank==3 )), ]
      final<-rbind(newdf, temp1[1:3,]) # take 3 rank3 + 2 original ranked 1 =5
    } else if (n_rank1==1 & n_rank2>=4 ) {
      temp <- dt_non_na[with( dt_non_na,which(rank==2 )), ]
      final<-rbind(newdf, temp1[1:4,]) # take 4 ranked2 + 1 original ranked1 =5
    }  else if (n_rank1==1 & n_rank2>=3 ) {
      temp <- dt_non_na[with( dt_non_na,which(rank==2 )), ]
      temp1 <- dt_non_na[with( dt_non_na,which(rank==3 )), ]
      final<-rbind(newdf, temp[1:3,], temp1[1,]) # take 3 ranked2 +1 rank3 +1 original ranked1 =5
    } else if (n_rank1==1 & n_rank2==2 ) {
      temp <- dt_non_na[with( dt_non_na,which(rank==2 )), ]
      temp1 <- dt_non_na[with( dt_non_na,which(rank==3 )), ]
      final<-rbind(newdf, temp, temp1[1:2,]) # take 2 ranked2 + 2 rank3 + 1 original ranked 1 =5
    } else if (n_rank1==1 & n_rank2==1 ) {
      temp <- dt_non_na[with( dt_non_na,which(rank==2 )), ]
      temp1 <- dt_non_na[with( dt_non_na,which(rank==3 )), ]
      final<-rbind(newdf, temp, temp1[1:3,]) # take 1 ranked2 + 3 rank3 + 1 original ranked1 =5
    } else if (n_rank1==1 & n_rank2==0 ) {
      temp1 <- dt_non_na[with( dt_non_na,which(rank==3 )), ]
      final<-rbind(newdf, temp1[1:4,]) # take 1 ranked2 + 2 rank3 + 2 original ranked 1 =5
    } else if (n_rank1==0 & n_rank2>=5 ) {
      temp <- dt_non_na[with( dt_non_na,which(rank==2 )), ]
      final<-rbind(temp[1:5,]) # take 5 ranked2
    } else if (n_rank1==0 & n_rank2==4 ) {
      temp <- dt_non_na[with( dt_non_na,which(rank==2 )), ]
      temp1 <- dt_non_na[with( dt_non_na,which(rank==3 )), ]
      final<-rbind(temp, temp1[1,]) # take 4 ranked2 and 1 ranked3
    } else if (n_rank1==0 & n_rank2==3 ) {
      temp <- dt_non_na[with( dt_non_na,which(rank==2 )), ]
      temp1 <- dt_non_na[with( dt_non_na,which(rank==3 )), ]
      final<-rbind(temp, temp1[1:2,]) # take 3 ranked2 and 2 ranked3
    } else if (n_rank1==0 & n_rank2==2 ) {
      temp <- dt_non_na[with( dt_non_na,which(rank==2 )), ]
      temp1 <- dt_non_na[with( dt_non_na,which(rank==3 )), ]
      final<-rbind(temp, temp1[1:3,]) # take 2 ranked2 and 3 ranked3 }
    } else if (n_rank1==0 & n_rank2==1 ) {
      temp <- dt_non_na[with( dt_non_na,which(rank==2 )), ]
      temp1 <- dt_non_na[with( dt_non_na,which(rank==3 )), ]
      final<-rbind(temp, temp1[1:4,]) # take 3 ranked2 and 2 ranked3
    } else if (n_rank1==0 & n_rank2==0 ) {
      temp1 <- dt_non_na[with( dt_non_na,which(rank==3 )), ]
      final<-temp1 # take all ranked3
    }

  }
}






