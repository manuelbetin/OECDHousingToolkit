get_vars<-function(n_rank1, n_rank2, n_rank3, dt_non_na ){
  newdf <- dt_non_na[with( dt_non_na,which(rank==1 )), ]

  if (n_rank1==3) { # there are 3 vars with rank 1, take the first ranked
    final<-newdf
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
    temp1 <- dt_non_na[with( dt_non_na,which(rank==2 )), ]
    final<-rbind(temp, temp1[1,]) # take 2 ranked 2 and 1 ranked 3
  } else if (n_rank1==0 & n_rank2==1 ) {
    temp <- dt_non_na[with( dt_non_na,which(rank==2 )), ]
    temp1 <- dt_non_na[with( dt_non_na,which(rank==2 )), ]
    final<-rbind(temp, temp1[1:2,]) # take 1 ranked 2 and 2 ranked 3
  } else if (n_rank1==0 & n_rank2==0 ) {
    temp1 <- dt_non_na[with( dt_non_na,which(rank==3 )), ]
    final<-temp1 # take 3 ranked 3
  }

}






