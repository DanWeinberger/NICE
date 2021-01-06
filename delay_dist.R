library(reshape2)
d1a <- readRDS('./Data/nice_data/2021_01_03_15_35.rds')
d1b <- readRDS('./Data/nice_data/2021_01_04_15_44.rds')
d1c <- readRDS('./Data/nice_data/2021_01_05_15_25.rds')
d1d <- readRDS('./Data/nice_data/2021_01_06_15_30.rds')

d2 <- rbind.data.frame(d1a,d1b, d1c, d1d)
d2$days_ago <- as.Date(d2$Date_of_report) - as.Date(d2$Date_of_statistics) 


d2.m <- melt(d2[,c("Hospital_admission","Date_of_statistics" , "days_ago")], id.vars=c("Date_of_statistics" , "days_ago"))
d2.c <- dcast(d2.m, Date_of_statistics ~ days_ago, fun.aggregate = sum, fill=9999)
d2.c[,-1][d2.c[,-1]==9999] <- NA 

dates <- d2.c$Date_of_statistics
delay.mat <- d2.c[,-1]

delay.mat <- t(apply(delay.mat, 1, function(x){
  y <- x
  for(n in 2:(length(x))){
    if( !is.na(x[n-1]) & !is.na(x[n]) ){
      y[n] <- x[n] - x[(n-1)]
    }
  }
  return(y)
  
}
))
delay.mat[delay.mat<0] <- 0


tot_date <- cbind.data.frame('date'=dates, 'N.hosp'=apply(delay.mat,1, sum, na.rm=T) )
pct.tot.date <- apply(delay.mat,2, function(x) x/tot_date$N.hosp) #reading across the rows, shows what proportion of the total that have been reported are reported on each day. Reay across the rows

