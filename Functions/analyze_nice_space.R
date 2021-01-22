analyze_nice_space_func <- function(DownLoadLatest=F) {
  
  if(DownLoadLatest==T){
    maxage.set=0
  }else{
    maxage.set=99999
  }

d1 <- runIfExpired('nice_data', maxage=maxage.set ,f=
                                ~ read.delim('https://data.rivm.nl/covid-19/COVID-19_ziekenhuisopnames.csv', sep=';') 
)

d1$Date_of_statistics <- as.Date(d1$Date_of_statistics)
#Hospital_admission_notification: The number of new COVID-19 patients reported to the NICE registry that were admitted to the hospital as of the reporting date to the NICE registry [Date_of_statistics]. The number stated here can therefore relate to hospital admission dates in the past.
#Hospital_admission: The number of COVID-19 patients reported to the NICE registry that were hospitalized on that date [Date_of_statistics].
d1$period <-NA
d1$period[d1$Date_of_statistics<as.Date('2020-06-01') ] <- 0

d1$period[d1$Date_of_statistics>=as.Date('2020-09-01') &d1$Date_of_statistics<=as.Date('2020-11-30')  ] <- 1
d1$period[d1$Date_of_statistics>=as.Date('2020-12-01')  ] <- 2

d1.m <- melt(d1[,c("Hospital_admission" ,'Municipality_name','period')], id.vars=c('Municipality_name','period'))
d1.c <-dcast(d1.m,  Municipality_name~period, fun.aggregate = sum)

d1.c.long <-dcast(d1.m,  Municipality_name+period ~., fun.aggregate = sum)
d1.c.long$days[d1.c.long$period==0] <- as.Date('2020-05-31')- min(d1$Date_of_statistics, na.rm=T)
d1.c.long$days[d1.c.long$period==1] <- as.Date('2020-11-30')- as.Date('2020-09-01')
d1.c.long$days[d1.c.long$period==2] <- max(d1$Date_of_statistics, na.rm=T)- as.Date('2020-12-01')
names(d1.c.long) <- c('muni','period','hosp','days')
d1.c.long <- d1.c.long[d1.c.long$muni != '' & !is.na(d1.c.long$hosp) & d1.c.long$period %in% c(0,1,2),]

d1.m.wk <- melt(d1[,c("Hospital_admission" ,'Municipality_name','Date_of_statistics')], id.vars=c('Municipality_name','Date_of_statistics'))
d1.c.wk <-dcast(d1.m.wk,  Date_of_statistics~Municipality_name, fun.aggregate = sum)


outlist <- list('d1.c.long'=d1.c.long, 'd1.c'=d1.c, 'd1.c.wk'=d1.c.wk)
return(outlist)
}

##STL Smoothing
stl.func <- function(ts.vec,ts.date, smooth.window=14){
  ts1 <- ts(sqrt(ts.vec[1:(length(ts.vec)-1)]), frequency=7)
  stl.date <- ts.date[1:(length(ts.date)-1)]
  stl7 <- stl(ts1, s.window=7, t.window=smooth.window)
  stl.trend <- c(exp(stl7$time.series[,'trend']),NA)
  return(stl.trend)
}

#7 day rr of smooth
rr.smooth.func <- function(smooth.ts){
  rr.smooth <- rep(NA, length(smooth.ts))
  for(i in 8: length(smooth.ts)){
    rr.smooth[i] <- smooth.ts[i]/smooth.ts[i-7]
  }
  return(rr.smooth)
}

#7 day move ave
smooth.ave.func <- function(ts.vec){
  smooth.ts.vec <- rep(NA, length(ts.vec))
  for(i in 7: length(smooth.ts.vec)){
    smooth.ts.vec[i] <- mean(ts.vec[i:(i-6)])
  }
  return(smooth.ts.vec)
}


