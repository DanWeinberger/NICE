analyze_case_func <- function(DownLoadLatest=F) {
  
  if(DownLoadLatest==T){
    maxage.set=0
  }else{
    maxage.set=99999
  }
  d1 <- runIfExpired(storeName='case_data',maxage=maxage.set ,f=
                     ~ read.delim('https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag.csv', sep=';') 
  )
  
  d1$Date_of_publication <- as.Date(d1$Date_of_publication)
  
  #Hospital_admission_notification: The number of new COVID-19 patients reported to the NICE registry that were admitted to the hospital as of the reporting date to the NICE registry [Date_of_statistics]. The number stated here can therefore relate to hospital admission dates in the past.
  #Hospital_admission: The number of COVID-19 patients reported to the NICE registry that were hospitalized on that date [Date_of_statistics].
  
  d1.agg <- aggregate(d1[,c('Total_reported'), drop=F], by=list('date'=d1$Date_of_publication), FUN=sum)
  
  #ut <- d1[d1$Municipality_name=="Utrecht",]
  #d1.agg <- aggregate(ut[,c('Total_reported'), drop=F], by=list('date'=ut$Date_of_publication), FUN=sum)
  
  #plot(d1.agg$Total_reported, type='l')
  d1.agg$roll_ave <- rollmean(d1.agg$Total_reported,7, na.pad=T, align='right')
  
  d1.agg$rr_7day <- NA
  d1.agg$roll_rr_7day <- NA
  d1.agg$roll_rr_3day <- NA
  
  for(i in 8: nrow(d1.agg)){
    d1.agg$rr_7day[i] <-d1.agg$Total_reported[i]/d1.agg$Total_reported[i-7]
    d1.agg$roll_rr_7day[i] <-d1.agg$roll_ave[i]/d1.agg$roll_ave[i-7]
    d1.agg$roll_rr_3day[i] <-d1.agg$roll_ave[i]/d1.agg$roll_ave[i-3]
    
  }
 return(d1.agg)
}
