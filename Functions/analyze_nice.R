analyze_nice_func <- function(DownLoadLatest=F) {
  
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

d1.agg <- aggregate(d1[,c("Hospital_admission" ,'Hospital_admission_notification'), drop=F], by=list('date'=d1$Date_of_statistics), FUN=sum)
d1.agg$roll_ave <- rollmean(d1.agg$Hospital_admission,7, na.pad=T, align='right')
d1.agg$roll_rr_7day <- NA

d1.agg$rr_7day <- NA
rr.7day <- for(i in 8: nrow(d1.agg)){
  d1.agg$rr_7day[i] <-d1.agg$Hospital_admission[i]/d1.agg$Hospital_admission[i-7]
  d1.agg$roll_rr_7day[i] <-d1.agg$roll_ave[i]/d1.agg$roll_ave[i-7]
  
}

d1.agg$rr_7day[nrow(d1.agg):(nrow(d1.agg)-1 )] <-NA
d1.agg$roll_rr_7day[nrow(d1.agg):(nrow(d1.agg)-1 )] <-NA

return(d1.agg)
}

