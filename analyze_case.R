library(ExcessILI)
library(lubridate)
library(zoo)
runIfExpired <- function(storeName, f, maxage=hours(9999)) {
  basepath <- "Data/"
  mostRecent <- mostRecentTimestamp(storeName, basepath=basepath)
  f <- rlang::as_function(f)
  
  runAndArchive <- function() {
    data <- f()
    storeRDS(data, storeName, basepath)
    data
  }
  
  if (is.na(mostRecent)) 
    return(runAndArchive())
  if (mostRecent %--% now() < maxage)
    return(retrieveRDS(storeName, basepath))
  runAndArchive()
}



d1 <- runIfExpired('case_data', 
                   ~ read.delim('https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_per_dag.csv', sep=';') 
)

d1$Date_of_publication <- as.Date(d1$Date_of_publication)

#Hospital_admission_notification: The number of new COVID-19 patients reported to the NICE registry that were admitted to the hospital as of the reporting date to the NICE registry [Date_of_statistics]. The number stated here can therefore relate to hospital admission dates in the past.
#Hospital_admission: The number of COVID-19 patients reported to the NICE registry that were hospitalized on that date [Date_of_statistics].

d1.agg <- aggregate(d1[,c('Total_reported'), drop=F], by=list('date'=d1$Date_of_publication), FUN=sum)

#ut <- d1[d1$Municipality_name=="Utrecht",]
#d1.agg <- aggregate(ut[,c('Total_reported'), drop=F], by=list('date'=ut$Date_of_publication), FUN=sum)

plot(d1.agg$Total_reported, type='l')
d1.agg$roll_ave <- rollmean(d1.agg$Total_reported,7, na.pad=T, align='right')

d1.agg$rr_7day <- NA
d1.agg$roll_rr_7day <- NA
d1.agg$roll_rr_3day <- NA

for(i in 8: nrow(d1.agg)){
  d1.agg$rr_7day[i] <-d1.agg$Total_reported[i]/d1.agg$Total_reported[i-7]
  d1.agg$roll_rr_7day[i] <-d1.agg$roll_ave[i]/d1.agg$roll_ave[i-7]
  d1.agg$roll_rr_3day[i] <-d1.agg$roll_ave[i]/d1.agg$roll_ave[i-3]
  
}

plot(d1.agg$date, d1.agg$rr_7day, type='l', ylim=c(0.5, 2), col='gray')
points(d1.agg$date, d1.agg$roll_rr_7day, type='l')
abline(h=c(0.6,0.8,1), lty=2, col='gray')
abline(v=as.Date('2020-12-18'), lty=2)
abline(v=as.Date('2020-03-25'), lty=2)

#3 day RR (gives view of short term shifts)
# plot(d1.agg$date, d1.agg$roll_rr_3day, type='l', ylim=c(0.75, 2), col='black')
# abline(h=c(0.8, 0.9,1), lty=2, col='gray')
# abline(v=as.Date('2020-12-18'), lty=2)
# abline(v=as.Date('2020-04-01'), lty=2)


ts1 <- ts(log(d1.agg$Total_reported+0.5), frequency=7)
stl.date <- d1.agg$date

stl7 <- stl(ts1, s.window=7)
plot(d1.agg$date,exp(stl7$time.series[,'trend']), type='l')
points(d1.agg$date, d1.agg$Total_reported, type='l', col=rgb(0,0,0,alpha=0.2))
abline(v=as.Date('2020-12-18'), lty=2)
abline(v=as.Date('2020-03-25'), lty=2)


rr.smooth <- rep(NA, length(ts1))
rr.smooth1 <- rep(NA, length(ts1))

for(i in 8: length(ts1)){
  rr.smooth[i] <- exp(stl7$time.series[i,'trend'])/exp(stl7$time.series[i-7,'trend'])
  rr.smooth1[i] <- exp(stl7$time.series[i,'trend'])/exp(stl7$time.series[i-1,'trend'])
  
}
plot(stl.date,rr.smooth, type='l', ylim=c(0.6, 1.5))
abline(h=c(0.8,0.9,1), col='gray', lty=c(3,3,1))

#plot(stl.date,rr.smooth1, type='l', ylim=c(0.9, 1.1))
#abline(h=c(0.9,0.95,1), col='gray', lty=c(3,3,1))

