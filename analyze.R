# if (!require("devtools")) {
#   install.packages("devtools")
# }
# devtools::install_github("weinbergerlab/ExcessILI")
library(ExcessILI)
library(lubridate)
library(zoo)
library(rjags)
source('./functions/refresh_data.R')
source('./functions/jags_negbin4.R')

d1 <- runIfExpired('nice_data', maxage=9999,
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

plot(d1.agg$date, d1.agg$rr_7day, type='l', ylim=c(0.5, 2), col='gray')
points(d1.agg$date, d1.agg$roll_rr_7day, type='l')
abline(h=c(0.6,0.8,1), lty=2, col='gray')
abline(v=as.Date('2020-10-12'), lty=2)
abline(v=as.Date('2020-12-18'), lty=2)
abline(v=as.Date('2020-03-25'), lty=2)

d1.agg$backfilling <- d1.agg$Hospital_admission_notification - d1.agg$Hospital_admission
d1.agg$backfilling.ratio <- d1.agg$Hospital_admission_notification / d1.agg$Hospital_admission

ts1 <- ts(log(d1.agg$Hospital_admission[1:(nrow(d1.agg)-1)]+0.5), frequency=7)
stl.date <- d1.agg$date[1:(nrow(d1.agg)-1)]
stl7 <- stl(ts1, s.window=7)
matplot(stl.date,exp(stl7$time.series[,'trend']), type='l')
points(stl.date, d1.agg$Hospital_admission[1:(nrow(d1.agg)-1)], type='l', col=rgb(0,0,0,alpha=0.2))
abline(v=as.Date('2020-12-18'), lty=2)
abline(v=as.Date('2020-03-25'), lty=2)


rr.smooth <- rep(NA, length(ts1))
for(i in 8: length(ts1)){
  rr.smooth[i] <- exp(stl7$time.series[i,'trend'])/exp(stl7$time.series[i-7,'trend'])
}
plot(stl.date,rr.smooth, type='l', ylim=c(0.6, 1.5))
abline(h=c(0.7,0.8,0.9,1), col='gray', lty=c(3,3,1))
