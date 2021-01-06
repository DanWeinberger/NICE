# if (!require("devtools")) {
#   install.packages("devtools")
# }
# devtools::install_github("weinbergerlab/ExcessILI")
library(ExcessILI)
library(lubridate)
runIfExpired <- function(storeName, f, maxage=hours(0)) {
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



d1 <- runIfExpired('nice_data', 
                                ~ read.delim('https://data.rivm.nl/covid-19/COVID-19_ziekenhuisopnames.csv', sep=';') 
)

d1$Date_of_statistics <- as.Date(d1$Date_of_statistics)
#Hospital_admission_notification: The number of new COVID-19 patients reported to the NICE registry that were admitted to the hospital as of the reporting date to the NICE registry [Date_of_statistics]. The number stated here can therefore relate to hospital admission dates in the past.
#Hospital_admission: The number of COVID-19 patients reported to the NICE registry that were hospitalized on that date [Date_of_statistics].

d1.agg <- aggregate(d1[,c("Hospital_admission" ,'Hospital_admission_notification'), drop=F], by=list('date'=d1$Date_of_statistics), FUN=sum)

d1.agg$rr_7day <- NA
rr.7day <- for(i in 8: nrow(d1.agg)){
  d1.agg$rr_7day[i] <-d1.agg$Hospital_admission[i]/d1.agg$Hospital_admission[i-7]
}


plot(d1.agg$date, d1.agg$rr_7day, type='l', ylim=c(0.5, 2))
abline(h=c(0.6,0.8,1), lty=2, col='gray')

d1.agg$backfilling <- d1.agg$Hospital_admission_notification - d1.agg$Hospital_admission
d1.agg$backfilling.ratio <- d1.agg$Hospital_admission_notification / d1.agg$Hospital_admission

