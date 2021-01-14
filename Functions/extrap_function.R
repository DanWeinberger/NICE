extrap.func<-function(week.rr=0.8, ds, n.extrap=30){
  ds <- ds[!is.na(ds$roll_rr_7day),]
  extrap <- as.data.frame(matrix(NA,nrow=n.extrap, ncol=1))
  extrap$date <- seq.Date(from=(max(ds$date)+1) ,to=(max(ds$date)+n.extrap), by='day'  )
  
  extrap <- bind_rows(ds,extrap)
  
  extrap$extrap.cases0.8 <- extrap$stl.trend
  
  for(i in (nrow(ds)+1):nrow(extrap)){
    extrap$extrap.cases0.8[i] <-  extrap$extrap.cases0.8[i-7]*0.8
  }
  
  extrap$extrap.cases0.8[extrap$date<= max(ds$date)] <- NA
  return(extrap)
}
