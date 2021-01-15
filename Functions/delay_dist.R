NoBbsFunc <- function(){
file_list <- list.files(path="./Data/nice_data/")

all.ds <- lapply(file_list, function(x) readRDS(paste0( "./Data/nice_data/",x )) )

d2 <- do.call('rbind.data.frame', all.ds)
d2 <- unique(d2)
d2$days_ago <- as.Date(d2$Date_of_report) - as.Date(d2$Date_of_statistics) 


d2.m <- melt(d2[,c("Hospital_admission","Date_of_statistics" , "days_ago")], id.vars=c("Date_of_statistics" , "days_ago"))
d2.c <- dcast(d2.m, Date_of_statistics ~ days_ago, fun.aggregate = sum, fill=9999)
d2.c[,-1][d2.c[,-1]==9999] <- NA 

dates <- as.Date(d2.c$Date_of_statistics)
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


###########
###NoBbs model
###########
mat.pre.func <- function(x){
  total_obs <- sum(x, na.rm=T)
  first.obs.t <- which(!is.na(x))[1]
  first.obs.value <- x[first.obs.t]
  complete.obs <- first.obs.t==1
  x.clean <- x
  if(complete.obs==F){
  x.clean[first.obs.t] <- NA
  }
  outlist= list('total_obs'=total_obs,'first.obs.t'=first.obs.t, 'first.obs.value'=first.obs.value,'complete.obs'=complete.obs,'x.clean'=x.clean)
  return(outlist)
}
T.include <- 100
delay.mat.recent <- delay.mat[(nrow(delay.mat)-100):nrow(delay.mat),1:(T.include+1)]
dates.keep <- dates[(nrow(delay.mat)-100):nrow(delay.mat)]
dow <- as.numeric(as.factor(weekdays(dates.keep)))
delay.mat.ls <- split(delay.mat.recent, 1:nrow(delay.mat.recent))
process.mat <- lapply(delay.mat.ls,mat.pre.func)
  
#Prep data
reporting.triangle <- t(sapply(process.mat, '[[','x.clean' )  )
N.first.obs <- sapply(process.mat, '[[','first.obs.t' )  

first.obs.value <- sapply(process.mat, '[[','first.obs.value' )  
complete.obs <- 1*sapply(process.mat, '[[','complete.obs' )  

now.T <- nrow(reporting.triangle)
max_D <- now.T # ifelse(is.null(moving_window),now.T-1,moving_window-1)

reporting.triangle <- cbind.data.frame(reporting.triangle,first.obs.value )

tot.n.obs <- apply(reporting.triangle,1,sum, na.rm=T)

beta.priors <-  rep(0.1, times=(max_D)+1) 


##############################################################
#Model Fitting
##############################################################
inits1=list(".RNG.seed"=c(123), ".RNG.name"='base::Wichmann-Hill')
inits2=list(".RNG.seed"=c(456), ".RNG.name"='base::Wichmann-Hill')
inits3=list(".RNG.seed"=c(789), ".RNG.name"='base::Wichmann-Hill')

##############################################
#Model Organization
##############################################
model_spec<-textConnection(model_string_Pois4_partial)
model_jags<-jags.model(model_spec, 
                       inits=list(inits1),
                       data=list('n' = reporting.triangle,
                                 'n.dates' = nrow(reporting.triangle),
                                 'D' = max_D,
                                 'dow'=dow,
                                 'N.first.obs'=N.first.obs,
                                 alphat.shape.prior=0.001,
                                 alphat.rate.prior=0.001,
                                 'beta.priors'=beta.priors
                                 
                       ),
                       n.adapt=10000, 
                       n.chains=1)
params<-c('sum.n','sum.lambda',
          'beta.logged', 'alpha','sum.beta', 'delta')

##############################################
#Posterior Sampling
##############################################
posterior_samples<-coda.samples(model_jags, 
                                params, 
                                n.iter=5000)
posterior_samples.all<-do.call(rbind,posterior_samples)

median.est <- apply(posterior_samples.all,2,median)
ci.est <-   t(hdi(posterior_samples.all, credMass = 0.95))

sum.n.index <- grep('sum.n',dimnames(posterior_samples.all)[[2]])

beta.index <- grep('beta.logged',dimnames(posterior_samples.all)[[2]])

alpha.index <- grep('alpha[',dimnames(posterior_samples.all)[[2]], fixed=T)

#r.index <- grep('r',dimnames(posterior_samples.all)[[2]])

EstN <- cbind.data.frame('median'=median.est[sum.n.index], 'ci'=ci.est[sum.n.index,])
ObsN <- tot.n.obs

alpha <- cbind.data.frame('median'=median.est[alpha.index], 'ci'=ci.est[alpha.index,])

EstN.empty <- as.data.frame(matrix(NA, nrow= (length(ObsN)-nrow(EstN)), ncol=ncol(EstN)))
names(EstN.empty) <- names(EstN)

beta.est <- cbind(exp(median.est[beta.index]), exp(ci.est[beta.index,]))

combine.Est <- rbind.data.frame(EstN.empty, EstN)

combine.Est <- cbind.data.frame(combine.Est,ObsN,dates.keep)

combine.Est <- combine.Est[-nrow(combine.Est),] #chop off most recent obs--too uncertain

ci.plot <- combine.Est


out.list =list('ci.plot'=ci.plot,'EstN'=EstN, 'alpha'=alpha,'combine.Est'=combine.Est,'beta.est'=beta.est)

}




