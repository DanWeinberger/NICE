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

cum.pct <- t(apply(pct.tot.date,1, function(x){
  x[is.na(x)] <-0
  y<-cumsum(x)
  return(y)
} ))

###########
###NoBbs model
###########

#Prep data
reporting.triangle <- delay.mat[!is.na(delay.mat[,1]) , ]
reporting.triangle <- reporting.triangle[,1:nrow(reporting.triangle)]

now.T <- nrow(reporting.triangle)
max_D <- now.T-1 # ifelse(is.null(moving_window),now.T-1,moving_window-1)

#beta.priors <-  rep(0.1, times=(max_D)+1) #θ = (3,30,50,10,4,3)
first.weeks <- c(25,  45, 15, 5 )
next.weeks <- rep(1, times=(max_D+1 - length(first.weeks) ))
beta.priors <- c(first.weeks, next.weeks)

##############################################################
#Model Fitting
##############################################################
inits1=list(".RNG.seed"=c(123), ".RNG.name"='base::Wichmann-Hill')
inits2=list(".RNG.seed"=c(456), ".RNG.name"='base::Wichmann-Hill')
inits3=list(".RNG.seed"=c(789), ".RNG.name"='base::Wichmann-Hill')

##############################################
#Model Organization
##############################################
model_spec<-textConnection(NobBs.NB)
model_jags<-jags.model(model_spec, 
                       inits=list(inits1,inits2, inits3),
                       data=list('n' = reporting.triangle,
                                 'Today' = now.T,
                                 'D' = max_D,
                                 alphat.shape.prior=0.001,
                                 alphat.rate.prior=0.001,
                                 alpha1.mean.prior=0,
                                  alpha1.prec.prior=0.001,
                                 dispersion.prior.shape=0.001,
                                 dispersion.prior.rate=0.001,
                                 'beta.priors'=beta.priors
                                 
                       ),
                       n.adapt=10000, 
                       n.chains=3)
params<-c('sum.n',
          'beta.logged', 'alpha')

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

EstN <- cbind.data.frame('median'=median.est[sum.n.index], 'ci'=ci.est[sum.n.index,])
ObsN <- apply(delay.mat,1,sum, na.rm=T)

EstN.empty <- as.data.frame(matrix(NA, nrow= (length(ObsN)-nrow(EstN)), ncol=ncol(EstN)))
names(EstN.empty) <- names(EstN)

beta.est <- cbind(exp(median.est[beta.index]), exp(ci.est[beta.index,]))

combine.Est <- rbind.data.frame(EstN.empty, EstN)

combine.Est <- cbind.data.frame(combine.Est,ObsN,dates)

combine.Est <- combine.Est[-nrow(combine.Est),] #chop off most recent obs--too uncertain
ci.plot <- combine.Est[(nrow(combine.Est)-nrow(EstN)):nrow(combine.Est),]


out.list =list('ci.plot'=ci.plot,'EstN'=EstN, 'combine.Est'=combine.Est,'beta.est'=beta.est)

}




