model_string_negbin4_partial<-
  "model
{
  # Likelihood
  for( t in 1:n.dates ){
    for(d in 1:(D+1)){
  
    #First likelihood is for full data
    n[t,d] ~ dnegbin(p1[t,d],r1)
    # Conversions
    p1[t,d] <- r1/(r1+lambda[t,d])
  
    log(lambda[t,d]) <- ( alpha[t] +
            beta.logged2[d]*step(D-d) +
            sum.beta.logged2[t]*(1-step(D-d)) +
                        delta[dow[t]]  #Day of week effect

                    ) 
    }
   
    sum.lambda[t] <- sum(lambda[t,1:D])
    
    #If D=1 is not observed, use column D+1 and (N.first.obs+1):D first obs is in col D+1
    sum.n[t]  <- (1-step(N.first.obs[t]-1.5))*sum(n[t,(1:D)]) + 
            step(N.first.obs[t]-1.5)*(n[t,(D+1)] +sum(n[t,(N.first.obs[t]+1):D]))
  }
   alpha[1] ~ dnorm(0, 0.001)
  for(t in 2:n.dates){
     alpha[t] ~ dnorm((alpha[t-1]), tau2.alpha)
   }
  # Prior for neg binomial rate
  r1 ~ dgamma(0.001,0.001)
  ## Prior for beta
  beta.logged <- log(beta)
  beta.logged2 <- c(beta.logged,0)
  beta ~ ddirch(beta.priors)
  
  delta[1] <-0
  for(i in 2:7){
  delta[i] ~dnorm(0, tau.day)
  }
  tau.day <- 1/sd.day^2
  sd.day ~ dunif(0,1e-4)
  
  for( t in 1:n.dates ){
    sum.beta[t] <- sum(beta[1:(N.first.obs[t]) ])
    sum.beta.logged2[t] <- log(sum.beta[t])
  }
  # Prior for variance
  tau2.alpha ~ dgamma(alphat.shape.prior,alphat.rate.prior)
}
"