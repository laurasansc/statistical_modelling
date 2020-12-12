# # Copy of R-code given in the Appendix 
# # A.1 Fit a stationary normal HMM by direct numerical maximization
# # probability mass function of gaussian distribution
# # g(x) = (1/ (sigma*sqrt(2*pi))) * exp*(-0.5 * ((x-mu)^2/sqrt(sigma)))

norm.HMM.pn2pw <- function(m, mu, sigma2, gamma)
{                                              
  tsigma2 <- log(sigma2)                         
  tgamma  <- NULL                              
  if(m>1)                                        
  {                                            
    foo   <- log(gamma/diag(gamma))           
    tgamma<- as.vector(foo[!diag(m)])             
  }                                             
  parvect <- c(mu, tsigma2, tgamma)                    
  parvect                                         
}  

norm.HMM.pw2pn <- function(m,parvect)                 
{
  mu <- parvect[1:m]
  epar   <- exp(parvect[(m+1):(2*m+m*(m-1))])                              
  sigma2 <- epar[1:m]                                   
  gamma  <- diag(m)                                    
  if(m>1)                                               
  {                                                  
    gamma[!gamma] <- epar[(m+1):(m*m)]                  
    gamma         <- gamma/apply(gamma,1,sum)          
  }                                                   
  delta  <- solve(t(diag(m)-gamma+1),rep(1,m))          
  list(mu=mu, sigma2=sigma2, gamma=gamma, delta=delta)           
}  


norm.HMM.mllk <- function(parvect,x,m,...)       
{
  #    print(parvect)
  if(m==1) return(-sum(dnorm(x, parvect[1], exp(parvect[2]), log=TRUE))) 
  n          <- length(x)                            
  pn         <- norm.HMM.pw2pn(m,parvect)
  
  allprobs   <- matrix(nrow = n, ncol = m)
  for (j in 1:m){
    allprobs[,j] = dnorm(x, pn$mu[j], pn$sigma2[j])
  }
  allprobs   <- ifelse(!is.na(allprobs),allprobs,1)
  lscale     <- 0                                    
  foo        <- pn$delta                             
  for (i in 1:n)                                    
  {                                                
    foo    <- foo%*%pn$gamma*allprobs[i,]            
    sumfoo <- sum(foo)                               
    lscale <- lscale+log(sumfoo)                    
    foo    <- foo/sumfoo                            
  }                                               
  mllk       <- -lscale                            
  mllk                                              
}    


norm.HMM.mle <- function(x,m,mu0,sigma20,gamma0,...)
{                                                      
  parvect0  <- norm.HMM.pn2pw(m,mu0,sigma20,gamma0)         
  mod       <- nlm(norm.HMM.mllk,parvect0,x=x,m=m,stepmax=30,iterlim=1000)       
  pn        <- norm.HMM.pw2pn(m,mod$estimate)            
  mllk      <- mod$minimum                              
  np        <- length(parvect0)                          
  AIC       <- 2*(mllk+np)                              
  n         <- sum(!is.na(x))                            
  BIC       <- 2*mllk+np*log(n)                         
  list(mu=pn$mu, sigma2=pn$sigma2, gamma=pn$gamma, delta=pn$delta,   
       code=mod$code,mllk=mllk,AIC=AIC,BIC=BIC)   
}  


norm.HMM.mle.nlminb <- function(x,m,mu0,sigma20,gamma0,...)
{
  parvect0 <- norm.HMM.pn2pw(m,mu0, sigma20,gamma0)
  np        <-length(parvect0)
  lower    <- rep(-10,np)
  upper    <- c(rep(max(x),m),rep(10,np-m))
  mod      <- nlminb(parvect0,norm.HMM.mllk,x=x,m=m,
                     lower=lower,upper=upper)
  if(mod$convergence!=0){
    print(mod)
  }
  pn       <- norm.HMM.pw2pn(m,mod$par)
  mllk     <- mod$objective
  AIC       <- 2*(mllk+np)
  n         <- sum(!is.na(x))
  BIC       <- 2*mllk+np*log(n)
  list(mu=pn$mu,sigma2=pn$sigma2,gamma=pn$gamma,delta=pn$delta,
       code=mod$convergence,mllk=mllk,AIC=AIC,BIC=BIC)
}