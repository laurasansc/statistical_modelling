# -----------------------------------------------------------------
# IMPORT LIBRARIES
# -----------------------------------------------------------------
require("tidyverse")
require("MASS")
require("gridExtra")
require("xtable")
# -----------------------------------------------------------------
# READ THE DATA
# -----------------------------------------------------------------
raw_finance <- read.csv("/Users/laurasansc/github/statistical_modelling/data/finance_data.csv", sep = ";")

# -----------------------------------------------------------------
# WRANGLE DATA
# -----------------------------------------------------------------
finance <- cbind(num_week = rownames(raw_finance), raw_finance)
rownames(finance) <- 1:nrow(finance)
finance <- finance %>% mutate(num_week = as.numeric(num_week))

# selected_finance <- finance[,"SLV"]
# # PRINT TABLE
# print(xtable(selected_finance,digits=5))
# 
# # -----------------------------------------------------------------
# # PRESENT THE DATA
# # -----------------------------------------------------------------
# # Summary of SLV
# summary(finance)
# 
# # PRINT SUMMARY
# print(xtable(summary(finance)))
# 
# # Overview data
# # Layout to split the screen
# #layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
# 
# # Draw the boxplot and the histogram 
# # par(mar=c(0, 4, 0, 1))
# # boxplot(finance$SLV, horizontal=TRUE, col=8, xaxt="n", frame=F, border=T, ylim=c(-0.3,0.35))
# # par(mar=c(4, 4, 0, 1))
# # h <- hist(finance$SLV, border=F, breaks=40, main="", col =4, xlab="Weekly returns", xlim=c(-0.3,0.35), ylim=c(0,50))
# # xfit <- seq(min(finance$SLV), max(finance$SLV), length = 40) 
# # yfit <- dnorm(xfit, mean = mean(finance$SLV), sd = sd(finance$SLV)) 
# # yfit <- yfit * diff(h$mids[1:2]) * length(finance$SLV) 
# # lines(xfit, yfit, col = "black", lwd = 1)
# 
# 
# # Distribution plot (normal distribution?)
# p1 <- ggplot(data = finance, aes(x = num_week, y = SLV)) +
#   geom_point(size = 1, shape = 1) +
#   labs(title = "Distribution of weekly returns", x = "Weeks", y = "SLV") +
#   theme_classic() +
#   theme(plot.title = element_text(size=12), axis.title.x=element_text(size=10), axis.title.y=element_text(size=10))
# 
# # Histogram
# p2 <- ggplot(data = finance, aes(x = SLV)) +
#   geom_histogram(aes(y=..density..),fill = "white", color = "black", size = 0.4) +
#   stat_function(fun = dnorm, args = list(mean = mean(finance$SLV), sd = sd(finance$SLV)), color="red") +
#   labs(title = "Histogram SLV", y = "Frequency", x = "SLV") +
#   theme_classic() +
#   theme(plot.title = element_text(size=12), axis.title.x=element_text(size=10), axis.title.y=element_text(size=10))
# 
# # Final plot
# # save plot to file without using ggsave
# png("/Users/laurasansc/github/statistical_modelling/plots/initial_plot.png",width=1800, height=900, res=300)
# grid.arrange(p1, p2, nrow = 1)
# dev.off()
# 
# # Observe the QQ plots We can see that the distribution is already as we suspected very approximate to normal distribution.
# png("/Users/laurasansc/github/statistical_modelling/plots/qq_plot.png",width=1000, height=1000, res=300)
# par(mfrow = c(1, 1))
# qqnorm(finance$SLV)
# qqline(finance$SLV)
# dev.off()
# 
# # -----------------------------------------------------------------
# # FIT NORMAL FUNCTION
# # -----------------------------------------------------------------
# ## likelihood for Normal Gaussian model
# n <- length(finance$SLV)
# s2 <- var(finance$SLV) * (n - 1)/n ## MLE of sigma^2
# sd <- sqrt(s2)
# 
# normal.ll <- sum(dnorm(finance$SLV, mean = mean(finance$SLV),
#                        sd = sqrt(s2), log = TRUE))
# normal.ll
# 
# # Visualize the likelihood
# pnll.normal <- function(mean, x, sd){
#   - sum(dnorm(x, mean,
#             sd), log = TRUE)
# }
# mean <- seq(min(finance$SLV), max(finance$SLV), by = 0.001)
# ll_n <- sapply(mean, FUN = pnll.normal, x = finance$SLV , sd = sd)
# 
# # -----------------------------------------------------------------
# # FIT CAUCHY FUNCTION
# # -----------------------------------------------------------------
# ## likelihood for Cauchy model
# ## pars = c(mu,sigma)
# ## Cauchir dist qual student-t with df=1 
# nll.cauchy <- function(pars,x){
#   -sum(dcauchy(x,location = pars[1],scale = pars[2],log=TRUE))
# }
# 
# opt <- nlminb(c(median(finance$SLV),2), nll.cauchy, lower=c(-Inf,0), x = finance$SLV)
# opt
# 
# 
# # PLOT PROFILE LIKELIHOOD FOR BOTH MODELS #
# plot.new
# plot(mean, ll_n/max(ll_n), type = "l", ylab = "Log-Likelihood/max(Log-Likelihood)", xlab="theta")
# lines(range(mean),-qchisq(0.95,df=1)/2*c(1,1),lty=2,col=2)
# # -----------------------------------------------------------------
# # COMPARE NORMAL vs CAUCHY MODELS by AIC
# # -----------------------------------------------------------------
# # AIC = 2k - 2ln(L) , k =  parameters and L max likelihood
# -2 * normal.ll + 2*2
# 2 * opt$objective + 2*2 # Negative log-likelihood
# 
# # -----------------------------------------------------------------
# # PRESENT FINAL MODEL
# # -----------------------------------------------------------------
# p3 <- ggplot(data = finance, aes(x = SLV)) +
#   geom_histogram(aes(y=..density..),fill = "white", color = "black", size = 0.4) +
#   stat_function(fun = dnorm, aes(color = "Normal"), args = list(mean = mean(finance$SLV), sd = sd(finance$SLV))) +
#   stat_function(fun = dcauchy, aes(color = "Cauchy"),args = list(location = mean(finance$SLV), scale = sd(finance$SLV),log=FALSE)) +
#   labs(y = "Frequency", x = "SLV") +
#   scale_colour_manual("Model", values = c("Normal" = "red","Cauchy" = "green")) +
#   theme_classic() +
#   theme(plot.title = element_text(size=12), axis.title.x=element_text(size=10), axis.title.y=element_text(size=10)) 
# 
# png("/Users/laurasansc/github/statistical_modelling/plots/final_plot.png",width=1500, height=1000, res=300)
# p3
# dev.off()
# 
# 
# ####
# # Overview data
# # Layout to split the screen
# layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
# 
# # Draw the boxplot and the histogram 
# par(mar=c(0, 4, 0, 1))
# boxplot(finance$SLV, horizontal=TRUE, col=8, xaxt="n", frame=F, border=T, ylim=c(-0.3,0.35))
# par(mar=c(4, 4, 0, 1))
# h <- hist(finance$SLV, border=F, breaks=40, main="", col =4, xlab="Weekly returns", xlim=c(-0.3,0.35), ylim=c(0,50))
# xfit <- seq(min(finance$SLV), max(finance$SLV), length = 40) 
# yfit <- dnorm(xfit, mean = mean(finance$SLV), sd = sd(finance$SLV)) 
# yfit <- yfit * diff(h$mids[1:2]) * length(finance$SLV) 
# lines(xfit, yfit, col = "green", lwd = 1)
# 
# yfit_c <- dcauchy(xfit, location= mean(finance$SLV), scale = sd(finance$SLV)) 
# yfit_c <- yfit_c * diff(h$mids[1:2]) * length(finance$SLV) 
# lines(xfit, yfit_c, col = "red", lwd = 1)
# legend("topright", legend=c("Normal", "Cauchy"),
#        col=c("red", "green"), lty=1:2, cex=0.8)

# -------------------------------------------------------------
#                    HIDDEN MARKOV MODELS
# -------------------------------------------------------------
par(mfrow=c(1,1))
plot(finance$num_week,finance$SLV,type="b",col="grey", xlab="Week", ylab="Weekly returns")

y <- finance$SLV

# Load the functions that we want
source("AR1.R")

## 1 - state 
## Initial values
m <- 1
mu <- mean(y)
sigma <- sd(y)
gamma <- 0

## optimize
fit1 <- norm.HMM.mle(y,m,mu,sigma,gamma)
fit1

## 2 - state 
## Initial values
m <- 2
mu <- quantile(y,c(0.25,0.75))
#sigma <- sd(y)*c(0.25,0.75)
sigma <- sd(y)*c(1,1)
gamma <- matrix(0.05,ncol=m,nrow=m)
diag(gamma) <- 1-(m-1)*gamma[1,1]

## optimize
fit2 <- norm.HMM.mle(y,m,mu,sigma,gamma)
fit2

optfit2 <- norm.HMM.mle.nlminb(y,m,mu,sigma,gamma)
optfit2

## 3 - state 
# ## Initial values
# m <- 3
# mu <- mean(y) * c(0.25,0.5,0.75)
# sigma <- sd(y)*c(0.25,0.5,0.75)
# gamma <- matrix(0.05,ncol=m,nrow=m)
# diag(gamma) <- 1-(m-1)*gamma[1,1]

## 3 - state 
## Initial values
m <- 3
mu <- quantile(y,c(0.25,0.5,0.75))
sigma <- sd(y)*c(1,1,1)
gamma <- matrix(0.05,ncol=m,nrow=m)  
diag(gamma) <- 1-(m-1)*gamma[1,1]    

## optimize
fit3 <- norm.HMM.mle(y,m,mu,sigma,gamma)
fit3

## 4 - state 
## Initial values
m <- 4
mu <- quantile(y,c(0.2,0.4,0.6,0.8))
sigma <- sd(y)*c(1,1,1,1)
gamma <- matrix(0.025,ncol=m,nrow=m)
diag(gamma) <- 1-(m-1)*gamma[1,1]

## optimize
fit4 <- norm.HMM.mle(y,m,mu,sigma,gamma)
fit4

AIC <- c(fit1$AIC,fit2$AIC,fit3$AIC,fit4$AIC)
ll <-  -c(fit1$mllk,fit2$mllk,fit3$mllk,fit4$mllk)
AIC
m <- c(1,2,3,4)
df <- m + m + (m^2-m) ## mu + sigma + gamma

## What should we report
cbind(df, ll, AIC)

bic <- c(fit1$BIC,fit2$BIC,fit3$BIC,fit4$BIC)
aic <- c(fit1$AIC,fit2$AIC,fit3$AIC,fit4$AIC)
states <- c("1 State","2 States", "3 States", "4 States")
selection <- data.frame(states, bic, aic)

plot(1:4,selection$bic,xaxt = "n",ty="b", lty=2, ylim=c(-1520,-1395), xlab ="States", ylab="Model Selection Criterion")
axis(1, at = factor(selection$states), label = selection$states)
lines(1:4,selection$aic,ty="b", pch = 2, lty=2)
legend("topleft",legend=c("BIC", "AIC"),pch=c(1,2), cex=0.8)

# As we see in the plot the best model is the one with 3 States that shows the best AIC and BIC values between the 4 states proposed.
# Stationary distribution plot
# Now we will show the confidence intervals for the working parameters of the 3 States
## Finding the standard errors
library(numDeriv)
library (Matrix)
m <- 3
## working parameters
parvect  <- norm.HMM.pn2pw(m,fit3$mu,fit3$sigma,fit3$gamma)

## Optimize (hessian = TRUE return hessian)
mod <- nlm(norm.HMM.mllk,parvect,x=y,m=m,
           hessian=TRUE)  
H <- mod$hessian

parvect <- mod$estimate
names(parvect) <- c("mu1","mu2","mu3","lambda1","lambda2","lambda3","tau21",
                    "tau31","tau12","tau32","tau13","tau23")

se <- sqrt(diag(solve(mod$hessian)))
se

round(cbind(parvect,se),digits=2) ## note se of tau31
fit3$gamma
##################
# Natural parameters:
fit3$mu
fit3$sigma2
fit3$gamma
fit3$delta

#################

##################################################
## Parametric bootstrap
source("AR2.R")
## Initailize
n <- length(y)
k <- 100
m <- 3
mu <- quantile(y,c(0.25,0.5,0.75))
sigma <- sd(y)*c(1,1,1)
gamma0 <- matrix(0.025,ncol=m,nrow=m)
diag(gamma0) <- 1-(m-1)*gamma0[1,1]

## Matrices to stores bootstap res.
tGAMMA <- matrix(ncol=m*2,nrow=k)
Mu <- matrix(ncol=m,nrow=k)
tSigma <- matrix(ncol=m,nrow=k)
Delta <- matrix(ncol=m,nrow=k)
Code <- numeric(k)

for(i in 1:k){
  set.seed(i)
  y.sim <- norm.HMM.generate_sample(n, m,
                                    fit3$mu,fit3$sigma2, fit3$gamma)
  mu0 <- mean(y.sim)*c(1/2,1,3/2)
  sigma0 <- sd(y.sim)*c(1/2,1,3/2)
  fit3.tmp <- norm.HMM.mle.nlminb(y.sim, m,mu0,sigma0, gamma0)
  print(fit3.tmp)
  wp <- norm.HMM.pn2pw(m,fit3.tmp$mu,fit3.tmp$sigma,fit3.tmp$gamma)
  
  tGAMMA[i, ] <- wp[7:12]
  Mu[i, ] <- wp[1:3]
  tSigma[i,] <- wp[4:6]
  Delta[i, ] <- fit3.tmp$delta
  Code[i] <- fit3.tmp$code
  print(c(i,Code[i]))
}
sum(Code!=0)

## Plot the results (i.e. statistical distribution of
## estimates
par(mfrow=c(1,3))
hist(Mu[ ,1])
rug(fit3$Mu,lwd=2,col=2)
hist(Mu[ ,2])
rug(fit3$Mu,lwd=2,col=2)
hist(Mu[ ,3])
rug(fit3$Mu,lwd=2,col=2)

## estimates
par(mfrow=c(1,3))
hist(tSigma[ ,1])
rug(fit3$tSigma,lwd=2,col=2)
hist(tSigma[ ,2])
rug(fit3$tSigma,lwd=2,col=2)
hist(tSigma[ ,3])
rug(fit3$tSigma,lwd=2,col=2)



## 95% CI for mu 
apply(Mu,2,quantile,prob=c(0.025,0.975))
## 95% CI for sigma
apply(tSigma,2,quantile,prob=c(0.025,0.975))
## 95% CI for gamma
t(round(apply(tGAMMA,2,quantile,prob=c(0.025,0.975)),
        digits=3))
## 95% CI for delta
round(t(apply(Delta,2,quantile,prob=c(0.025,0.975))),digits=3)


## Correlation of lambda
round(cov2cor(cov(tSigma)),digits=2)

## Plot the interdepence
plot(data.frame(tSigma))
plot(data.frame(Delta))





##################################################
## Profile likelihood for sigma21
PL.sigma21 <- function(sigma21,m,y,others_init){
  ## Fun for inner optim
  fun.tmp <- function(pars,sigma21,y,m){
    parvect <- c(log(sigma21),pars)
    norm.HMM.mllk(parvect,y,m)
  }
  ## Initialize
  mu0 <- others_init[1]
  mu1 <- others_init[2]
  sigma20 <- others_init[3]
  gamma0 <- others_init[4]
  gamma1 <- others_init[5]
  sigma20 <- c(sigma21,sigma20)
  parvect0 <- norm.HMM.pn2pw(m, mu0, sigma20, gamma0)
  parvect0 <- parvect0[-1]
  np <- length(parvect0)
  lower    <- rep(-10,np)
  upper    <- c(rep(max(y),m-1),rep(10,np+1-m))
  ## optimize to find profile likelihood
  nlminb(parvect0,fun.tmp, sigma21=sigma21,
         y=y, m=m, lower=lower,
         upper=upper)$objective    
}
gamma0 <- matrix(0.025,ncol=m,nrow=m)
diag(gamma0) <- 1-(m-1)*gamma0[1,1]

## Initial values for estimation
others_init <- c((-0.1),(0.1),log(0.1),gamma0, gamma0) #mu1, mu2, sd2, gamma1, gamma2
PL.sigma21(sigma21=29,m=m,y=y,others_init)

## Which lamdas should we look at
sigma21 <- seq(min(y),max(y),length=100)

## The profile liklielihood 
llp.sigma21 <- sapply(sigma21,PL.sigma21,m=m,y=y, others_init)

## Plot the profile likelihood
par(mfrow=c(1,1))
plot(sigma21,exp(-(llp.sigma21-fit3$mllk)),
     type="l")
lines(range(sigma21),
      c(1,1)*exp(-qchisq(0.95,df=1)/2),col=2,lty=2,lwd=2)
rug(fit3$sigma2,col=3,lwd=2)

## Wald statistic
cbind(mod$estimate,se)

## Quadratic (local) approximation
cbind(exp(mod$estimate-1.96*se),
      exp(mod$estimate+1.96*se))[1:3, ]
plot(exp(mod$estimate[1]-1.96*se[1]*c(-1,1)),
      c(1,1)*exp(-qchisq(0.95,df=1)/2),
      col=4,lwd=2, xlim=c(0.935,1.050), lty="b")
lines(exp(mod$estimate[2]-1.96*se[2]*c(-1,1)),
      c(1,1)*exp(-qchisq(0.95,df=1)/2),
      col=4,lwd=2,lty="b")
lines(exp(mod$estimate[3]-1.96*se[3]*c(-1,1)),
      c(1,1)*exp(-qchisq(0.95,df=1)/2),
      col=4,lwd=2,lty="b")
##################################################



### STATIONARY PLOTS #####

## Estimation with two distributions
m <- 2 ## No of states
## Initial values
mu <- quantile(y,c(0.25,0.75))
sigma <- sd(y)*c(1,1)
gamma <- matrix(0.05,ncol=m,nrow=m)
diag(gamma) <- 1-(m-1)*gamma[1,1]

## MLE
opt2 <- norm.HMM.mle.nlminb(x=y, m=m, mu0=mu,sigma20=sigma, gamma0=gamma)

## Natural parameters
theta <- c(opt2$mu,opt2$sigma2,opt2$gamma)
npars2 <-norm.HMM.pw2pn(m,theta)

m <- 3 ## No of states
## Initial values
mu <- quantile(y,c(0.25,0.5,0.75))
sigma <- sd(y)*c(1,1,1)
gamma <- matrix(0.05,ncol=m,nrow=m)
diag(gamma) <- 1-(m-1)*gamma[1,1]

## MLE
opt3 <- norm.HMM.mle(x=y, m=m, mu0=mu,sigma20=sigma, gamma0=gamma)

## Natural parameters
theta <- c(opt3$mu,opt3$sigma2,opt3$gamma)
npars3 <-norm.HMM.pw2pn(m,theta)

## Plot the result
#Generate X
x <- seq(from=min(y),to=max(y), by= 0.01)
#transform lists into vectors
npars3 <- unlist(npars3)
npars2 <- unlist(npars2)

#histogram
hist(finance$SLV, breaks = 100, prob=TRUE, ylim=c(0,25), xlab="Weekly returns", main="Histogram of weekly returns")

# Calculate y for the plots
hmm.dist3 = npars3[16]*dnorm(x,mean=npars3[1],sd=npars3[4])+
  npars3[17]*dnorm(x,mean=npars3[2],sd=npars3[5])+
  npars3[18]*dnorm(x,mean=npars3[3],sd=npars3[6])

hmm.dist2 = npars2[9]*dnorm(x,mean=npars2[1],sd=npars2[3])+
  npars2[10]*dnorm(x,mean=npars2[2],sd=npars2[4])

# Draw lines
lines(x,hmm.dist3,type="b",col=3,pch=19,cex=0.5)
lines(x,hmm.dist2,type="b",col=2,cex=0.5)
legend("topright",legend=c("3 States","2 States"),pch=c(19,1),col=c(3,2), cex=0.8)