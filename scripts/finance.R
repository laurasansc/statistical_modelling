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

selected_finance <- finance[,"SLV"]
# PRINT TABLE
print(xtable(selected_finance,digits=5))

# -----------------------------------------------------------------
# PRESENT THE DATA
# -----------------------------------------------------------------
# Summary of SLV
summary(finance)

# PRINT SUMMARY
print(xtable(summary(finance)))

# Overview data
# Layout to split the screen
#layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

# Draw the boxplot and the histogram
# par(mar=c(0, 4, 0, 1))
# boxplot(finance$SLV, horizontal=TRUE, col=8, xaxt="n", frame=F, border=T, ylim=c(-0.3,0.35))
# par(mar=c(4, 4, 0, 1))
# h <- hist(finance$SLV, border=F, breaks=40, main="", col =4, xlab="Weekly returns", xlim=c(-0.3,0.35), ylim=c(0,50))
# xfit <- seq(min(finance$SLV), max(finance$SLV), length = 40)
# yfit <- dnorm(xfit, mean = mean(finance$SLV), sd = sd(finance$SLV))
# yfit <- yfit * diff(h$mids[1:2]) * length(finance$SLV)
# lines(xfit, yfit, col = "black", lwd = 1)


# Distribution plot (normal distribution?)
p1 <- ggplot(data = finance, aes(x = num_week, y = SLV)) +
  geom_point(size = 1, shape = 1) +
  labs(title = "Distribution of weekly returns", x = "Weeks", y = "SLV") +
  theme_classic() +
  theme(plot.title = element_text(size=12), axis.title.x=element_text(size=10), axis.title.y=element_text(size=10))

# Histogram
p2 <- ggplot(data = finance, aes(x = SLV)) +
  geom_histogram(aes(y=..density..),fill = "white", color = "black", size = 0.4) +
  stat_function(fun = dnorm, args = list(mean = mean(finance$SLV), sd = sd(finance$SLV)), color="red") +
  labs(title = "Histogram SLV", y = "Frequency", x = "SLV") +
  theme_classic() +
  theme(plot.title = element_text(size=12), axis.title.x=element_text(size=10), axis.title.y=element_text(size=10))

# Final plot
# save plot to file without using ggsave
png("/Users/laurasansc/github/statistical_modelling/plots/initial_plot.png",width=1800, height=900, res=300)
grid.arrange(p1, p2, nrow = 1)
dev.off()

# Observe the QQ plots We can see that the distribution is already as we suspected very approximate to normal distribution.
png("/Users/laurasansc/github/statistical_modelling/plots/qq_plot.png",width=1000, height=1000, res=300)
par(mfrow = c(1, 1))
qqnorm(finance$SLV)
qqline(finance$SLV)
dev.off()

# -----------------------------------------------------------------
# FIT NORMAL FUNCTION
# -----------------------------------------------------------------
## likelihood for Normal Gaussian model
n <- length(finance$SLV)
s2 <- var(finance$SLV) * (n - 1)/n ## MLE of sigma^2
sd <- sqrt(s2)

normal.ll <- sum(dnorm(finance$SLV, mean = mean(y),
                       sd = sqrt(s2), log = TRUE))
normal.ll
ll.normal <- function(x, mean, sd){
  - sum(dnorm(x=x, mean=mean, sd=sd, log = TRUE))
}
par <-c(mean(y),sd(y))
mod_norm <- nlm(ll.normal,x=y, par[1], par[2], hessian=TRUE)  

library(numDeriv)

#Standard error for the parameters
(se <- sqrt(diag(solve(mod_norm$hessian))))


ci_par1 <- mean(y) + c(-1,1) * qnorm(0.975) * sqrt(diag(solve(mod_norm$hessian)))
ci_par1
ci_par2 <- sd(y) + c(-1,1) * qnorm(0.975) * sqrt(diag(solve(mod_norm$hessian)))
ci_par2



# Visualize the likelihood
pnll.normal <- function(mean, x, sd){
  - sum(dnorm(x=x, mean=mean,
            sd=sd), log = TRUE)
}
mean <- seq(min(finance$SLV), max(finance$SLV), by = 0.001)
ll_n <- sapply(mean, FUN = pnll.normal, x = finance$SLV , sd = sd)
opt_norm<- nlminb(c(0.5, 0.1), normal.ll, lower=c(0,0), data = finance$SLV)
# -----------------------------------------------------------------
# FIT CAUCHY FUNCTION
# -----------------------------------------------------------------
## likelihood for Cauchy model
## pars = c(mu,sigma)
## Cauchir dist qual student-t with df=1
nll.cauchy <- function(pars,x){
  -sum(dcauchy(x,location = pars[1],scale = pars[2],log=TRUE))
}

opt <- nlminb(c(mean(finance$SLV),sd(finance$SLV)), nll.cauchy, lower=c(-Inf,0), x = finance$SLV, hessian=T)
opt

mod <- nlm(nll.cauchy,c(median(finance$SLV),2),x=finance$SLV,
           hessian=TRUE)  

library(numDeriv)

#Standard error for the parameters
(se <- sqrt(diag(solve(mod$hessian))))


ci_par1 <- mod$estimate[1] + c(-1,1) * qnorm(0.975) * sqrt(diag(solve(mod$hessian)))
ci_par1
ci_par2 <- mod$estimate[2] + c(-1,1) * qnorm(0.975) * sqrt(diag(solve(mod$hessian)))
ci_par2

# PLOT PROFILE LIKELIHOOD FOR BOTH MODELS #
# plot.new
# plot(mean, ll_n/max(ll_n), type = "l", ylab = "Log-Likelihood/max(Log-Likelihood)", xlab="theta")
# lines(range(mean),-qchisq(0.95,df=1)/2*c(1,1),lty=2,col=2)
# -----------------------------------------------------------------
# COMPARE NORMAL vs CAUCHY MODELS by AIC
# -----------------------------------------------------------------
# AIC = 2k - 2ln(L) , k =  parameters and L max likelihood
-2 * normal.ll + 2*2
2 * opt$objective + 2*2 # Negative log-likelihood

# -----------------------------------------------------------------
# PRESENT FINAL MODEL
# -----------------------------------------------------------------
p3 <- ggplot(data = finance, aes(x = SLV)) +
  geom_histogram(aes(y=..density..),fill = "white", color = "black", size = 0.4) +
  stat_function(fun = dnorm, aes(color = "Normal"), args = list(mean = mean(finance$SLV), sd = sd(finance$SLV))) +
  stat_function(fun = dcauchy, aes(color = "Cauchy"),args = list(location = mean(finance$SLV), scale = sd(finance$SLV),log=FALSE)) +
  labs(y = "Frequency", x = "SLV") +
  scale_colour_manual("Model", values = c("Normal" = "red","Cauchy" = "green")) +
  theme_classic() +
  theme(plot.title = element_text(size=12), axis.title.x=element_text(size=10), axis.title.y=element_text(size=10))

png("/Users/laurasansc/github/statistical_modelling/plots/final_plot.png",width=1500, height=1000, res=300)
p3
dev.off()


####
# Overview data
# Layout to split the screen
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))

# Draw the boxplot and the histogram
par(mar=c(0, 4, 0, 1))
boxplot(finance$SLV, horizontal=TRUE, col=8, xaxt="n", frame=F, border=T, ylim=c(-0.3,0.35))
par(mar=c(4, 4, 0, 1))
h <- hist(finance$SLV, border=F, breaks=40, main="", col =4, xlab="Weekly returns", xlim=c(-0.3,0.35), ylim=c(0,50))
xfit <- seq(min(finance$SLV), max(finance$SLV), length = 40)
yfit <- dnorm(xfit, mean = mean(finance$SLV), sd = sd(finance$SLV))
yfit <- yfit * diff(h$mids[1:2]) * length(finance$SLV)
lines(xfit, yfit, col = "black", lwd = 2)

yfit_c <- dcauchy(xfit, location= mean(finance$SLV), scale = sd(finance$SLV))
yfit_c <- yfit_c * diff(h$mids[1:2]) * length(finance$SLV)
lines(xfit, yfit_c, col = "red", lwd = 2)
legend("topright", legend=c("Cauchy", "Normal"),
       col=c("red", "black"), lty=1, lwd=2, cex=0.8)

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
names(parvect) <- c("mu1","mu2","mu3","sigma21","sigma22","sigma23","tau21",
                    "tau31","tau12","tau32","tau13","tau23")

se <- sqrt(diag(solve(mod$hessian)))
se
alpha <- 0.05

n <- length(se)
for (i in 1:n)                                    
{                                                
  print(parvect[i] + c(-1,1) * qnorm(0.975) * se[i])
} 
#ci_par1 <- optbeta$par[1] + c(-1,1) * qnorm(0.975) * sqrt(c(solve(H[1,1])))
#round(cbind(parvect,apply(se,2,quantile,prob=c(0.025,0.975))),digits=5) ## note se of tau31
round(cbind(parvect,se),digits=5) ## note se of tau31
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
k <- 3000
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
hist(exp(tSigma[ ,1]))
rug(fit3$sigma2,lwd=2,col=2)
hist(exp(tSigma[ ,2]))
rug(fit3$sigma2,lwd=2,col=2)
hist(exp(tSigma[ ,3]))
rug(fit3$sigma2,lwd=2,col=2)



## 95% CI for mu 
t(apply(Mu,2,quantile,prob=c(0.025,0.975)))
## 95% CI for sigma
t(apply(exp(tSigma),2,quantile,prob=c(0.025,0.975)))
## 95% CI for gamma
t(round(apply(tGAMMA,2,quantile,prob=c(0.025,0.975)),
        digits=3))
## 95% CI for delta
round(t(apply(Delta,2,quantile,prob=c(0.025,0.975))),digits=3)



## Distribution of transition probability matrices
par(mfrow=c(3,3))
hist(exp(tGAMMA[ ,1]))
rug(fit3$gamma,lwd=2,col=2)
hist(exp(tGAMMA[ ,2]))
rug(fit3$gamma,lwd=2,col=2)
hist(exp(tGAMMA[ ,3]))
rug(fit3$gamma,lwd=2,col=2)
hist(exp(tGAMMA[ ,4]))
rug(fit3$gamma,lwd=2,col=2)
hist(exp(tGAMMA[ ,5]))
rug(fit3$gamma,lwd=2,col=2)
hist(exp(tGAMMA[ ,6]))
rug(fit3$gamma,lwd=2,col=2)
hist(exp(tGAMMA[ ,7]))
rug(fit3$gamma,lwd=2,col=2)
hist(exp(tGAMMA[ ,8]))
rug(fit3$gamma,lwd=2,col=2)
hist(exp(tGAMMA[ ,9]))
rug(fit3$gamma,lwd=2,col=2)


## Correlation of lambda
round(cov2cor(cov(tSigma)),digits=2)

## Plot the interdepence
plot(data.frame(tSigma))
plot(data.frame(Delta))


normal.HMM.forecast <- function(xf, h=1, m, x, mod){
  n <- length (x)
  nxf <- length (xf)
  dxf <- matrix (0, nrow =h, ncol = nxf)
  foo <- mod$delta * dnorm (x[1] , mod$mu, mod$sigma2 )
  sumfoo <- sum(foo)
  lscale <- log ( sumfoo )
  foo <- foo / sumfoo
  for (i in 2:n){
    foo <- foo %*% mod$gamma * dnorm(x[i], mod$mu, mod$sigma2)
    sumfoo <- sum( foo)
    lscale <- lscale + log ( sumfoo )
    foo<- foo / sumfoo
  }
 for (i in 1:h)
 {
    foo <- foo %*% mod$gamma
    for (j in 1: m ) dxf[i ,] <- dxf[i ,] +
      foo [j]* dnorm (xf , mod$mu[j], mod$sigma2[j])
  }
  return ( dxf)
}

## 3 - state 
## Initial values
m <- 3
mu <- quantile(y,c(0.25,0.5,0.75))
sigma <- sd(y)*c(1,1,1)
gamma <- matrix(0.05,ncol=m,nrow=m)  
diag(gamma) <- 1-(m-1)*gamma[1,1]    

## optimize
mod3s <- norm.HMM.mle(y,m,mu,sigma,gamma, stationary=TRUE)
mod3s

d <- finance$num_week
h <-50
xf <- 0:45
forecasts <- normal.HMM.forecast (xf=xf, h=1, m=m, x=y, mod=mod3s)
fc <- forecasts[1 ,]
par(mfrow =c(1, 1),las =1)
plot(xf,fc, type ="h", main = paste ("Financial series: forecast distribution for ", d[n]+1) ,xlab =" count ", ylab =" probability ", lwd =3)


m < -3
xf <- 0:45
mu <- fit3$mu
sigma2 <- fit3$sigma2
delta <- solve (t( diag (m)- fit3$gamma +1) ,rep (1,m))
dstat <- numeric ( length (xf))
for (j in 1:m) dstat <- dstat + delta [j]* dnorm(xf , mu[j], sigma2[j])

plot(dstat)


DIM <- function( ... ){
  args <- list(...)
  lapply( args , function(x) { if( is.null( dim(x) ) )
    return( length(x) )
    dim(x) } )
}
