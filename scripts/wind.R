# -----------------------------------------------------------------
# IMPORT LIBRARIES
# -----------------------------------------------------------------
require("tidyverse")
require("MASS")
require("gridExtra")
require("xtable")
library("circular")

# -----------------------------------------------------------------
# READ THE DATA
# -----------------------------------------------------------------
dat <- read.table("/Users/laurasansc/github/statistical_modelling/data/tuno.txt", header = T)

# -----------------------------------------------------------------
# PRESENT THE DATA
# -----------------------------------------------------------------
# Summary statistics
summary <- summary(dat[c("pow.obs", "ws30", "wd30")])
# PRINT TABLE
print(xtable(summary, digits = 5))


# Plot the density 3 numerical variables
p1 <- ggplot(data = dat, aes(x = pow.obs)) +
  geom_density() +
  labs(title = "Probability density plot of wind power production", y = "Density", x = "Generated power (kW)") +
  theme_classic()

p2 <- ggplot(data = dat, aes(x = ws30)) +
  geom_density() +
  labs(title = "Probability density plot of wind speed", y = "Density", x = "Wind speed m/s") +
  theme_classic()

p3 <- ggplot(data = dat, aes(x = wd30)) +
  geom_density() +
  labs(title = "Probability density plot of wind direction", y = "Density", x = "Wind direction") +
  theme_classic()

png("/Users/laurasansc/github/statistical_modelling/plots/wind_3_vars_plot.png",width=1500, height=2700, res=300)
grid.arrange(p1, p2, p3, nrow = 3)
dev.off()


# Plot the points of numerical variables
p4 <- ggplot(data = dat, aes(y = pow.obs, x = r.day)) +
  geom_point() +
  labs(title = "(a) Power generated through the year", y = "Generated power (kW)", x = "Days") +
  theme_classic()

p5 <- ggplot(data = dat, aes(x = ws30, y = pow.obs,)) +
  geom_point() +
  labs(title = "(b) Power generated vs wind speed", y = "Generated power (kW)", x = "Predicted wind speed (m/s)") +
  theme_classic()

p6 <- ggplot(data = dat, aes(x = wd30, y = pow.obs,y)) +
  geom_point() +
  labs(title = "(c) Power generated vs wind direcction", y = "Generated power (kW)", x = "Predicted wind direction (Rad)") +
  theme_classic()

png("/Users/laurasansc/github/statistical_modelling/plots/wind_3_points_plot.png",width=1500, height=2700, res=300)
grid.arrange(p4, p5, p6, nrow = 3)
dev.off()


# Lets see normality
png("/Users/laurasansc/github/statistical_modelling/plots/wind_3_points_plot.png",width=2700, height=1000, res=300)
par(mfrow=c(1,3))
# is y normal?
qqnorm(dat$pow.obs,main =  "Normal Q-Q plot: Wind power")
qqline(dat$pow.obs) # it has tails

qqnorm(dat$ws30, main = "Normal Q-Q plot: Wind speed")
qqline(dat$ws30)

qqnorm(dat$wd30, main = "Normal Q-Q plot: Wind direction")
qqline(dat$wd30)
dev.off()

# -----------------------------------------------------------------
# WRANGLE DATA
# -----------------------------------------------------------------
#normalize the data of power production
min <- 0
max <- 5000
normPow <- (dat$pow.obs-min)/(max-min)

# -----------------------------------------------------------------
# ANALYSIS - Likelihood models
# -----------------------------------------------------------------
#negative log/likelihood
nll.gamma <- function(params, data){
  -sum(dgamma(x = data, shape=params[1],rate = params[2], log = T))
}

nll.lognorm <- function(params, data){
  -sum(dlnorm(x = data, meanlog=params[1], sdlog= params[2], log = T))
}

nll.beta <- function(params, data){
  -sum(dbeta(x = data, shape1=params[1], shape2=params[2], log = T))
}

nll.vonmises <- function(params, data){
  -sum(dvonmises(x = data, mu=params[1], kappa= params[2], log = T))
}
# ------------------------------------------
# Optimization and model comparison normPow
# ------------------------------------------
optgamma <- nlminb(c(0.5, 0.1), nll.gamma, lower=c(0,0), data = normPow)
optlognorm <- nlminb(c(0.5, 0.1), nll.lognorm, lower=c(0,0), data = normPow)
optvonmises <- nlminb(c(0.5, 0.1), nll.vonmises, lower=c(0,0), data = normPow)
optbeta <- nlminb(c(0.5, 0.1), nll.beta, lower=c(0,0), data = normPow)


# Visualise - for ppt make table with them
optgamma$par
optlognorm$par
optvonmises$par
optbeta$par

# AIC comparison
#AIC = -2(log-likelihood) + 2K
#AIC = 2(-log-likelihood) + 2K
aic_gamma = 2 * optgamma$objective + 2 * 2
aic_gamma
aic_lognorm = 2 * optlognorm$objective + 2 * 2
aic_lognorm
aic_vonmises = 2 * optvonmises$objective + 2 * 2
aic_vonmises
aic_beta = 2 * optbeta$objective + 2 * 2
aic_beta


# create likelihoods df
theta <- seq(0,1,by=0.01)
likelihoods <- do.call(rbind, Map(data.frame, theta = theta, ll.gamma = dgamma(theta,optgamma$par[1],optgamma$par[2]), ll.lognorm = dlnorm(theta,optlognorm$par[1],optlognorm$par[2]), ll.vonmises = dlnorm(theta,optvonmises$par[1],optvonmises$par[2]), ll.beta = dbeta(theta,optbeta$par[1],optbeta$par[2])))

df_normPow <- data.frame("normpow" = normPow)
png("/Users/laurasansc/github/statistical_modelling/plots/wind_likelihood_plot.png", width = 1300, height = 1000, res = 300)
ggplot(likelihoods, aes(x = theta)) +
  #geom_histogram(data = df_normPow, aes(x= normPow)) +
  geom_line(aes(y = ll.gamma-max(ll.gamma), color="gamma")) +
  geom_line(aes(y = ll.lognorm-max(ll.lognorm), color="lognorm")) +
  geom_line(aes(y = ll.vonmises-max(ll.vonmises), color = "vonmises")) +
  geom_line(aes(y = ll.beta-max(ll.beta), color = "beta")) +
  labs(title = "Likelihood", y = "Log-Likelihood/max(Log-Likelihood)", x = "Theta") +
  theme_classic() +
  scale_colour_manual(name = "Model", values = c("gamma" ="red", "lognorm" ="blue", "vonmises" = "green", "beta"="orange")) +
  theme(plot.title = element_text(size = 12), axis.title.x = element_text(size = 9), axis.title.y = element_text(size = 8),legend.position="bottom")
dev.off()

#Standard errors:
library(numDeriv)
H <- hessian(nll.beta, optbeta$par, data=normPow)
#Standard error for the parameters
(se <- sqrt(diag(solve(H))))
# -------------------------------------------------------
# Optimization and model comparison (wind speed)
# -------------------------------------------------------
optgamma <- nlminb(c(0.5, 0.1), nll.gamma, lower=c(0,0), data = dat$ws30)
optlognorm <- nlminb(c(0.5, 0.1), nll.lognorm, lower=c(0,0), data = dat$ws30)
optvonmises <- nlminb(c(0.5, 0.1), nll.vonmises, lower=c(0,0), data = dat$ws30)
optbeta <- nlminb(c(0.5, 0.1), nll.beta, lower=c(0,0), data = dat$ws30)

# Obtain AICs
aic_gamma = 2 * optgamma$objective + 2 * 2
aic_gamma
aic_lognorm = 2 * optlognorm$objective + 2 * 2
aic_lognorm
aic_vonmises = 2 * optvonmises$objective + 2 * 2
aic_vonmises
aic_beta = 2 * optbeta$objective + 2 * 2
aic_beta

#Standard errors:
H <- hessian(nll.vonmises, optvonmises$par, data=dat$ws30)
#Standard error for the parameters
(se <- sqrt(diag(solve(H))))

# -------------------------------------------------------
# Optimization and model comparison (wind direction)
# -------------------------------------------------------
optgamma <- nlminb(c(0.5, 0.1), nll.gamma, lower=c(0,0), data = dat$wd30)
optlognorm <- nlminb(c(0.5, 0.1), nll.lognorm, lower=c(0,0), data = dat$wd30)
optvonmises <- nlminb(c(1, 0.1), nll.vonmises, lower=c(0,0), data = dat$wd30)
optbeta <- nlminb(c(0.5, 0.1), nll.beta, lower=c(0,0), data = dat$wd30)

# Compare AIC
aic_gamma = 2 * optgamma$objective + 2 * 2
aic_gamma
aic_lognorm = 2 * optlognorm$objective + 2 * 2
aic_lognorm
aic_vonmises = 2 * optvonmises$objective + 2 * 2
aic_vonmises
aic_beta = 2 * optbeta$objective + 2 * 2
aic_beta


#Standard errors:
library(numDeriv)
H <- hessian(nll.vonmises, optvonmises$par, data=dat$wd30)
#Standard error for the parameters
(se <- sqrt(diag(solve(H))))


# -------------------------------------------------------
# TRANSFORM DATA
# -------------------------------------------------------
t_normPow <- normPow

## box-cox transformation
bc.trans <- function(lambda,y){
  y.l <- (y^lambda-1)/lambda
  if(lambda==0){y.l <- log(y)}
  return(y.l)
}
png("/Users/laurasansc/github/statistical_modelling/plots/wind_transformed_qq_plot.png", width = 2000, height = 2000, res = 300)
par(mfrow=c(2,2))
lambda <- 1/3
qqnorm(bc.trans(lambda ,t_normPow), main = "Normal Q-Q plot: lambda 1/3")
qqline(bc.trans(lambda ,t_normPow))

lambda <- 1/2.5
qqnorm(bc.trans(lambda ,t_normPow),main = "Normal Q-Q plot: lambda 1/2.5")
qqline(bc.trans(lambda ,t_normPow))

lambda <- 1/5
qqnorm(bc.trans(lambda ,t_normPow),main = "Normal Q-Q plot: lambda 1/5")
qqline(bc.trans(lambda ,t_normPow))

lambda <- 1/4
qqnorm(bc.trans(lambda ,t_normPow),main = "Normal Q-Q plot: lambda 1/4")
qqline(bc.trans(lambda ,t_normPow))
dev.off()

# When Lambda 0.25 ------------------------
## profile likelihood for lambda
lp.lambda <- function(lambda,y){
  n <- length(y)
  y.l <- bc.trans(lambda ,y)
  sigmasq <- 1/n * sum((y.l-mean(y.l))^2)
  -n/2 * log(sigmasq) + (lambda-1)*sum(log(y))
}

## Plot the profile likelihood
lambda <- seq(-0.5,1.5,by=0.01)
lp <- sapply(lambda,lp.lambda, y= t_normPow)
profile_l <- do.call(rbind, Map(data.frame, lambda = lambda, lp = lp))
#hline
y = qchisq(0.95,df=1)/2*c(1,1)[1]

png("/Users/laurasansc/github/statistical_modelling/plots/wind_lp_plot.png",  width = 1500, height = 1000, res = 300)
ggplot(profile_l, aes(x = lambda)) +
  geom_line(aes(y = lp-max(lp))) +
  geom_hline(yintercept = y, color="red", linetype="dashed") +
  labs(title = "Likelihood", y = "Log-Likelihood/max(Log-Likelihood)", x = "Lambda") +
  theme_classic() +
  theme(plot.title = element_text(size = 12), axis.title.x = element_text(size = 9), axis.title.y = element_text(size = 8))
dev.off()

## Perform R boxcox
boxcox(lm(t_normPow~1),lambda=lambda)

## and finally we could optimize it
optimization <- optimize(lp.lambda,c(-2,2),y=t_normPow,maximum=TRUE)

lambda_opt <- optimization$maximum
lambda_opt

#Transformation with optimal lambda 1
#$$y^{\lambda} =  \frac{1}{\lambda}log(\frac{y^\lambda}{1-y^\lambda}); \lambda>0 $$
t_normPow_1 = (1/(lambda_opt))*log((t_normPow^(lambda_opt))/((1-t_normPow^lambda_opt)))
lm_t_1 = lm(t_normPow_1 ~ 1)

#Transformation with optimal lambda 2.
#$$y^{\lambda} =  2log(\frac{y^\lambda}{(1-y)^{1-\lambda}}); \lambda \epsilon (0, 1) $$
t_normPow_2 = 2*log((t_normPow^(lambda_opt))/((1-t_normPow)^(1-lambda_opt)))
lm_t_2 = lm(t_normPow_2 ~ 1)

# Q-Q plots
png("/Users/laurasansc/github/statistical_modelling/plots/wind_qq_plot_boxcox.png",  width = 2400, height = 900, res = 300)
par(mfrow=c(1,3))
qqnorm(t_normPow, main = "Normal Q-Q plot: Not transformed")
qqline(t_normPow)

qqnorm(t_normPow_1, main = "Normal Q-Q plot: Optimal lambda 1")
qqline(t_normPow_1)

qqnorm(t_normPow_2, main = "Normal Q-Q plot: Optimal lambda 2")
qqline(t_normPow_2)
dev.off()

# Check summaries - make tables  annexes
summary(lm_t_1)
summary(lm_t_2)
normal_lm =lm(t_normPow ~ 1)
summary(normal_lm)

# Do ANOVA test
anova(lm_t_1, lm_t_2)
anova(normal_lm, lm_t_2)
anova(normal_lm, lm_t_1)

# AIC
AIC(normal_lm, lm_t_1, lm_t_2)

# Wald intervals of confidence