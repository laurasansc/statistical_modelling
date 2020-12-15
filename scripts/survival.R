# -----------------------------------------------------------------
# IMPORT LIBRARIES
# -----------------------------------------------------------------
require("tidyverse")
require("MASS")
require("gridExtra")
require("xtable")
require("numDeriv")
require("survival")
require("coin")

# -----------------------------------------------------------------
# READ THE DATA
# -----------------------------------------------------------------
raw_logistic <- read.csv("/Users/laurasansc/github/statistical_modelling/data/Logistic.txt", sep = "\t")
raw_survival <- read.csv("/Users/laurasansc/github/statistical_modelling/data/actg320.txt", sep = "\t")

# -----------------------------------------------------------------
# ASSIGNMENT 1
# -----------------------------------------------------------------
# Present the data
print(xtable(raw_logistic, digits = 5))

# Plot
# ????

# -----------------------------------------------------------------
# Analysis
#  LIKELIHOOD FUNCTION 
Likelihood_binomial <- function(theta, x, n) {
  prod(dbinom(prob = theta, x = x, size = n))
}

# ----- GROUPED DATA BINOMIAL FITTING -----------------------------
# Fit the binomial distribution to the data (same population joint groups).
# Binomial density parameters without grouping

n <- sum(raw_logistic$n)
x <- sum(raw_logistic$AIDS_yes)

# Get the likelihood for theta [0,1] by 0.01
theta <- seq(0, 1, by = 0.01)
ll <- sapply(theta, FUN = Likelihood_binomial, x = x, n = n)


# Plot the likelihood for theta [0,1] by 0.01
par(mfrow=c(1,1))
plot(theta, ll/max(ll), type = "l", main = "Likelihood of HIV regardless of treatment", ylab = "Log-Likelihood/max(Log-Likelihood)")

# MLE GROUPED
MLE_grouped <- optimize(Likelihood_binomial, c(0.01, 0.99), x = x, n = n)

# ????
# nlminb for maximizing
# test the fitness of the binomial is not necessary
# Compare the likelihoods by p-value using chi-square
# wald CI

# ----- NON-GROUPED DATA BINOMIAL FITTING ------------------------
# Fit the binomial separately to the two distributions and test if there is a difference between groups.
n_AZT <- sum(raw_logistic$n[1])
n_no_AZT <- sum(raw_logistic$n[2])

x_AZT <- sum(raw_logistic$AIDS_yes[1])
x_no_AZT <- sum(raw_logistic$AIDS_yes[2])

# Plot the likelihood for theta [0,1] by 0.01
theta <- seq(0, 1, by = 0.01)
ll_AZT <- sapply(theta, FUN = Likelihood_binomial, x = x_AZT, n = n_AZT)
ll_no_AZT <- sapply(theta, FUN = Likelihood_binomial, x = x_no_AZT, n = n_no_AZT)

par(mfrow=c(1,2))
plot(theta, ll_AZT/max(ll_AZT), type = "l", main = "Likelihood of HIV under AZT treatment", ylab = "Log-Likelihood/max(Log-Likelihood)")
plot(theta, ll_no_AZT/max(ll_no_AZT), type = "l", main = "Likelihood of HIV under no AZT treatment", ylab = "Log-Likelihood/max(Log-Likelihood)")

# MLE AZT/no AZT
MLE_AZT <- optimize(Likelihood_binomial, c(0.01, 0.99), x = x_AZT, n = n_AZT)$minimum
MLE_no_AZT <- optimize(Likelihood_binomial, c(0.01, 0.99), x = x_no_AZT, n = n_no_AZT)$minimum

#-------- save plot with all Likelihood functions ----------------------
# create likelihoods df
likelihoods <- do.call(rbind, Map(data.frame, theta = theta, ll = ll, ll_AZT = ll_AZT, ll_no_AZT = ll_no_AZT))

png("/Users/laurasansc/github/statistical_modelling/plots/survival_binary_likelihood_plot.png", width = 1300, height = 1000, res = 300)
ggplot(likelihoods, aes(x = theta)) +
  geom_line(aes(y = ll/max(ll), color="Both")) +
  geom_line(aes(y = ll_AZT/max(ll_AZT), color="yes AZT")) +
  geom_line(aes(y = ll_no_AZT/max(ll_no_AZT), color = "no AZT")) +
  labs(title = "Likelihood of HIV", y = "Log-Likelihood/max(Log-Likelihood)", x = "Theta") +
  theme_classic() +
  scale_colour_manual(name = "Treatment/No treatment", values = c("Both" ="red", "yes AZT" ="blue", "no AZT" = "green")) +
  theme(plot.title = element_text(size = 12), axis.title.x = element_text(size = 9), axis.title.y = element_text(size = 8),legend.position="bottom")
dev.off()

# -----------------------------------------------------------------
# ASSIGNMENT 1 - SURVIVAL DATA
# -----------------------------------------------------------------



# -----------------------------------------------------------------
# ASSIGNMENT 2 - LOGISTIC DATA
# -----------------------------------------------------------------
# Get count data
tx <- c(rep(1,170), rep(0,168))
aids <- c(rep(1,25), rep(0,170-25), rep(1,44), rep(0,168-44))

# Final dataframe
dat <- data.frame(tx, aids)

# assign model variables
y <- dat$aids
x <- dat$tx


# Model 1
mod1 <- glm(y~x, family = binomial)
summary(mod1)
#model.matrix(mod1)

# Model 2 - H0
mod2 <- glm(y~1,family = binomial )
summary(mod2)


# Log odds ratio
exp(c(coef(mod1)[2],confint(mod1)[2,]))

# # Calculate log-likelihood for the logistic regression model
# ## negative log likelihood function
# nll <- function(beta,y,X,n){
#   p <- exp(X %*% beta) / (1 + exp(X %*% beta))
#   -sum(dbinom(y,size=n,prob=p,log=TRUE))
# }
# 
# ## Observation and design matrix
# y <- dat[ ,2]
# X <- cbind(1,dat[ ,1]-mean(dat[ ,1]))
# 
# opt <- nlminb(c(-1,1),nll,y=y,X=X,n=1)
# 
# ## Parameter uncertainty
# H <- hessian(nll, opt$par, y=y, X=X, n=1)
# se.beta <- sqrt(diag(solve(H)))
# se.beta

# Wald test
z <- mod1$coefficients[2] / coef(summary(mod1))[2,2]
# Estimate x (tx) / stdE  estimate x (tx)
1 - pnorm(q = abs(z))

# Likelihood ratio test - formula
Q <-  2 * (logLik(mod1)-logLik(mod2))
pvalue <- 1 - pchisq(q=Q, df=1)
pvalue

# Score test - formula 
beta_0 <- mod2$coefficients[1]
beta_1 <- 0

p = exp(beta_0+beta_1*x) / (1 + exp(beta_0+beta_1*x))
S = c(sum(y - p), sum(x*(y - p)))

num = exp(beta_0+beta_1*x)
den = (1 + num)^2

i11 = sum(num/den)
i12 = sum(x * num/den)
i22 = i12 # x^2 = x because x = 0 or x = 1
I = matrix(data = c(i11,i12,i12,i22), nrow = 2, ncol = 2)

solve(I)
t(S)     ## -1.870984e-11 -9.704142
X2 = t(S) %*% solve(I) %*% S

1 - pchisq(q = X2, df = 1)

par(mfrow = c(1, 1))
# -----------------------------------------------------------------
# ASSIGNMENT 2 - SURVIVAL DATA
# -----------------------------------------------------------------
# DESCRIPTIVE STATISTICS
# -----------------------------------------------------------------
# Get data
survival <- raw_survival

# count how many people had aids
count <- survival %>% group_by(tx) %>% count(event)
count
# PRINT TABLE
print(xtable(count, digits = 5))

#How long was the total follow up
max_total <- survival %>% summarise(time = max(time))
max_total
max_grouped <- survival %>% group_by(event) %>% summarise(time = max(time))
max_grouped

print(xtable(max_grouped))
# Survival function two groups event/tx
surv.tx <- survfit(Surv(time, event == 1) ~ tx,
                   conf.type = "log-log", data = survival, type="kaplan-meier")

summary(surv.tx)

png("/Users/laurasansc/github/statistical_modelling/plots/survival_survival_plot.png",width=1200, height=900, res=150)
plot(surv.tx, conf.int = TRUE, las = 1, xlab = "time",
     ylab = "Estimated Survival Prob.", col=2:3,
     lwd = 2, mark.time=TRUE, ylim = c(0.75,1))
legend("bottomleft", col=2:3, c("No Treatment","Treatment"), lwd=2,adj = c(0, 1))

dev.off()

# Cumulative incidence
png("/Users/laurasansc/github/statistical_modelling/plots/survival_cuminc_plot.png",width=1200, height=900, res=150)
plot(surv.tx, fun=function(x) { 1- x}, conf.int = T,
     las = 1, xlab = "Days since admission",
     ylab = "Estimated Failure Prob.", col=2:3, lwd = 2)
legend("topleft", col=2:3, c("No treatment","Treatment"), lwd=2,adj = c(0, 1))
dev.off()

# Log-rank test
# transform treatment into factor
survdiff <- survdiff(Surv(time, event==1) ~ tx, data = survival, rho=1)
survdiff
survival$tx <- as.factor(survival$tx) 
logrank <- logrank_test(Surv(time, event==1) ~ tx, data = survival)
logrank

### PLOT EVENT 

surv.event <- survfit(Surv(time, tx == 1) ~ event,
                   conf.type = "log-log", data = survival, type="kaplan-meier")

summary(surv.event)

png("/Users/laurasansc/github/statistical_modelling/plots/survival_survival_plot_event.png",width=1200, height=900, res=150)
plot(surv.event, conf.int = TRUE, las = 1, xlab = "time",
     ylab = "Estimated Survival Prob.", col=2:3,
     lwd = 2, mark.time=TRUE, ylim = c(0.75,1))
legend("bottomleft", col=2:3, c("Event","No event"), lwd=2,adj = c(0, 1))

dev.off()

# Cumulative incidence
png("/Users/laurasansc/github/statistical_modelling/plots/survival_cuminc_plot_event.png",width=1200, height=900, res=150)
plot(surv.event, fun=function(x) { 1- x}, conf.int = T,
     las = 1, xlab = "Days since admission",
     ylab = "Estimated Failure Prob.", col=2:3, lwd = 2)
legend("topleft", col=2:3, c("Event","No event"), lwd=2,adj = c(0, 1))
dev.off()

# -----------------------------------------------------------------
# ANALYSIS
# -----------------------------------------------------------------
# parametric survival models containing treatment (tx) and CD4 count (cd4) as explanatory variables
# --------- Exponential ---------------
mod_exponential <- survreg(Surv(time, event==1) ~ cd4 + tx, data = survival,
                           dist="exponential")
summary(mod_exponential)
confint.exp <- confint(mod_exponential)

# model prediction plot
pct.exp <- 1:98/100   # The 100th percentile of predicted survival is at +infinity
ptime.exp <- predict(mod_exponential,  type='quantile',
                 p=pct.exp, se=TRUE)
plot(x=ptime.exp$fit[1,], y=1-pct.exp, type="l", title="Exponential function predicted survival", xlab="Time", ylab="Predicted survival")

# Goodness of fit of the exponential model
survival$CoxSnell <- survival$time*exp(-mod_exponential$linear.predictors)
surv2 <- survfit(Surv(CoxSnell, event==1)~1 , data = survival)

png("/Users/laurasansc/github/statistical_modelling/plots/survival_diagnostic_exp.png",width=1200, height=900, res=150)
plot(surv2$time, -log(surv2$surv))
abline(a=0, b=1)
dev.off()

# --------- Weibull -----------------
mod_weibull <- survreg(Surv(time, event==1) ~ cd4 + tx, data = survival,
                       dist="weibull")
summary(mod_weibull)
confint.wb <- confint(mod_weibull)
# model prediction plot
pct.wb <- 1:98/100   # The 100th percentile of predicted survival is at +infinity
ptime.wb <- predict(mod_weibull,  type='quantile',
                     p=pct.wb, se=TRUE)
plot(x=ptime.wb$fit[1,], y=1-pct.wb, type="l", xlab="Time", ylab="Predicted survival")

# Goodness of fit of the weibull model
survival$CoxSnell <- exp((log(survival$time)-mod_weibull$linear.predictors)/mod_weibull$scale)
surv3 <- survfit(Surv(CoxSnell, event==1)~1 , data = survival)

png("/Users/laurasansc/github/statistical_modelling/plots/survival_diagnostic_wb.png",width=1200, height=900, res=150)
plot(surv3$time, -log(surv3$surv))
abline(a=0, b=1)
dev.off()

# --------- Log-logistic -----------------
mod_loglogistic <- survreg(Surv(time, event==1) ~ cd4 + tx, data = survival,
                           dist="loglogistic")
summary(mod_loglogistic)
confint.log<- confint(mod_loglogistic)

# model prediction plot
pct.log <- 1:98/100   # The 100th percentile of predicted survival is at +infinity
ptime.log <- predict(mod_loglogistic,  type='quantile',
                    p=pct.log, se=TRUE)
plot(x=ptime.log$fit[1,], y=1-pct.log, type="l", xlab="Time", ylab="Predicted survival")

# Goodness of fit of the log logistic model
survival$z <- (log(survival$time)-mod_loglogistic$linear.predictors)/mod_loglogistic$scale 
survival$CoxSnell <- log(1+exp(survival$z)) # do log log here
surv4 <- survfit(Surv(CoxSnell, event==1)~1 , data = survival)

png("/Users/laurasansc/github/statistical_modelling/plots/survival_diagnostic_log.png",width=1200, height=900, res=150)
plot(surv4$time, -log(surv4$surv))
abline(a=0, b=1)
dev.off()

# AIC values to compare the 3 models
AIC_exp <- -2*mod_exponential$loglik[2]+ 2*mod_exponential$df

AIC_w <- -2*mod_weibull$loglik[2]+ 2*mod_weibull$df

AIC_ll <- -2*mod_loglogistic$loglik[2]+ 2*mod_loglogistic$df

AIC_exp
AIC_w
AIC_ll 

# > AIC_exp
# [1] 1645.838
# > AIC_w
# [1] 1640.671
# > AIC_ll
# [1] 1639.655 # BEST 


### CHOOSE BEST MODEL ###
# Confidence intervals 95% & # Time ratio
# The estimated time ratio and confidence intervals are
tr <- exp(cbind(TR = coef(mod_loglogistic),confint(mod_loglogistic)))
print(xtable(tr))
tr

# tr if we increment the cd40 by 50
exp(cbind(TR=coef(mod_loglogistic)[2]*50,"2,5 %"=confint(mod_loglogistic)[2, 1]*50, "97,5 %"= confint(mod_loglogistic)[2, 2]*50))

# The estimated hazard ratio and confidence interval are
hr <- round(exp(cbind(HR = -coef(mod_loglogistic),-confint(mod_loglogistic)[,2:1])),4)
print(xtable(hr))

exp(cbind(HR=-coef(mod_loglogistic)[2]*50,"2,5 %"=-confint(mod_loglogistic)[2, 1]*50, "97,5 %"= -confint(mod_loglogistic)[2, 2]*50))

# Cox-snell residuals plot 
# Goodness of fit of the log logistic model
survival$z <- (log(survival$time)-mod_loglogistic$linear.predictors)/mod_loglogistic$scale 
survival$CoxSnell <- log(1+exp(survival$z)) # do log log here
surv4 <- survfit(Surv(CoxSnell, event==1)~1 , data = survival)

plot(surv4$time, -log(surv4$surv), ylab="Survival ", xlab="Time")
abline(a=0, b=1)

# Model representation
# Plot kaplan meier with cd4 levels (23,75,137) 
# make cd4 groups

survival$cd4grp <- cut(survival$cd4,breaks = c(min(survival$cd4), 35, 75, 137, max(survival$cd4)))
summary(survival[,c("cd4", "cd4grp")])

surv.cd4 <- survfit(Surv(time, event == 1) ~ cd4grp ,
                    conf.type = "log-log", data = survival, type="log-logistic")
png("/Users/laurasansc/github/statistical_modelling/plots/survival_kaplan_final.png",width=1200, height=900, res=150)
par(mfrow = c(1, 2))
plot(surv.cd4, conf.int = F, las = 1, xlab = "Days since admission", ylab = "Estimated Survival Prob.", col=2:5, lwd = 2, ylim = c(0.75,1))
legend("bottomleft", col=2:5,title= "CD4", c("0-34","35-74","75-137","137+"),lwd=2,adj = c(0, 0.6))

plot(surv.tx, conf.int = F , las = 1, xlab = "Days since admission", ylab = "Estimated Survival Prob.", col=2:3, lwd = 2, ylim = c(0.75,1))
legend("bottomleft", col=2:5, c("No treatment","Treatment"),lwd=2,adj = c(0,1))
dev.off()

# More model representation
# For people with treatment, which cd4 charge gives the best survival

xrange <- range(survival$time)
t <- seq(xrange[1],xrange[2], length=1000)

#LOG LOGISTIC
coef4 <- mod_loglogistic$coefficients
z42 <- (log(t)-(coef4[1]+coef4[2]*35+coef4[3]*1))/mod_loglogistic$scale
z41 <- (log(t)-(coef4[1]+coef4[2]*75+coef4[3]*1))/mod_loglogistic$scale
z40 <- (log(t)-(coef4[1]+coef4[2]*137+coef4[3]*1))/mod_loglogistic$scale
S42 <- (1+exp(z42))^-1
S41 <- (1+exp(z41))^-1
S40 <- (1+exp(z40))^-1

yrange <- range(S41)

par(mfrow = c(1, 1))
png("/Users/laurasansc/github/statistical_modelling/plots/survival_survprob.png",width=1200, height=900, res=150)
plot(xrange, yrange, type="n", xlab="Time to event",
     ylab="Probability of survival (tx=1, cd4=(35,75,135)",las=1, ylim = c(0.75,1.5)) 
#PLOT THE SURVIVAL FUNCTIONS
lines(t, S42, type="l", col=1, lty=2, lwd=2)
lines(t, S41, type="l", col=2, lwd=2)
lines(t, S40, type="l", col=3, lwd=2)
legend(x="topright", lwd=2, col=1:3, title="CD4", legend=c("35","75","137"),adj = c(0, 1))
dev.off()


