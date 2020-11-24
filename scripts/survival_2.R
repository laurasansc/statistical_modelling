# -----------------------------------------------------------------
# IMPORT LIBRARIES
# -----------------------------------------------------------------
require("tidyverse")
require("MASS")
require("gridExtra")
require("xtable")
require("numDeriv")
require("cmprsk")
library("survival")
library("survminer")
library("coin")
# -----------------------------------------------------------------
# READ THE DATA
# -----------------------------------------------------------------
raw_logistic <- read.csv("/Users/laurasansc/github/statistical_modelling/data/Logistic.txt", sep = "\t")
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

1 - pnorm(q = abs(z))

# Likelihood ratio test - formula
Q <- 2 * (logLik(mod1)-logLik(mod2))
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

# -----------------------------------------------------------------
# READ THE DATA
# -----------------------------------------------------------------
raw_survival <- read.csv("/Users/laurasansc/github/statistical_modelling/data/actg320.txt", sep = "\t")
raw_survival
# -----------------------------------------------------------------
# ASSIGNMENT 2 - SURVIVAL DATA
# -----------------------------------------------------------------
# DESCRIPTIVE STATISTICS
# -----------------------------------------------------------------
# count how many people had aids
count <- raw_survival %>% group_by(tx) %>% count(event)
count
# PRINT TABLE
print(xtable(count, digits = 5))

#How long was the total follow up
max_total <- raw_survival %>% summarise(time = max(time))
max_total
max_grouped <- raw_survival %>% group_by(event) %>% summarise(time = max(time))
max_grouped

print(xtable(max_grouped))
# Survival function two groups event/tx
surv.tx <- survfit(Surv(time, event == 1) ~ tx,
                   conf.type = "log-log", data = raw_survival)
dev.new(width=5, height=4, unit="in")
png("/Users/laurasansc/github/statistical_modelling/plots/survival_survival_plot.png",width=1200, height=900, res=150)
plot(surv.tx, conf.int = TRUE, las = 1, xlab = "time",
     ylab = "Estimated Survival Prob.", col=2:3,
     lwd = 2, mark.time=TRUE, ylim = c(0.75,1))
legend("bottomleft", col=2:3, c("Treatment","No treatment"), lwd=2,adj = c(0, 1))
dev.off()

# Cumulative incidence
dev.new(width=15, height=10, unit="in")
png("/Users/laurasansc/github/statistical_modelling/plots/survival_cuminc_plot.png",width=1200, height=900, res=150)
plot(surv.tx, fun=function(x) { 1- x}, conf.int = T,
     las = 1, xlab = "Days since admission",
     ylab = "Estimated Failure Prob.", col=2:3, lwd = 2)
legend("topleft", col=2:3, c("Treatment","No treatment"), lwd=2,adj = c(0, 1))
dev.off()

# Log-rank test
# transform treatment into factor
survdiff <- survdiff(Surv(time, event==1) ~ tx, data = raw_survival, rho=1)
survdiff
raw_survival$tx <- as.factor(raw_survival$tx) 
logrank <- logrank_test(Surv(time, event==1) ~ tx, data = raw_survival)
logrank

# -----------------------------------------------------------------
# DESCRIPTIVE STATISTICS
# -----------------------------------------------------------------
# parametric survival models containing treatment (tx) and CD4 count (cd4) as explanatory variables
# Exponential



# Weibull



# Log-logistic



# Use cox-snell to compare the 3 models  + AIC values
# get also b0,1,2 and aic table

# plot kaplan meier with cd4 levels (23,75, 137) 





