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

normal.ll <- sum(dnorm(finance$SLV, mean = mean(finance$SLV),
                       sd = sqrt(s2), log = TRUE))
normal.ll

# -----------------------------------------------------------------
# FIT CAUCHY FUNCTION
# -----------------------------------------------------------------
## likelihood for Cauchy model
## pars = c(mu,sigma)
## Cauchir dist qual student-t with df=1 
nll.cauchy <- function(pars,x){
  -sum(dcauchy(x,location = pars[1],scale = pars[2],log=TRUE))
}


opt <- nlminb(c(median(finance$SLV),2), nll.cauchy, lower=c(-Inf,0), x = finance$SLV)

opt
mean(finance$SLV)

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
