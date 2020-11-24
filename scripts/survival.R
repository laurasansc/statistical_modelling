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
raw_logistic <- read.csv("/Users/laurasansc/github/statistical_modelling/data/Logistic.txt", sep = "\t")
raw_trial <- read.csv("/Users/laurasansc/github/statistical_modelling/data/actg320.txt", sep = "\t")

# -----------------------------------------------------------------
# ASSIGNMENT 1
# -----------------------------------------------------------------
# -----------------------------------------------------------------
# PRESENT THE DATA - LOGISTIC DATA
# -----------------------------------------------------------------
# PRINT TABLE
print(xtable(raw_logistic, digits = 5))

# Plot
# ????

# -----------------------------------------------------------------
# ANALYSIS - LOGISTIC DATA
# -----------------------------------------------------------------
# ---------- LIKELIHOOD FUNCTION ----------------------------------
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
# PRESENT THE DATA - ACTG320 DATA
# -----------------------------------------------------------------


# -----------------------------------------------------------------
# WRANGLE DATA - ACTG320 DATA
# -----------------------------------------------------------------
raw_trial <- raw_trial[c("time", "event", "tx")]
