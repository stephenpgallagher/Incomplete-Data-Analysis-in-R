# Installing and loading maxLik package for maximum likelihood estimation
install.packages("maxLik")
library(maxLik)

# Loading in data
data <- load("~/Downloads/dataex2.Rdata")

# Defining the log likelihood function
log_lik = function(param, data) {
  mu = param[1]
  sigma = 1.5
  x = data[,1]; r = data[,2]
  sum(r*log(dnorm(x, mu, sigma)) + (1 - r)*log(pnorm(x, mu, sigma)))
}

# Computing the MLE
mle = maxLik(logLik = log_lik, data = dataex2, start = c(mu=1))
# Results summary
summary(mle)