# Loading the data
load("~/Downloads/dataex5.Rdata")

# EM algorithm
em = function(y, theta0, eps){
  # Initializing variables
  n = length(y)
  theta = theta0
  p = theta[1]
  mu = theta[2]
  sigma = theta[3]
  lambda = theta[4]
  diff = 1
  
  while(diff > eps){
    theta_old = theta
  
    #E-step
    ptilde1 = p*dlnorm(y, meanlog = mu, sdlog = sigma)
    ptilde2 = (1 - p) * dexp(y, lambda)
    ptilde = ptilde1/(ptilde1 + ptilde2)
    
    #M-step
    p = mean(ptilde)
    mu = sum(log(y)*ptilde)/sum(ptilde)
    sigma = sqrt(sum(((log(y) - mu)^2)*ptilde)/sum(ptilde))
    lambda = sum(1 - ptilde)/sum(y*(1 - ptilde))
    theta = c(p, mu, sigma, lambda)
    diff = sum(abs(theta - theta_old))
  }
  return(theta)
}

# Computing the MLE theta parameters
theta = em(dataex5, c(0.1, 1, 0.5, 2), 0.00001)
p = theta[1]
mu = theta[2]
sigma2 = theta[3]^2
lambda = theta[4]
p; mu; sigma2; lambda

# Creating a histogram of data with estimated density curve
hist(dataex5, breaks = 20, main="Mixture Distribution for Random Sample Y",
     xlab = "y",
     cex.main = 1.2,
     col= "lavender",
     ylim= c(0,0.20),
     freq = FALSE)
# Superimposing mixed density distribution
curve(p*dlnorm(x, meanlog = mu, sdlog= sqrt(sigma2)) + (1-p)*dexp(x, lambda),
      add= TRUE, col= "midnightblue",lwd=1.5)
legend("topright", c("Data", "Estimated Density"),
       fill=c("lavender", "midnightblue"))