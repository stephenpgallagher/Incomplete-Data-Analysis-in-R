# Loading in data
load("~/Downloads/dataex4.Rdata")
#Initializing variables for missing and observed data
ind_mis = which(is.na(dataex4$Y))
y_obs = dataex4$Y[-ind_mis] # observed values of y
y_mis = dataex4$Y[ind_mis] # missing values of y
x = dataex4$X # complete column for x
x_m = dataex4$X[-ind_mis] # x values corresponding to observed values of y
x_n = dataex4$X[ind_mis] # x values corresponding to missing values of y

# E-step
# Defining the Q function
Q <- function(beta){
  # Defining beta0 and beta1
  beta0 <- beta[1]
  beta1 <- beta[2]
  # Computing value of Q
  -sum(y_obs*(beta0 + x_m*beta1))
  + sum(log(1 + exp(beta0 + x*beta1)))
  - sum(exp(beta_old[1] + x_n*beta_old[2])/(1 + exp(beta_old[1] + x_n*beta_old[2])) * (beta0 + x_n*beta1))
}

# M-step
#Defining the starting point for beta
diff <- 1
eps <- 0.00001
beta_old <- c(1, 1)
while(diff > eps){
  beta <- opt$par
  diff <- sqrt(sum((beta_old - beta)^2))
  beta_old <- beta
  optimum <- optim(beta_old, Q)
}
optimum