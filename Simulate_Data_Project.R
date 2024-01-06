###################################### Simulate  ##############################
#Loading necessary packages
install.packages("MASS")
install.packages("statmod")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("statmod")

# เรียกใช้ packages
library(MASS)
library(statmod)
library(ggplot2)
library(dplyr)

vec <- c(1, c)
vec[2]


############## In the graph below, we visualize the curves generated using the logit, probit, and cloglog transformations ######
# Load the ggplot2 package
library(ggplot2)

# Create a sequence of values for the x-axis
x <- seq(-4, 4, by = 0.1)


# Calculate the values for the logit and probit functions
logit_link <- exp(x) / (1 + exp(x))
probit_link <- pnorm(x)

# Calculate the values for the cloglog function manually
cloglog_link <- 1 - exp(-exp(x))


# Create a data frame to store the values
data <- data.frame(x, logit_link, probit_link, cloglog_link)

# Create the plot using ggplot2
ggplot(data, aes(x = x)) +
  geom_line(aes(y = logit_link, color = "Logit"), size = 1) +
  geom_line(aes(y = probit_link, color = "Probit"), size = 1) +
  geom_line(aes(y = cloglog_link, color = "CLogLog"), size = 1) +
  labs(title = " ",
       x = "x", y = "Probability") +
  scale_color_manual(values = c("Logit" = "red", "Probit" = "blue", "CLogLog" = "green")) +
  theme_minimal()




######################################## Stat Simulate data ##########################################
options(digits = 7)
###################################Link function logit is pi ###############################

# Set the number of observations
n0 <- 20
n1 <- 50
n2 <- 200
n3 <- 750

###################################### n = 20 ###################################
# Set n0
n0 <- 20

# Set the number of iterations
niter_n20_linklogit <- 1000

# Create empty vectors to store AIC and BIC values
AIC_n20_logit_linklogit <- rep(0, niter_n20_linklogit)
BIC_n20_logit_linklogit <- rep(0, niter_n20_linklogit)
AIC_n20_probit_linklogit <- rep(0, niter_n20_linklogit)
BIC_n20_probit_linklogit <- rep(0, niter_n20_linklogit)
AIC_n20_cloglog_linklogit <- rep(0, niter_n20_linklogit)
BIC_n20_cloglog_linklogit <- rep(0, niter_n20_linklogit)


for (i in 1:niter_n20_linklogit) {
  
  
  # Define the coefficients for the binomial regression model
  #beta <- c(0.13279, -0.04084, 0.12983, -0.02155) # SetNew_logit
  beta <- c(0.1328000, -0.0408400, 0.0001298, -0.0215500) # SetNewNew_logit
  
  # Set the Poisson parameter
  lambda1 <- 54  # Average number of students admitted
  lambda2 <- 50 # Average of number of students accepted in the 3rd round 
  
  # Simulate independent variables x1 and x2 from a normal distribution
  x1 <- runif(n0, min = 30.33,   max = 75.87)      # Highest Score Admission Round 3 runif(n, min, max) X5
  x2 <- runif(n0, min = 12127.1, max = 17239.5)    # Highest score Admission Round 4 runif(n, min, max) X8
  
  # Simulate independent variables x3 from a Bernoulli distribution
  x3 <- rpois(n0, lambda2) # Number of students accepted in the 3rd round  X7
  
  # Combine independent variables into a matrix X
  X <- matrix(0, nrow = n0, ncol = 4)
  X[, 1] <- t(rep(1, n0))  # intercept term
  X[, 2] <- x1
  X[, 3] <- x2
  X[, 4] <- x3
  
  # Generate the Poisson counts
  ni <- rpois(n0, lambda1) # Number of students admitted
  
  # Calculate the predicted probabilities pi for each observation in X
  PropY_linklogit <- exp(X %*% beta) / (1 + exp(X %*% beta))
  
  # Simulate the dependent variable y for each observation from a binomial distribution
  Y_logit <- rbinom(n0, ni, PropY_linklogit)

  
  
  # Fit the binomial regression model using glm function
  binomial_model_logit <- glm( cbind(Y_logit, ni-Y_logit) ~ x1 + x2 + x3, family = binomial(link = "logit"))
  binomial_model_probit <- glm( cbind(Y_logit, ni-Y_logit) ~ x1 + x2 + x3, family = binomial(link = "probit"))
  binomial_model_cll <- glm( cbind(Y_logit, ni-Y_logit) ~ x1 + x2 + x3, family = binomial(link = "cloglog"))

  
  # View the summary of the model
  summary(binomial_model_logit)
  summary(binomial_model_probit)
  summary(binomial_model_cll)
  
  # Store the AIC and BIC values
  AIC_n20_logit_linklogit[i] <- AIC(binomial_model_logit)
  BIC_n20_logit_linklogit[i] <- BIC(binomial_model_logit)
  AIC_n20_probit_linklogit[i] <- AIC(binomial_model_probit)
  BIC_n20_probit_linklogit[i] <- BIC(binomial_model_probit)
  AIC_n20_cloglog_linklogit[i] <- AIC(binomial_model_cll)
  BIC_n20_cloglog_linklogit[i] <- BIC(binomial_model_cll)

  
}

meanAIC_logit_linklogit_n20 <- mean(AIC_n20_logit_linklogit)
meanBIC_logit_linklogit_n20 <- mean(BIC_n20_logit_linklogit)
meanAIC_probit_linklogit_n20 <- mean(AIC_n20_probit_linklogit)
meanBIC_probit_linklogit_n20 <- mean(BIC_n20_probit_linklogit)
meanAIC_cloglog_linklogit_n20 <- mean(AIC_n20_cloglog_linklogit)
meanBIC_cloglog_linklogit_n20 <- mean(BIC_n20_cloglog_linklogit)


# Create a data frame to store the confidence intervals and model fit statistics
model_comparison_n20_linklogit <- data.frame(
  Model = c("Logit", "Probit", "CLog-Log"),
  AIC = c(meanAIC_logit_linklogit_n20, meanAIC_probit_linklogit_n20 , meanAIC_cloglog_linklogit_n20),
  BIC = c(meanBIC_logit_linklogit_n20, meanBIC_probit_linklogit_n20, meanBIC_cloglog_linklogit_n20)
)

# Print the table
print(model_comparison_n20_linklogit)


# Count the lowest number of times
numberAIC_lesslogit_n20_logit   <- 0
numberAIC_lessprobit_n20_logit  <- 0
numberAIC_lesscloglog_n20_logit <- 0
numberBIC_lesslogit_n20_logit   <- 0
numberBIC_lessprobit_n20_logit  <- 0
numberBIC_lesscloglog_n20_logit <- 0


######################################### Check lower AIC #########################################
for (i in 1:1000) {
  if (AIC_n20_logit_linklogit[i] < AIC_n20_probit_linklogit[i] &&
      AIC_n20_logit_linklogit[i] < AIC_n20_cloglog_linklogit[i]) {
    numberAIC_lesslogit_n20_logit = numberAIC_lesslogit_n20_logit + 1
  } else if (AIC_n20_probit_linklogit[i] < AIC_n20_logit_linklogit[i] &&
             AIC_n20_probit_linklogit[i] < AIC_n20_cloglog_linklogit[i]) {
    numberAIC_lessprobit_n20_logit = numberAIC_lessprobit_n20_logit + 1
  } else {
    numberAIC_lesscloglog_n20_logit = numberAIC_lesscloglog_n20_logit + 1
  }
}

cat("Total number of AIC (n = 20) :", numberAIC_lesslogit_n20_logit + numberAIC_lessprobit_n20_logit +numberAIC_lesscloglog_n20_logit, "\n")
cat("Minimum number of AIC logit  : ",  numberAIC_lesslogit_n20_logit,"or ",(numberAIC_lesslogit_n20_logit/1000)*100, "%" ,"\n")
cat("Minimum number of AIC probit : ",  numberAIC_lessprobit_n20_logit,"or ",(numberAIC_lessprobit_n20_logit/1000)*100,"%" , "\n")
cat("Minimum number of ACI cll    : ",  numberAIC_lesscloglog_n20_logit,"or ",(numberAIC_lesscloglog_n20_logit/1000)*100,"%", "\n")


######################################### Check lower BIC #########################################
for (i in 1:1000) {
  if (BIC_n20_logit_linklogit[i] < BIC_n20_probit_linklogit[i] &&
      BIC_n20_logit_linklogit[i] < BIC_n20_cloglog_linklogit[i]) {
    numberBIC_lesslogit_n20_logit = numberBIC_lesslogit_n20_logit + 1
  } else if (BIC_n20_probit_linklogit[i] < BIC_n20_logit_linklogit[i] &&
             BIC_n20_probit_linklogit[i] < BIC_n20_cloglog_linklogit[i]) {
    numberBIC_lessprobit_n20_logit = numberBIC_lessprobit_n20_logit + 1
  } else {
    numberBIC_lesscloglog_n20_logit = numberBIC_lesscloglog_n20_logit + 1
  }
}
cat("Total number of BIC (n = 20) :", numberBIC_lesslogit_n20_logit + numberBIC_lessprobit_n20_logit +numberBIC_lesscloglog_n20_logit, "\n")
cat("Minimum number of BIC logit  : ",  numberBIC_lesslogit_n20_logit,"or ",(numberBIC_lesslogit_n20_logit/1000)*100, "%" ,"\n")
cat("Minimum number of BIC probit : ",  numberBIC_lessprobit_n20_logit,"or ",(numberBIC_lessprobit_n20_logit/1000)*100,"%" , "\n")
cat("Minimum number of BCI cll    : ",  numberBIC_lesscloglog_n20_logit,"or ",(numberBIC_lesscloglog_n20_logit/1000)*100,"%", "\n")




###################################### n = 50 ###################################
# Set n1
n1 <- 50

# Set the number of iterations
niter_n50_linklogit <- 1000

# Create empty vectors to store AIC and BIC values
AIC_n50_logit_linklogit <- rep(0, niter_n50_linklogit)
BIC_n50_logit_linklogit <- rep(0, niter_n50_linklogit)
AIC_n50_probit_linklogit <- rep(0, niter_n50_linklogit)
BIC_n50_probit_linklogit <- rep(0, niter_n50_linklogit)
AIC_n50_cloglog_linklogit <- rep(0, niter_n50_linklogit)
BIC_n50_cloglog_linklogit <- rep(0, niter_n50_linklogit)


for (i in 1:niter_n50_linklogit) {
  
  # Define the coefficients for the binomial regression model
  #beta <- c(0.13279, -0.04084, 0.12983, -0.02155) # SetNew_logit
  beta <- c(0.1328000, -0.0408400, 0.0001298, -0.0215500) # SetNewNew_logit
  
  # Set the Poisson parameter
  lambda1 <- 54  # Average number of students admitted
  lambda2 <- 50 # Average of number of students accepted in the 3rd round 
  
  # Simulate independent variables x1 and x2 from a normal distribution
  x1 <- runif(n1, min = 30.33,   max = 75.87)      # Highest Score Admission Round 3 runif(n, min, max) X5
  x2 <- runif(n1, min = 12127.1, max = 17239.5)    # Highest score Admission Round 4 runif(n, min, max) X8
  
  # Simulate independent variables x3 from a Bernoulli distribution
  x3 <- rpois(n1, lambda2) # Number of students accepted in the 3rd round  X7
  
  # Combine independent variables into a matrix X
  X <- matrix(0, nrow = n1, ncol = 4)
  X[, 1] <- t(rep(1, n1))  # intercept term
  X[, 2] <- x1
  X[, 3] <- x2
  X[, 4] <- x3
  
  # Generate the Poisson counts
  ni <- rpois(n1, lambda1) # Number of students admitted
  
  # Calculate the predicted probabilities pi for each observation in X
  PropY_linklogit <- exp(X %*% beta) / (1 + exp(X %*% beta))
  
  # Simulate the dependent variable y for each observation from a binomial distribution
  Y_logit <- rbinom(n1, ni, PropY_linklogit)
  
  
  # Fit the binomial regression model using glm function
  binomial_model_logit <- glm( cbind(Y_logit, ni-Y_logit) ~ x1 + x2 + x3, family = binomial(link = "logit"))
  binomial_model_probit <- glm( cbind(Y_logit, ni-Y_logit) ~ x1 + x2 + x3, family = binomial(link = "probit"))
  binomial_model_cll <- glm( cbind(Y_logit, ni-Y_logit) ~ x1 + x2 + x3, family = binomial(link = "cloglog"))
  
  # View the summary of the model
  summary(binomial_model_logit)
  summary(binomial_model_probit)
  summary(binomial_model_cll)
  
  # Store the AIC and BIC values
  AIC_n50_logit_linklogit[i] <- AIC(binomial_model_logit)
  BIC_n50_logit_linklogit[i] <- BIC(binomial_model_logit)
  AIC_n50_probit_linklogit[i] <- AIC(binomial_model_probit)
  BIC_n50_probit_linklogit[i] <- BIC(binomial_model_probit)
  AIC_n50_cloglog_linklogit[i] <- AIC(binomial_model_cll)
  BIC_n50_cloglog_linklogit[i] <- BIC(binomial_model_cll)
  
}

meanAIC_logit_linklogit_n50 <- mean(AIC_n50_logit_linklogit)
meanBIC_logit_linklogit_n50 <- mean(BIC_n50_logit_linklogit)
meanAIC_probit_linklogit_n50 <- mean(AIC_n50_probit_linklogit)
meanBIC_probit_linklogit_n50 <- mean(BIC_n50_probit_linklogit)
meanAIC_cloglog_linklogit_n50 <- mean(AIC_n50_cloglog_linklogit)
meanBIC_cloglog_linklogit_n50 <- mean(BIC_n50_cloglog_linklogit)


# Create a data frame to store the confidence intervals and model fit statistics
model_comparison_n50_linklogit <- data.frame(
  Model = c("Logit", "Probit", "Complementary Log-Log"),
  AIC = c(meanAIC_logit_linklogit_n50, meanAIC_probit_linklogit_n50 , meanAIC_cloglog_linklogit_n50),
  BIC = c(meanBIC_logit_linklogit_n50, meanBIC_probit_linklogit_n50, meanBIC_cloglog_linklogit_n50)
)

# Print the table
print(model_comparison_n50_linklogit)

# Count the lowest number of times
numberAIC_lesslogit_n50_logit   <- 0
numberAIC_lessprobit_n50_logit  <- 0
numberAIC_lesscloglog_n50_logit <- 0
numberBIC_lesslogit_n50_logit   <- 0
numberBIC_lessprobit_n50_logit  <- 0
numberBIC_lesscloglog_n50_logit <- 0


############################ Check lower AIC ############################
for (i in 1:1000) {
  if (AIC_n50_logit_linklogit[i] < AIC_n50_probit_linklogit[i] &&
      AIC_n50_logit_linklogit[i] < AIC_n50_cloglog_linklogit[i]) {
    numberAIC_lesslogit_n50_logit = numberAIC_lesslogit_n50_logit + 1
  } else if (AIC_n50_probit_linklogit[i] < AIC_n50_logit_linklogit[i] &&
             AIC_n50_probit_linklogit[i] < AIC_n50_cloglog_linklogit[i]) {
    numberAIC_lessprobit_n50_logit = numberAIC_lessprobit_n50_logit + 1
  } else {
    numberAIC_lesscloglog_n50_logit = numberAIC_lesscloglog_n50_logit + 1
  }
}

cat("Total number of AIC (n = 50) :", numberAIC_lesslogit_n50_logit + numberAIC_lessprobit_n50_logit +numberAIC_lesscloglog_n50_logit, "\n")
cat("Minimum number of AIC logit  : ",  numberAIC_lesslogit_n50_logit,"or ",(numberAIC_lesslogit_n50_logit/1000)*100, "%" ,"\n")
cat("Minimum number of AIC probit : ",  numberAIC_lessprobit_n50_logit,"or ",(numberAIC_lessprobit_n50_logit/1000)*100,"%" , "\n")
cat("Minimum number of ACI cll    : ",  numberAIC_lesscloglog_n50_logit,"or ",(numberAIC_lesscloglog_n50_logit/1000)*100,"%", "\n")


######################################### Check lower BIC #########################################
for (i in 1:1000) {
  if (BIC_n50_logit_linklogit[i] < BIC_n50_probit_linklogit[i] &&
      BIC_n50_logit_linklogit[i] < BIC_n50_cloglog_linklogit[i]) {
    numberBIC_lesslogit_n50_logit = numberBIC_lesslogit_n50_logit + 1
  } else if (BIC_n50_probit_linklogit[i] < BIC_n50_logit_linklogit[i] &&
             BIC_n50_probit_linklogit[i] < BIC_n50_cloglog_linklogit[i]) {
    numberBIC_lessprobit_n50_logit = numberBIC_lessprobit_n50_logit + 1
  } else {
    numberBIC_lesscloglog_n50_logit = numberBIC_lesscloglog_n50_logit + 1
  }
}
cat("Total number of BIC (n = 50) :", numberBIC_lesslogit_n50_logit + numberBIC_lessprobit_n50_logit +numberBIC_lesscloglog_n50_logit, "\n")
cat("Minimum number of BIC logit  : ",  numberBIC_lesslogit_n50_logit,"or ",(numberBIC_lesslogit_n50_logit/1000)*100, "%" ,"\n")
cat("Minimum number of BIC probit : ",  numberBIC_lessprobit_n50_logit,"or ",(numberBIC_lessprobit_n50_logit/1000)*100,"%" , "\n")
cat("Minimum number of BCI cll    : ",  numberBIC_lesscloglog_n50_logit,"or ",(numberBIC_lesscloglog_n50_logit/1000)*100,"%", "\n")



###################################### n = 200 ###################################
n2 = 200

# Set the number of iterations
niter_n200_linklogit <- 1000

# Create empty vectors to store AIC and BIC values
AIC_n200_logit_linklogit <- rep(0, niter_n200_linklogit)
BIC_n200_logit_linklogit <- rep(0, niter_n200_linklogit)
AIC_n200_probit_linklogit <- rep(0, niter_n200_linklogit)
BIC_n200_probit_linklogit <- rep(0, niter_n200_linklogit)
AIC_n200_cloglog_linklogit <- rep(0, niter_n200_linklogit)
BIC_n200_cloglog_linklogit <- rep(0, niter_n200_linklogit)

for (i in 1:niter_n200_linklogit) {
  
  # Define the coefficients for the binomial regression model
  #beta <- c(0.13279, -0.04084, 0.12983, -0.02155) # SetNew_logit
  beta <- c(0.1328000, -0.0408400, 0.0001298, -0.0215500) # SetNewNew_logit
  
  # Set the Poisson parameter
  lambda1 <- 54  # Average number of students admitted
  lambda2 <- 50 # Average of number of students accepted in the 3rd round 
  
  # Simulate independent variables x1 and x2 from a normal distribution
  x1 <- runif(n2, min = 30.33,   max = 75.87)      # Highest Score Admission Round 3 runif(n, min, max)
  x2 <- runif(n2, min = 12127.1, max = 17239.5)    # Highest score Admission Round 4 runif(n, min, max) X8
  
  # Simulate independent variables x3 from a Bernoulli distribution
  x3 <- rpois(n2, lambda2) # Number of students accepted in the 3rd round
  
  # Combine independent variables into a matrix X
  X <- matrix(0, nrow = n2, ncol = 4)
  X[, 1] <- t(rep(1, n2))  # intercept term
  X[, 2] <- x1
  X[, 3] <- x2
  X[, 4] <- x3
  
  # Generate the Poisson counts
  ni <- rpois(n2, lambda1) # Number of students admitted
  
  # Calculate the predicted probabilities pi for each observation in X
  PropY_linklogit <- exp(X %*% beta) / (1 + exp(X %*% beta))
  
  # Simulate the dependent variable y for each observation from a binomial distribution
  Y_logit <- rbinom(n2, ni, PropY_linklogit)
  Y_logit
  
  
  # Fit the binomial regression model using glm function
  binomial_model_logit <- glm( cbind(Y_logit, ni-Y_logit) ~ x1 + x2 + x3, family = binomial(link = "logit"))
  binomial_model_probit <- glm( cbind(Y_logit, ni-Y_logit) ~ x1 + x2 + x3, family = binomial(link = "probit"))
  binomial_model_cll <- glm( cbind(Y_logit, ni-Y_logit) ~ x1 + x2 + x3, family = binomial(link = "cloglog"))
  
  # View the summary of the model
  summary(binomial_model_logit)
  summary(binomial_model_probit)
  summary(binomial_model_cll)
  
  # Store the AIC and BIC values
  AIC_n200_logit_linklogit[i] <- AIC(binomial_model_logit)
  BIC_n200_logit_linklogit[i] <- BIC(binomial_model_logit)
  AIC_n200_probit_linklogit[i] <- AIC(binomial_model_probit)
  BIC_n200_probit_linklogit[i] <- BIC(binomial_model_probit)
  AIC_n200_cloglog_linklogit[i] <- AIC(binomial_model_cll)
  BIC_n200_cloglog_linklogit[i] <- BIC(binomial_model_cll)
  
}

# set mean
meanAIC_logit_linklogit_n200 <- mean(AIC_n200_logit_linklogit)
meanBIC_logit_linklogit_n200 <- mean(BIC_n200_logit_linklogit)
meanAIC_probit_linklogit_n200 <- mean(AIC_n200_probit_linklogit)
meanBIC_probit_linklogit_n200 <- mean(BIC_n200_probit_linklogit)
meanAIC_cloglog_linklogit_n200 <- mean(AIC_n200_cloglog_linklogit)
meanBIC_cloglog_linklogit_n200 <- mean(BIC_n200_cloglog_linklogit)

# Create a data frame to store the confidence intervals and model fit statistics
model_comparison_n200_linklogit <- data.frame(
  Model = c("Logit", "Probit", "Complementary Log-Log"),
  AIC = c(meanAIC_logit_linklogit_n200, meanAIC_probit_linklogit_n200 , meanAIC_cloglog_linklogit_n200),
  BIC = c(meanBIC_logit_linklogit_n200, meanBIC_probit_linklogit_n200, meanBIC_cloglog_linklogit_n200)
)

# Print the table
print(model_comparison_n200_linklogit)

# Count the lowest number of times
numberAIC_lesslogit_n200_logit   <- 0
numberAIC_lessprobit_n200_logit  <- 0
numberAIC_lesscloglog_n200_logit <- 0
numberBIC_lesslogit_n200_logit   <- 0
numberBIC_lessprobit_n200_logit  <- 0
numberBIC_lesscloglog_n200_logit <- 0


######################################### Check lower AIC #########################################
for (i in 1:1000) {
  if (AIC_n200_logit_linklogit[i] < AIC_n200_probit_linklogit[i] &&
      AIC_n200_logit_linklogit[i] < AIC_n200_cloglog_linklogit[i]) {
    numberAIC_lesslogit_n200_logit = numberAIC_lesslogit_n200_logit + 1
  } else if (AIC_n200_probit_linklogit[i] < AIC_n200_logit_linklogit[i] &&
             AIC_n200_probit_linklogit[i] < AIC_n200_cloglog_linklogit[i]) {
    numberAIC_lessprobit_n200_logit = numberAIC_lessprobit_n200_logit + 1
  } else {
    numberAIC_lesscloglog_n200_logit = numberAIC_lesscloglog_n200_logit + 1
  }
}

cat("Total number of AIC (n = 200):", numberAIC_lesslogit_n200_logit + numberAIC_lessprobit_n200_logit +numberAIC_lesscloglog_n200_logit, "\n")
cat("Minimum number of AIC logit  : ",  numberAIC_lesslogit_n200_logit,"or ",(numberAIC_lesslogit_n200_logit/1000)*100, "%" ,"\n")
cat("Minimum number of AIC probit : ",  numberAIC_lessprobit_n200_logit,"or ",(numberAIC_lessprobit_n200_logit/1000)*100,"%" , "\n")
cat("Minimum number of ACI cll    : ",  numberAIC_lesscloglog_n200_logit,"or ",(numberAIC_lesscloglog_n200_logit/1000)*100,"%", "\n")


######################################### Check lower BIC #########################################
for (i in 1:1000) {
  if (BIC_n200_logit_linklogit[i] < BIC_n200_probit_linklogit[i] &&
      BIC_n200_logit_linklogit[i] < BIC_n200_cloglog_linklogit[i]) {
    numberBIC_lesslogit_n200_logit = numberBIC_lesslogit_n200_logit + 1
  } else if (BIC_n200_probit_linklogit[i] < BIC_n200_logit_linklogit[i] &&
             BIC_n200_probit_linklogit[i] < BIC_n200_cloglog_linklogit[i]) {
    numberBIC_lessprobit_n200_logit = numberBIC_lessprobit_n200_logit + 1
  } else {
    numberBIC_lesscloglog_n200_logit = numberBIC_lesscloglog_n200_logit + 1
  }
}
cat("Total number of BIC (n = 200):", numberBIC_lesslogit_n200_logit + numberBIC_lessprobit_n200_logit + numberBIC_lesscloglog_n200_logit, "\n")
cat("Minimum number of BIC logit  : ",  numberBIC_lesslogit_n200_logit,"or ",(numberBIC_lesslogit_n200_logit/1000)*100, "%" ,"\n")
cat("Minimum number of BIC probit : ",  numberBIC_lessprobit_n200_logit,"or ",(numberBIC_lessprobit_n200_logit/1000)*100,"%" , "\n")
cat("Minimum number of BCI cll    : ",  numberBIC_lesscloglog_n200_logit,"or ",(numberBIC_lesscloglog_n200_logit/1000)*100,"%", "\n")


###################################### n = 750 ###################################
n3 <- 750

# Set the number of iterations
niter_n750_linklogit <- 1000

# Create empty vectors to store AIC and BIC values
AIC_n750_logit_linklogit <- rep(0, niter_n750_linklogit)
BIC_n750_logit_linklogit <- rep(0, niter_n750_linklogit)
AIC_n750_probit_linklogit <- rep(0, niter_n750_linklogit)
BIC_n750_probit_linklogit <- rep(0, niter_n750_linklogit)
AIC_n750_cloglog_linklogit <- rep(0, niter_n750_linklogit)
BIC_n750_cloglog_linklogit <- rep(0, niter_n750_linklogit)

for (i in 1:niter_n750_linklogit) {
  
  # Define the coefficients for the binomial regression model
  #beta <- c(0.13279, -0.04084, 0.12983, -0.02155) # SetNew_logit
  beta <- c(0.1328000, -0.0408400, 0.0001298, -0.0215500) # SetNewNew_logit
  
  # Set the Poisson parameter
  lambda1 <- 54  # Average number of students admitted
  lambda2 <- 50 # Average of number of students accepted in the 3rd round 
  
  # Simulate independent variables x1 and x2 from a normal distribution
  x1 <- runif(n3, min = 30.33,   max = 75.87)      # Highest Score Admission Round 3 runif(n, min, max)
  x2 <- runif(n3, min = 12127.1, max = 17239.5)    # Highest score Admission Round 4 runif(n, min, max) X8
  
  # Simulate independent variables x3 from a Bernoulli distribution
  x3 <- rpois(n3, lambda2) # Number of students accepted in the 3rd round
  
  # Combine independent variables into a matrix X
  X <- matrix(0, nrow = n3, ncol = 4)
  X[, 1] <- t(rep(1, n3))  # intercept term
  X[, 2] <- x1
  X[, 3] <- x2
  X[, 4] <- x3
  
  # Generate the Poisson counts
  ni <- rpois(n3, lambda1) # Number of students admitted
  
  # Calculate the predicted probabilities pi for each observation in X
  PropY_linklogit <- exp(X %*% beta) / (1 + exp(X %*% beta))
  
  # Simulate the dependent variable y for each observation from a binomial distribution
  Y_logit <- rbinom(n3, ni, PropY_linklogit)
  Y_logit
  
  
  # Fit the binomial regression model using glm function
  binomial_model_logit <- glm( cbind(Y_logit, ni-Y_logit) ~ x1 + x2 + x3, family = binomial(link = "logit"))
  binomial_model_probit <- glm( cbind(Y_logit, ni-Y_logit) ~ x1 + x2 + x3, family = binomial(link = "probit"))
  binomial_model_cll <- glm( cbind(Y_logit, ni-Y_logit) ~ x1 + x2 + x3, family = binomial(link = "cloglog"))
  
  # View the summary of the model
  summary(binomial_model_logit)
  summary(binomial_model_probit)
  summary(binomial_model_cll)
  
  # Store the AIC and BIC values
  AIC_n750_logit_linklogit[i] <- AIC(binomial_model_logit)
  BIC_n750_logit_linklogit[i] <- BIC(binomial_model_logit)
  AIC_n750_probit_linklogit[i] <- AIC(binomial_model_probit)
  BIC_n750_probit_linklogit[i] <- BIC(binomial_model_probit)
  AIC_n750_cloglog_linklogit[i] <- AIC(binomial_model_cll)
  BIC_n750_cloglog_linklogit[i] <- BIC(binomial_model_cll)
  
  
}

# set mean
meanAIC_logit_linklogit_n750 <- mean(AIC_n750_logit_linklogit)
meanBIC_logit_linklogit_n750 <- mean(BIC_n750_logit_linklogit)
meanAIC_probit_linklogit_n750 <- mean(AIC_n750_probit_linklogit)
meanBIC_probit_linklogit_n750 <- mean(BIC_n750_probit_linklogit)
meanAIC_cloglog_linklogit_n750 <- mean(AIC_n750_cloglog_linklogit)
meanBIC_cloglog_linklogit_n750 <- mean(BIC_n750_cloglog_linklogit)

# Create a data frame to store the confidence intervals and model fit statistics
model_comparison_n750_linklogit <- data.frame(
  Model = c("Logit", "Probit", "Clog-log"),
  AIC = c(meanAIC_logit_linklogit_n750, meanAIC_probit_linklogit_n750 , meanAIC_cloglog_linklogit_n750),
  BIC = c(meanBIC_logit_linklogit_n750, meanBIC_probit_linklogit_n750, meanBIC_cloglog_linklogit_n750)
)

# Print the table
print(model_comparison_n750_linklogit)

# Count the lowest number of times
numberAIC_lesslogit_n750_logit   <- 0
numberAIC_lessprobit_n750_logit  <- 0
numberAIC_lesscloglog_n750_logit <- 0
numberBIC_lesslogit_n750_logit   <- 0
numberBIC_lessprobit_n750_logit  <- 0
numberBIC_lesscloglog_n750_logit <- 0


######################################### Check lower AIC #########################################
for (i in 1:1000) {
  if (AIC_n750_logit_linklogit[i] < AIC_n750_probit_linklogit[i] &&
      AIC_n750_logit_linklogit[i] < AIC_n750_cloglog_linklogit[i]) {
    numberAIC_lesslogit_n750_logit = numberAIC_lesslogit_n750_logit + 1
  } else if (AIC_n750_probit_linklogit[i] < AIC_n750_logit_linklogit[i] &&
             AIC_n750_probit_linklogit[i] < AIC_n750_cloglog_linklogit[i]) {
    numberAIC_lessprobit_n750_logit = numberAIC_lessprobit_n750_logit + 1
  } else {
    numberAIC_lesscloglog_n750_logit = numberAIC_lesscloglog_n750_logit + 1
  }
}

cat("Total number of AIC (n = 750):", numberAIC_lesslogit_n750_logit + numberAIC_lessprobit_n750_logit +numberAIC_lesscloglog_n750_logit, "\n")
cat("Minimum number of AIC logit  : ",  numberAIC_lesslogit_n750_logit,"or ",(numberAIC_lesslogit_n750_logit/1000)*100, "%" ,"\n")
cat("Minimum number of AIC probit : ",  numberAIC_lessprobit_n750_logit,"or ",(numberAIC_lessprobit_n750_logit/1000)*100,"%" , "\n")
cat("Minimum number of ACI cll    : ",  numberAIC_lesscloglog_n750_logit,"or ",(numberAIC_lesscloglog_n750_logit/1000)*100,"%", "\n")


######################################### Check lower BIC #########################################
for (i in 1:1000) {
  if (BIC_n750_logit_linklogit[i] < BIC_n750_probit_linklogit[i] &&
      BIC_n750_logit_linklogit[i] < BIC_n750_cloglog_linklogit[i]) {
    numberBIC_lesslogit_n750_logit = numberBIC_lesslogit_n750_logit + 1
  } else if (BIC_n750_probit_linklogit[i] < BIC_n750_logit_linklogit[i] &&
             BIC_n750_probit_linklogit[i] < BIC_n750_cloglog_linklogit[i]) {
    numberBIC_lessprobit_n750_logit = numberBIC_lessprobit_n750_logit + 1
  } else {
    numberBIC_lesscloglog_n750_logit = numberBIC_lesscloglog_n750_logit + 1
  }
}
cat("Total number of BIC (n = 750):", numberBIC_lesslogit_n750_logit + numberBIC_lessprobit_n750_logit + numberBIC_lesscloglog_n750_logit, "\n")
cat("Minimum number of BIC logit  : ",  numberBIC_lesslogit_n750_logit,"or ",(numberBIC_lesslogit_n750_logit/1000)*100, "%" ,"\n")
cat("Minimum number of BIC probit : ",  numberBIC_lessprobit_n750_logit,"or ",(numberBIC_lessprobit_n750_logit/1000)*100,"%" , "\n")
cat("Minimum number of BCI cll    : ",  numberBIC_lesscloglog_n750_logit,"or ",(numberBIC_lesscloglog_n750_logit/1000)*100,"%", "\n")




###################################Link function Probit is pi ###############################

# Set the number of observations
n0 < 20
n1 <- 50
n2 <- 200
n3 <- 750

###################################### n = 20 ###################################
n0 <- 20

# Set the number of iterations
niter_n20_linkprobit <- 1000

# Create empty vectors to store AIC and BIC values
AIC_n20_logit_linkprobit <- rep(0, niter_n20_linkprobit)
BIC_n20_logit_linkprobit <- rep(0, niter_n20_linkprobit)
AIC_n20_probit_linkprobit <- rep(0, niter_n20_linkprobit)
BIC_n20_probit_linkprobit <- rep(0, niter_n20_linkprobit)
AIC_n20_cloglog_linkprobit <- rep(0, niter_n20_linkprobit)
BIC_n20_cloglog_linkprobit <- rep(0, niter_n20_linkprobit)

for (i in 1:niter_n20_linkprobit) {
  
  # Define the coefficients for the binomial regression model
  #beta <- c(0.06908, -0.02454, 0.07802, -0.01291) # SetNew_probit
  beta <- c(0.0690800, -0.0245400, 0.0000780, -0.0129100) # SetNewNew_probit
  
  # Set the Poisson parameter
  lambda1 <- 54  # Average number of students admitted
  lambda2 <- 50 # Average of number of students accepted in the 3rd round 
  
  # Simulate independent variables x1 and x2 from a normal distribution
  x1 <- runif(n0, min = 30.33,   max = 75.87)      # Highest Score Admission Round 3 runif(n, min, max)
  x2 <- runif(n0, min = 12127.1, max = 17239.5)    # Highest score Admission Round 4 runif(n, min, max) X8
  
  # Simulate independent variables x3 from a Bernoulli distribution
  x3 <- rpois(n0, lambda2) # Number of students accepted in the 3rd round
  
  # Combine independent variables into a matrix X
  X <- matrix(0, nrow = n0, ncol = 4)
  X[, 1] <- t(rep(1, n0))  # intercept term
  X[, 2] <- x1
  X[, 3] <- x2
  X[, 4] <- x3
  
  # Generate the Poisson counts
  ni <- rpois(n0, lambda1) # Number of students admitted
  
  # Define the Probit link function
  phi <- function(x) {
    pnorm(x)
  }
  
  # Calculate the predicted probabilities pi for each observation in X using the Probit link function
  pi_probit <- phi(X %*% beta)
  pi_probit
  
  # Simulate the dependent variable y for each observation from a binomial distribution
  Y_probit <- rbinom(n0, ni, pi_probit)
  
  
  # Fit the binomial regression model using glm function
  binomial_model_logit <- glm( cbind(Y_probit, ni-Y_probit) ~ x1 + x2 + x3, family = binomial(link = "logit"))
  binomial_model_probit <- glm( cbind(Y_probit, ni-Y_probit) ~ x1 + x2 + x3, family = binomial(link = "probit"))
  binomial_model_cll <- glm( cbind(Y_probit, ni-Y_probit) ~ x1 + x2 + x3, family = binomial(link = "cloglog"))
  
  # View the summary of the model
  summary(binomial_model_logit)
  summary(binomial_model_probit)
  summary(binomial_model_cll)
  
  AIC_n20_logit_linkprobit[i] <- AIC(binomial_model_logit)
  BIC_n20_logit_linkprobit[i] <- BIC(binomial_model_logit)
  AIC_n20_probit_linkprobit[i] <- AIC(binomial_model_probit)
  BIC_n20_probit_linkprobit[i] <- BIC(binomial_model_probit)
  AIC_n20_cloglog_linkprobit[i] <- AIC(binomial_model_cll)
  BIC_n20_cloglog_linkprobit[i] <- BIC(binomial_model_cll)
  
}

# Set mean
meanAIC_logit_linkprobit_n20 <- mean(AIC_n20_logit_linkprobit)
meanBIC_logit_linkprobit_n20 <- mean(BIC_n20_logit_linkprobit)
meanAIC_probit_linkprobit_n20 <- mean(AIC_n20_probit_linkprobit)
meanBIC_probit_linkprobit_n20 <- mean(BIC_n20_probit_linkprobit)
meanAIC_cloglog_linkprobit_n20 <- mean(AIC_n20_cloglog_linkprobit)
meanBIC_cloglog_linkprobit_n20 <- mean(BIC_n20_cloglog_linkprobit)

# Create a data frame to store the confidence intervals and model fit statistics
model_comparison_n20_linkprobit <- data.frame(
  Model = c("Logit", "Probit", "Complementary Log-Log"),
  AIC = c(meanAIC_logit_linkprobit_n20, meanAIC_probit_linkprobit_n20 , meanAIC_cloglog_linkprobit_n20),
  BIC = c(meanBIC_logit_linkprobit_n20, meanBIC_probit_linkprobit_n20, meanBIC_cloglog_linkprobit_n20)
)

# Print the table
print(model_comparison_n20_linkprobit)

# Count the lowest number of times
numberAIC_lesslogit_n20_probit   <- 0
numberAIC_lessprobit_n20_probit  <- 0
numberAIC_lesscloglog_n20_probit <- 0
numberBIC_lesslogit_n20_probit   <- 0
numberBIC_lessprobit_n20_probit  <- 0
numberBIC_lesscloglog_n20_probit <- 0


######################################### Check lower AIC #########################################
for (i in 1:1000) {
  if (AIC_n20_logit_linkprobit[i] < AIC_n20_probit_linkprobit[i] &&
      AIC_n20_logit_linkprobit[i] < AIC_n20_cloglog_linkprobit[i]) {
    numberAIC_lesslogit_n20_probit = numberAIC_lesslogit_n20_probit + 1
  } else if (AIC_n20_probit_linkprobit[i] < AIC_n20_logit_linkprobit[i] &&
             AIC_n20_probit_linkprobit[i] < AIC_n20_cloglog_linkprobit[i]) {
    numberAIC_lessprobit_n20_probit = numberAIC_lessprobit_n20_probit + 1
  } else {
    numberAIC_lesscloglog_n20_probit = numberAIC_lesscloglog_n20_probit + 1
  }
}

cat("Total number of AIC (n = 20):", numberAIC_lesslogit_n20_probit + numberAIC_lessprobit_n20_probit +numberAIC_lesscloglog_n20_probit, "\n")
cat("Minimum number of AIC logit  : ",  numberAIC_lesslogit_n20_probit,"or ",(numberAIC_lesslogit_n20_probit/1000)*100, "%" ,"\n")
cat("Minimum number of AIC probit : ",  numberAIC_lessprobit_n20_probit,"or ",(numberAIC_lessprobit_n20_probit/1000)*100,"%" , "\n")
cat("Minimum number of ACI cll    : ",  numberAIC_lesscloglog_n20_probit,"or ",(numberAIC_lesscloglog_n20_probit/1000)*100,"%", "\n")


######################################### Check lower BIC #########################################
for (i in 1:1000) {
  if (BIC_n20_logit_linkprobit[i] < BIC_n20_probit_linkprobit[i] &&
      BIC_n20_logit_linkprobit[i] < BIC_n20_cloglog_linkprobit[i]) {
    numberBIC_lesslogit_n20_probit = numberBIC_lesslogit_n20_probit + 1
  } else if (BIC_n20_probit_linkprobit[i] < BIC_n20_logit_linkprobit[i] &&
             BIC_n20_probit_linkprobit[i] < BIC_n20_cloglog_linkprobit[i]) {
    numberBIC_lessprobit_n20_probit = numberBIC_lessprobit_n20_probit + 1
  } else {
    numberBIC_lesscloglog_n20_probit = numberBIC_lesscloglog_n20_probit + 1
  }
}
cat("Total number of BIC (n = 20):", numberBIC_lesslogit_n20_probit + numberBIC_lessprobit_n20_probit + numberBIC_lesscloglog_n20_probit, "\n")
cat("Minimum number of BIC logit  : ",  numberBIC_lesslogit_n20_probit,"or ",(numberBIC_lesslogit_n20_probit/1000)*100, "%" ,"\n")
cat("Minimum number of BIC probit : ",  numberBIC_lessprobit_n20_probit,"or ",(numberBIC_lessprobit_n20_probit/1000)*100,"%" , "\n")
cat("Minimum number of BCI cll    : ",  numberBIC_lesscloglog_n20_probit,"or ",(numberBIC_lesscloglog_n20_probit/1000)*100,"%", "\n")

###################################### n = 50 ###################################
n1 <- 50

# Set the number of iterations
niter_n50_linkprobit <- 1000

# Create empty vectors to store AIC and BIC values
AIC_n50_logit_linkprobit <- rep(0, niter_n50_linkprobit)
BIC_n50_logit_linkprobit <- rep(0, niter_n50_linkprobit)
AIC_n50_probit_linkprobit <- rep(0, niter_n50_linkprobit)
BIC_n50_probit_linkprobit <- rep(0, niter_n50_linkprobit)
AIC_n50_cloglog_linkprobit <- rep(0, niter_n50_linkprobit)
BIC_n50_cloglog_linkprobit <- rep(0, niter_n50_linkprobit)

for (i in 1:niter_n50_linkprobit) {
  
  # Define the coefficients for the binomial regression model
  #beta <- c(0.06908, -0.02454, 0.07802, -0.01291) # SetNew_probit
  beta <- c(0.0690800, -0.0245400, 0.0000780, -0.0129100) # SetNewNew_probit
  
  # Set the Poisson parameter
  lambda1 <- 54  # Average number of students admitted
  lambda2 <- 50 # Average of number of students accepted in the 3rd round 
  
  # Simulate independent variables x1 and x2 from a normal distribution
  x1 <- runif(n1, min = 30.33,   max = 75.87)      # Highest Score Admission Round 3 runif(n, min, max)
  x2 <- runif(n1, min = 12127.1, max = 17239.5)    # Highest score Admission Round 4 runif(n, min, max) X8
  
  # Simulate independent variables x3 from a Bernoulli distribution
  x3 <- rpois(n1, lambda2) # Number of students accepted in the 3rd round
  
  # Combine independent variables into a matrix X
  X <- matrix(0, nrow = n1, ncol = 4)
  X[, 1] <- t(rep(1, n1))  # intercept term
  X[, 2] <- x1
  X[, 3] <- x2
  X[, 4] <- x3
  
  # Generate the Poisson counts
  ni <- rpois(n1, lambda1) # Number of students admitted
  
  # Define the Probit link function
  phi <- function(x) {
    pnorm(x)
  }
  
  # Calculate the predicted probabilities pi for each observation in X using the Probit link function
  pi_probit <- phi(X %*% beta)
  pi_probit
  
  # Simulate the dependent variable y for each observation from a binomial distribution
  Y_probit <- rbinom(n1, ni, pi_probit)
  
  
  # Fit the binomial regression model using glm function
  binomial_model_logit <- glm( cbind(Y_probit, ni-Y_probit) ~ x1 + x2 + x3, family = binomial(link = "logit"))
  binomial_model_probit <- glm( cbind(Y_probit, ni-Y_probit) ~ x1 + x2 + x3, family = binomial(link = "probit"))
  binomial_model_cll <- glm( cbind(Y_probit, ni-Y_probit) ~ x1 + x2 + x3, family = binomial(link = "cloglog"))
  
  # View the summary of the model
  summary(binomial_model_logit)
  summary(binomial_model_probit)
  summary(binomial_model_cll)
  
  AIC_n50_logit_linkprobit[i] <- AIC(binomial_model_logit)
  BIC_n50_logit_linkprobit[i] <- BIC(binomial_model_logit)
  AIC_n50_probit_linkprobit[i] <- AIC(binomial_model_probit)
  BIC_n50_probit_linkprobit[i] <- BIC(binomial_model_probit)
  AIC_n50_cloglog_linkprobit[i] <- AIC(binomial_model_cll)
  BIC_n50_cloglog_linkprobit[i] <- BIC(binomial_model_cll)
  
}

# Set mean
meanAIC_logit_linkprobit_n50 <- mean(AIC_n50_logit_linkprobit)
meanBIC_logit_linkprobit_n50 <- mean(BIC_n50_logit_linkprobit)
meanAIC_probit_linkprobit_n50 <- mean(AIC_n50_probit_linkprobit)
meanBIC_probit_linkprobit_n50 <- mean(BIC_n50_probit_linkprobit)
meanAIC_cloglog_linkprobit_n50 <- mean(AIC_n50_cloglog_linkprobit)
meanBIC_cloglog_linkprobit_n50 <- mean(BIC_n50_cloglog_linkprobit)

# Create a data frame to store the confidence intervals and model fit statistics
model_comparison_n50_linkprobit <- data.frame(
  Model = c("Logit", "Probit", "Complementary Log-Log"),
  AIC = c(meanAIC_logit_linkprobit_n50, meanAIC_probit_linkprobit_n50 , meanAIC_cloglog_linkprobit_n50),
  BIC = c(meanBIC_logit_linkprobit_n50, meanBIC_probit_linkprobit_n50, meanBIC_cloglog_linkprobit_n50)
)

# Print the table
print(model_comparison_n50_linkprobit)

# Count the lowest number of times
numberAIC_lesslogit_n50_probit   <- 0
numberAIC_lessprobit_n50_probit  <- 0
numberAIC_lesscloglog_n50_probit <- 0
numberBIC_lesslogit_n50_probit   <- 0
numberBIC_lessprobit_n50_probit  <- 0
numberBIC_lesscloglog_n50_probit <- 0


######################################### Check lower AIC #########################################
for (i in 1:1000) {
  if (AIC_n50_logit_linkprobit[i] < AIC_n50_probit_linkprobit[i] &&
      AIC_n50_logit_linkprobit[i] < AIC_n50_cloglog_linkprobit[i]) {
    numberAIC_lesslogit_n50_probit = numberAIC_lesslogit_n50_probit + 1
  } else if (AIC_n50_probit_linkprobit[i] < AIC_n50_logit_linkprobit[i] &&
             AIC_n50_probit_linkprobit[i] < AIC_n50_cloglog_linkprobit[i]) {
    numberAIC_lessprobit_n50_probit = numberAIC_lessprobit_n50_probit + 1
  } else {
    numberAIC_lesscloglog_n50_probit = numberAIC_lesscloglog_n50_probit + 1
  }
}

cat("Total number of AIC (n = 50):", numberAIC_lesslogit_n50_probit + numberAIC_lessprobit_n50_probit +numberAIC_lesscloglog_n50_probit, "\n")
cat("Minimum number of AIC logit  : ",  numberAIC_lesslogit_n50_probit,"or ",(numberAIC_lesslogit_n50_probit/1000)*100, "%" ,"\n")
cat("Minimum number of AIC probit : ",  numberAIC_lessprobit_n50_probit,"or ",(numberAIC_lessprobit_n50_probit/1000)*100,"%" , "\n")
cat("Minimum number of ACI cll    : ",  numberAIC_lesscloglog_n50_probit,"or ",(numberAIC_lesscloglog_n50_probit/1000)*100,"%", "\n")


######################################### Check lower BIC #########################################
for (i in 1:1000) {
  if (BIC_n50_logit_linkprobit[i] < BIC_n50_probit_linkprobit[i] &&
      BIC_n50_logit_linkprobit[i] < BIC_n50_cloglog_linkprobit[i]) {
    numberBIC_lesslogit_n50_probit = numberBIC_lesslogit_n50_probit + 1
  } else if (BIC_n50_probit_linkprobit[i] < BIC_n50_logit_linkprobit[i] &&
             BIC_n50_probit_linkprobit[i] < BIC_n50_cloglog_linkprobit[i]) {
    numberBIC_lessprobit_n50_probit = numberBIC_lessprobit_n50_probit + 1
  } else {
    numberBIC_lesscloglog_n50_probit = numberBIC_lesscloglog_n50_probit + 1
  }
}
cat("Total number of BIC (n = 50):", numberBIC_lesslogit_n50_probit + numberBIC_lessprobit_n50_probit + numberBIC_lesscloglog_n50_probit, "\n")
cat("Minimum number of BIC logit  : ",  numberBIC_lesslogit_n50_probit,"or ",(numberBIC_lesslogit_n50_probit/1000)*100, "%" ,"\n")
cat("Minimum number of BIC probit : ",  numberBIC_lessprobit_n50_probit,"or ",(numberBIC_lessprobit_n50_probit/1000)*100,"%" , "\n")
cat("Minimum number of BCI cll    : ",  numberBIC_lesscloglog_n50_probit,"or ",(numberBIC_lesscloglog_n50_probit/1000)*100,"%", "\n")


###################################### n = 200 ###################################
n2 <- 200

# Set the number of iterations
niter_n200_linkprobit <- 1000

# Create empty vectors to store AIC and BIC values
AIC_n200_logit_linkprobit <- rep(0, niter_n200_linkprobit)
BIC_n200_logit_linkprobit <- rep(0, niter_n200_linkprobit)
AIC_n200_probit_linkprobit <- rep(0, niter_n200_linkprobit)
BIC_n200_probit_linkprobit <- rep(0, niter_n200_linkprobit)
AIC_n200_cloglog_linkprobit <- rep(0, niter_n200_linkprobit)
BIC_n200_cloglog_linkprobit <- rep(0, niter_n200_linkprobit)

for (i in 1:niter_n200_linkprobit) {
  
  # Define the coefficients for the binomial regression model
  #beta <- c(0.06908, -0.02454, 0.07802, -0.01291) # SetNew_probit
  beta <- c(0.0690800, -0.0245400, 0.0000780, -0.0129100) # SetNewNew_probit
  
  # Set the Poisson parameter
  lambda1 <- 54  # Average number of students admitted
  lambda2 <- 50 # Average of number of students accepted in the 3rd round 
  
  # Simulate independent variables x1 and x2 from a normal distribution
  x1 <- runif(n2, min = 30.33,   max = 75.87)      # Highest Score Admission Round 3 runif(n, min, max)
  x2 <- runif(n2, min = 12127.1, max = 17239.5)    # Highest score Admission Round 4 runif(n, min, max) X8
  
  # Simulate independent variables x3 from a Bernoulli distribution
  x3 <- rpois(n2, lambda2) # Number of students accepted in the 3rd round
  
  # Combine independent variables into a matrix X
  X <- matrix(0, nrow = n2, ncol = 4)
  X[, 1] <- t(rep(1, n2))  # intercept term
  X[, 2] <- x1
  X[, 3] <- x2
  X[, 4] <- x3
  
  # Generate the Poisson counts
  ni <- rpois(n2, lambda1) # Number of students admitted
  
  # Define the Probit link function
  phi <- function(x) {
    pnorm(x)
  }
  
  # Calculate the predicted probabilities pi for each observation in X using the Probit link function
  pi_probit <- phi(X %*% beta)
  pi_probit
  
  # Simulate the dependent variable y for each observation from a binomial distribution
  Y_probit <- rbinom(n2, ni, pi_probit)
  Y_probit
  
  
  # Fit the binomial regression model using glm function
  binomial_model_logit <- glm( cbind(Y_probit, ni-Y_probit) ~ x1 + x2 + x3, family = binomial(link = "logit"))
  binomial_model_probit <- glm( cbind(Y_probit, ni-Y_probit) ~ x1 + x2 + x3, family = binomial(link = "probit"))
  binomial_model_cll <- glm( cbind(Y_probit, ni-Y_probit) ~ x1 + x2 + x3, family = binomial(link = "cloglog"))
  
  # View the summary of the model
  summary(binomial_model_logit)
  summary(binomial_model_probit)
  summary(binomial_model_cll)
  
  AIC_n200_logit_linkprobit[i] <- AIC(binomial_model_logit)
  BIC_n200_logit_linkprobit[i] <- BIC(binomial_model_logit)
  AIC_n200_probit_linkprobit[i] <- AIC(binomial_model_probit)
  BIC_n200_probit_linkprobit[i] <- BIC(binomial_model_probit)
  AIC_n200_cloglog_linkprobit[i] <- AIC(binomial_model_cll)
  BIC_n200_cloglog_linkprobit[i] <- BIC(binomial_model_cll)
  
}

# Set mean
meanAIC_logit_linkprobit_n200 <- mean(AIC_n200_logit_linkprobit)
meanBIC_logit_linkprobit_n200 <- mean(BIC_n200_logit_linkprobit)
meanAIC_probit_linkprobit_n200 <- mean(AIC_n200_probit_linkprobit)
meanBIC_probit_linkprobit_n200 <- mean(BIC_n200_probit_linkprobit)
meanAIC_cloglog_linkprobit_n200 <- mean(AIC_n200_cloglog_linkprobit)
meanBIC_cloglog_linkprobit_n200 <- mean(BIC_n200_cloglog_linkprobit)

# Create a data frame to store the confidence intervals and model fit statistics
model_comparison_n200_linkprobit <- data.frame(
  Model = c("Logit", "Probit", "Complementary Log-Log"),
  AIC = c(meanAIC_logit_linkprobit_n200, meanAIC_probit_linkprobit_n200 , meanAIC_cloglog_linkprobit_n200),
  BIC = c(meanBIC_logit_linkprobit_n200, meanBIC_probit_linkprobit_n200, meanBIC_cloglog_linkprobit_n200)
)

# Print the table
print(model_comparison_n200_linkprobit)

# Count the lowest number of times
numberAIC_lesslogit_n200_probit   <- 0
numberAIC_lessprobit_n200_probit  <- 0
numberAIC_lesscloglog_n200_probit <- 0
numberBIC_lesslogit_n200_probit   <- 0
numberBIC_lessprobit_n200_probit  <- 0
numberBIC_lesscloglog_n200_probit <- 0


######################################### Check lower AIC #########################################
for (i in 1:1000) {
  if (AIC_n200_logit_linkprobit[i] < AIC_n200_probit_linkprobit[i] &&
      AIC_n200_logit_linkprobit[i] < AIC_n200_cloglog_linkprobit[i]) {
    numberAIC_lesslogit_n200_probit = numberAIC_lesslogit_n200_probit + 1
  } else if (AIC_n200_probit_linkprobit[i] < AIC_n200_logit_linkprobit[i] &&
             AIC_n200_probit_linkprobit[i] < AIC_n200_cloglog_linkprobit[i]) {
    numberAIC_lessprobit_n200_probit = numberAIC_lessprobit_n200_probit + 1
  } else {
    numberAIC_lesscloglog_n200_probit = numberAIC_lesscloglog_n200_probit + 1
  }
}

cat("Total number of AIC (n = 200):", numberAIC_lesslogit_n200_probit + numberAIC_lessprobit_n200_probit +numberAIC_lesscloglog_n200_probit, "\n")
cat("Minimum number of AIC logit  : ",  numberAIC_lesslogit_n200_probit,"or ",(numberAIC_lesslogit_n200_probit/1000)*100, "%" ,"\n")
cat("Minimum number of AIC probit : ",  numberAIC_lessprobit_n200_probit,"or ",(numberAIC_lessprobit_n200_probit/1000)*100,"%" , "\n")
cat("Minimum number of ACI cll    : ",  numberAIC_lesscloglog_n200_probit,"or ",(numberAIC_lesscloglog_n200_probit/1000)*100,"%", "\n")


######################################### Check lower BIC #########################################
for (i in 1:1000) {
  if (BIC_n200_logit_linkprobit[i] < BIC_n200_probit_linkprobit[i] &&
      BIC_n200_logit_linkprobit[i] < BIC_n200_cloglog_linkprobit[i]) {
    numberBIC_lesslogit_n200_probit = numberBIC_lesslogit_n200_probit + 1
  } else if (BIC_n200_probit_linkprobit[i] < BIC_n200_logit_linkprobit[i] &&
             BIC_n200_probit_linkprobit[i] < BIC_n200_cloglog_linkprobit[i]) {
    numberBIC_lessprobit_n200_probit = numberBIC_lessprobit_n200_probit + 1
  } else {
    numberBIC_lesscloglog_n200_probit = numberBIC_lesscloglog_n200_probit + 1
  }
}
cat("Total number of BIC (n = 200):", numberBIC_lesslogit_n200_probit + numberBIC_lessprobit_n200_probit + numberBIC_lesscloglog_n200_probit, "\n")
cat("Minimum number of BIC logit  : ",  numberBIC_lesslogit_n200_probit,"or ",(numberBIC_lesslogit_n200_probit/1000)*100, "%" ,"\n")
cat("Minimum number of BIC probit : ",  numberBIC_lessprobit_n200_probit,"or ",(numberBIC_lessprobit_n200_probit/1000)*100,"%" , "\n")
cat("Minimum number of BCI cll    : ",  numberBIC_lesscloglog_n200_probit,"or ",(numberBIC_lesscloglog_n200_probit/1000)*100,"%", "\n")

###################################### n = 750 ###################################
n3 <- 750

# Set the number of iterations
niter_n750_linkprobit <- 1000

# Create empty vectors to store AIC and BIC values
AIC_n750_logit_linkprobit <- rep(0, niter_n750_linkprobit)
BIC_n750_logit_linkprobit <- rep(0, niter_n750_linkprobit)
AIC_n750_probit_linkprobit <- rep(0, niter_n750_linkprobit)
BIC_n750_probit_linkprobit <- rep(0, niter_n750_linkprobit)
AIC_n750_cloglog_linkprobit <- rep(0, niter_n750_linkprobit)
BIC_n750_cloglog_linkprobit <- rep(0, niter_n750_linkprobit)

for (i in 1:niter_n750_linkprobit) {
  
  # Define the coefficients for the binomial regression model
  #beta <- c(0.06908, -0.02454, 0.07802, -0.01291) # SetNew_probit
  beta <- c(0.0690800, -0.0245400, 0.0000780, -0.0129100) # SetNewNew_probit
  
  # Set the Poisson parameter
  lambda1 <- 54  # Average number of students admitted
  lambda2 <- 50 # Average of number of students accepted in the 3rd round 
  
  # Simulate independent variables x1 and x2 from a normal distribution
  x1 <- runif(n3, min = 30.33,   max = 75.87)      # Highest Score Admission Round 3 runif(n, min, max)
  x2 <- runif(n3, min = 12127.1, max = 17239.5)    # Highest score Admission Round 4 runif(n, min, max) X8
  
  # Simulate independent variables x3 from a Bernoulli distribution
  x3 <- rpois(n3, lambda2) # Number of students accepted in the 3rd round
  
  # Combine independent variables into a matrix X
  X <- matrix(0, nrow = n3, ncol = 4)
  X[, 1] <- t(rep(1, n3))  # intercept term
  X[, 2] <- x1
  X[, 3] <- x2
  X[, 4] <- x3
  
  # Generate the Poisson counts
  ni <- rpois(n3, lambda1) # Number of students admitted
  
  # Define the Probit link function
  phi <- function(x) {
    pnorm(x)
  }
  
  # Calculate the predicted probabilities pi for each observation in X using the Probit link function
  pi_probit <- phi(X %*% beta)
  pi_probit
  
  # Simulate the dependent variable y for each observation from a binomial distribution
  Y_probit <- rbinom(n3, ni, pi_probit)
  Y_probit
  
  
  # Fit the binomial regression model using glm function
  binomial_model_logit <- glm( cbind(Y_probit, ni-Y_probit) ~ x1 + x2 + x3, family = binomial(link = "logit"))
  binomial_model_probit <- glm( cbind(Y_probit, ni-Y_probit) ~ x1 + x2 + x3, family = binomial(link = "probit"))
  binomial_model_cll <- glm( cbind(Y_probit, ni-Y_probit) ~ x1 + x2 + x3 , family = binomial(link = "cloglog"))
  
  # View the summary of the model
  summary(binomial_model_logit)
  summary(binomial_model_probit)
  summary(binomial_model_cll)
  
  AIC_n750_logit_linkprobit[i] <- AIC(binomial_model_logit)
  BIC_n750_logit_linkprobit[i] <- BIC(binomial_model_logit)
  AIC_n750_probit_linkprobit[i] <- AIC(binomial_model_probit)
  BIC_n750_probit_linkprobit[i] <- BIC(binomial_model_probit)
  AIC_n750_cloglog_linkprobit[i] <- AIC(binomial_model_cll)
  BIC_n750_cloglog_linkprobit[i] <- BIC(binomial_model_cll)
  
}

# Set mean
meanAIC_logit_linkprobit_n750 <- mean(AIC_n750_logit_linkprobit)
meanBIC_logit_linkprobit_n750 <- mean(BIC_n750_logit_linkprobit)
meanAIC_probit_linkprobit_n750 <- mean(AIC_n750_probit_linkprobit)
meanBIC_probit_linkprobit_n750 <- mean(BIC_n750_probit_linkprobit)
meanAIC_cloglog_linkprobit_n750 <- mean(AIC_n750_cloglog_linkprobit)
meanBIC_cloglog_linkprobit_n750 <- mean(BIC_n750_cloglog_linkprobit)

# Create a data frame to store the confidence intervals and model fit statistics
model_comparison_n750_linkprobit <- data.frame(
  Model = c("Logit", "Probit", "Complementary Log-Log"),
  AIC = c(meanAIC_logit_linkprobit_n750, meanAIC_probit_linkprobit_n750 , meanAIC_cloglog_linkprobit_n750),
  BIC = c(meanBIC_logit_linkprobit_n750, meanBIC_probit_linkprobit_n750, meanBIC_cloglog_linkprobit_n750)
)

# Print the table
print(model_comparison_n750_linkprobit)

# Count the lowest number of times
numberAIC_lesslogit_n750_probit   <- 0
numberAIC_lessprobit_n750_probit  <- 0
numberAIC_lesscloglog_n750_probit <- 0
numberBIC_lesslogit_n750_probit   <- 0
numberBIC_lessprobit_n750_probit  <- 0
numberBIC_lesscloglog_n750_probit <- 0


######################################### Check lower AIC #########################################
for (i in 1:1000) {
  if (AIC_n750_logit_linkprobit[i] < AIC_n750_probit_linkprobit[i] &&
      AIC_n750_logit_linkprobit[i] < AIC_n750_cloglog_linkprobit[i]) {
    numberAIC_lesslogit_n750_probit = numberAIC_lesslogit_n750_probit + 1
  } else if (AIC_n750_probit_linkprobit[i] < AIC_n750_logit_linkprobit[i] &&
             AIC_n750_probit_linkprobit[i] < AIC_n750_cloglog_linkprobit[i]) {
    numberAIC_lessprobit_n750_probit = numberAIC_lessprobit_n750_probit + 1
  } else {
    numberAIC_lesscloglog_n750_probit = numberAIC_lesscloglog_n750_probit + 1
  }
}

cat("Total number of AIC (n = 750):", numberAIC_lesslogit_n750_probit + numberAIC_lessprobit_n750_probit +numberAIC_lesscloglog_n750_probit, "\n")
cat("Minimum number of AIC logit  : ",  numberAIC_lesslogit_n750_probit,"or ",(numberAIC_lesslogit_n750_probit/1000)*100, "%" ,"\n")
cat("Minimum number of AIC probit : ",  numberAIC_lessprobit_n750_probit,"or ",(numberAIC_lessprobit_n750_probit/1000)*100,"%" , "\n")
cat("Minimum number of ACI cll    : ",  numberAIC_lesscloglog_n750_probit,"or ",(numberAIC_lesscloglog_n750_probit/1000)*100,"%", "\n")


######################################### Check lower BIC #########################################
for (i in 1:1000) {
  if (BIC_n750_logit_linkprobit[i] < BIC_n750_probit_linkprobit[i] &&
      BIC_n750_logit_linkprobit[i] < BIC_n750_cloglog_linkprobit[i]) {
    numberBIC_lesslogit_n750_probit = numberBIC_lesslogit_n750_probit + 1
  } else if (BIC_n750_probit_linkprobit[i] < BIC_n750_logit_linkprobit[i] &&
             BIC_n750_probit_linkprobit[i] < BIC_n750_cloglog_linkprobit[i]) {
    numberBIC_lessprobit_n750_probit = numberBIC_lessprobit_n750_probit + 1
  } else {
    numberBIC_lesscloglog_n750_probit = numberBIC_lesscloglog_n750_probit + 1
  }
}
cat("Total number of BIC (n = 750):", numberBIC_lesslogit_n750_probit + numberBIC_lessprobit_n750_probit + numberBIC_lesscloglog_n750_probit, "\n")
cat("Minimum number of BIC logit  : ",  numberBIC_lesslogit_n750_probit,"or ",(numberBIC_lesslogit_n750_probit/1000)*100, "%" ,"\n")
cat("Minimum number of BIC probit : ",  numberBIC_lessprobit_n750_probit,"or ",(numberBIC_lessprobit_n750_probit/1000)*100,"%" , "\n")
cat("Minimum number of BCI cll    : ",  numberBIC_lesscloglog_n750_probit,"or ",(numberBIC_lesscloglog_n750_probit/1000)*100,"%", "\n")

###################################Link function Cloglog is pi ###############################

# Set the number of observations
n0 <- 20
n1 <- 50
n2 <- 200
n3 <- 750

##################################### n = 20 ############################
n0 <- 20

# Set the number of iterations
niter_n20_linkcll <- 1000

# Create empty vectors to store AIC and BIC values
AIC_n20_logit_linkcll <- rep(0, niter_n20_linkcll)
BIC_n20_logit_linkcll <- rep(0, niter_n20_linkcll)
AIC_n20_probit_linkcll <- rep(0, niter_n20_linkcll)
BIC_n20_probit_linkcll <- rep(0, niter_n20_linkcll)
AIC_n20_cloglog_linkcll <- rep(0, niter_n20_linkcll)
BIC_n20_cloglog_linkcll <- rep(0, niter_n20_linkcll)

for (i in 1:niter_n20_linkcll) {
  
  # Define the coefficients for the binomial regression model
  #beta <- c(-0.24652, -0.03357, 0.10740, -0.01795) ## SetNew_Cloglog
  beta <- c(-0.2465240, -0.0335700, 0.0001074, -0.0179550) ## SetNewNew_Cloglog
  
  # Set the Poisson parameter
  lambda1 <- 54  # Average number of students admitted
  lambda2 <- 50 # Average of number of students accepted in the 3rd round 
  
  # Simulate independent variables x1 and x2 from a normal distribution
  x1 <- runif(n0, min = 30.33,   max = 75.87)      # Highest Score Admission Round 3 runif(n, min, max)
  x2 <- runif(n0, min = 12127.1, max = 17239.5)    # Highest score Admission Round 4 runif(n, min, max) X8
  
  # Simulate independent variables x3 from a Bernoulli distribution
  x3 <- rpois(n0, lambda2) # Number of students accepted in the 3rd round
  
  # Combine independent variables into a matrix X
  X <- matrix(0, nrow = n0, ncol = 4)
  X[, 1] <- t(rep(1, n0))  # intercept term
  X[, 2] <- x1
  X[, 3] <- x2
  X[, 4] <- x3
  
  # Generate the Poisson counts
  ni <- rpois(n0, lambda1) # Number of students admitted
  
  # Calculate the predicted probabilities pi for each observation in X
  pi_cloglog <- 1 - exp(-exp(X %*% beta))
  
  # Simulate the dependent variable y for each observation from a binomial distribution
  Y_cloglog <- rbinom(n0, ni, pi_cloglog)
  
  # Fit the binomial regression model using glm function
  binomial_model_logit <- glm( cbind(Y_cloglog, ni-Y_cloglog) ~ x1 + x2 + x3, family = binomial(link = "logit"))
  binomial_model_probit <- glm( cbind(Y_cloglog, ni-Y_cloglog) ~ x1 + x2 + x3, family = binomial(link = "probit"))
  binomial_model_cll <- glm( cbind(Y_cloglog, ni-Y_cloglog) ~ x1 + x2 + x3, family = binomial(link = "cloglog"))
  
  # View the summary of the model
  summary(binomial_model_logit)
  summary(binomial_model_probit)
  summary(binomial_model_cll)
  
  AIC_n20_logit_linkcll[i] <- AIC(binomial_model_logit)
  BIC_n20_logit_linkcll[i] <- BIC(binomial_model_logit)
  AIC_n20_probit_linkcll[i] <- AIC(binomial_model_probit)
  BIC_n20_probit_linkcll[i] <- BIC(binomial_model_probit)
  AIC_n20_cloglog_linkcll[i] <- AIC(binomial_model_cll)
  BIC_n20_cloglog_linkcll[i] <- BIC(binomial_model_cll)
  
}

# Set mean
meanAIC_logit_linkcll_n20 <- mean(AIC_n20_logit_linkcll)
meanBIC_logit_linkcll_n20 <- mean(BIC_n20_logit_linkcll)
meanAIC_probit_linkcll_n20 <- mean(AIC_n20_probit_linkcll)
meanBIC_probit_linkcll_n20 <- mean(BIC_n20_probit_linkcll)
meanAIC_cloglog_linkcll_n20 <- mean(AIC_n20_cloglog_linkcll)
meanBIC_cloglog_linkcll_n20 <- mean(BIC_n20_cloglog_linkcll)

# Create a data frame to store the confidence intervals and model fit statistics
model_comparison_n20_linkcll <- data.frame(
  Model = c("Logit", "Probit", "Complementary Log-Log"),
  AIC = c(meanAIC_logit_linkcll_n20, meanAIC_probit_linkcll_n20 , meanAIC_cloglog_linkcll_n20),
  BIC = c(meanBIC_logit_linkcll_n20, meanBIC_probit_linkcll_n20, meanBIC_cloglog_linkcll_n20)
)

# Print the table
print(model_comparison_n20_linkcll)

# Count the lowest number of times
numberAIC_lesslogit_n20_cll   <- 0
numberAIC_lessprobit_n20_cll  <- 0
numberAIC_lesscloglog_n20_cll <- 0
numberBIC_lesslogit_n20_cll   <- 0
numberBIC_lessprobit_n20_cll  <- 0
numberBIC_lesscloglog_n20_cll <- 0


######################################### Check lower AIC #########################################
for (i in 1:1000) {
  if (AIC_n20_logit_linkcll[i] < AIC_n20_probit_linkcll[i] &&
      AIC_n20_logit_linkcll[i] < AIC_n20_cloglog_linkcll[i]) {
    numberAIC_lesslogit_n20_cll = numberAIC_lesslogit_n20_cll + 1
  } else if (AIC_n20_probit_linkcll[i] < AIC_n20_logit_linkcll[i] &&
             AIC_n20_probit_linkcll[i] < AIC_n20_cloglog_linkcll[i]) {
    numberAIC_lessprobit_n20_cll = numberAIC_lessprobit_n20_cll + 1
  } else {
    numberAIC_lesscloglog_n20_cll = numberAIC_lesscloglog_n20_cll + 1
  }
}

cat("Total number of AIC (n = 20):", numberAIC_lesslogit_n20_cll + numberAIC_lessprobit_n20_cll +numberAIC_lesscloglog_n20_cll, "\n")
cat("Minimum number of AIC logit  : ",  numberAIC_lesslogit_n20_cll,"or ",(numberAIC_lesslogit_n20_cll/1000)*100, "%" ,"\n")
cat("Minimum number of AIC probit : ",  numberAIC_lessprobit_n20_cll,"or ",(numberAIC_lessprobit_n20_cll/1000)*100,"%" , "\n")
cat("Minimum number of ACI cll    : ",  numberAIC_lesscloglog_n20_cll,"or ",(numberAIC_lesscloglog_n20_cll/1000)*100,"%", "\n")


######################################### Check lower BIC #########################################
for (i in 1:1000) {
  if (BIC_n20_logit_linkcll[i] < BIC_n20_probit_linkcll[i] &&
      BIC_n20_logit_linkcll[i] < BIC_n20_cloglog_linkcll[i]) {
    numberBIC_lesslogit_n20_cll = numberBIC_lesslogit_n20_cll + 1
  } else if (BIC_n20_probit_linkcll[i] < BIC_n20_logit_linkcll[i] &&
             BIC_n20_probit_linkcll[i] < BIC_n20_cloglog_linkcll[i]) {
    numberBIC_lessprobit_n20_cll = numberBIC_lessprobit_n20_cll + 1
  } else {
    numberBIC_lesscloglog_n20_cll = numberBIC_lesscloglog_n20_cll + 1
  }
}
cat("Total number of BIC (n = 20):", numberBIC_lesslogit_n20_cll + numberBIC_lessprobit_n20_cll + numberBIC_lesscloglog_n20_cll, "\n")
cat("Minimum number of BIC logit  : ",  numberBIC_lesslogit_n20_cll,"or ",(numberBIC_lesslogit_n20_cll/1000)*100, "%" ,"\n")
cat("Minimum number of BIC probit : ",  numberBIC_lessprobit_n20_cll,"or ",(numberBIC_lessprobit_n20_cll/1000)*100,"%" , "\n")
cat("Minimum number of BCI cll    : ",  numberBIC_lesscloglog_n20_cll,"or ",(numberBIC_lesscloglog_n20_cll/1000)*100,"%", "\n")


##################################### n = 50 ############################
n1 <- 50

# Set the number of iterations
niter_n50_linkcll <- 1000

# Create empty vectors to store AIC and BIC values
AIC_n50_logit_linkcll <- rep(0, niter_n50_linkcll)
BIC_n50_logit_linkcll <- rep(0, niter_n50_linkcll)
AIC_n50_probit_linkcll <- rep(0, niter_n50_linkcll)
BIC_n50_probit_linkcll <- rep(0, niter_n50_linkcll)
AIC_n50_cloglog_linkcll <- rep(0, niter_n50_linkcll)
BIC_n50_cloglog_linkcll <- rep(0, niter_n50_linkcll)

for (i in 1:niter_n50_linkcll) {
  
  # Define the coefficients for the binomial regression model
  #beta <- c(-0.24652, -0.03357, 0.10740, -0.01795) ## SetNew_Cloglog
  beta <- c(-0.2465240, -0.0335700, 0.0001074, -0.0179550) ## SetNewNew_Cloglog
  
  # Set the Poisson parameter
  lambda1 <- 54  # Average number of students admitted
  lambda2 <- 50 # Average of number of students accepted in the 3rd round 
  
  # Simulate independent variables x1 and x2 from a normal distribution
  x1 <- runif(n1, min = 30.33,   max = 75.87)      # Highest Score Admission Round 3 runif(n, min, max)
  x2 <- runif(n1, min = 12127.1, max = 17239.5)    # Highest score Admission Round 4 runif(n, min, max) X8
  
  # Simulate independent variables x3 from a Bernoulli distribution
  x3 <- rpois(n1, lambda2) # Number of students accepted in the 3rd round
  
  # Combine independent variables into a matrix X
  X <- matrix(0, nrow = n1, ncol = 4)
  X[, 1] <- t(rep(1, n1))  # intercept term
  X[, 2] <- x1
  X[, 3] <- x2
  X[, 4] <- x3
  
  # Generate the Poisson counts
  ni <- rpois(n1, lambda1) # Number of students admitted
  
  # Calculate the predicted probabilities pi for each observation in X
  pi_cloglog <- 1 - exp(-exp(X %*% beta))
  
  # Simulate the dependent variable y for each observation from a binomial distribution
  Y_cloglog <- rbinom(n1, ni, pi_cloglog)
  
  # Fit the binomial regression model using glm function
  binomial_model_logit <- glm( cbind(Y_cloglog, ni-Y_cloglog) ~ x1 + x2 + x3, family = binomial(link = "logit"))
  binomial_model_probit <- glm( cbind(Y_cloglog, ni-Y_cloglog) ~ x1 + x2 + x3, family = binomial(link = "probit"))
  binomial_model_cll <- glm( cbind(Y_cloglog, ni-Y_cloglog) ~ x1 + x2 + x3, family = binomial(link = "cloglog"))
  
  # View the summary of the model
  summary(binomial_model_logit)
  summary(binomial_model_probit)
  summary(binomial_model_cll)
  
  AIC_n50_logit_linkcll[i] <- AIC(binomial_model_logit)
  BIC_n50_logit_linkcll[i] <- BIC(binomial_model_logit)
  AIC_n50_probit_linkcll[i] <- AIC(binomial_model_probit)
  BIC_n50_probit_linkcll[i] <- BIC(binomial_model_probit)
  AIC_n50_cloglog_linkcll[i] <- AIC(binomial_model_cll)
  BIC_n50_cloglog_linkcll[i] <- BIC(binomial_model_cll)
  
}

# Set mean
meanAIC_logit_linkcll_n50 <- mean(AIC_n50_logit_linkcll)
meanBIC_logit_linkcll_n50 <- mean(BIC_n50_logit_linkcll)
meanAIC_probit_linkcll_n50 <- mean(AIC_n50_probit_linkcll)
meanBIC_probit_linkcll_n50 <- mean(BIC_n50_probit_linkcll)
meanAIC_cloglog_linkcll_n50 <- mean(AIC_n50_cloglog_linkcll)
meanBIC_cloglog_linkcll_n50 <- mean(BIC_n50_cloglog_linkcll)

# Create a data frame to store the confidence intervals and model fit statistics
model_comparison_n50_linkcll <- data.frame(
  Model = c("Logit", "Probit", "Complementary Log-Log"),
  AIC = c(meanAIC_logit_linkcll_n50, meanAIC_probit_linkcll_n50 , meanAIC_cloglog_linkcll_n50),
  BIC = c(meanBIC_logit_linkcll_n50, meanBIC_probit_linkcll_n50, meanBIC_cloglog_linkcll_n50)
)

# Print the table
print(model_comparison_n50_linkcll)

# Count the lowest number of times
numberAIC_lesslogit_n50_cll   <- 0
numberAIC_lessprobit_n50_cll  <- 0
numberAIC_lesscloglog_n50_cll <- 0
numberBIC_lesslogit_n50_cll   <- 0
numberBIC_lessprobit_n50_cll  <- 0
numberBIC_lesscloglog_n50_cll <- 0


######################################### Check lower AIC #########################################
for (i in 1:1000) {
  if (AIC_n50_logit_linkcll[i] < AIC_n50_probit_linkcll[i] &&
      AIC_n50_logit_linkcll[i] < AIC_n50_cloglog_linkcll[i]) {
    numberAIC_lesslogit_n50_cll = numberAIC_lesslogit_n50_cll + 1
  } else if (AIC_n50_probit_linkcll[i] < AIC_n50_logit_linkcll[i] &&
             AIC_n50_probit_linkcll[i] < AIC_n50_cloglog_linkcll[i]) {
    numberAIC_lessprobit_n50_cll = numberAIC_lessprobit_n50_cll + 1
  } else {
    numberAIC_lesscloglog_n50_cll = numberAIC_lesscloglog_n50_cll + 1
  }
}

cat("Total number of AIC (n = 50):", numberAIC_lesslogit_n50_cll + numberAIC_lessprobit_n50_cll +numberAIC_lesscloglog_n50_cll, "\n")
cat("Minimum number of AIC logit  : ",  numberAIC_lesslogit_n50_cll,"or ",(numberAIC_lesslogit_n50_cll/1000)*100, "%" ,"\n")
cat("Minimum number of AIC probit : ",  numberAIC_lessprobit_n50_cll,"or ",(numberAIC_lessprobit_n50_cll/1000)*100,"%" , "\n")
cat("Minimum number of ACI cll    : ",  numberAIC_lesscloglog_n50_cll,"or ",(numberAIC_lesscloglog_n50_cll/1000)*100,"%", "\n")


######################################### Check lower BIC #########################################
for (i in 1:1000) {
  if (BIC_n50_logit_linkcll[i] < BIC_n50_probit_linkcll[i] &&
      BIC_n50_logit_linkcll[i] < BIC_n50_cloglog_linkcll[i]) {
    numberBIC_lesslogit_n50_cll = numberBIC_lesslogit_n50_cll + 1
  } else if (BIC_n50_probit_linkcll[i] < BIC_n50_logit_linkcll[i] &&
             BIC_n50_probit_linkcll[i] < BIC_n50_cloglog_linkcll[i]) {
    numberBIC_lessprobit_n50_cll = numberBIC_lessprobit_n50_cll + 1
  } else {
    numberBIC_lesscloglog_n50_cll = numberBIC_lesscloglog_n50_cll + 1
  }
}
cat("Total number of BIC (n = 50):", numberBIC_lesslogit_n50_cll + numberBIC_lessprobit_n50_cll + numberBIC_lesscloglog_n50_cll, "\n")
cat("Minimum number of BIC logit  : ",  numberBIC_lesslogit_n50_cll,"or ",(numberBIC_lesslogit_n50_cll/1000)*100, "%" ,"\n")
cat("Minimum number of BIC probit : ",  numberBIC_lessprobit_n50_cll,"or ",(numberBIC_lessprobit_n50_cll/1000)*100,"%" , "\n")
cat("Minimum number of BCI cll    : ",  numberBIC_lesscloglog_n50_cll,"or ",(numberBIC_lesscloglog_n50_cll/1000)*100,"%", "\n")


##################################### n = 200 ############################
n2 <- 200

# Set the number of iterations
niter_n200_linkcll <- 1000

# Create empty vectors to store AIC and BIC values
AIC_n200_logit_linkcll <- rep(0, niter_n200_linkcll)
BIC_n200_logit_linkcll <- rep(0, niter_n200_linkcll)
AIC_n200_probit_linkcll <- rep(0, niter_n200_linkcll)
BIC_n200_probit_linkcll <- rep(0, niter_n200_linkcll)
AIC_n200_cloglog_linkcll <- rep(0, niter_n200_linkcll)
BIC_n200_cloglog_linkcll <- rep(0, niter_n200_linkcll)

for (i in 1:niter_n200_linkcll) {
  
  # Define the coefficients for the binomial regression model
  #beta <- c(-0.24652, -0.03357, 0.10740, -0.01795) ## SetNew_Cloglog
  beta <- c(-0.2465240, -0.0335700, 0.0001074, -0.0179550) ## SetNewNew_Cloglog
  
  # Set the Poisson parameter
  lambda1 <- 54  # Average number of students admitted
  lambda2 <- 50 # Average of number of students accepted in the 3rd round 
  
  # Simulate independent variables x1 and x2 from a normal distribution
  x1 <- runif(n2, min = 30.33,   max = 75.87)      # Highest Score Admission Round 3 runif(n, min, max)
  x2 <- runif(n2, min = 12127.1, max = 17239.5)    # Highest score Admission Round 4 runif(n, min, max) X8
  
  # Simulate independent variables x3 from a Bernoulli distribution
  x3 <- rpois(n2, lambda2) # Number of students accepted in the 3rd round
  
  # Combine independent variables into a matrix X
  X <- matrix(0, nrow = n2, ncol = 4)
  X[, 1] <- t(rep(1, n2))  # intercept term
  X[, 2] <- x1
  X[, 3] <- x2
  X[, 4] <- x3
  
  # Generate the Poisson counts
  ni <- rpois(n2, lambda1) # Number of students admitted
  
  # Calculate the predicted probabilities pi for each observation in X
  pi_cloglog <- 1 - exp(-exp(X %*% beta))
  
  # Simulate the dependent variable y for each observation from a binomial distribution
  Y_cloglog <- rbinom(n2, ni, pi_cloglog)
  Y_cloglog
  
  # Fit the binomial regression model using glm function
  binomial_model_logit <- glm( cbind(Y_cloglog, ni-Y_cloglog) ~ x1 + x2 + x3, family = binomial(link = "logit"))
  binomial_model_probit <- glm( cbind(Y_cloglog, ni-Y_cloglog) ~ x1 + x2 + x3, family = binomial(link = "probit"))
  binomial_model_cll <- glm( cbind(Y_cloglog, ni-Y_cloglog) ~ x1 + x2 + x3, family = binomial(link = "cloglog"))
  
  # View the summary of the model
  summary(binomial_model_logit)
  summary(binomial_model_probit)
  summary(binomial_model_cll)
  
  AIC_n200_logit_linkcll[i] <- AIC(binomial_model_logit)
  BIC_n200_logit_linkcll[i] <- BIC(binomial_model_logit)
  AIC_n200_probit_linkcll[i] <- AIC(binomial_model_probit)
  BIC_n200_probit_linkcll[i] <- BIC(binomial_model_probit)
  AIC_n200_cloglog_linkcll[i] <- AIC(binomial_model_cll)
  BIC_n200_cloglog_linkcll[i] <- BIC(binomial_model_cll)
  
}

# Set mean
meanAIC_logit_linkcll_n200 <- mean(AIC_n200_logit_linkcll)
meanBIC_logit_linkcll_n200 <- mean(BIC_n200_logit_linkcll)
meanAIC_probit_linkcll_n200 <- mean(AIC_n200_probit_linkcll)
meanBIC_probit_linkcll_n200 <- mean(BIC_n200_probit_linkcll)
meanAIC_cloglog_linkcll_n200 <- mean(AIC_n200_cloglog_linkcll)
meanBIC_cloglog_linkcll_n200 <- mean(BIC_n200_cloglog_linkcll)

# Create a data frame to store the confidence intervals and model fit statistics
model_comparison_n200_linkcll <- data.frame(
  Model = c("Logit", "Probit", "Complementary Log-Log"),
  AIC = c(meanAIC_logit_linkcll_n200, meanAIC_probit_linkcll_n200 , meanAIC_cloglog_linkcll_n200),
  BIC = c(meanBIC_logit_linkcll_n200, meanBIC_probit_linkcll_n200, meanBIC_cloglog_linkcll_n200)
)

# Print the table
print(model_comparison_n200_linkcll)

# Count the lowest number of times
numberAIC_lesslogit_n200_cll   <- 0
numberAIC_lessprobit_n200_cll  <- 0
numberAIC_lesscloglog_n200_cll <- 0
numberBIC_lesslogit_n200_cll   <- 0
numberBIC_lessprobit_n200_cll  <- 0
numberBIC_lesscloglog_n200_cll <- 0


######################################### Check lower AIC #########################################
for (i in 1:1000) {
  if (AIC_n200_logit_linkcll[i] < AIC_n200_probit_linkcll[i] &&
      AIC_n200_logit_linkcll[i] < AIC_n200_cloglog_linkcll[i]) {
    numberAIC_lesslogit_n200_cll = numberAIC_lesslogit_n200_cll + 1
  } else if (AIC_n200_probit_linkcll[i] < AIC_n200_logit_linkcll[i] &&
             AIC_n200_probit_linkcll[i] < AIC_n200_cloglog_linkcll[i]) {
    numberAIC_lessprobit_n200_cll = numberAIC_lessprobit_n200_cll + 1
  } else {
    numberAIC_lesscloglog_n200_cll = numberAIC_lesscloglog_n200_cll + 1
  }
}

cat("Total number of AIC (n = 20):", numberAIC_lesslogit_n200_cll + numberAIC_lessprobit_n200_cll +numberAIC_lesscloglog_n200_cll, "\n")
cat("Minimum number of AIC logit  : ",  numberAIC_lesslogit_n200_cll,"or ",(numberAIC_lesslogit_n200_cll/1000)*100, "%" ,"\n")
cat("Minimum number of AIC probit : ",  numberAIC_lessprobit_n200_cll,"or ",(numberAIC_lessprobit_n200_cll/1000)*100,"%" , "\n")
cat("Minimum number of ACI cll    : ",  numberAIC_lesscloglog_n200_cll,"or ",(numberAIC_lesscloglog_n200_cll/1000)*100,"%", "\n")


######################################### Check lower BIC #########################################
for (i in 1:1000) {
  if (BIC_n200_logit_linkcll[i] < BIC_n200_probit_linkcll[i] &&
      BIC_n200_logit_linkcll[i] < BIC_n200_cloglog_linkcll[i]) {
    numberBIC_lesslogit_n200_cll = numberBIC_lesslogit_n200_cll + 1
  } else if (BIC_n200_probit_linkcll[i] < BIC_n200_logit_linkcll[i] &&
             BIC_n200_probit_linkcll[i] < BIC_n200_cloglog_linkcll[i]) {
    numberBIC_lessprobit_n200_cll = numberBIC_lessprobit_n200_cll + 1
  } else {
    numberBIC_lesscloglog_n200_cll = numberBIC_lesscloglog_n200_cll + 1
  }
}
cat("Total number of BIC (n = 20):", numberBIC_lesslogit_n200_cll + numberBIC_lessprobit_n200_cll + numberBIC_lesscloglog_n200_cll, "\n")
cat("Minimum number of BIC logit  : ",  numberBIC_lesslogit_n200_cll,"or ",(numberBIC_lesslogit_n200_cll/1000)*100, "%" ,"\n")
cat("Minimum number of BIC probit : ",  numberBIC_lessprobit_n200_cll,"or ",(numberBIC_lessprobit_n200_cll/1000)*100,"%" , "\n")
cat("Minimum number of BCI cll    : ",  numberBIC_lesscloglog_n200_cll,"or ",(numberBIC_lesscloglog_n200_cll/1000)*100,"%", "\n")


##################################### n = 750 ############################
n3 <- 750

# Set the number of iterations
niter_n750_linkcll <- 1000

# Create empty vectors to store AIC and BIC values
AIC_n750_logit_linkcll <- rep(0, niter_n750_linkcll)
BIC_n750_logit_linkcll <- rep(0, niter_n750_linkcll)
AIC_n750_probit_linkcll <- rep(0, niter_n750_linkcll)
BIC_n750_probit_linkcll <- rep(0, niter_n750_linkcll)
AIC_n750_cloglog_linkcll <- rep(0, niter_n750_linkcll)
BIC_n750_cloglog_linkcll <- rep(0, niter_n750_linkcll)


for (i in 1:niter_n750_linkcll) {
  
  # Define the coefficients for the binomial regression model
  #beta <- c(-0.24652, -0.03357, 0.10740, -0.01795) ## SetNew_Cloglog
  beta <- c(-0.2465240, -0.0335700, 0.0001074, -0.0179550) ## SetNewNew_Cloglog
  
  # Set the Poisson parameter
  lambda1 <- 54  # Average number of students admitted
  lambda2 <- 50 # Average of number of students accepted in the 3rd round 
  
  # Simulate independent variables x1 and x2 from a normal distribution
  x1 <- runif(n3, min = 30.33,   max = 75.87)      # Highest Score Admission Round 3 runif(n, min, max)
  x2 <- runif(n3, min = 12127.1, max = 17239.5)    # Highest score Admission Round 4 runif(n, min, max) X8
  
  # Simulate independent variables x3 from a Bernoulli distribution
  x3 <- rpois(n3, lambda2) # Number of students accepted in the 3rd round
  
  # Combine independent variables into a matrix X
  X <- matrix(0, nrow = n3, ncol = 4)
  X[, 1] <- t(rep(1, n3))  # intercept term
  X[, 2] <- x1
  X[, 3] <- x2
  X[, 4] <- x3
  
  # Generate the Poisson counts
  ni <- rpois(n3, lambda1) # Number of students admitted
  
  # Calculate the predicted probabilities pi for each observation in X
  pi_cloglog <- 1 - exp(-exp(X %*% beta))
  
  # Simulate the dependent variable y for each observation from a binomial distribution
  Y_cloglog <- rbinom(n3, ni, pi_cloglog)
  Y_cloglog
  
  
  # Fit the binomial regression model using glm function
  binomial_model_logit <- glm( cbind(Y_cloglog, ni-Y_cloglog) ~ x1 + x2 + x3, family = binomial(link = "logit"))
  binomial_model_probit <- glm( cbind(Y_cloglog, ni-Y_cloglog) ~ x1 + x2 + x3, family = binomial(link = "probit"))
  binomial_model_cll <- glm( cbind(Y_cloglog, ni-Y_cloglog) ~ x1 + x2 + x3, family = binomial(link = "cloglog"))
  
  # View the summary of the model
  summary(binomial_model_logit)
  summary(binomial_model_probit)
  summary(binomial_model_cll)

  AIC_n750_logit_linkcll[i] <- AIC(binomial_model_logit)
  BIC_n750_logit_linkcll[i] <- BIC(binomial_model_logit)
  AIC_n750_probit_linkcll[i] <- AIC(binomial_model_probit)
  BIC_n750_probit_linkcll[i] <- BIC(binomial_model_probit)
  AIC_n750_cloglog_linkcll[i] <- AIC(binomial_model_cll)
  BIC_n750_cloglog_linkcll[i] <- BIC(binomial_model_cll)
  
}

# Set mean
meanAIC_logit_linkcll_n750 <- mean(AIC_n750_logit_linkcll)
meanBIC_logit_linkcll_n750 <- mean(BIC_n750_logit_linkcll)
meanAIC_probit_linkcll_n750 <- mean(AIC_n750_probit_linkcll)
meanBIC_probit_linkcll_n750 <- mean(BIC_n750_probit_linkcll)
meanAIC_cloglog_linkcll_n750 <- mean(AIC_n750_cloglog_linkcll)
meanBIC_cloglog_linkcll_n750 <- mean(BIC_n750_cloglog_linkcll)


# Create a data frame to store the confidence intervals and model fit statistics
model_comparison_n750_linkcll <- data.frame(
  Model = c("Logit", "Probit", "Clog-Log"),
  AIC = c(meanAIC_logit_linkcll_n750, meanAIC_probit_linkcll_n750 , meanAIC_cloglog_linkcll_n750),
  BIC = c(meanBIC_logit_linkcll_n750, meanBIC_probit_linkcll_n750, meanBIC_cloglog_linkcll_n750)
)

# Print the table
print(model_comparison_n750_linkcll)

# Count the lowest number of times
numberAIC_lesslogit_n750_cll   <- 0
numberAIC_lessprobit_n750_cll  <- 0
numberAIC_lesscloglog_n750_cll <- 0
numberBIC_lesslogit_n750_cll   <- 0
numberBIC_lessprobit_n750_cll  <- 0
numberBIC_lesscloglog_n750_cll <- 0


######################################### Check lower AIC #########################################
for (i in 1:1000) {
  if (AIC_n750_logit_linkcll[i] < AIC_n750_probit_linkcll[i] &&
      AIC_n750_logit_linkcll[i] < AIC_n750_cloglog_linkcll[i]) {
    numberAIC_lesslogit_n750_cll = numberAIC_lesslogit_n750_cll + 1
  } else if (AIC_n750_probit_linkcll[i] < AIC_n750_logit_linkcll[i] &&
             AIC_n750_probit_linkcll[i] < AIC_n750_cloglog_linkcll[i]) {
    numberAIC_lessprobit_n750_cll = numberAIC_lessprobit_n750_cll + 1
  } else {
    numberAIC_lesscloglog_n750_cll = numberAIC_lesscloglog_n750_cll + 1
  }
}

cat("Total number of AIC (n = 750):", numberAIC_lesslogit_n750_cll + numberAIC_lessprobit_n750_cll +numberAIC_lesscloglog_n750_cll, "\n")
cat("Minimum number of AIC logit  : ",  numberAIC_lesslogit_n750_cll,"or ",(numberAIC_lesslogit_n750_cll/1000)*100, "%" ,"\n")
cat("Minimum number of AIC probit : ",  numberAIC_lessprobit_n750_cll,"or ",(numberAIC_lessprobit_n750_cll/1000)*100,"%" , "\n")
cat("Minimum number of ACI cll    : ",  numberAIC_lesscloglog_n750_cll,"or ",(numberAIC_lesscloglog_n750_cll/1000)*100,"%", "\n")


######################################### Check lower BIC #########################################
for (i in 1:1000) {
  if (BIC_n750_logit_linkcll[i] < BIC_n750_probit_linkcll[i] &&
      BIC_n750_logit_linkcll[i] < BIC_n750_cloglog_linkcll[i]) {
    numberBIC_lesslogit_n750_cll = numberBIC_lesslogit_n750_cll + 1
  } else if (BIC_n750_probit_linkcll[i] < BIC_n750_logit_linkcll[i] &&
             BIC_n750_probit_linkcll[i] < BIC_n750_cloglog_linkcll[i]) {
    numberBIC_lessprobit_n750_cll = numberBIC_lessprobit_n750_cll + 1
  } else {
    numberBIC_lesscloglog_n750_cll = numberBIC_lesscloglog_n750_cll + 1
  }
}
cat("Total number of BIC (n = 750):", numberBIC_lesslogit_n750_cll + numberBIC_lessprobit_n750_cll + numberBIC_lesscloglog_n750_cll, "\n")
cat("Minimum number of BIC logit  : ",  numberBIC_lesslogit_n750_cll,"or ",(numberBIC_lesslogit_n750_cll/1000)*100, "%" ,"\n")
cat("Minimum number of BIC probit : ",  numberBIC_lessprobit_n750_cll,"or ",(numberBIC_lessprobit_n750_cll/1000)*100,"%" , "\n")
cat("Minimum number of BCI cll    : ",  numberBIC_lesscloglog_n750_cll,"or ",(numberBIC_lesscloglog_n750_cll/1000)*100,"%", "\n")


###################################### finished Simulation ############################















# Test likelihood value calculation with glm
############################### Link function logit is pi #################################
n0 <- 20

# Define the coefficients for the binomial regression model
beta <- c(1.33e-01, -4.08e-02, 1.30e-04, -2.15e-02) # SetNew_logit

# Set the Poisson parameter
lambda1 <- 54  # Average number of students admitted
lambda2 <- 50 # Average of number of students accepted in the 3rd round 

# Simulate independent variables x1 and x2 from a normal distribution
x1 <- runif(n0, min = 30.33,   max = 75.87)      # Highest Score Admission Round 3 runif(n, min, max) X5
x2 <- runif(n0, min = 12127.1, max = 17239.5)    # Highest score Admission Round 4 runif(n, min, max) X8

# Simulate independent variables x3 from a Bernoulli distribution
x3 <- rpois(n0, lambda2) # Number of students accepted in the 3rd round  X7

# Combine independent variables into a matrix X
X <- matrix(0, nrow = n0, ncol = 4)
X[, 1] <- t(rep(1, n0))  # intercept term
X[, 2] <- x1
X[, 3] <- x2
X[, 4] <- x3

# Generate the Poisson counts
ni <- rpois(n0, lambda1) # Number of students admitted

# Calculate the predicted probabilities pi for each observation in X
PropY_linklogit <- exp(X %*% beta) / (1 + exp(X %*% beta))

# Simulate the dependent variable y for each observation from a binomial distribution
Y_logit <- rbinom(n0, ni, PropY_linklogit)


# Fit the binomial regression model using glm function
binomial_model_logit <- glm( cbind(Y_logit, ni-Y_logit) ~ x1 + x2 + x3, family = binomial(link = "logit"))
binomial_model_probit <- glm( cbind(Y_logit, ni-Y_logit) ~ x1 + x2 + x3, family = binomial(link = "probit"))
binomial_model_cll <- glm( cbind(Y_logit, ni-Y_logit) ~ x1 + x2 + x3, family = binomial(link = "cloglog"))

# View the summary of the model
summary(binomial_model_logit)
summary(binomial_model_probit)
summary(binomial_model_cll)

# Find likelihood value calculation with glm
logLik(binomial_model_logit)
logLik(binomial_model_probit)
logLik(binomial_model_cll)


############################### Link function probit is pi #################################
n0 <- 20
# Define the coefficients for the binomial regression model
beta <- c(6.91e-02, -2.45e-02, 7.80e-05, -1.29e-02) # SetNew_probit

# Set the Poisson parameter
lambda1 <- 54  # Average number of students admitted
lambda2 <- 50 # Average of number of students accepted in the 3rd round 

# Simulate independent variables x1 and x2 from a normal distribution
x1 <- runif(n0, min = 30.33,   max = 75.87)      # Highest Score Admission Round 3 runif(n, min, max)
x2 <- runif(n0, min = 12.1271, max = 17.2395)    # Highest score Admission Round 4 runif(n, min, max) หน่วยเป็น พันคะเเนน

# Simulate independent variables x3 from a Bernoulli distribution
x3 <- rpois(n0, lambda2) # Number of students accepted in the 3rd round

# Combine independent variables into a matrix X
X <- matrix(0, nrow = n0, ncol = 4)
X[, 1] <- t(rep(1, n0))  # intercept term
X[, 2] <- x1
X[, 3] <- x2
X[, 4] <- x3

# Generate the Poisson counts
ni <- rpois(n0, lambda1) # Number of students admitted

# Define the Probit link function
phi <- function(x) {
  pnorm(x)
}

# Calculate the predicted probabilities pi for each observation in X using the Probit link function
pi_probit <- phi(X %*% beta)
pi_probit

# Simulate the dependent variable y for each observation from a binomial distribution
Y_probit <- rbinom(n0, ni, pi_probit)
Y_probit


# Fit the binomial regression model using glm function
binomial_model_logit <- glm( cbind(Y_probit, ni-Y_probit) ~ x1 + x2 + x3, family = binomial(link = "logit"))
binomial_model_probit <- glm( cbind(Y_probit, ni-Y_probit) ~ x1 + x2 + x3, family = binomial(link = "probit"))
binomial_model_cll <- glm( cbind(Y_probit, ni-Y_probit) ~ x1 + x2 + x3 , family = binomial(link = "cloglog"))

# View the summary of the model
summary(binomial_model_logit)
summary(binomial_model_probit)
summary(binomial_model_cll)

# Find likelihood value calculation with glm
logLik(binomial_model_logit)
logLik(binomial_model_probit)
logLik(binomial_model_cll)


############################### Link function Cloglog is pi #################################
n0 <- 20
# Define the coefficients for the binomial regression model
beta <- c(-0.246524, -0.033570, 0.000107, -0.017955) ## SetNew_Cloglog

# Set the Poisson parameter
lambda1 <- 54  # Average number of students admitted
lambda2 <- 50 # Average of number of students accepted in the 3rd round 

# Simulate independent variables x1 and x2 from a normal distribution
x1 <- runif(n0, min = 30.33,   max = 75.87)      # Highest Score Admission Round 3 runif(n, min, max)
x2 <- runif(n0, min = 12.1271, max = 17.2395)    # Highest score Admission Round 4 runif(n, min, max) หน่วยเป็น พันคะเเ

# Simulate independent variables x3 from a Bernoulli distribution
x3 <- rpois(n0, lambda2) # Number of students accepted in the 3rd round

# Combine independent variables into a matrix X
X <- matrix(0, nrow = n0, ncol = 4)
X[, 1] <- t(rep(1, n0))  # intercept term
X[, 2] <- x1
X[, 3] <- x2
X[, 4] <- x3

# Generate the Poisson counts
ni <- rpois(n0, lambda1) # Number of students admitted

# Calculate the predicted probabilities pi for each observation in X
pi_cloglog <- 1 - exp(-exp(X %*% beta))

# Simulate the dependent variable y for each observation from a binomial distribution
Y_cloglog <- rbinom(n0, ni, pi_cloglog)
Y_cloglog


# Fit the binomial regression model using glm function
binomial_model_logit <- glm( cbind(Y_cloglog, ni-Y_cloglog) ~ x1 + x2 + x3, family = binomial(link = "logit"))
binomial_model_probit <- glm( cbind(Y_cloglog, ni-Y_cloglog) ~ x1 + x2 + x3, family = binomial(link = "probit"))
binomial_model_cll <- glm( cbind(Y_cloglog, ni-Y_cloglog) ~ x1 + x2 + x3, family = binomial(link = "cloglog"))

# View the summary of the model
summary(binomial_model_logit)
summary(binomial_model_probit)
summary(binomial_model_cll)

# Find likelihood value calculation with glm
logLik(binomial_model_logit)
logLik(binomial_model_probit)
logLik(binomial_model_cll)
