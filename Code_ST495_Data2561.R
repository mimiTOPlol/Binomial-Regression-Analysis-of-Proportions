#Loading necessary packages
install.packages("MASS")
install.packages("statmod")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("statmod")
install.packages("dgof")
install.packages("stats")

# เรียกใช้ packages
library(MASS)
library(statmod)
library(ggplot2)
library(dplyr)
library(statmod)
library(dgof)
library(stats)

########################################### Set Data เรียงจากน้อยไปมาก  ###############################
# Set Yi Number of students dropping out of each program Variables
Yi <- c(16, 16, 8, 10, 4, 3, 15, 14, 25, 3, 27, 14, 13, 16, 10, 12, 16, 11, 15, 13)

# Set Number of students admitted in Year 1, Semester 1
ni <- c(61, 58, 52, 38, 20, 50, 60, 85, 66, 58, 43, 65, 47, 50, 54, 72, 53, 57, 60, 32)

# Calculate the proportion of students dropping out for each program
pi <- Yi/ni

# Create a data frame with program names and corresponding dropout proportions
program_data <- data.frame(Program = 1:length(Yi), DropoutProportion = pi)

# Order the data frame by DropoutProportion in descending order
program_data <- program_data[order(program_data$DropoutProportion, decreasing = TRUE), ]

# Create a bar plot
barplot(program_data$DropoutProportion, names.arg = program_data$Program, 
        xlab = "Program", ylab = "Dropout Proportion 2561",
        main = "Proportion of Students Dropping Out by Program 2561", las = 1, pch = 1, ylam = c(0, 0.8))

# Print the ordered program data
print(program_data)

# Create a bar plot
barplot(Yi, names.arg = 1:length(Yi), xlab = "Program", ylab = "Number of Students Dropping",
        main = "Number of Students Dropping Out by Program 2561", ylim = c(0, 30))
# Create a bar plot
barplot(ni, names.arg = 1:length(ni), xlab = "Program", ylab = "number of students admitted",
        main = "Number of students accepted for each program 2561", ylim = c(0, 100))

plot(pi ~ ni, xlab = "Number of students accepted for each program 2561", ylab = "Proportion with Number of Students Dropping",las = 1, pch = 19, ylim = c(0,1))

barplot(pi, names.arg = 1:length(pi), xlab = "Program", ylab = "Proportion with Number of Students Dropping",las = 1, pch = 19, ylim = c(0,0.8),
        main = "Proportion of Students Dropping Out by Program 2561")

############################################ Set Data ###################################

# Set Yi Number of students dropping out of each program Variables
Yi <- c(16, 16, 8, 10, 4, 3, 15, 14, 25, 3, 27, 14, 13, 16, 10, 12, 16, 11, 15, 13)
# Create a bar plot
barplot(Yi, names.arg = 1:length(Yi), xlab = "Program", ylab = "Number of Students Dropping 2561",
        main = "Number of Students Dropping Out by Program 2561", las = 1, pch = 19,  ylim = c(0, 30))

# Set Number of students admitted in Year 1, Semester 1
ni <- c(61, 58, 52, 38, 20, 50, 60, 85, 66, 58, 43, 65, 47, 50, 54, 72, 53, 57, 60, 32)
barplot(ni, names.arg = 1:length(ni), xlab = "Program", ylab = "Number of students admitted",
        main = "Number of students accepted for each program 2561", ylim = c(0, 100))
# Calculate the proportion of students dropping out for each program
options(digits = 3)
pi <- Yi/ni
barplot(pi, names.arg = 1:length(pi), xlab = "Program", ylab = "Proportion with Number of Students Dropping 2561",las = 1, pch = 19, ylim = c(0,0.8),
        main = "Proportion of Students Dropping Out by Program 2561")

# Set X1  compulsory subjects C
x1 <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0) #เเก้เเล้ว
plot(pi ~ x1, xlab = "Compulsory Subjects C", ylab = "Proportion with Number of Students Dropping",las = 1, pch = 19, ylim = c(0,1))

# Set X2 a few credits each course
x2 <- c(137, 133, 133, 137, 133, 122, 137, 138, 132, 138, 129, 138, 129, 138, 138, 129, 138, 138, 129, 138)
plot(pi ~ x2, xlab = "A Few Cedits Each Course", ylab = "Proportion with Number of Students Dropping", las = 1, pch = 19, ylim = c(0,1))
shapiro.test(x2)
qqnorm(x2, main='Normal')
qqline(x2)

# Set X3 college tuition fees
x3 <- c(17300, 17300, 17300, 33900, 33800, 44300, 17300, 17300, 17300, 17300, 17300, 17300, 17300, 17300, 17300,
        17300, 17300, 17300, 45700, 47900)
plot(pi ~ x3, xlab = "College tuition fees", ylab = "Proportion with Number of Students Dropping", las = 1, pch = 19, ylim = c(0,1))

# Set X4 Normal Project (1) or Special Project (0)
x4 <- c(1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0) #เเก้เเล้ว
plot(pi ~ x4, xlab = "Normal Project (1) or Special Project (0)", ylab = "Proportion with Number of Students Dropping", las = 1, pch = 19, ylim = c(0,1))

# Set X5 Highest Score Admission Round 3
x5 <- c(65.24, 67.71, 59.34, 57.41, 56.7, 75.87, 62.11, 42.04, 65.02, 72.45, 30.33, 60.41, 63.46,
        56.61, 54.97, 66.65, 58.43, 60.37, 54.29, 55.25)
plot(pi ~ x3, xlab = "Highest Score Admission Round 3", ylab = "Proportion with Number of Students Dropping", las = 1, pch = 19, ylim = c(0,1))
shapiro.test(x5)

# Set X6 Lowest score Admission Round 3
x6 <- c(51.42, 48.02, 45.96, 33.40, 32.06, 56.56, 48.56, 55.66, 53.14, 54.01, 56.12, 46.76, 46.03,
        42.99, 40.26, 52.34, 35.04, 48.83, 37.67, 35.6)
plot(pi ~ x6, xlab = "Lowest score Admission Round 3", ylab = "Proportion with Number of Students Dropping", las = 1, pch = 19, ylim = c(0,1))
shapiro.test(x6)

# Set X7 The number of students admitted in Round 3
x7 <- c(48, 47, 45, 31, 18, 42, 52, 74, 46, 41, 24, 44, 43, 32, 36, 44, 46, 42, 40, 23)
plot(pi ~ x7, xlab = "The number of students admitted in Round 3", ylab = "Proportion with Number of Students Dropping", las = 1, pch = 19, ylim = c(0,1))
shapiro.test(x7)

# Set X8 Highest Score Admission Round 4
x8 <- c(16644.10, 16597.90, 16867.00, 14842.90, 12127.10, 14125.50, 16567.00, 14247.40,
       17075.70, 17017.10, 14920.40, 16750.50, 14484.20, 14870.10, 15926.30, 17239.50,
        15236.80, 17017.10, 14030.20, 15976.00)
#x8 <- c(16.6441, 16.5979, 16.867, 14.8429, 12.1271, 14.1255, 16.567, 14.2474, 17.0757,
#       17.0171, 14.9204, 16.7505, 14.4842, 14.8701, 15.9263, 17.2395, 15.2368, 17.0171,
#        14.0302, 15.976)
plot(pi ~ x8, xlab = "Highest Score Admission Round 4", ylab = "Proportion with Number of Students Dropping", las = 1, pch = 19, ylim = c(0,1))

# Set X9 Lowest score Admission Round 4
x9 <- c(14784.70, 14225.30, 13786.80, 10962.10, 12089.10, 17821.00, 14030.20, 13229.50 ,14863.90,
        15318.80, 12230.70, 13893.10, 14022.40, 13095.40, 13163.60, 13867.20, 13416.70, 15318.8,
        10578.40, 13158.7)
#x9 <- c(14.7847, 14.2253, 13.7868, 10.9621, 12.0891, 17.821, 14.0302, 13.2295, 14.8639,
#        15.3188, 12.2307, 13.8931, 14.0224, 13.0954, 13.1636, 13.8672, 13.4167, 15.3188,
#        10.5784, 13.1587)
plot(pi ~ x9, xlab = "Lowest score Admission Round 4", ylab = "Proportion with Number of Students Dropping", las = 1, pch = 19, ylim = c(0,1))

# Set X10 The number of students admitted in Round 4
x10 <- c(12, 7, 7, 7, 2, 8, 7, 11, 16, 12, 18, 21,  2, 18, 18, 27, 7, 15, 20, 9)
plot(pi ~ x10, xlab = "The number of students admitted in Round 4", ylab = "Proportion with Number of Students Dropping", las = 1, pch = 19, ylim = c(0,1))


# Test the correlation between X and X using Pearson's correlation test
cor_test_X1_X2 <- cor.test(x1, x2, method = "pearson")
cor_test_X1_X3 <- cor.test(x1, x3, method = "pearson")
cor_test_X1_X4 <- cor.test(x1, x4, method = "pearson")
cor_test_X1_X5 <- cor.test(x1, x5, method = "pearson")
cor_test_X1_X6 <- cor.test(x1, x6, method = "pearson")
cor_test_X1_X7 <- cor.test(x1, x7, method = "pearson")
cor_test_X1_X8 <- cor.test(x1, x8, method = "pearson")
cor_test_X1_X9 <- cor.test(x1, x9, method = "pearson")
cor_test_X1_X10 <- cor.test(x1, x10, method = "pearson")
cor_test_X2_X3 <- cor.test(x2, x3, method = "pearson")
cor_test_X2_X4 <- cor.test(x2, x4, method = "pearson")
cor_test_X2_X5 <- cor.test(x2, x5, method = "pearson")
cor_test_X2_X6 <- cor.test(x2, x6, method = "pearson")
cor_test_X2_X7 <- cor.test(x2, x7, method = "pearson")
cor_test_X2_X8 <- cor.test(x2, x8, method = "pearson")
cor_test_X2_X9 <- cor.test(x2, x9, method = "pearson")
cor_test_X2_X10 <- cor.test(x2, x10, method = "pearson")
cor_test_X3_X4 <- cor.test(x3, x4, method = "pearson")
cor_test_X3_X5 <- cor.test(x3, x4, method = "pearson")
cor_test_X3_X6 <- cor.test(x3, x6, method = "pearson")
cor_test_X3_X7 <- cor.test(x3, x7, method = "pearson")
cor_test_X3_X8 <- cor.test(x3, x8, method = "pearson")
cor_test_X3_X9 <- cor.test(x3, x9, method = "pearson")
cor_test_X3_X10 <- cor.test(x3, x10, method = "pearson")
cor_test_X4_X5 <- cor.test(x4, x5, method = "pearson")
cor_test_X4_X6 <- cor.test(x4, x6, method = "pearson")
cor_test_X4_X7 <- cor.test(x4, x7, method = "pearson")
cor_test_X4_X8 <- cor.test(x4, x8, method = "pearson")
cor_test_X4_X9 <- cor.test(x4, x9, method = "pearson")
cor_test_X4_X10 <- cor.test(x4, x10, method = "pearson")
cor_test_X5_X6 <- cor.test(x5, x6, method = "pearson")
cor_test_X5_X7 <- cor.test(x5, x7, method = "pearson")
cor_test_X5_X8 <- cor.test(x5, x8, method = "pearson")
cor_test_X5_X9 <- cor.test(x5, x9, method = "pearson")
cor_test_X5_X10 <- cor.test(x5, x10, method = "pearson")
cor_test_X6_X7 <- cor.test(x6, x7, method = "pearson")
cor_test_X6_X8 <- cor.test(x6, x8, method = "pearson")
cor_test_X6_X9 <- cor.test(x6, x9, method = "pearson")
cor_test_X6_X10 <- cor.test(x6, x10, method = "pearson")
cor_test_X7_X8 <- cor.test(x7, x8, method = "pearson")
cor_test_X7_X9 <- cor.test(x7, x9, method = "pearson")
cor_test_X7_X10 <- cor.test(x7, x10, method = "pearson")
cor_test_X8_X9 <- cor.test(x8, x9, method = "pearson")
cor_test_X8_X10 <- cor.test(x8, x10, method = "pearson")
cor_test_X9_X10 <- cor.test(x9, x10, method = "pearson")
# Assuming you have already defined x1, x2, x3, x4, x5, and x6
# Define the pairs of variables
variable_pairs <- list(
  c("x1", "x2"), c("x1", "x3"), c("x1", "x4"), c("x1", "x5"), c("x1", "x6"), c("x1", "x7"), c("x1", "x8"), c("x1", "x9"), c("x1", "x10"),  
  c("x2", "x3"), c("x2", "x4"), c("x2", "x5"), c("x2", "x6"), c("x2", "x7"), c("x2", "x8"), c("x2", "x9"), c("x2", "x10"), 
  c("x3", "x4"), c("x3", "x5"), c("x3", "x6"), c("x3", "x7"), c("x3", "x8"), c("x3", "x9"), c("x3", "x10"), 
  c("x4", "x5"), c("x4", "x6"), c("x4", "x7"), c("x4", "x8"), c("x4", "x9"), c("x4", "x10"),
  c("x5", "x6"), c("x5", "x7"), c("x5", "x8"), c("x5", "x9"), c("x5", "x10"),
  c("x6", "x7"), c("x6", "x8"), c("x6", "x9"), c("x6", "x10"),
  c("x7", "x8"), c("x7", "x9"), c("x7", "x10"),
  c("x8", "x9"), c("x8", "x10"),
  c("x9", "x10")
)

# Create an empty data frame to store the results
cor_pvalues <- data.frame(Variable1 = character(0), Variable2 = character(0), PValue = numeric(0))

# Loop through each variable pair and perform correlation tests
for(pair in variable_pairs) {
  var1 <- pair[1]
  var2 <- pair[2]
  
  cor_test_result <- cor.test(get(var1), get(var2), method = "pearson")
  
  cor_pvalues <- rbind(cor_pvalues, data.frame(Variable1 = var1, Variable2 = var2, PValue = cor_test_result$p.value))
}

# Print the summary table
print(cor_pvalues)

# Construct a correlation matrix for X1 and X2
cor_matrix <- cor(cbind(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10))
cor_matrix
# Plot correlation
install.packages("corrplot")
library(corrplot)
corrplot(cor_matrix, type = "lower")

################### Point-Biserial Correlation in R #####################################
# https://medium.com/@RaharditoDP/point-biserial-correlation-in-r-70e713caa7a0
# X1
cor.test(x1, x4)
cor.test(x5,x6)


#################### Point Biserial Correlation in R-Quick Guide  ########################
install.packages("ppcor") 
library(ppcor)
cor_matrix_new <- pcor(cbind(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10))
cor_matrix_new <- pcor.test(x1,x2, method = c("pearson"))
cor_matrix_new
shapiro.test(x9)

### data visualization
# Plot data to reser
x_values <- 1:20
#pi_new <- format(round(pi, 4), nsmall = 4)
options(digits = 3)  
dataplot_61 <- data.frame(Yi_61 = Yi, ni_61 = ni, pi_61 = pi, x = x_values)
dataplot_61
# plot Yi_61
data_plotYi61 <- ggplot(dataplot_61, aes(x = x, y = Yi_61)) +
  geom_bar(stat = "identity", fill = "gray") +
  geom_text(aes(label = Yi_61), vjust = -0.5, color = "black", size = 3.5) +
  labs(title = "", x = "Program", y = "Number of Students Dropping 2561") +
  theme_minimal()+
  theme(legend.key = element_rect(fill = "white", color = "black"),
        legend.key.size = unit(2, "line"),
        plot.background = element_rect(fill = "white"), # Set plot background color
        panel.background = element_rect(fill = "white"), # Set panel background color
        panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank())+ # Remove minor grid lines) +
  guides(color = guide_legend(title = NULL), linetype = guide_legend(title = ""))+
  scale_x_continuous(breaks = data_frame$x) +  # Set x-axis breaks
  scale_y_continuous(limits = c(0, 35)) # Adjust the y-axis limits
data_plotYi61

# plot ni_61
data_plotni61 <- ggplot(dataplot_61, aes(x = x, y = ni_61)) +
  geom_bar(stat = "identity", fill = "gray") +
  geom_text(aes(label = ni_61), vjust = -0.5, color = "black", size = 3.5) +
  labs(title = "", x = "Program", y = "Number of students admitted 2561") +
  theme_minimal()+
  theme(legend.key = element_rect(fill = "white", color = "black"),
        legend.key.size = unit(2, "line"),
        plot.background = element_rect(fill = "white"), # Set plot background color
        panel.background = element_rect(fill = "white"), # Set panel background color
        panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank())+ # Remove minor grid lines) +
  guides(color = guide_legend(title = NULL), linetype = guide_legend(title = ""))+
  scale_x_continuous(breaks = dataplot_61$x) +  # Set x-axis breaks
  scale_y_continuous(limits = c(0, 95)) # Adjust the y-axis limits
data_plotni61

options(digits = 4)  
# plot pi_61
data_plotpi61 <- ggplot(dataplot_61, aes(x = x, y = pi_61)) +
  geom_bar(stat = "identity", fill = "gray") +
  geom_text(aes(label = sprintf("%.4f", pi_61)), vjust = -0.5, color = "black", size = 3.5) +  # Format the label to show 4 decimal places
  labs(title = "", x = "Program", y = "Proportion with Number of Students Dropping 2561") +
  theme_minimal() +
  theme(legend.key = element_rect(fill = "white", color = "black"),
        legend.key.size = unit(2, "line"),
        plot.background = element_rect(fill = "white"),  # Set plot background color
        panel.background = element_rect(fill = "white"),  # Set panel background color
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank()) +  # Remove minor grid lines) +
  guides(color = guide_legend(title = NULL), linetype = guide_legend(title = "")) +
  scale_x_continuous(breaks = dataplot_61$x) +  # Set x-axis breaks
  scale_y_continuous(limits = c(0, 0.8), labels = scales::number_format(accuracy = 0.0001))  # Format y-axis labels to show 4 decimal places

data_plotpi61


# Set Use Data to simulation
mean(ni) ##  set lamda 
mean(x5) ##  set paramiter 4
sd(x5)   ##  set paramiter 4
var(x5)
mean(x6) ##  set paramiter 5
sd(x6)
var(x6)
binomial_model_logit <- glm(cbind(Yi, ni-Yi) ~ x5 + x6 + x1 + x2, family = binomial(link = "logit"))
printCoefmat(coef(summary(binomial_model_logit)))
binomial_model_probit <- glm(cbind(Yi, ni-Yi) ~ x5 + x6 + x1 + x2, family = binomial(link = "probit"))
printCoefmat(coef(summary(binomial_model_probit)))
binomial_model_cll <- glm(cbind(Yi, ni-Yi) ~ x5 + x7 + x8 + x10, family = binomial(link = "cloglog"))
summary(binomial_model_cll)
binomial_model_logit <- glm(cbind(Yi, ni-Yi) ~ x5, family = binomial(link = "logit"))
printCoefmat(coef(summary(binomial_model_logit)))
binomial_model_logit <- glm(cbind(Yi, ni-Yi) ~ x6, family = binomial(link = "logit"))
printCoefmat(coef(summary(binomial_model_logit)))
binomial_model_logit <- glm(cbind(Yi, ni-Yi) ~ x1, family = binomial(link = "logit"))
printCoefmat(coef(summary(binomial_model_logit)))
binomial_model_logit <- glm(cbind(Yi, ni-Yi) ~ x2, family = binomial(link = "logit"))
printCoefmat(coef(summary(binomial_model_logit)))
binomial_model_logit <- glm(cbind(Yi, ni-Yi) ~ x2, family = binomial(link = "logit"))
binomial_model_logit <- glm(cbind(Yi, ni-Yi) ~ x5, family = binomial(link = "probit"))
binomial_model_logit <- glm(cbind(Yi, ni-Yi) ~ x5, family = binomial(link = "cloglog"))



####################################  คัดเลือกตัวเเปร  Enter ###################################
# Fit the binomial regression model using glm function 
binomial_model_logit <- glm(cbind(Yi, ni-Yi) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 , family = binomial(link = "logit"))
binomial_model_probit <- glm(cbind(Yi, ni-Yi) ~ x1 + x2 + x3 + x4 + x5 +x6 + x7 +x8 +x9 + x10, family = binomial(link = "probit"))
binomial_model_cll <- glm(cbind(Yi, ni-Yi) ~ x1 + x2 + x3 + x4 + x5 +x6 + x7 +x8 +x9 + x10, family = binomial(link = "cloglog"))
printCoefmat(coef(summary(binomial_model_logit)))
printCoefmat(coef(summary(binomial_model_probit)))
printCoefmat(coef(summary(binomial_model_cll)))

# AIC
AIC(binomial_model_logit)
AIC(binomial_model_probit)
AIC(binomial_model_cll)

# BIC
BIC(binomial_model_logit)
BIC(binomial_model_probit)
BIC(binomial_model_cll)

###################################################  คัดเลือกตัวเเปร  backwards ####################################################
library(stats)
library(MASS)
library(tidyverse)
library(caret)
install.packages("leaps")
library(leaps)
############################# Define the null model Logit by backwards #######################
# Define the full model logit
fullModel_logit <- glm(cbind(Yi, ni - Yi) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, family = binomial(link = "logit"))
nullModel_logit <- glm(cbind(Yi, ni - Yi) ~ 1, family = binomial(link = "logit")) ### model with the intercept only

# Perform backward elimination
backward_logit <- stepAIC(fullModel_logit, # start with a model containing all variables (เริ่มต้นด้วยโมเดลที่มีตัวแปรทั้งหมด)
                          direction = "backward", # run backward selection 
                          scope = list(upper = fullModel_logit, # the maximum to consider is a model with all variables (สูงสุดที่ต้องพิจารณาคือโมเดลที่มีตัวแปรทั้งหมด)
                                       lower = nullModel_logit)) # the minimum to consider is a model with no variables (ขั้นต่ำที่ต้องพิจารณาคือโมเดลที่ไม่มีตัวแปร)

# Print the final selected model
formula(backward_logit)
summary(backward_logit)
AIC(backward_logit)
BIC(backward_logit)


############################# Define the null model probit by backwards #######################
# Define the full model probit
fullModel_probit <- glm(cbind(Yi, ni - Yi) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, family = binomial(link = "probit"))
nullModel_probit <- glm(cbind(Yi, ni - Yi) ~ 1, family = binomial(link = "probit")) ### model with the intercept only

# Perform backward elimination
backward_probit <- stepAIC(fullModel_probit, # start with a model containing all variables (เริ่มต้นด้วยโมเดลที่มีตัวแปรทั้งหมด)
                          direction = "backward", # run backward selection 
                          scope = list(upper = fullModel_probit, # the maximum to consider is a model with all variables (สูงสุดที่ต้องพิจารณาคือโมเดลที่มีตัวแปรทั้งหมด)
                                       lower = nullModel_probit)) # the minimum to consider is a model with no variables (ขั้นต่ำที่ต้องพิจารณาคือโมเดลที่ไม่มีตัวแปร)

# Print the final selected model
summary(backward_probit)
AIC(backward_probit)
BIC(backward_probit)

############################# Define the null model cloglog by backwards #######################
# Define the full model cloglog
fullModel_cloglog <- glm(cbind(Yi, ni - Yi) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, family = binomial(link = "cloglog"))
nullModel_cloglog <- glm(cbind(Yi, ni - Yi) ~ 1, family = binomial(link = "cloglog")) ### model with the intercept only

# Perform backward elimination
backward_cloglog <- stepAIC(fullModel_cloglog, # start with a model containing all variables (เริ่มต้นด้วยโมเดลที่มีตัวแปรทั้งหมด)
                           direction = "backward", # run backward selection 
                           scope = list(upper = fullModel_cloglog, # the maximum to consider is a model with all variables (สูงสุดที่ต้องพิจารณาคือโมเดลที่มีตัวแปรทั้งหมด)
                                        lower = nullModel_cloglog)) # the minimum to consider is a model with no variables (ขั้นต่ำที่ต้องพิจารณาคือโมเดลที่ไม่มีตัวแปร)

# Print the final selected model
summary(backward_cloglog)
AIC(backward_cloglog)
BIC(backward_cloglog)

#############################################  คัดเลือกตัวเเปร  forwards ##########################################

################################# Define the null model Logit by forwards ##########################
# Define the null model logit
fullModel_logit <- glm(cbind(Yi, ni - Yi) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, family = binomial(link = "logit")) # model with all 10 variables
nullModel_logit <- glm(cbind(Yi, ni - Yi) ~ 1, family = binomial(link = "logit")) ### model with the intercept only

# Perform forward selection
forward_logit <- step(nullModel_logit, # start with a model containing no variables (เริ่มต้นด้วยโมเดลที่ไม่มีตัวแปร)
                      direction = 'forward', # run forward selection
                      scope = list(upper = fullModel_logit, # the maximum to consider is a model with all variables (สูงสุดที่ต้องพิจารณาคือโมเดลที่มีตัวแปรทั้งหมด)
                                   lower = nullModel_logit)) ## the minimum to consider is a model with no variables (ขั้นต่ำที่ต้องพิจารณาคือโมเดลที่ไม่มีตัวแปร)

# Print the final selected model
summary(forward_logit)
AIC(forward_logit)
BIC(forward_logit)



################################# Define the null model Probit by forwards ##########################
# Define the null model probit
fullModel_probit <- glm(cbind(Yi, ni - Yi) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, family = binomial(link = "probit")) # model with all 10 variables
nullModel_probit <- glm(cbind(Yi, ni - Yi) ~ 1, family = binomial(link = "probit")) ### model with the intercept only

# Perform forward selection
forward_probit <- step(nullModel_probit, # start with a model containing no variables (เริ่มต้นด้วยโมเดลที่ไม่มีตัวแปร)
                      direction = 'forward', # run forward selection
                      scope = list(upper = fullModel_probit, # the maximum to consider is a model with all variables (สูงสุดที่ต้องพิจารณาคือโมเดลที่มีตัวแปรทั้งหมด)
                                   lower = nullModel_probit)) ## the minimum to consider is a model with no variables (ขั้นต่ำที่ต้องพิจารณาคือโมเดลที่ไม่มีตัวแปร)

# Print the final selected model
summary(forward_probit)
AIC(forward_probit)
BIC(forward_probit)

################################# Define the null model Cloglog by forwards ##########################
# Define the null model Cloglog
fullModel_cloglog <- glm(cbind(Yi, ni - Yi) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, family = binomial(link = "cloglog")) # model with all 10 variables
nullModel_cloglog <- glm(cbind(Yi, ni - Yi) ~ 1, family = binomial(link = "cloglog")) ### model with the intercept only

# Perform forward selection
forward_cloglog <- step(nullModel_cloglog, # start with a model containing no variables (เริ่มต้นด้วยโมเดลที่ไม่มีตัวแปร)
                      direction = 'forward', # run forward selection
                      scope = list(upper = fullModel_cloglog, # the maximum to consider is a model with all variables (สูงสุดที่ต้องพิจารณาคือโมเดลที่มีตัวแปรทั้งหมด)
                                   lower = nullModel_cloglog)) ## the minimum to consider is a model with no variables (ขั้นต่ำที่ต้องพิจารณาคือโมเดลที่ไม่มีตัวแปร)

# Print the final selected model
summary(forward_cloglog)
AIC(forward_cloglog)
BIC(forward_cloglog)

#############################################  คัดเลือกตัวเเปร stepwise ####################################

################################# Define the null model Logit by Stepwise ##########################
# Define the full model logit
fullModel_logit <- glm(cbind(Yi, ni - Yi) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, family = binomial(link = "logit"))
nullModel_logit <- glm(cbind(Yi, ni - Yi) ~ 1, family = binomial(link = "logit")) ### model with the intercept only

# PerformStepwise elimination
bothways_logit <- stepAIC(nullModel_logit, # start with a model containing all variables (เริ่มต้นด้วยโมเดลที่มีตัวแปรทั้งหมด)
                          direction = "both", # runStepwise selection 
                          scope = list(upper = fullModel_logit, # the maximum to consider is a model with all variables (สูงสุดที่ต้องพิจารณาคือโมเดลที่มีตัวแปรทั้งหมด)
                                       lower = nullModel_logit)) # the minimum to consider is a model with no variables (ขั้นต่ำที่ต้องพิจารณาคือโมเดลที่ไม่มีตัวแปร)

# Print the final selected model
summary(bothways_logit)
AIC(bothways_logit)
BIC(bothways_logit)

################################# Define the null model Probit by Stepwise ##########################
# Define the full model logit
fullModel_probit <- glm(cbind(Yi, ni - Yi) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, family = binomial(link = "probit"))
nullModel_probit <- glm(cbind(Yi, ni - Yi) ~ 1, family = binomial(link = "probit")) ### model with the intercept only

# Perform Stepwise elimination
bothways_probit <- stepAIC(nullModel_probit, # start with a model containing all variables (เริ่มต้นด้วยโมเดลที่มีตัวแปรทั้งหมด)
                          direction = "both", # run Stepwise selection 
                          scope = list(upper = fullModel_probit, # the maximum to consider is a model with all variables (สูงสุดที่ต้องพิจารณาคือโมเดลที่มีตัวแปรทั้งหมด)
                                       lower = nullModel_probit)) # the minimum to consider is a model with no variables (ขั้นต่ำที่ต้องพิจารณาคือโมเดลที่ไม่มีตัวแปร)

# Print the final selected model
summary(bothways_probit)
AIC(bothways_probit)
BIC(bothways_probit)


################################# Define the null model Cloglog by Stepwise ##########################
# Define the full model logit
fullModel_cloglog <- glm(cbind(Yi, ni - Yi) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, family = binomial(link = "cloglog"))
nullModel_cloglog <- glm(cbind(Yi, ni - Yi) ~ 1, family = binomial(link = "cloglog")) ### model with the intercept only

# Perform Stepwise elimination

bothways_cloglog <- stepAIC(nullModel_cloglog, # start with a model containing all variables (เริ่มต้นด้วยโมเดลที่มีตัวแปรทั้งหมด)
                          direction = "both", # run Stepwise selection 
                          scope = list(upper = fullModel_cloglog, # the maximum to consider is a model with all variables (สูงสุดที่ต้องพิจารณาคือโมเดลที่มีตัวแปรทั้งหมด)
                                       lower = nullModel_cloglog)) # the minimum to consider is a model with no variables (ขั้นต่ำที่ต้องพิจารณาคือโมเดลที่ไม่มีตัวแปร)

# Print the final selected model
formula(bothways_cloglog)
summary(bothways_cloglog)
AIC(bothways_cloglog)
BIC(bothways_cloglog)
bothways_cloglog <- stepAIC(nullModel_cloglog, direction = "both",  scope = list(upper = fullModel_cloglog, lower = nullModel_cloglog)) 

######################################### Use data 2561 #############################
data61 <- data.frame(Yi,ni,x5,x7,x8)
tranModel <- glm(Yi/ni ~ x5 + x7 + x8, data = data61, family = binomial(link = "cloglog"), weights = ni)
summary(tranModel)
samex5_61 <- c(65.24, 67.71, 59.34, 57.41, 56.7, 75.87, 62.11, 42.04, 65.02, 72.45, 30.33, 60.41, 63.46,
               56.61, 54.97, 66.65, 58.43, 60.37, 54.29, 55.25)
samex7_61 <- c(48, 47, 45, 31, 18, 42, 52, 74, 46, 41, 24, 44, 43, 32, 36, 44, 46, 42, 40, 23)
samex8_61 <- c(16644.10, 16597.90, 16867.00, 14842.90, 12127.10, 14125.50, 16567.00, 14247.40,
               17075.70, 17017.10, 14920.40, 16750.50, 14484.20, 14870.10, 15926.30, 17239.50,
               15236.80, 17017.10, 14030.20, 15976.00)
newdf_61 <- data.frame(x5 = samex5_61, x7 = samex7_61, x8 = samex8_61)
Test_model <- predict(tranModel, newdata = newdf_61, type  = "response")
Test_model
Yi_new61 <- Test_model * ni
Yi_new61
Yi
options(digits = 2)
Bias <- (Yi/ni - Test_model)
Bias
######################################### Use data 2563 #############################
data61 <- data.frame(Yi,ni,x5,x7,x8)
tranModel <- glm(Yi/ni ~ x5 + x7 + x8, data = data61, family = binomial(link = "cloglog"), weights = ni)
summary(tranModel)
newx5_63 <- c(58.08, 51.93, 55.21, 41.55, 40.07, 65.95, 45.33, 39.85, 53.6, 50.82,
              42.07, 53.73, 45.53, 42.92, 49.88, 58.83, 44.25, 52.55, 65.05,45.52)
newx7_63 <- c(50, 35, 50, 50, 40, 40, 40, 45, 35, 40, 25, 40, 25, 25, 25, 40, 25,
              25, 40, 20)
newx8_63 <- c(18478.5, 17747.5, 15856.5, 14514.5, 15895.5, 18259.2, 15789.5, 15276,
              18051, 17707, 16805, 17000.3, 16312.8, 16045.4,15487,19288.5,
              15150.5, 16894.4, 16828.5, 16727)
newdf_63 <- data.frame(x5 = newx5_63, x7 = newx7_63, x8 = newx8_63)
Test_model <- predict(tranModel, newdata = newdf_63, type  = "response")
Test_model
plot(Yi/ni ~ x5, data = data61, pch = 19, las = 1, xlab = "x5", ylab = "Prportion")
lines(Test_model ~ newx5_63, lty = 1, lwd = 2)


N <- c(2,2,3)
p <- c(0.2, 0.0994, 0.133)
plot(p ~ N,  pch = 19, las = 1, xlab = "k", ylab = "Ratio")
line(p ~ N,lty = 1, lwd = 2)

############################################### finished Data ###############################

################################# X5 and X7 ##########################
data1 = data.frame(Yi,ni,x5,x7,x8)
Test_x75_logit <- glm(cbind(Yi, ni - Yi) ~ x5 + x7, family = binomial(link = "logit"))
summary(Test_x75_logit)
AIC(Test_x75_logit)
BIC(Test_x75_logit)

Test_x75_probit <- glm(cbind(Yi, ni - Yi) ~ x5 + x7 , family = binomial(link = "probit"))
summary(Test_x75_probit)
AIC(Test_x75_probit)
BIC(Test_x75_probit)
# https://sdcastillo.github.io/PA-R-Study-Manual/glms-for-classification.html#link-functions
# https://www.karlin.mff.cuni.cz/~komarek/vyuka/2021_22/nmst432/glm_binary_links.html
# https://bookdown.org/chua/ber642_advanced_regression/probit-analysis.html#introduction-to-probit-analysis
Test_x75_cloglog <- glm(cbind(Yi, ni - Yi) ~ x5 + x7, family = binomial(link = "cloglog"))
summary(Test_x75_cloglog)
AIC(Test_x75_cloglog)
BIC(Test_x75_cloglog)


Test_x75_cauchit <- glm(cbind(Yi, ni - Yi) ~ x5 + x7, family = binomial(link = "cauchit"))
summary(Test_x75_cauchit)
AIC(Test_x75_cauchit)
BIC(Test_x75_cauchit)
################################################ Variable selection LRT  ################################
# The likelihood ratio test (LRT)
library(MASS)
Test_LRT_logit <- glm(cbind(Yi, ni - Yi) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, family = binomial(link = "logit"))
summary(Test_LRT_logit)
step(Test_LRT_logit, test="LRT")
Test_LRT_logit_Test <- glm(cbind(Yi, ni - Yi) ~ x1 + x2 + x5  + x7 + x8 + x9 + x10, family = binomial(link = "logit"))
summary(Test_LRT_logit_Test)

Test_LRT_probit <- glm(cbind(Yi, ni - Yi) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, family = binomial(link = "probit"))
summary(Test_LRT_probit)
step(Test_LRT_probit, test="LRT")
Test_LRT_probit_Test <- glm(cbind(Yi, ni - Yi) ~ x1 + x2 + x5  + x7 + x8 + x9 + x10, family = binomial(link = "probit"))
summary(Test_LRT_probit_Test)

Test_LRT_cloglog <- glm(cbind(Yi, ni - Yi) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, family = binomial(link = "cloglog"))
summary(Test_LRT_cloglog)
step(Test_LRT_cloglog, test="LRT")
Test_LRT_cloglog_Test <- glm(cbind(Yi, ni - Yi) ~ x1 + x2 + x5  + x7 + x8 + x9 + x10, family = binomial(link = "cloglog"))
summary(Test_LRT_cloglog_Test)

Test_LRT_cauchit <- glm(cbind(Yi, ni - Yi) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, family = binomial(link = "cauchit"))
summary(Test_LRT_cauchit)
step(Test_LRT_cauchit, test="LRT")
Test_LRT_cauchit_Test <- glm(cbind(Yi, ni - Yi) ~ x1 + x2 + x5  + x7 + x8 + x9 + x10, family = binomial(link = "cauchit"))
summary(Test_LRT_cauchit_Test)













################################################ Median Effective Dose, ED50  ################################
library(MASS)
ED50s <- cbind("Logit" = dose.p(bothways_logit),
               "Probit" = dose.p(bothways_probit),
               "C-log-log" = dose.p(bothways_cloglog))
ED50s

############################################# Anova ################################
gm.m1 <- glm(cbind(Yi, ni - Yi) ~ x5 * x7 * x8, family = binomial(link = "cloglog"))
anova(gm.m1, test = "Chisq")
c(deviance(gm.m1), df.residual(gm.m1))
sum(resid(gm.m1, type = "pearson")^2) # Pearson.X2
library(statmod)
qres <- qresid(gm.m1); qqnorm(qres, las = 1); abline(0,1)
scatter.smooth(qres ~ fitted(gm.m1), las = 1, main = "Residuals vs fitted",
               xlab = "Fitted value", ylab = "Quantile residuals")


################################################ check AIC BIC ################################
# backward logit
AIC(backward_logit)
BIC(backward_logit)
summary(backward_logit)
formula(backward_logit)
# backward probit
AIC(backward_probit)
BIC(backward_probit)
summary(backward_probit)
# backward cloglog
AIC(backward_cloglog)
BIC(backward_cloglog)
summary(backward_cloglog)

# forward_logit
AIC(forward_logit)
BIC(forward_logit)
summary(forward_logit)
# forward_probit
AIC(forward_probit)
BIC(forward_probit)
summary(forward_probit)
# forward_cloglog
AIC(forward_cloglog)
BIC(forward_cloglog)
summary(forward_cloglog)

# Stepwise_logit
AIC(bothways_logit)
BIC(bothways_logit)
summary(bothways_logit)
# Stepwise_probit
AIC(bothways_probit)
BIC(bothways_probit)
summary(bothways_probit)
# Stepwise_cloglog
AIC(bothways_cloglog)
BIC(bothways_cloglog)
summary(bothways_probit)

# Compare the forward model and both-direction model
anova(bothways_probit, bothways_logit, test = "Chisq")

# Compare AIC values of all three models Logit
AIC(backward_logit, backward_probit, backward_cloglog)
AIC(forward_logit, forward_probit, forward_cloglog)
AIC(bothways_logit, bothways_probit, bothways_cloglog)
BIC(bothways_logit, bothways_probit, bothways_cloglog)
# Performance : Calculate the accuracy, precision, recall, and F1-score of bothways_cloglog
install.packages("caret")
library(caret)
data_tran <- data.frame(Yi, ni, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
best_model <- glm(Yi/ni ~ x5 + x7 + x8, data = data_tran, family = binomial(link = "cloglog"), weights = ni )
summary(best_model)
x5_62 <- c(65.24,67.71, 59.34, 57.41, 56.7, 75.87, 62.11, 55.66, 65.02, 72.45, 56.12, 60.41, 63.46, 56.61, 54.97, 66.65, 58.43, 60.37, 51.92, 55.25)
x7_62 <- c(32, 29, 30, 60, 43, 42, 13, 42, 27, 11, 23, 31, 18, 17, 21, 26, 24, 26, 66, 11)
x8_62 <- c(17751, 17490.1, 16423.4, 15423, 17271.6, 18999.7, 16841.9, 14937.6, 17229.3, 17020.7, 15260.8, 16241.4, 16830.2, 15980.2, 15246.1,
           17876.2, 16357.5, 16450.1, 15563.2, 15850.8)
Yi_62 <- c(12, 13, 14, 7, 9, 7, 23, 16, 23, 7, 8, 18, 17, 14, 11, 12, 22, 2, 13, 3)
test_data <- c(0.14814, 0.27083, 0.30434, 0.11666, 0.20930, 0.16666, 0.39655, 0.21917, 0.35384, 0.09859, 0.2,
               0.28571, 0.44736, 0.31818, 0.28205, 0.16438, 0.47826, 0.05405 ,0.19696, 0.15)
newtest <- data.frame(x5 = x5_62, x7 = x7_62, x8 = x8_62)
# Set the number of decimal places to 5
options(digits = 5)
glm.predicted_linkcll <- predict(best_model, newdata = newtest, type = "response")
glm.predicted_linkcll
Predicted_Linkcll = glm.predicted_linkcll[1:20]
data_Model <- round(Predicted_Linkcll, digits=4)
data_Model

# Create a data frame with x values from 1 to 20
x_values <- 1:20
data_frame <- data.frame(x = x_values, Test_Data = test_data[1:20], Data_Model = data_Model, Yi = Yi_62)
data_frame
# Create a combined line graph with points and a linetype legend
ggplot(data_frame, aes(x = x)) +
  geom_line(aes(y = Test_Data, linetype = "Data62"), color = "black",size = 0.7) +
  geom_line(aes(y = Predicted_Linkcll, linetype = "Model_Linkcll"), color = "black", size = 0.7) +
  geom_point(aes(y = Test_Data), color = "black", size = 2) +
  geom_point(aes(y = Predicted_Linkcll), color = "black", size = 2) +
  labs(x = "Program", 
       y = "The proportion of students dropping out for each program", 
       title = "") +
  scale_color_manual(values = c("Data62" = "black", "Model_Linkcll" = "black")) +
  scale_linetype_manual(values = c("Data62" = "solid", "Model_Linkcll" = "dashed")) +  # Define linetypes
  theme_minimal() +  # Set a white background
  theme(legend.key = element_rect(fill = "white", color = "black"),
        legend.key.size = unit(2, "line"),
        plot.background = element_rect(fill = "white"), # Set plot background color
        panel.background = element_rect(fill = "white"), # Set panel background color
        panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank())+ # Remove minor grid lines) +
  guides(color = guide_legend(title = NULL), linetype = guide_legend(title = ""))


# Create barplot betweent Test_Data Predicted_Linkcll
# Create a barplot for Test_Data
test_data_plot <- ggplot(data_frame, aes(x = x, y = Test_Data)) +
  geom_bar(stat = "identity", fill = "gray") +
  geom_text(aes(label = Test_Data), vjust = -0.5, color = "black", size = 3.5) +
  labs(title = "", x = "Program", y = "The proportion of students dropping out for each program data 62") +
  theme_minimal()+
  theme(legend.key = element_rect(fill = "white", color = "black"),
        legend.key.size = unit(2, "line"),
        plot.background = element_rect(fill = "white"), # Set plot background color
        panel.background = element_rect(fill = "white"), # Set panel background color
        panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank())+ # Remove minor grid lines) +
  guides(color = guide_legend(title = NULL), linetype = guide_legend(title = ""))+
  scale_x_continuous(breaks = data_frame$x) +  # Set x-axis breaks
  scale_y_continuous(limits = c(0, 0.6)) # Adjust the y-axis limits
test_data_plot

# Create a barplot for Predicted_Linkcll
Model_Linkcll <- ggplot(data_frame, aes(x = x, y = Data_Model)) +
  geom_bar(stat = "identity", fill = "gray") +
  geom_text(aes(label = Data_Model), vjust = -0.5, color = "black", size = 3.5) +
  labs(title = "", x = "Program", y = "The proportion of students dropping out for each program Model Cll") +
  theme_minimal()+
  theme(legend.key = element_rect(fill = "white", color = "black"),
        legend.key.size = unit(2, "line"),
        plot.background = element_rect(fill = "white"), # Set plot background color
        panel.background = element_rect(fill = "white"), # Set panel background color
        panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank())+ # Remove minor grid lines) +
  guides(color = guide_legend(title = NULL), linetype = guide_legend(title = ""))+
  scale_x_continuous(breaks = data_frame$x) +  # Set x-axis breaks
  scale_y_continuous(limits = c(0, 0.6)) # Adjust the y-axis limits
Model_Linkcll


# Display the two barplots side by side
library(gridExtra)
grid.arrange(test_data_plot, Model_Linkcll, ncol = 2)









