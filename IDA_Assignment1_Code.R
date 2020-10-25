#QUESTION 3 (a)

#Set the seed so that the results are reproducible
set.seed(123)

#Dataset of size 500
n <- 500

#Random variables following independent standard normal distributions
z1 <- rnorm(n)
z2 <- rnorm(n)
z3 <- rnorm(n)

#Designing Y1 and Y2
y1 <- 1+z1
y2 <- 5 + 2*(z1) + z2

#Complete dataset on (Y1, Y2)
complete_data <- data.frame(y1, y2)
head(complete_data, 10)

#Imposing missingness on y2
y2_missing_mar <- y2

#Setting conditions for missingness
a = 2
b = 0
index <- which(((a*(y1-1))+(b*(y2-5))+z3)<0)
y2_missing_mar[index] <- NA

#Corresponding observed dataset with missingness imposed on Y2
df_mar <- data.frame(y1,y2,y2_missing_mar)
head(df_mar, 10)

#installing and loading the necessary packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
library(ggplot2)
library(dplyr)
library(tidyr)

#plotting the distributions
df_mar %>%
  #remove y1 from the plot
  select(-y1) %>% 
  #increase the number of rows, decrease the number of columns
  pivot_longer(everything()) %>%
  #set list of name-value pairs
  ggplot(aes(x=value,fill=name))+ 
  #calculate the density estimate
  geom_density(alpha=0.5)+
  #set graph title
  ggtitle('Marginal Distribution of Complete and Observed MAR data')+
  #set x-axis name
  xlab("Y2") +
  #set y-axis name
  ylab("Density") +
  #set legend title
  labs(fill='Variables')+
  #set variable colors and legend lable names
  scale_fill_manual(values = c("red","blue"), labels=c("Complete","Observed")) +
  #set theme of the graph
  theme_gray()

# mean for the complete and observed data
mean(df_mar$y2) # completed data
mean(df_mar$y2_missing_mar, na.rm = TRUE) # observed data
# std deviation for the observed and completed data
sd(df_mar$y2_missing_mar, na.rm = TRUE) # observed data
sd(df_mar$y2) # completed data
# correlation for the observed and completed data
cor(df_mar$y2_missing_mar, df_mar$y1, use = "complete") # observed data
cor(df_mar$y2, df_mar$y1) # completed data

#QUESTION 3(b)

#Fitting a regression model on the completed MNAR data
fit_mar <- lm(y2_missing_mar ~ y1, data = df_mar)
summary(fit_mar)

# predicting the y2 score for those with missing values, adding noise to the predictions
predicted_sri_mar <- predict(fit_mar, newdata = df_mar) + rnorm(nrow(df_mar), 0, sigma(fit_mar))

# completed Y2 variable
y2_sri_mar <- ifelse(is.na(df_mar$y2_missing_mar) == TRUE, predicted_sri_mar, df_mar$y2_missing_mar)

#creating data frame with imputed y2 values
sri_data_mar <- data.frame(y1, y2, y2_sri_mar)

sri_data_mar %>%
  #remove y1 from the plot
  select(-y1) %>%
  # increase the number of rows, decrease the number of columns
  pivot_longer(everything()) %>%
  #set list of name-value pairs
  ggplot(aes(x=value,fill=name))+
  #calculate the density estimate
  geom_density(alpha=0.5)+
  #set graph title
  ggtitle('Marginal Distribution of Complete and Imputed MAR Data')+
  #set x-axis name
  xlab("Y2") +
  #set y-axis name
  ylab("Density") +
  #set legend title
  labs(fill='Variables')+
  #set variable colors and legend lable names
  scale_fill_manual(values = c("red","blue"), labels=c("Complete", "Imputed"))+
  #set theme of the graph
  theme_gray()

# mean for the complete and imputated data
mean(df_mar$y2) # complete data
mean(y2_sri_mar) # sri completed data
# std deviation for the observed and completed data
sd(df_mar$y2_missing_mar, na.rm = TRUE) # observed data
sd(y2_sri_mar) # completed data
# correlation for the observed and completed data
cor(df_mar$y2_missing_mar, df_mar$y1, use = "complete") # observed data
cor(y2_sri_mar, df_mar$y1) # completed data

# QUESTION 3(c)
#Imposing missingness on new y2
y2_missing_mnar <- y2

#Setting conditions for missingness
a = 0
b = 2
index_mnar <- which(((a*(y1-1))+(b*(y2-5))+z3)<0)
y2_missing_mnar[index_mnar] <- NA

#Corresponding observed dataset with missingness imposed on Y2
df_mnar <- data.frame(y1,y2,y2_missing_mnar)
head(df_mnar, 10)

#plotting the distributions
df_mnar %>%
  #remove y1 from the plot
  select(-y1) %>%
  #increase the number of rows, decrease the number of columns
  pivot_longer(everything()) %>%
  #set list of name-value pairs
  ggplot(aes(x=value,fill=name))+
  #calculate the density estimate
  geom_density(alpha=0.5)+
  #set graph title
  ggtitle('Marginal Distribution of Complete and Observed MNAR data')+
  #set x-axis name
  xlab("Y2") + 
  #set y-axis name
  ylab("Density") + 
  #set legend title
  labs(fill='Variables')+
  #set variable colors and legend lable names
  scale_fill_manual(values = c("red","blue"), labels=c("Complete", "Observed"))+
  #set theme of the graph
  theme_gray()

# mean for the complete and observed data
mean(df_mnar$y2) # complete data
mean(df_mnar$y2_missing_mnar, na.rm = TRUE) # observed data
# std deviation for the observed and complete data
sd(df_mnar$y2_missing_mnar, na.rm = TRUE) # observed data
sd(df_mnar$y2) # completed data
# correlation for the observed and completed data
cor(df_mnar$y2_missing_mnar, df_mnar$y1, use = "complete") # observed data
cor(df_mnar$y2, df_mnar$y1) # complete data

# QUESTION 3(d)
fit_mnar <- lm(y2_missing_mnar ~ y1, data = df_c)
summary(fit_mnar)

# predicting the y2 score for those with missing values, adding noise to the predictions
predicted_sri_mnar <- predict(fit_mnar, newdata = df_c) + rnorm(nrow(df_c), 0, sigma(fit_mnar))

# completed Y2 variable
y2_sri_mnar <- ifelse(is.na(df_c$y2_missing_mnar) == TRUE, predicted_sri_mnar, df_c$y2_missing_mnar)

#creating data frame with imputed y2 values
sri_mnar_data <- data.frame(y1, y2, y2_sri_mnar)

sri_mnar_data %>%
  #remove y1 from the plot
  select(-y1) %>%
  # increase the number of rows, decrease the number of columns
  pivot_longer(everything()) %>%
  #set list of name-value pairs
  ggplot(aes(x=value,fill=name))+
  #calculate the density estimate
  geom_density(alpha=0.5)+
  #set graph title
  ggtitle('Mraginal Distribution of Complete and Imputed MNAR Data')+
  #set x-axis name
  xlab("Y2") + 
  #set y-axis name
  ylab("Density") + 
  #set legend title
  labs(fill='Variables')+
  #set variable colors and legend lable names
  scale_fill_manual(values = c("red","blue"), labels=c("Complete","Imputed"))+
  #set theme of the graph
  theme_gray()

# mean for the complete and imputated data
mean(df_mnar$y2) # complete data
mean(y2_sri_mnar) # completed data
# std deviation for the observed and completed data
sd(df_mnar$y2_missing_mnar, na.rm = TRUE) # observed data
sd(y2_sri_mnar) # completed data
# correlation for the observed and completed data
cor(df_mnar$y2_missing_mnar, df_mnar$y1, use = "complete") # observed data
cor(y2_sri_mnar, df_mnar$y1) # completed data

#QUESTION 4
load("databp.Rdata")
recovtime <- databp$recovtime
logdose <- databp$logdose
bloodp <- databp$bloodp

#QUESTION 4(a)
ind <- which(is.na(databp$recovtime) == FALSE) #indices of subjects with recovtime observed
mccoverall <- mean(databp$recovtime, na.rm = TRUE) #mean value of the recovery time 
seccoverall <- sd(databp$recovtime, na.rm = TRUE)/sqrt(length(ind)) #standard error of the recovery time 
mccoverall; seccoverall

cor_recov_dose_cc <- cor(databp$recovtime, databp$logdose, use = "complete") #correlation between recovery time and dose
cor_recov_bloodp_cc <- cor(databp$recovtime, databp$bloodp, use = "complete") #correlation between between recovery time and blood pressure
cor_recov_dose_cc; cor_recov_bloodp_cc

#QUESTION 4(b)
recovtime_mi <- ifelse(is.na(databp$recovtime) == TRUE, mean(databp$recovtime, na.rm = TRUE), databp$recovtime)
n <- length(recovtime_mi)
mmi <- mean(recovtime_mi)
semi <- sd(recovtime_mi)/sqrt(n)
mmi; semi

cor_recov_dose_mi <- cor(recovtime_mi, databp$logdose, use = "complete") #correlation between recovery time and dose
cor_recov_bloodp_mi <- cor(recovtime_mi, databp$bloodp, use = "complete") #correlation between between recovery time and blood pressure
cor_recov_dose_mi; cor_recov_bloodp_mi

#QUESTION 4(c)
fitrecov <- lm(recovtime ~ logdose + bloodp)

summary(fitrecov)
coef(fitrecov)

predri <- predict(fitrecov, newdata = databp)
predri[4]; predri[10]; predri[22]

recovtime_ri <- ifelse(is.na(databp$recovtime) == TRUE, predri, databp$recovtime)
mri <- mean(recovtime_ri)
seri <- sd(recovtime_ri)/sqrt(n)
mri; seri

cor_recov_dose_ri <- cor(recovtime_ri, databp$logdose, use = "complete") #correlation between recovery time and dose
cor_recov_bloodp_ri <- cor(recovtime_ri, databp$bloodp, use = "complete") #correlation between between recovery time and blood pressure
cor_recov_dose_ri; cor_recov_bloodp_ri

plot(fitrecov$fitted.values, residuals(fitrecov), xlab = "Fitted values", ylab = "Residuals")
qqnorm(rstandard(fitrecov), xlim = c(-3, 3), ylim = c(-3, 3))
qqline(rstandard(fitrecov), col = 2)

# QUESTION 4(d)
set.seed(123)
predsri <- predict(fitrecov, newdata = databp) + rnorm(n, 0, sigma(fitrecov))
predsri[4]; predsri[10]; predsri[22]

recovtime_sri <- ifelse(is.na(databp$recovtime) == TRUE, predsri, databp$recovtime)
msri <- mean(recovtime_sri)
sesri <- sd(recovtime_sri)/sqrt(n)
msri; sesri

cor_recov_dose_sri <- cor(recovtime_sri, databp$logdose, use = "complete") #correlation between recovery time and dose
cor_recov_bloodp_sri <- cor(recovtime_sri, databp$bloodp, use = "complete") #correlation between between recovery time and blood pressure
cor_recov_dose_sri; cor_recov_bloodp_sri

#QUESTION 4(e)
#extracting patient numbers
donor = 1:nrow(databp)

#imputing missing values from the regression imputation
predmis = ifelse(is.na(databp$recovtime), NA, predri)
#looping over the missing subjects
for (i in 1:nrow(databp)){
  donor[i] = databp$recovtime[which.min((predmis - predri[[i]])**2)]
}
donor

#predictive mean matching
recovtime_pmm <- ifelse(is.na(databp$recovtime), donor, databp$recovtime)

#mean value of recovery time from predictive mean matching
mean(recovtime_pmm)
#standard error of recovery time from predictive mean matching
sd(recovtime_pmm)/sqrt(length(recovtime_pmm))
#correlation between the recovery time and the dose from predictive mean matching
cor(recovtime_pmm, databp$logdose)
#correlation between the recovery time and blood pressure from predictive mean matching
cor(recovtime_pmm, databp$bloodp)