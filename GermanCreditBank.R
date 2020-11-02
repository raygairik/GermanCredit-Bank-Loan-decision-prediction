library(knitr)
library(tidyr)
library(reshape2)
library(RColorBrewer)
library(GGally)
library(ggplot2)
library(caret)
library(glmnet)
library(boot)
library(verification)
library(readr)
library(corrplot)
library(tidyverse)
library(dplyr)
library(factoextra)
library(klaR)
library(plyr)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(gains)

raw_data <- read.csv("C:/Users/gairi/OneDrive/kelley_files/Predictive analytics_assignments/Project/GermanCredit.csv", header = TRUE)

data <-raw_data
colnames(data)
data$LoanPurpose <- 0
data$LoanPurpose[data$NEW_CAR == 1] <- 1
data$LoanPurpose[data$USED_CAR == 1] <- 2
data$LoanPurpose[data$FURNITURE == 1] <- 3
data$LoanPurpose[data$`RADIO/TV` == 1] <- 4
data$LoanPurpose[data$EDUCATION == 1] <- 5
data$LoanPurpose[data$RETRAINING == 1] <- 6

data <- data[-c(5:10)]
colnames(data)
dim(data)

#Basic data anaylysis
str(data)
head(data,4)
summary(data)

#Check the new dimensions of the data set
dim(data)

#count sum of total missing fields 
sum(is.na(data))

# Count the missing values in each column
data %>% summarise_all(funs(sum(is.na(.)))) 

colnames(data)

glimpse(data)

summary(data)

str(data)

# EDA for categorical variables 
ggplot(data, aes(CHK_ACCT, ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge") 

ggplot(data, aes(HISTORY, ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge") 

ggplot(data, aes(CHK_ACCT, ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge") 

ggplot(data, aes(HISTORY, ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge") 

ggplot(data, aes(CHK_ACCT, ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge") 

ggplot(data, aes(LoanPurpose, ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge") 

ggplot(data, aes(SAV_ACCT, ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge") 

ggplot(data, aes(EMPLOYMENT, ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge") 

ggplot(data, aes(INSTALL_RATE, ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge") 

ggplot(data, aes(MALE_DIV, ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge") 

ggplot(data, aes(SAV_ACCT, ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge") 

ggplot(data, aes(MALE_SINGLE, ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge") 

ggplot(data, aes(MALE_MAR_or_WID, ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge") 

ggplot(data, aes("CO-APPLICANT", ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge") 

ggplot(data, aes(GUARANTOR, ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge") 

ggplot(data, aes(PRESENT_RESIDENT, ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge") 

ggplot(data, aes(REAL_ESTATE, ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge") 

ggplot(data, aes(PROP_UNKN_NONE, ..count..)) + 
  geom_bar(aes(fill = RESPONSE), position = "dodge")


#Analysis of continuous Variables
#histogram
brksCredit <- seq(0, 80, 10)
hist(data$DURATION, breaks=brksCredit, xlab = "Credit Month", ylab = "Frequency", main = " ", cex=0.4)
hist(data$AMOUNT, xlab = "Credit Amount", ylab = "Frequency", main = " ", cex=0.4)
hist(data$AGE, breaks=brksCredit, xlab = "Age", ylab = "Frequency", main = " ", cex=0.4)

# Box Plot
boxplot(data$DURATION, bty="n",xlab = "Credit Month", cex=0.4)
boxplot(data$AMOUNT, bty="n",xlab = "Credit Amount", cex=0.4)
boxplot(data$AGE, bty="n",xlab = "Credit Age", cex=0.4)

#to find the unique values
unique(data$CHK_ACCT)
unique(data$INSTALL_RATE)
unique(data$NUM_CREDITS)

colnames(data)

## Logistic Regression
set.seed(123)
train.df <- createDataPartition(as.factor(data$RESPONSE), p=0.7, list=FALSE)
my_data.train <- data[train.df,]
my_data.test <- data[-train.df,]


credit.glm <- glm(RESPONSE ~ ., family = binomial, my_data.train)
#credit.glm.step <- step(credit.glm0, direction = "forward")

#AIC=678.48

summary(credit.glm)

# Based upon the summary, CHK_ACCT, HISTORY, SAV_ACCT appear to be significant

credit.glm.step <- step(credit.glm, k = log(nrow(my_data.train)))
summary(credit.glm.step)
#Here we noticed that the step() function kept all the features used previously.
#While we used the AIC criteria to compare models, there are other criteria we could have used.


# Let us create a confusin matrix to evaluate the performance of algorithm
logit.reg.pred <- predict(credit.glm, newdata=my_data.test, type="response")

my_data.test$predicted.response = predict(credit.glm, newdata=my_data.test, type="response")

table(my_data.test$RESPONSE, my_data.test$predicted.response > 0.5)

#Calculate accuracy 
(42+177)/(48+177+42+33)

## 0.73 73% accuracy of the model

#Calculate recall(Sensitivity)
(177)/(177+48)

## 0.786  78.6% of good debtors are classified as good debtors. That's a great sensisitivity

# visualize the model
plot(logit.reg.pred)

#plotting residuals
plot.df <- data.frame(actual = my_data.test$RESPONSE, predicted = logit.reg.pred)

ggplot(plot.df) + 
  geom_point(aes(x=actual, y=predicted))

#box Plot 
ggplot(plot.df) + 
  geom_boxplot(aes(x=factor(actual), y=predicted))

#lift chart  in process


gain <- gains(my_data.test$RESPONSE, logit.reg.pred, groups=10)

# plot lift chart
{  # got to put the curly brackers or it breaks!
  plot(c(0,gain$cume.pct.of.total*sum(my_data.test$RESPONSE))~c(0,gain$cume.obs), 
       xlab="# cases", ylab="Cumulative", main="", type="l")
  lines(c(0,sum(my_data.test$RESPONSE))~c(0, dim(my_data.test)[1]), lty=2)
}

# ROC
library(pROC)
r <- roc(data$RESPONSE, data$CHK_ACCT)
r
plot.roc(r)

