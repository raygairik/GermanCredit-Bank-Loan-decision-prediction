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


#unsupervised learning and K Means analysis
GC.df <- raw_data
head(GC.df)
colnames(GC.df)
GC.df$LoanPurpose <- 0
GC.df$LoanPurpose[GC.df$NEW_CAR == 1] <- 1
GC.df$LoanPurpose[GC.df$USED_CAR == 1] <- 2
GC.df$LoanPurpose[GC.df$FURNITURE == 1] <- 3
GC.df$LoanPurpose[GC.df$`RADIO/TV` == 1] <- 4
GC.df$LoanPurpose[GC.df$EDUCATION == 1] <- 5
GC.df$LoanPurpose[GC.df$RETRAINING == 1] <- 6

GC.df <- GC.df[-c(5:10)]
sapply(GC.df, function(x) sum(is.na(x)))
dim(GC.df)


#doing cluster means analysis

# k means
# normalized distance:

kmeanData <- GC.df[c(2, 3, 5, 6, 8, 21, 23, 26)]
kmeanData <- rename(kmeanData, c("CHK_ACCT"="Chk_acct", "DURATION"="Duration", "AMOUNT"="Amt", "SAV_ACCT"="Sav_Acct", "INSTALL_RATE"="Installment","NUM_CREDITS"="Num_Credits", "NUM_DEPENDENTS"="Dependents", "RESPONSE"="CreditRating"))
head(kmeanData)
kmeanData.norm <- sapply(kmeanData, scale)
row.names(kmeanData) <- row.names(kmeanData) 

kmeanData.norm
# run kmeans algorithm 
set.seed(1)
km <- kmeans(kmeanData.norm, 6)
km
km$cluster


{
  #### Figure 15.5
  
  # plot an empty scatter plot
  plot(c(0), xaxt = 'n', ylab = "", type = "l", 
       ylim = c(min(km$centers), max(km$centers)), xlim = c(0,8 ))
  
  # label x-axes
  axis(1, at = c(1:8), labels = names(kmeanData[c(1:8)]))
  
  # plot centroids
  for (i in c(1:6))
    lines(km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),
                                                         "black", "dark grey"))
  
  # name clusters
  text(x = 0.5, y = km$centers[, 1], labels = c("Low Rating", "Low Val Customer","Past Borrowers ","Many Dependents","New borrower-Liquid","Long term large Amt"))
}


colnames(GC.df)
kmodeData <- GC.df[c(4, 7, 22, 26, 27)]
kmodeData <- rename(kmodeData, c("HISTORY"="History", "EMPLOYMENT"="Employment", "JOB"="Job", "RESPONSE"="Response","LoanPurpose"="LoanPurpose"))
head(kmodeData)
kmode <- kmodes(kmodeData, 4, weighted = FALSE)
show(kmode)

gc.df <- raw_data
#decision tree
class.tree <- rpart(RESPONSE ~ ., data = gc.df, 
                    control = rpart.control(maxdepth = 2, minsplit = 10), method = "class")

class.tree
help(rpart)
prp(class.tree, type = 1, extra = 1, split.font = 1, varlen = -10)  

gc.df <- select(gc.df, -c(AMOUNT))
glimpse(gc.df)

set.seed(1)  
train.index <- sample(c(1:dim(gc.df)[1]), dim(gc.df)[1]*0.6)  
train.df <- GC.df[train.index, ]
valid.df <- GC.df[-train.index, ]

# classification tree
default.ct <- rpart(RESPONSE ~ ., data = train.df, method = "class")
# plot tree
prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)



