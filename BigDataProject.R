#teleco <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv",header=TRUE)
dim(teleco)
names(teleco)
str(teleco)

###### Data Cleaning #####

# Convert SeniorCitizen to categorical variable
teleco$SeniorCitizen[teleco$SeniorCitizen==0] <- "No"
teleco$SeniorCitizen[teleco$SeniorCitizen==1] <- "Yes"

##Remove unnecessary columns -- customerID gives no meaning to our model
teleco$customerID <- NULL

##check for any missing data

library(tidyverse)

teleco$TotalCharges <- as.numeric(teleco$TotalCharges, na.rm = TRUE)



## some values are TRUE , we have to handle them

# Fill missing values in TotalCharges with mean
teleco$TotalCharges[is.na(teleco$TotalCharges)] <- mean(teleco$TotalCharges, na.rm = TRUE)


##check for tenure columns
teleco[teleco$tenure == 0, ]
## since there is rows where tenure =0 while the montly charge is not 0 
## drop those rows
teleco<- teleco[teleco$tenure != 0, ]
head(teleco)



## There are three continuous variables and they are Tenure, MonthlyCharges and TotalCharges


###### Visualization Charts used #####

#1#
library(ggplot2)
ggplot(teleco, aes(x=teleco$Churn))+geom_bar(fill = c("#E0E74E", "#800e7b"))+labs(x='Churn')+ggtitle("Churn Percent")
  # from the bar chart we deduce that CHURN columns tells us about the number of Customers who left within the last month, around 30% of the customers left
  # the platform within the last month

#2#
par(mfrow=c(1,3))
boxplot(teleco$TotalCharges,main="Total Charges",col='darkred')
boxplot(teleco$MonthlyCharges,main="Monthly Charges",col='darkblue')
boxplot(teleco$tenure,main="Tenure",col='darkgreen')
## After visualizing the outliers of the three continuous attributes that we have in the dataset, no outliers were obvioused in them

#3#
Churn<-teleco$Churn
gender<-teleco$gender
SeniorCitizen<-teleco$SeniorCitizen
Dependents<-teleco$Dependents
Partner<-teleco$Partner
PhoneService<-teleco$PhoneService
MultipleLines<-teleco$MultipleLines

library(gridExtra)
plot1<-ggplot(teleco, aes(x=gender, fill = Churn)) + geom_bar(position = 'fill')
plot2<-ggplot(teleco, aes(x=SeniorCitizen, fill = Churn)) + geom_bar(position = 'fill')
plot3<-ggplot(teleco, aes(x=Partner, fill = Churn)) + geom_bar(position = 'fill')
plot4<-ggplot(teleco, aes(x=Dependents, fill = Churn)) + geom_bar(position = 'fill')
plot5<-ggplot(teleco, aes(x=PhoneService, fill = Churn)) + geom_bar(position = 'fill')
plot6<-ggplot(teleco, aes(x=MultipleLines, fill = Churn)) + geom_bar(position = 'fill')
grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,nrow=2,ncol=3)

##Observations##
#1, The Churn percent is almost equal in case of Male and Female
#2. The Percent of the churn is higher in case of senior Citizens
#3. By Comparing the Customers with Partners and Dependants to those who don't have partners & Dependants,
#   those who have Partners and Dependants have lower churn rate than others  

#4#
library(ggplot2)
InternetService<-teleco$InternetService
OnlineSecurity<-teleco$OnlineSecurity
OnlineBackup<-teleco$OnlineBackup
DeviceProtection<-teleco$DeviceProtection
TechSupport<-teleco$TechSupport
StreamingTV<-teleco$StreamingTV
library(gridExtra)
plot7<-ggplot(teleco, aes(x=InternetService, fill = Churn)) + geom_bar(position = 'fill')
plot8<-ggplot(teleco, aes(x=OnlineSecurity, fill = Churn)) + geom_bar(position = 'fill')
plot9<-ggplot(teleco, aes(x=OnlineBackup, fill = Churn)) + geom_bar(position = 'fill')
plot10<-ggplot(teleco, aes(x=DeviceProtection, fill = Churn)) + geom_bar(position = 'fill')
plot11<-ggplot(teleco, aes(x=TechSupport, fill = Churn)) + geom_bar(position = 'fill')
plot12<-ggplot(teleco, aes(x=StreamingTV, fill = Churn)) + geom_bar(position = 'fill')
grid.arrange(plot7,plot8,plot9,plot10,plot11,plot12,nrow=2,ncol=3)

##Observations##
#1.Customers who do not have services like No OnlineSecurity , OnlineBackup and TechSupport have left the platform in the past month. 
#2.The Internet service churn higher rate exists in case of fiber optic InternetServices 

#5#
StreamingMovies<-teleco$StreamingMovies
Contract<-teleco$Contract
PaperlessBilling<-teleco$PaperlessBilling
PaymentMethod<-teleco$PaymentMethod
plt1<-ggplot(teleco, aes(x=StreamingMovies, fill = Churn)) + geom_bar(position = 'fill')
plt2<-ggplot(teleco, aes(x=Contract, fill = Churn)) + geom_bar(position = 'fill')
plt3<-ggplot(teleco, aes(x=PaperlessBilling, fill = Churn)) + geom_bar(position = 'fill')
plt4<-ggplot(teleco, aes(x=PaymentMethod, fill = Churn)) + geom_bar(position = 'fill')
grid.arrange(plt1,plt2,plt3,plt4,nrow=2,ncol=2)

##Observations##
#1. Customers with monthly subscription are the largest percentage that have left the platform compared to Customers' contract options.
#2. Churn percent is higher in case of cutsomers having paperless billing option.
#3. Customers who have ElectronicCheck PaymentMethod tend to leave the platform with high percentage rather than other options.


#6#
ggplot(teleco,aes(y=tenure,x="",fill=Churn))+geom_boxplot()+xlab(" ")
#The median tenure for customers who have left is around 10 months.

#7#
ggplot(teleco,aes(y=MonthlyCharges,x="",fill=Churn))+geom_boxplot()+xlab(" ")
#The median of the Monthly Charges is very high as it is above 75 which means that the customers who have churned have high monthly charges

#8#
ggplot(teleco,aes(y=TotalCharges,x="",fill=Churn))+geom_boxplot()+xlab(" ")
#The median Total charges of customers who have churned is low.





object_to_int <- function(teleco_series) {
  if (is.character(teleco_series)) {
    teleco_series <- as.integer(factor(teleco_series))
  }
  return(teleco_series)
}





teleco <- as.data.frame(lapply(teleco, object_to_int))
teleco$Churn<- ifelse(teleco$Churn== 1, 0, 1)


X <- teleco[, !(colnames(teleco) %in% c('Churn'))]
y <- teleco$Churn

library(dplyr)  
library(ggplot2)  
library(caret)
library(caTools)


set.seed(40) 
trainIndex <- createDataPartition(y, p = 0.7, list = FALSE, times = 1)
X_train <- X[trainIndex, ]
y_train <- y[trainIndex]
X_test <- X[-trainIndex, ]
y_test <- y[-trainIndex]

##standarlization

num_cols <- c("tenure", "MonthlyCharges", "TotalCharges")
cat_cols_ohe <- c("PaymentMethod", "Contract", "InternetService") # those that need one-hot encoding
cat_cols_le <- setdiff(names(X_train), c(num_cols, cat_cols_ohe)) #those that need label encoding

scaler <- scale(X_train[, num_cols])
X_train[, num_cols] <- scaler
X_test[, num_cols] <- scale(X_test[, num_cols], center = attr(scaler, "scaled:center"), scale = attr(scaler, "scaled:scale"))


##Decision tree classifier
library(rpart)

dt_model <- rpart(y_train ~ ., data = X_train, method = "class")
dt_pred <- predict(dt_model, X_test, type = "class")
accuracy_dt <- mean(dt_pred == y_test)
print(paste("Decision tree accuracy is:", accuracy_dt))

#Logistic regression

library(glmnet)
lr_model <- glm(y_train ~ ., family = binomial(link = "logit"), data = cbind(y_train, X_train))
y_pred <- predict(lr_model, newdata = X_test, type = "response")
accuracy_lr <- mean(ifelse(y_pred > 0.5, 1, 0) == y_test)
cat("Logistic Regression accuracy is:", accuracy_lr, "\n")
y_pred_prob <- predict(lr_model, newdata = X_test, type = "response")
library(ROCR)
y_test2 <- as.numeric(y_test)

pred = prediction(y_pred_prob, y_test)
perf <- performance(pred, "tpr", "fpr")
#par(mfrow=c(1,1))
plot(perf, col = "red", main = "Logistic Regression ROC Curve", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "black")
legend("bottomright", legend = "Logistic Regression", col = "red", lty = 1, cex = 0.8)


##KNN

library(class)
knn_model <- knn(X_train, X_test, y_train, k = 11)
predicted_y <- knn_model
accuracy_knn <- mean(predicted_y == y_test)
print(paste("KNN accuracy is:", accuracy_knn))




