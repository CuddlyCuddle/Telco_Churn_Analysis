# Churn Analysis:
# 
# 
# 
# 
# 
# 

### Packages
library(ROSE)
library(caret)
library(e1071)
library(lattice)
library(pROC)
library(ggplot2)
library(plotROC)
library(tidyverse)
library(dplyr)
library(ROCR)
library(rpart)
library(rattle)
library(rpart.plot)
library(caTools)

### Preliminary Functions
undersample_ds <- function(x, classCol, nsamples_class){
  for (i in 1:length(unique(x[, classCol]))){
    class.i <- unique(x[, classCol])[i]
    if((sum(x[, classCol] == class.i) - nsamples_class) != 0){
      x <- x[-sample(which(x[, classCol] == class.i), 
                     sum(x[, classCol] == class.i) - nsamples_class), ]
    }
  }
  return(x)
}
data[,"Churn"]
### Readigng in Data
data  <- read.csv("C:/Users/jeanp/OneDrive/Documents/GitHub/Telco_Churn_Analysis/WA_Fn-UseC_-Telco-Customer-Churn.csv")
summary(data)
str(data)

### Recoding Data to reduce number of redundent factors within each Variable

# For this sitation cols 8, 10:15 have three factors Yes, no, and "No phone service" or "No Internet Service", respectively.
data[c(8,10:15)] <- as.factor(ifelse(data[c(8,10:15)] == "Yes", "Yes" , "No"))

# Col 3 was originally an indicator, however, to match the respective data it was returned to a str.
data[,3] <- as.factor(ifelse(data[,3] == 1, "Yes" , "No"))

# Ensuring that the base group for churn is "No" to determine what methods help retain customers
data$Churn <- relevel(data$Churn, ref = "No")


#########################################################################################


### Data Exploration:
for(i in c(2:20)){
  temp0 <- names(data)
  if(is.factor(data[,i]) == TRUE){
    plot(data[,i], main = temp0[i])
    print(table(data[,i]))
  }
}

# Creating a Bar Graph for the binomial factor "Churn"
plot(data$Churn, main = "Churn")
table(data$Churn)
t_p <- data.frame(Response = c("No", "Yes"), Count = c(5174, 1869))
pie(x = t_p$Count, labels = t_p$Response, main = "Churn")

# Based on the Bar Chart given above, the data is not balanced. Thus, it will need to be balanced:
test <- undersample_ds(data, "Churn", 500)
table(test$Churn)


#########################################################################################


# Univariate Visualization
test <- glm(Churn ~ MonthlyCharges, data = data, family = binomial(link = "logit"))
prob <- predict.glm(test, type = "response")
plot(data$MonthlyCharges, ifelse(data$Churn == "Yes", 1, 0))
curve(predict.glm(test,data.frame(MonthlyCharges=x), type = "response"), add = TRUE)


#########################################################################################


# Model Building (Backward Selection)
Model0 <- glm(Churn ~ 1, data = data, family = binomial(link = logit))
CompleteModel <- glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges, data = data, family = binomial(link = "logit"))
ReducedModel <- step(Model0, direction = 'both', scope = formula(CompleteModel))
summary(ReducedModel)


# Testing Model with ANOVA against the saturated model and the null model against the reduced model
Model_test <- anova(ReducedModel, CompleteModel)
Model_test0 <- anova(Model0, CompleteModel)
pchisq(Model_test$Deviance, Model_test$Df, lower.tail=FALSE)
pchisq(Model_test0$Deviance, Model_test0$Df, lower.tail=FALSE)

#Reconsolidating data based on results of the binomial model
data$PaymentMethod2 <- as.factor(ifelse(data$PaymentMethod == "Electronic check", "Electronic check", "Other"))
ReducedModel2 <- glm(Churn ~ SeniorCitizen + Dependents + tenure + PhoneService + MultipleLines + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + PaperlessBilling + PaymentMethod2 + MonthlyCharges, data = data, family = binomial(link = "logit"))
summary(ReducedModel2)

#########################################################################################

# Logistic Regression (Bagged)
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, summaryFunction = twoClassSummary, classProbs = TRUE, seeds = )
Baggedlogit_model <- train(Churn ~ SeniorCitizen + Dependents + tenure + PhoneService + MultipleLines + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + PaperlessBilling + PaymentMethod2 + MonthlyCharges,
                          data = data,
                          method = "LogitBoost",
                          trControl = ctrl)

prob <- predict.train(Baggedlogit_model, type = "prob")
pred <- ifelse(prob[,2] > .7, "Yes", "No")
table(pred, data$Churn)
pred_f <- as.factor(pred)
confusionMatrix(pred_f, data$Churn)

# Recieving Operating Characteristics for CART (Bagged)
data$Chun_I <- ifelse(data$Churn == "Yes", 1, 0)
pred = prediction(prob[,2], data$Churn)
perf = performance(pred, "acc")
plot(perf)

roc = performance(pred,"tpr","fpr")
plot(roc, colorize = T, lwd = 2)
abline(a = 0, b = 1)



#########################################################################################


# Classification Tree (Bagged)
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, summaryFunction = twoClassSummary, classProbs = TRUE, seeds = )
BaggedCART_model <- train(Churn ~ SeniorCitizen + Dependents + tenure + PhoneService + MultipleLines + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + PaperlessBilling + PaymentMethod2 + MonthlyCharges,
                   data = data,
                   method = "treebag",
                   verbose = FALSE,
                   trControl = ctrl)

prob <- predict.train(BaggedCART_model, type = "prob")
pred <- ifelse(prob[,2] > .5, "Yes", "No")
table(pred, data$Churn)
pred_f <- as.factor(pred)
confusionMatrix(pred_f, data$Churn)

# Recieving Operating Characteristics for CART (Bagged)
data$Chun_I <- ifelse(data$Churn == "Yes", 1, 0)
pred = prediction(prob[,2], data$Churn)
perf = performance(pred, "acc")
plot(perf)

roc = performance(pred,"tpr","fpr")
plot(roc, colorize = T, lwd = 2)
abline(a = 0, b = 1)


#########################################################################################

# Classification Tree 
TREE_model <- train(Churn ~ SeniorCitizen + tenure + MonthlyCharges + Dependents + PhoneService + MultipleLines + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + PaperlessBilling + PaymentMethod2,
                           data = data,
                           method = "rpart",
                           metric = "ROC",
                           trControl = ctrl)


prob <- predict.train(TREE_model, type = "prob")
pred <- ifelse(prob[,2] > .5, "Yes", "No")
table(pred, data$Churn)
pred_f <- as.factor(pred)
confusionMatrix(pred_f, data$Churn)

# Recieving Operating Characteristics for CART (Bagged)
data$Chun_I <- ifelse(data$Churn == "Yes", 1, 0)
pred = prediction(prob[,2], data$Churn)
perf = performance(pred, "acc")
plot(perf)

roc = performance(pred,"tpr","fpr")
plot(roc, colorize = T, lwd = 2)
abline(a = 0, b = 1)

#########################################################################################

# Random Forrest
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, summaryFunction = twoClassSummary, classProbs = TRUE)
rf_model <- train(Churn ~ SeniorCitizen + Dependents + tenure + PhoneService + MultipleLines + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + PaperlessBilling + PaymentMethod2 + MonthlyCharges,
                  data = data,
                  method = "rf",
                  verbose = FALSE,
                  trControl = ctrl)

prob <- predict.train(rf_model, type = "prob")
pred <- ifelse(prob[,2] > .3, "Yes", "No")
table(pred, data$Churn)
pred_f <- as.factor(pred)
confusionMatrix(pred_f, data$Churn)

# Recieving Operating Characteristics for Random Forrest
data$Chun_I <- ifelse(data$Churn == "Yes", 1, 0)
pred = prediction(prob[,2], data$Churn)
perf = performance(pred, "acc")
plot(perf)

roc = performance(pred,"tpr","fpr")
plot(roc, colorize = T, lwd = 2)
abline(a = 0, b = 1)


#########################################################################################


# Gradient Boosted Model
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, summaryFunction = twoClassSummary, classProbs = TRUE)
gbm_model <- train(Churn ~ SeniorCitizen + Dependents + tenure + PhoneService + MultipleLines + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + PaperlessBilling + PaymentMethod2 + MonthlyCharges,
                  data = data,
                  method = "gbm",
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = ctrl)

prob <- predict.train(gbm_model, type = "prob")
pred <- ifelse(prob[1] > .6, "No", "Yes")
table(pred, data$Churn)

pred_f <- as.factor(pred)
confusionMatrix(pred_f, data$Churn)


# Recieving Operating Characteristics for Gradient Boosted Model
data$Chun_I <- ifelse(data$Churn == "Yes", 1, 0)
pred = prediction(prob[2], data$Churn)
perf = performance(pred, "acc")
plot(perf)

roc = performance(pred,"tpr","fpr")
plot(roc, colorize = T, lwd = 2)
abline(a = 0, b = 1)


#########################################################################################


# Weighted GBM
model_weights <- ifelse(data$Churn == "Yes", (1/table(data$Churn)[1]) * 0.5,(1/table(data$Churn)[2]) * 0.5)

gbm_model_w <- train(Churn ~ SeniorCitizen + Dependents + tenure + PhoneService + MultipleLines + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + PaperlessBilling + PaymentMethod2 + MonthlyCharges,
                   data = data,
                   method = "gbm",
                   verbose = FALSE,
                   weights = model_weights,
                   metric = "ROC",
                   trControl = ctrl)


# Reciever Operating Characteristics for Gradient Boosted Model
prob <- predict.train(gbm_model_w, type = "prob")
pred_f <- ifelse(prob[,1] > .8, "No", "Yes")
table(pred_f, data$Churn)

pred_f <- as.factor(pred_f)
confusionMatrix(pred_f, data$Churn)

# Reciever Operating Characteristics for Weighted Gradient Boosted Model
pred = prediction(prob[,2], data$Churn)
perf = performance(pred, "acc")
plot(perf)

roc = performance(pred,"tpr","fpr")
plot(roc, colorize = T, lwd = 2)
abline(a = 0, b = 1)


#########################################################################################


set.seed(1)
train_id <- sample(1:nrow(data),nrow(data)*.8 ,replace=FALSE)
traind <- data[train_id, ]
testd <- data[-train_id, ]


# Classification Tree (Bagged)
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, summaryFunction = twoClassSummary, classProbs = TRUE)
BaggedCART_model <- train(Churn ~ SeniorCitizen + Dependents + tenure + PhoneService + MultipleLines + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + PaperlessBilling + PaymentMethod2 + MonthlyCharges,
                          data = traind,
                          method = "treebag",
                          verbose = FALSE,
                          trControl = ctrl)

prob <- predict.train(BaggedCART_model, newdata = testd, type = "prob")
pred <- ifelse(prob[,2] > .7, "Yes", "No")
table(pred, testd$Churn)
pred_f <- as.factor(pred)
confusionMatrix(pred_f, testd$Churn)

# Recieving Operating Characteristics for CART (Bagged)
data$Chun_I <- ifelse(data$Churn == "Yes", 1, 0)
pred = prediction(prob[,2], testd$Churn)
perf = performance(pred, "acc")
plot(perf)

roc = performance(pred,"tpr","fpr")
plot(roc, colorize = T, lwd = 2)
abline(a = 0, b = 1)





# Random Forest
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, summaryFunction = twoClassSummary, classProbs = TRUE, seeds = )
T_rf_model <- train(Churn ~ SeniorCitizen + Dependents + tenure + PhoneService + MultipleLines + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + PaperlessBilling + PaymentMethod2 + MonthlyCharges,
                          data = traind,
                          method = "rf",
                          verbose = FALSE,
                          trControl = ctrl)

prob <- predict.train(T_rf_model, newdata = testd, type = "prob")
pred <- ifelse(prob[,2] > .4, "Yes", "No")
table(pred, testd$Churn)
pred_f <- as.factor(pred)
confusionMatrix(pred_f, testd$Churn)

# Recieving Operating Characteristics for CART (Bagged)
data$Chun_I <- ifelse(data$Churn == "Yes", 1, 0)
pred = prediction(prob[,2], testd$Churn)
perf = performance(pred, "acc")
plot(perf)

roc = performance(pred,"tpr","fpr")
plot(roc, colorize = T, lwd = 2)
abline(a = 0, b = 1)











# Weighted GBM
model_weights <- ifelse(traind$Churn == "Yes", (1/table(traind$Churn)[1]) * 0.5,(1/table(traind$Churn)[2]) * 0.5)

gbm_model_w_train <- train(Churn ~ SeniorCitizen + Dependents + tenure + PhoneService + MultipleLines + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + PaperlessBilling + PaymentMethod2 + MonthlyCharges,
                     data = traind,
                     method = "gbm",
                     verbose = FALSE,
                     weights = model_weights,
                     metric = "ROC",
                     trControl = ctrl)


# Reciever Operating Characteristics for Gradient Boosted Model
prob <- predict.train(gbm_model_w_train, newdata = testd, type = "prob")
pred_f <- ifelse(prob[,2] > .25, "Yes", "No")
table(pred_f, testd$Churn)

pred_f <- as.factor(pred_f)
confusionMatrix(pred_f, testd$Churn)

# Reciever Operating Characteristics for Weighted Gradient Boosted Model
pred = prediction(prob[,2], testd$Churn)
perf = performance(pred, "acc")
plot(perf)

roc = performance(pred,"tpr","fpr")
plot(roc, colorize = T, lwd = 2)
abline(a = 0, b = 1)







# Random Forest
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, summaryFunction = twoClassSummary, classProbs = TRUE, seeds = )
T_rf_model_w <- train(Churn ~ SeniorCitizen + Dependents + tenure + PhoneService + MultipleLines + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + PaperlessBilling + PaymentMethod2 + MonthlyCharges,
                    data = traind,
                    method = "rf",
                    verbose = FALSE,
                    weights = model_weights,
                    trControl = ctrl)

prob <- predict.train(T_rf_model_w, newdata = testd, type = "prob")
pred <- ifelse(prob[,2] > .35, "Yes", "No")
table(pred, testd$Churn)
pred_f <- as.factor(pred)
confusionMatrix(pred_f, testd$Churn)

# Recieving Operating Characteristics for CART (Bagged)
data$Chun_I <- ifelse(data$Churn == "Yes", 1, 0)
pred = prediction(prob[,2], testd$Churn)
perf = performance(pred, "acc")
plot(perf)

roc = performance(pred,"tpr","fpr")
plot(roc, colorize = T, lwd = 2)
abline(a = 0, b = 1)
