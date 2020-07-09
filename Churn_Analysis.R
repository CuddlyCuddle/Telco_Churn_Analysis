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
library(pROC)
library(bnclassify)
library(HiDimDA)
library(broom)

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

### Reading in Data
churn <- na.omit(read.csv("C:/Users/jeanp/OneDrive/Documents/GitHub/Telco_Churn_Analysis/WA_Fn-UseC_-Telco-Customer-Churn.csv"))
summary(churn)
str(churn)

### Recoding Data to reduce number of redundent factors within each Variable

# For this sitation cols 8, 10:15 have three factors Yes, no, and "No phone service" or "No Internet Service", respectively.
churn[c(8,10:15)] <- ifelse(churn[c(8,10:15)] == "Yes", "Yes" , "No")
churn[sapply(churn, is.character)] <- lapply(churn[sapply(churn, is.character)], as.factor)

# Col 3 was originally an indicator, however, to match the respective data it was returned to a str.
churn[,3] <- as.factor(ifelse(churn[,3] == 1, "Yes" , "No"))


# Ensuring that the base group for churn is "No" to predict the individuals who churn
churn$Churn <- relevel(churn$Churn, ref = "No")

#########################################################################################


### Data Exploration:
for(i in 2:20){
  temp0 <- names(churn)
  if(is.factor(churn[,i]) == TRUE){
    gg_plot  <- ggplot(churn, aes(churn[,i], fill = Churn)) +
      geom_bar(position = position_dodge()) +
      ggtitle(temp0[i]) +
      xlab(temp0[i]) +
      coord_flip()
    print(gg_plot)
  }
}

# Creating a Bar Graph for the binomial factor "Churn"
plot(churn$Churn, main = "Churn")
table(churn$Churn)
t_p <- data.frame(Response = c("No", "Yes"), Count = c(5174, 1869))
pie(x = t_p$Count, labels = t_p$Response, main = "Churn")

# Based on the Bar Chart given above, the data is not balanced and needs to be addressed or the data may face baises:
table(churn$Churn)

#########################################################################################


# Univariate Visualization
test <- glm(Churn ~ MonthlyCharges, data = churn, family = binomial(link = "logit"))
prob <- predict.glm(test, type = "response")
plot(churn$MonthlyCharges, ifelse(churn$Churn == "Yes", 1, 0))
curve(predict.glm(test,data.frame(MonthlyCharges=x), type = "response"), add = TRUE)


#########################################################################################


# Model Building (Backward Selection)
Model0 <- glm(Churn ~ 1, data = churn, family = binomial(link = logit))
CompleteModel <- glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingMovies + StreamingTV + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges, data = churn, family = binomial(link = "logit"))
ReducedModel <- step(Model0, direction = 'both', scope = formula(CompleteModel))
summary(ReducedModel)


# Testing Model with ANOVA against the saturated model and the null model against the reduced model
Model_test <- anova(ReducedModel, CompleteModel)
Model_test0 <- anova(Model0, CompleteModel)
pchisq(Model_test$Deviance, Model_test$Df, lower.tail=FALSE)
pchisq(Model_test0$Deviance, Model_test0$Df, lower.tail=FALSE)

# Looking at combining Streaming
summary(Model_streamingTV <- glm(Churn ~ StreamingTV, data = churn, family = binomial))
summary(Model_streamingMovies <- glm(Churn ~ StreamingMovies, data = churn, family = binomial))
summary(Model_streaming <- glm(Churn ~ StreamingMovies + StreamingTV, data = churn, family = binomial))
churn$streaming <- ifelse(churn$StreamingTV == "Yes"| churn$StreamingMovies == "Yes", "Yes", "No")

#Reconsolidating data based on results of the binomial model
churn$PaymentMethod2 <- as.factor(ifelse(data$PaymentMethod == "Electronic check", "Electronic check", "Other"))
ReducedModel2 <- glm(Churn ~ SeniorCitizen + streaming + Dependents + tenure + PhoneService + MultipleLines + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + PaperlessBilling + PaymentMethod2 + MonthlyCharges, data = churn, family = binomial(link = "logit"))
summary(ReducedModel2)

# Model Building 2 (Backward Selection)
Model0 <- glm(Churn ~ 1, data = churn, family = binomial(link = logit))
CompleteModel <- glm(Churn ~ gender + SeniorCitizen + streaming + Partner + Dependents + tenure + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + Contract + PaperlessBilling + PaymentMethod2 + MonthlyCharges, data = churn, family = binomial(link = "logit"))
ReducedModel <- step(Model0, direction = 'both', scope = formula(CompleteModel))
summary(ReducedModel)



#########################################################################################
#####################              Predictive Modeling              #####################
#########################################################################################
# Setting seed for reproducibility
set.seed(1)
train_id <- sample(1:nrow(churn),nrow(churn)*.8 ,replace=FALSE)
traind <- churn[train_id, ]
testd <- churn[-train_id, ]


# creating model weights to deal with imbalanced response data
model_weights <- ifelse(traind$Churn == "Yes", (1/table(traind$Churn)[1]) * 0.5,(1/table(traind$Churn)[2]) * 0.5)

# Control for Caret function (cross validation)
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, summaryFunction = twoClassSummary, classProbs = TRUE, seeds = )



#########################################################################################
# Logistic Regression (Boosted)
Boostlogit_model <- train(Churn ~ Contract + InternetService + tenure + PaymentMethod + 
                            PaperlessBilling + OnlineSecurity + TechSupport + 
                            streaming + PhoneService + MultipleLines + SeniorCitizen + 
                            Dependents + OnlineBackup,
                          data = traind,
                          method = "LogitBoost",
                          metric = "ROC",
                          wt = model_weights,
                          trControl = ctrl)

prob <- predict.train(Baggedlogit_model, newdata = testd,type = "prob")
pred <- ifelse(prob[,2] > .6, "Yes", "No")
table(pred, testd$Churn)
pred_f <- as.factor(pred)
pred_f <- relevel(pred_f, ref = "Yes")
confusionMatrix(pred_f, testd$Churn)

# Recieving Operating Characteristics for CART (Bagged)
pred = prediction(prob[,2], testd$Churn)
perf = performance(pred, "acc")
plot(perf)

roc = performance(pred,"tpr","fpr")
plot(roc, colorize = T, lwd = 2)
abline(a = 0, b = 1)


#########################################################################################
# Classification Tree (Bagged)
BaggedCART_model <- train(Churn ~ Contract + tenure + PaymentMethod2 + PaperlessBilling + 
                            OnlineSecurity + streaming + TechSupport + PhoneService + 
                            MonthlyCharges + OnlineBackup + SeniorCitizen + DeviceProtection + 
                            Dependents + MultipleLines,
                   data = traind,
                   method = "treebag",
                   metric = "ROC",
                   wt = model_weights,
                   verbose = FALSE,
                   trControl = ctrl)

prob <- predict.train(BaggedCART_model, newdata = testd, type = "prob")
pred <- ifelse(prob[,2] > .58, "Yes", "No")
table(pred, testd$Churn)
pred_f <- as.factor(pred)
confusionMatrix(pred_f, testd$Churn)

# Recieving Operating Characteristics for CART (Bagged)
pred = prediction(prob[,2], testd$Churn)
perf = performance(pred, "acc")
plot(perf)

roc = performance(pred,"tpr","fpr")
plot(roc, colorize = T, lwd = 2)
abline(a = 0, b = 1)



#########################################################################################
# Classification Tree 
TREE_model <- train(Churn ~ Contract + tenure + PaymentMethod2 + PaperlessBilling + 
                      OnlineSecurity + streaming + TechSupport + PhoneService + 
                      MonthlyCharges + OnlineBackup + SeniorCitizen + DeviceProtection + 
                      Dependents + MultipleLines,
                           data = traind,
                           method = "rpart",
                           metric = "ROC",
                           trControl = ctrl)


prob <- predict.train(TREE_model, newdata = testd, type = "prob")
pred <- ifelse(prob[,2] > .5, "Yes", "No")
table(pred, testd$Churn)
pred_f <- as.factor(pred)
confusionMatrix(pred_f, testd$Churn)

# Recieving Operating Characteristics for CART (Bagged)
pred = prediction(prob[,2], testd$Churn)
perf = performance(pred, "acc")
plot(perf)

roc = performance(pred,"tpr","fpr")
plot(roc, colorize = T, lwd = 2)
abline(a = 0, b = 1)

#########################################################################################
# Random Forrest
rf_model <- train(Churn ~ Contract + tenure + PaymentMethod2 + PaperlessBilling + 
                    OnlineSecurity + streaming + TechSupport + PhoneService + 
                    MonthlyCharges + OnlineBackup + SeniorCitizen + DeviceProtection + 
                    Dependents + MultipleLines,
                  data = traind,
                  method = "rf",
                  metric = "ROC",
                  verbose = FALSE,
                  trControl = ctrl)

prob <- predict.train(rf_model, newdata = testd, type = "prob")
pred <- ifelse(prob[,2] > .36, "Yes", "No")
table(pred, testd$Churn)
pred_f <- as.factor(pred)
confusionMatrix(pred_f, testd$Churn)

# Recieving Operating Characteristics for Random Forrest
pred = prediction(prob[,2], testd$Churn)
perf = performance(pred, "acc")
plot(perf)

roc = performance(pred,"tpr","fpr")
plot(roc, colorize = T, lwd = 2)
abline(a = 0, b = 1)
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)



#########################################################################################
# Weighted GBM
gbm_model_w <- train(Churn ~ Contract + tenure + PaymentMethod2 + PaperlessBilling + 
                       OnlineSecurity + streaming + TechSupport + PhoneService + 
                       MonthlyCharges + OnlineBackup + SeniorCitizen + DeviceProtection + 
                       Dependents + MultipleLines,
                   data = traind,
                   method = "gbm",
                   verbose = FALSE,
                   weights = model_weights,
                   metric = "ROC",
                   trControl = ctrl)


# Reciever Operating Characteristics for Gradient Boosted Model
prob <- predict.train(gbm_model_w, newdata = testd, type = "prob")
pred_f <- ifelse(prob[,2] > .229, "Yes", "No")
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
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)



#########################################################################################
# Linear Discriminant Analysis
LDA <- train(Churn ~ Contract + tenure + PaymentMethod2 + PaperlessBilling + 
                       OnlineSecurity + streaming + TechSupport + PhoneService + 
                       MonthlyCharges + OnlineBackup + SeniorCitizen + DeviceProtection + 
                       Dependents + MultipleLines,
                     data = traind,
                     method = "lda",
                     weights = model_weights,
                     metric = "ROC",
                     trControl = ctrl)

# Reciever Operating Characteristics for Gradient Boosted Model
prob <- predict.train(LDA, newdata = testd, type = "prob")
pred_f <- ifelse(prob[,2] > .55, "Yes", "No")
pred_f <- as.factor(pred_f)
confusionMatrix(pred_f, testd$Churn)

# Reciever Operating Characteristics for Weighted Gradient Boosted Model
pred = prediction(prob[,2], testd$Churn)
perf = performance(pred, "acc")
plot(perf)

roc = performance(pred,"tpr","fpr")
plot(roc, colorize = T, lwd = 2)
abline(a = 0, b = 1)
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)




#########################################################################################
#################              Predictive Modeling Part II              #################
#########################################################################################

#########################################################################################
temp <- churn[which(churn$Churn == "Yes"),]
temp1 <- churn[which(churn$Churn == "No"),]
train_us <- rbind(temp[sample(1:nrow(temp), nrow(temp)*.8, replace = FALSE),],
             temp1[sample(1:nrow(temp1), nrow(temp)*.8, replace = FALSE),])
test_us <- rbind(temp[-sample(1:nrow(temp), nrow(temp)*.8, replace = FALSE),],
             temp1[-sample(1:nrow(temp1), nrow(temp)*.8, replace = FALSE),])
test_us_c <- rbind(temp[-sample(1:nrow(temp), nrow(temp)*.8, replace = FALSE),],
                 temp1[-sample(1:nrow(temp1), 4063, replace = FALSE),])
nrow(test_us_c)
#########################################################################################
# Linear Discriminant Analysis
LDA_us <- train(Churn ~ Contract + tenure + PaymentMethod2 + PaperlessBilling + 
               OnlineSecurity + streaming + TechSupport + PhoneService + 
               MonthlyCharges + OnlineBackup + SeniorCitizen + DeviceProtection + 
               Dependents + MultipleLines,
             data = train_us,
             method = "lda",
             metric = "ROC",
             trControl = ctrl)

# Reciever Operating Characteristics for Gradient Boosted Model
prob <- predict.train(LDA_us, newdata = test_us, type = "prob")
pred_f <- ifelse(prob[,2] > .6, "Yes", "No")
table(pred_f, test_us$Churn)
pred_f <- as.factor(pred_f)
confusionMatrix(pred_f, test_us$Churn)

# Reciever Operating Characteristics for Weighted Gradient Boosted Model
pred = prediction(prob[,2], test_us$Churn)
perf = performance(pred, "acc")
plot(perf)

roc = performance(pred,"tpr","fpr")
plot(roc, colorize = T, lwd = 2)
abline(a = 0, b = 1)
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)

#########################################################################################
# GBM
gbm_model_us <- train(Churn ~ Contract + tenure + PaymentMethod2 + PaperlessBilling + 
                       OnlineSecurity + streaming + TechSupport + PhoneService + 
                       MonthlyCharges + OnlineBackup + SeniorCitizen + DeviceProtection + 
                       Dependents + MultipleLines,
                     data = train_us,
                     method = "gbm",
                     verbose = FALSE,
                     metric = "ROC",
                     trControl = ctrl)


# Reciever Operating Characteristics for Gradient Boosted Model
prob <- predict.train(gbm_model_us, newdata = test_us_c, type = "prob")
pred_f <- ifelse(prob[,2] > .6, "Yes", "No")
table(pred_f, test_us_c$Churn)

pred_f <- as.factor(pred_f)
confusionMatrix(pred_f, test_us_c$Churn)

# Reciever Operating Characteristics for Weighted Gradient Boosted Model
pred = prediction(prob[,2], test_us_c$Churn)
perf = performance(pred, "acc")
plot(perf)

roc = performance(pred,"tpr","fpr")
plot(roc, colorize = T, lwd = 2)
abline(a = 0, b = 1)
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)



#########################################################################################
# Random Forrest
rf_model_us <- train(Churn ~ Contract + tenure + PaymentMethod2 + PaperlessBilling + 
                    OnlineSecurity + streaming + TechSupport + PhoneService + 
                    MonthlyCharges + OnlineBackup + SeniorCitizen + DeviceProtection + 
                    Dependents + MultipleLines,
                  data = train_us,
                  method = "rf",
                  metric = "ROC",
                  verbose = FALSE,
                  trControl = ctrl)

prob <- predict.train(rf_model_us, newdata = test_us, type = "prob")
pred <- ifelse(prob[,2] > .5, "Yes", "No")
table(pred, test_us$Churn)
pred_f <- as.factor(pred)
confusionMatrix(pred_f, test_us$Churn)

# Recieving Operating Characteristics for Random Forrest
pred = prediction(prob[,2], test_us$Churn)
perf = performance(pred, "acc")
plot(perf)

roc = performance(pred,"tpr","fpr")
plot(roc, colorize = T, lwd = 2)
abline(a = 0, b = 1)
auc.tmp <- performance(pred,"auc")
auc <- as.numeric(auc.tmp@y.values)



