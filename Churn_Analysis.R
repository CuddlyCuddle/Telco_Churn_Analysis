# Churn Analysis:
# 
# 
# 
# 
# 
# 

# Packages
library(ROSE)

# Readigng in Data
data  <- read.csv("C:/Users/jeanp/OneDrive/Documents/GitHub/Telco_Churn_Analysis/WA_Fn-UseC_-Telco-Customer-Churn.csv")
summary(data)
str(data)

### Recoding Data to reduce number of redundent factors within each Variable

# For this sitation cols 8, 10:15 have three factors Yes, no, and "No phone service" or "No Internet Service", respectively.
data[c(8,10:15)] <- as.factor(ifelse(data[c(8,10:15)] == "Yes", "Yes" , "No"))

# Col 3 was originally an indicator, however, to match the respective data it was returned to a str.
data[,3] <- as.factor(ifelse(data[,3] == 1, "Yes" , "No"))

# Ensuring that the base group for churn is "No" to determine what methods help retain customers
data$Churn <- relevel(data$Churn, ref = "Yes")


### Data Exploration:
for(i in c(2:20)){
  temp0 <- names(data)
  if(is.factor(data[,i]) == TRUE){
    plot(data[,i], main = temp0[i])
    print(table(data[,i]))
  }
}

# Creating a Bar Graph for the binomial factor "Churn"
plot(data$Churn)
table(data$Churn)


# Based on the Bar Chart given above, the data is not balanced. Thus, it will need to be balanced:

test <- data[, -20]
ovun.sample(formula, data, method="both", N, p=0.5, 
            subset=options("subset")$subset,
            na.action=options("na.action")$na.action, seed)

# creating and testing a logistic model
temp0 <- glm(Churn ~ MonthlyCharges, data = test, family = binomial(link = "logit"))
summary(temp0)
prob <- predict.glm(temp0, type = "response")
pred <- ifelse(prob > .8, "Yes", "No")
table(pred, data$Churn)
