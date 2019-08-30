
# Packages installed
install.packages("ggpubr")
install.packages("rmarkdown")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("ggthemes")
install.packages("caret")
install.packages("randomForest")
install.packages("party")
install.packages("stringi")
install.packages("Hmisc")
install.packages("pastecs")

# Library List
library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)
library(Hmisc)
library(pastecs)
library(psych)
library(dplyr)

# Load the Telco Churn Data
Telco_Churn_2 <- read.csv("~/Desktop/r_intro/Telco_Churn.csv")

############################### EDA #######################################################

## Display basic distribution of variables "income"
str(Telco_Churn_2)
summary(Telco_Churn_2)
class(Telco_Churn_2)
head(Telco_Churn_2)
View(Telco_Churn_2)


################################## Data Cleaning #########################################
## Find missing values
sapply(Telco_Churn_2, function(x) sum(is.na(x))) #TotalCharges Variable has 11 missing values

## Remove the 11 missing values in the TotalCharges Variable
Telco_Churn_2 <- Telco_Churn_2[complete.cases(Telco_Churn_2), ]

## "No "nternet service"" to "No"
###Columns changed: OnlineSecurity, OnlineBackup, DeviceProtection, TechSupport, streamingTV, streamingMovies.
recode_Columns <- c(10:15)
for(i in 1:ncol(Telco_Churn_2[,recode_Columns])) {
  Telco_Churn_2[,recode_Columns][,i] <- as.factor(mapvalues
                                                  (Telco_Churn_2[,recode_Columns][,i], from =c("No internet service"),to=c("No")))
}

##Change "No phone service" to "No" for column "MultipleLines"
Telco_Churn_2$MultipleLines <- as.factor(mapvalues(Telco_Churn_2$MultipleLines, 
                                                   from=c("No phone service"),
                                                   to=c("No")))


##Verify that the Data was changed
summary(Telco_Churn_2)
str(Telco_Churn_2)
hist(Telco_Churn_2$tenure)
boxplot(Telco_Churn_2$tenure, horizontal = TRUE)

plot(density(Telco_Churn_2$tenure), main = "Tenure of Customers")

#-------------NUmeric Variables---------------

# -----Senior Citizen-----
# Senior Citizen: Change Senior Citizen identifier from '0 or 1' to 'No or Yes'
is.numeric(Telco_Churn_2$SeniorCitizen)
is.factor(Telco_Churn_2$SeniorCitizen)
Telco_Churn_2$SeniorCitizen <- as.factor(mapvalues(Telco_Churn_2$SeniorCitizen,
                                                   from=c("0","1"),
                                                   to=c("No", "Yes")))

# -----Tenure-----
##Discover the minimum and Maximum of the variable "Tenure"
min(Telco_Churn_2$tenure); max(Telco_Churn_2$tenure)

## Group tenure into 5 groups
group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}
Telco_Churn_2$tenure_group <- sapply(Telco_Churn_2$tenure,group_tenure)
Telco_Churn_2$tenure_group <- as.factor(Telco_Churn_2$tenure_group)

plot(Telco_Churn_2$tenure_group)

# -----Correlation-----
# Discover Correlation between Numneric Variables
numeric_variables <- sapply(Telco_Churn_2, is.numeric)
matrix <- cor(Telco_Churn_2[,numeric_variables])
corrplot(matrix, main="\n\nCorrelation for Numerical Variables", method="number")

# Remove Total Charges due to strong correlation between variables
Telco_Churn_2$TotalCharges <- NULL

# Remove columns not needed for analysis
Telco_Churn_2$customerID <- NULL
Telco_Churn_2$tenure <- NULL

# View Data to ensure that all changes have been successful
View(Telco_Churn_2)
str(Telco_Churn_2)
summary(Telco_Churn_2)



############################### Unvariate Alaysis #########################################
str(Telco_Churn_2)

## gender
str(Telco_Churn_2$gender)
summary(Telco_Churn_2$gender)
plot(Telco_Churn_2$gender)

## Churn
str(Telco_Churn_2$Churn)
summary(Telco_Churn_2$Churn)


## Contract
str(Telco_Churn_2$Contract)
summary(Telco_Churn_2$Contract)




















#----------------------------------------Univariate Statistics-------------------------------------------------
str(Telco_Churn_2)
### Tables
##Categorical Variables
# Gender
head(Telco_Churn_2$gender)
table(Telco_Churn_2$gender)
table(Telco_Churn_2$gender)/length(Telco_Churn_2$gender)
pie(table(Telco_Churn_2$gender))

barplot(table(Telco_Churn_2$gender))
barplot(table(Telco_Churn_2$gender)/length(Telco_Churn_2$gender))

###Numeric Variables
# Monthly Charges
head(Telco_Churn_2$MonthlyCharges)
mean(Telco_Churn_2$MonthlyCharges)
median(Telco_Churn_2$MonthlyCharges)
var(Telco_Churn_2$MonthlyCharges)
sd(Telco_Churn_2$MonthlyCharges)
range(Telco_Churn_2$MonthlyCharges)
quantile(Telco_Churn_2$MonthlyCharges)
hist(Telco_Churn_2$MonthlyCharges)
boxplot(Telco_Churn_2$MonthlyCharges)
boxplot(Telco_Churn_2$MonthlyCharges, horizontal = TRUE, main = "Boxplot of Monthly Charges", xlab = "Monthly Charges")
?boxplot

#-----------------------------------------bivariate Statistics-----------------------------------------------

### Numerical to Categorical Comparison
# Side-by-Side Box Plots 
plot(MonthlyCharges ~ gender, data = Telco_Churn_2)
tapply(Telco_Churn_2$MonthlyCharges, Telco_Churn_2$gender, mean)
tapply(Telco_Churn_2$MonthlyCharges, Telco_Churn_2$gender, sd)
aggregate(MonthlyCharges ~ gender, data = Telco_Churn_2, FUN =  mean)

### Numerical to Numerical Comparison
head(Telco_Churn_2$TotalCharges)
mean(Telco_Churn_2$TotalCharges, na.rm = TRUE)
median(Telco_Churn_2$TotalCharges, na.rm = TRUE)
sd(Telco_Churn_2$TotalCharges, na.rm = TRUE)
hist(Telco_Churn_2$TotalCharges)
boxplot(Telco_Churn_2$TotalCharges, horizontal = TRUE)
plot(TotalCharges ~ MonthlyCharges, data = Telco_Churn_2)
plot(TotalCharges ~ tenure, data = Telco_Churn_2)
plot(tenure ~ MonthlyCharges, data = Telco_Churn_2)

# Three Variables
plot(TotalCharges ~ tenure, data = Telco_Churn_2, pch = as.integer(Churn), col = as.integer(Churn)+2)
legend("topleft", legend = c("No", "Yes"), pch=c(1,2), col=c(3,4 ))
plot(MonthlyCharges ~ tenure, data = Telco_Churn_2, pch = as.integer(Churn), col = as.integer(Churn)+2)




# ---------------------------------------------Multivariate---------------------------------------------------------------
#Pairs is a matrix of scatter plots. handles numeric, factors and Characters best use numeric
str(Telco_Churn_2)
pairs(Telco_Churn_2[,c(6, 19, 20)])
cor()
aggregate(MonthlyCharges ~ gender, data = Telco_Churn_2, FUN =  mean)
aggregate(MonthlyCharges ~ gender + SeniorCitizen + Churn, data = Telco_Churn_2, FUN =  mean)
aggregate(MonthlyCharges ~ gender + SeniorCitizen + Churn, data = Telco_Churn_2, FUN =  sd)
aggregate(MonthlyCharges ~ gender + SeniorCitizen + Churn, data = Telco_Churn_2, FUN =  length)
aggregate(MonthlyCharges ~ gender + SeniorCitizen + Churn + Contract, data = Telco_Churn_2, FUN =  length)
aggregate(MonthlyCharges ~ gender + SeniorCitizen + Churn + Contract + tenure_group, data = Telco_Churn_2, FUN =  length)

table(Telco_Churn_2$gender, Telco_Churn_2$Churn)/length(Telco_Churn_2$gender)
table(Telco_Churn_2$tenure_group, Telco_Churn_2$Churn)/length(Telco_Churn_2$gender)
round(table(Telco_Churn_2$tenure_group, Telco_Churn_2$Churn)/length(Telco_Churn_2$gender), 2)

# Relationship between Categorical and numeric Variables
## Bow plots
boxplot(Telco_Churn_2$MonthlyCharges, horizontal = TRUE, main = "Monthly Charges Boxplot")
plot(tenure_group ~ Churn, data=Telco_Churn_2)
plot(MonthlyCharges ~ Churn, data=Telco_Churn_2)
plot(MonthlyCharges ~ tenure_group + Churn, data=Telco_Churn_2)




# Bar plot Theme
windowsFonts()
theme_new <- theme_bw() +
  theme(plot.background = element_rect(size = 1, color = "white", fill = "white"),
        text=element_text(size = 14, family = "serif", color = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = ("gray")))

# Bar PLots 1, Variables: Gender, Senior Citizen, Partner, Dependants
"Shows percentages of each response per a variable. Trying to Determine distribution of variables"
p1 <- ggplot(Telco_Churn_2, aes(x=gender)) + ggtitle("Gender") + xlab("") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_new
p2 <- ggplot(Telco_Churn_2, aes(x=SeniorCitizen)) + ggtitle("Senior Citizen") + xlab("") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() +theme_new
p3 <- ggplot(Telco_Churn_2, aes(x=Partner)) + ggtitle("Partner") + xlab("") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_new
p4 <- ggplot(Telco_Churn_2, aes(x=Dependents)) + ggtitle("Dependents") + xlab("") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_new
grid.arrange(p1, p2, p3, p4, ncol=2)

# Bar Plot 2, Variables: PHone Serivce, Mulptiple Lines, Internet Servic, Online Security.
p5 <- ggplot(Telco_Churn_2, aes(x=PhoneService)) + ggtitle("Phone Service") + xlab("") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_new
p6 <- ggplot(Telco_Churn_2, aes(x=MultipleLines)) + ggtitle("Multiple Lines") + xlab("") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_new
p7 <- ggplot(Telco_Churn_2, aes(x=InternetService)) + ggtitle("Internet Service") + xlab("") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_new
p8 <- ggplot(Telco_Churn_2, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_new
grid.arrange(p5, p6, p7, p8, ncol=2)

# Bar PLots 3, Variables: Online Backup, Device Protection, Tech Support, Streaming TV
p9 <- ggplot(Telco_Churn_2, aes(x=OnlineBackup)) + ggtitle("Online Backup") + xlab("") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_new
p10 <- ggplot(Telco_Churn_2, aes(x=DeviceProtection)) + ggtitle("Device Protection") + xlab("") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_new
p11 <- ggplot(Telco_Churn_2, aes(x=TechSupport)) + ggtitle("Tech Support") + xlab("") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_new
p12 <- ggplot(Telco_Churn_2, aes(x=StreamingTV)) + ggtitle("Streaming TV") + xlab("") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_new
grid.arrange(p9, p10, p11, p12, ncol=2)

#Bar Plots 4, Variables: Streaming Moveis, Contact, Paperless Billing, Payment Method, Tenure Group
p13 <- ggplot(Telco_Churn_2, aes(x=StreamingMovies)) + ggtitle("Streaming Movies") + xlab("") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_new
p14 <- ggplot(Telco_Churn_2, aes(x=Contract)) + ggtitle("Contract") + xlab("") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_new
p15 <- ggplot(Telco_Churn_2, aes(x=PaperlessBilling)) + ggtitle("Paperless Billing") + xlab("") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_new
grid.arrange(p13, p14, p15, ncol=2)

#Bar Plots 5, Variables: Payment Method
p16 <- ggplot(Telco_Churn_2, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_new
grid.arrange(p16, ncol=1)

#Bar Plots 6, Variables: Tenure Group
p17 <- ggplot(Telco_Churn_2, aes(x=tenure_group)) + ggtitle("Tenure Group") + xlab("") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_new
grid.arrange(p17, ncol=1)

#Bar Plots 6, Variables: Tenure Group
p18 <- ggplot(Telco_Churn_2, aes(x=Churn)) + ggtitle("Churn") + xlab("") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_new
grid.arrange(p18, ncol=1)
hist(Telco_Churn_2$MonthlyCharges)





# -------------------------------LOGISITIC REGRESSION----------------------------------------------
nrow(Telco_Churn_2)
# 1st Split data into training and testing sets:

train <- createDataPartition(Telco_Churn_2$Churn,p=0.7,list=FALSE)
set.seed(2017)
training <- Telco_Churn_2[train,]
testing <- Telco_Churn_2[-train,]
# Check Spliting Results
dim(training); dim(testing)

# Fitting the LOg Regresssion Model
mod_fit <- glm(Churn ~ .,family=binomial(link="logit"),data=training)
mod_fit
summary(mod_fit)
predict(mod_fit, newdata = testing)
predict(mod_fit, newdata=testing, type="response")
"Top FOUR most-relevant variables are: Contract, Payment Method, 
tenure_group, and PaperlessBilling"

# ANOVA of model_log
anova(mod_fit, test="Chisq")

# Logistic Regression Accuracy or the predictive ability of the model_log
testing$Churn <- as.character(testing$Churn)
testing$Churn[testing$Churn=="No"] <- "0"
testing$Churn[testing$Churn=="Yes"] <- "1"
fitted.results <- predict(model_log,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))


# Log Reg Confusion Matrix
print("Confusion Matrix for Logistic Regression"); table(testing$Churn, fitted.results > 0.5)








plot(Telco_Churn$tenure_group, Telco_Churn$Churn)








plot(Telco_Churn$MonthlyCharges_group, Telco_Churn$Churn)




install.packages("klaR")


# load libraries
library(mlbench)
library(caret)
library(klaR)

# rename dataset to keep code below generic
dataset_test <- Telco_Churn_2

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7

metric <- "Accuracy"
preProcess=c("center", "scale")


# Linear Discriminant Analysis
set.seed(seed)
fit.lda <- train(Churn~., data=dataset_test, method="lda", metric=metric, preProc=c("center", "scale"), trControl=control)
# Logistic Regression
set.seed(seed)
fit.glm <- train(Churn~., data=dataset_test, method="glm", metric=metric, trControl=control)
# GLMNET
set.seed(seed)
fit.glmnet <- train(Churn~., data=dataset_test, method="glmnet", metric=metric, preProc=c("center", "scale"), trControl=control)
# SVM Radial
set.seed(seed)
fit.svmRadial <- train(Churn~., data=dataset_test, method="svmRadial", metric=metric, preProc=c("center", "scale"), trControl=control, fit=FALSE)
# kNN
set.seed(seed)
fit.knn <- train(Churn~., data=dataset_test, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)
# Naive Bayes
set.seed(seed)
fit.nb <- train(Churn~., data=dataset_test, method="nb", metric=metric, trControl=control)
# CART
set.seed(seed)
fit.cart <- train(Churn~., data=dataset_test, method="rpart", metric=metric, trControl=control)
# C5.0
set.seed(seed)
fit.c50 <- train(Churn~., data=dataset_test, method="C5.0", metric=metric, trControl=control)
# Bagged CART
set.seed(seed)
fit.treebag <- train(Churn~., data=dataset_test, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(Churn~., data=dataset_test, method="rf", metric=metric, trControl=control)
# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed)
fit.gbm <- train(Churn~., data=dataset_test, method="gbm", metric=metric, trControl=control, verbose=FALSE)

# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed)
fit.gbm <- train(Churn~., data=dataset_test, method="neural net", metric=metric, trControl=control, verbose=FALSE)

results <- resamples(list(lda=fit.lda, logistic=fit.glm, glmnet=fit.glmnet,
                          svm=fit.svmRadial, knn=fit.knn, nb=fit.nb, cart=fit.cart, c50=fit.c50,
                          bagging=fit.treebag, rf=fit.rf, gbm=fit.gbm))
# Table comparison
summary(results)




# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)
