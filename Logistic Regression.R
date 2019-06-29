# Install ISLR
library(ISLR)

# Default Dataset as df
df <- Default
head(df)
dim(df)
summary(df)

# Split Data into Test and Train
set.seed(100) #set.seed to random a number
dfShuffle= df[sample(nrow(df)),]
split = sample(x = nrow(dfShuffle),size = 0.8*nrow(dfShuffle)) # 20% Test, 80% Train
dfTrain = dfShuffle[split,]
dfTest = dfShuffle[-split,]

# Exploratory Analysis
head(dfTrain)
str(dfTrain)
summary(dfTrain)
balanceDensity=density(dfTrain$balance)
plot(balanceDensity) # better predictor, more of a bell curve
incomeDensity=density(dfTrain$income)
plot(incomeDensity)

# Fit Logistic Regression Model on Useful Predictors
glmDefaultBalance=glm(default~balance,data=dfTrain,family="binomial")
summary(glmDefaultBalance)

# Predict Response for the `Defaults`
## Convert Predictions to classification $>=0.5$ as "yes" and $< 0.5$ as "no".
library(Matrix)
library(foreach)
library(glmnet)
dfTrain$ProbDefault=predict(glmDefaultBalance,type = "response")
dfTrain$Response=ifelse(dfTrain$ProbDefault>=0.5,1,0)
dfTrain$defaultVal=ifelse(dfTrain$default=='Yes',1,0)
head(dfTrain)
summary(dfTrain)

# Confusion Matrix
library(lattice)
library(ggplot2)
library(caret)
dfTest$ProbDefault= predict(glmDefaultBalance,newdata =dfTest, type = "response")
dfTest$Response = ifelse(dfTest$ProbDefault>=0.5,1,0)
dfTest$defaultVal=ifelse(dfTest$default=='Yes',1,0)
levels(dfTest$Response)=factor(c("0.5","1","0"))
levels(dfTest$Response)=factor(c("0.5","1","0"))
levels(dfTest$defaultVal)=factor(c("0.5","1","0"))
levels(dfTest$defaultVal)=factor(c("0.5","1","0"))
confusionMatrix(table(dfTrain$Response, dfTrain$defaultVal))
(180 + 32)/(7714+32+180+74) # Training Error/Misclassification Rate = 0.0265
## Accuracy : 0.9735 
## Sensitivity : 0.9959 the % of true defaulters identified by the test,
## Specificity : 0.2913 the % of non-defaulters correctly identified
confusionMatrix(table(dfTest$Response, dfTest$defaultVal))
(55 + 3)/(1918+3+55+24) #Test Error/Misclassification Rate = 0.029

# AUC
library(ROCR)
ROCRpred=prediction(as.numeric(dfTrain$Response),as.numeric(dfTrain$defaultVal)) 
## When leveled, the values had turned to string, so converted them to num
ROCRperf=performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize=TRUE,print.cutoffs.at=seq(0,1,0.2),text.adj=c(-0.3,2),xlab="1 - Specificity",ylab="Sensitivity") #this is just adding color
## AUC plot looks off....
