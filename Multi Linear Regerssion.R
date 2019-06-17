#install ISLR
install.packages("ISLR")
library("ISLR", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")

#Default file (flower dataset) as DF
DF=ISLR::Auto
summary(DF)
head(DF)
str(DF)

#Continuous Variables:
## MPG,displacement,horsepower,weight,acceleration

#Exploritory Analysis
AutoDataContinuous=data.frame(DF$mpg,DF$displacement,DF$horsepower,DF$weight,DF$acceleration)
head(AutoDataContinuous)
##Cor Matrix
plot(AutoDataContinuous)
## Same as...
pairs(~mpg+displacement+horsepower+weight+acceleration,data=DF)
## Cor in numbers
cor(AutoDataContinuous)
## displacement and weight are most correlated @ 0.9329944

# Multi Linear Regression to Predict MPG
MLRAutoData=lm(mpg~displacement+horsepower+weight+acceleration,data=DF)
MLRAutoData
summary(MLRAutoData)
## weight and hoursepower are good predictors

# Plot True mpg Against Estimated mpg
PredMPG=predict(MLRAutoData,DF)
MPGCompare=data.frame(PredMPG,DF$mpg)
plot(PredMPG,DF$mpg) 
abline(lm(DF$mpg~PredMPG))
plot(MLRAutoData)