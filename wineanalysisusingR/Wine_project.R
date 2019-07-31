wine <-read.csv(file.choose(),header = T,sep = ",")
summary(wine)
print(wine)
wine_test <-read.csv(file.choose(),header = T,sep = ",")
summary(wine_test)
# Read in data
wine = read.csv("wine.csv")


str(wine)
summary(wine)

# Linear Regression (one variable)
model1 = lm(Price ~ AGST,data=wine)
summary(model1)
plot(wine$AGST,wine$Price, col="blue", cex=1,pch=18)
abline(model1,col("red"))

# Sum of Squared Errors
model1$residuals
SSE = sum(model1$residuals^2)
SSE

# Linear Regression (two variables)
model2 = lm(Price ~ AGST + HarvestRain, data=wine)
summary(model2)

# Sum of Squared Errors
SSE = sum(model2$residuals^2)
SSE
# Linear Regression (all variables)
model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine)
summary(model3)
# Sum of Squared Errors
SSE = sum(model3$residuals^2)
SSE
# Remove FrancePop
model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model4)
# Sum of Squared Errors
SSE =sum(model4$residuals^2)
SSE
# Correlations
cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
cor(wine)
# Remove Age and FrancePop
model5 = lm(Price ~ AGST + HarvestRain + WinterRain, data=wine)
summary(model5)
# Sum of Squared Errors
SSE =sum(model5$residuals^2)
SSE
# Read in test set
wine_test = read.csv("wine_test.csv")
str(wine_test)

# Linear Regression (one variable)
model11 = lm(Price ~ AGST,data=wine_test)
summary(model11)
plot(wine_test$AGST,wine_test$Price, col="blue", cex=1,pch=18)
abline(model11,col("red"))

# Linear Regression (two variables)
model22 = lm(Price ~ AGST + HarvestRain, data=wine_test)
summary(model22)

# Linear Regression (all variables)
model33 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine_test)
summary(model33)

# Remove FrancePop
model44 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine_test)
summary(model4)

# Correlations
cor(wine_test$WinterRain, wine_test$Price)
cor(wine_test$Age, wine_test$FrancePop)
cor(wine_test)

# Remove Age and FrancePop
model55 = lm(Price ~ AGST + HarvestRain + WinterRain, data=wine_test)
summary(model55)
# Make test set predictions
predictTest = predict(model4, newdata=wine_test)
predictTest
# Compute R-squared
SSE = sum((wine_test$Price - predictTest)^2)
SST = sum((wine_test$Price - mean(wine$Price))^2)
1 - SSE/SST


