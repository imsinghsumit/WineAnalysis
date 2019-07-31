
getwd()
#importing the dataset.
dataset <- read.csv(file.choose(),head=TRUE)
maindata=dataset$Price
summary(dataset)
str(dataset)

#puting acolumn named quality into dataset.
p=length(maindata)

for (val in seq(1,p+1))
{
  if(maindata[val]>7.5)
  {
    maindata[val]="GOOD"
  } 
  else if(maindata[val]>6.5 & maindata[val]<=7.5)
  {
    maindata[val]="MEDIUM"
  }
  else
  {
    maindata[val]="POOR"
  }
  
}
maindata

dataset$Quality=maindata
dataset  
write.csv(dataset,file.choose())
#finding the correlation. 

cor(dataset$WinterRain,dataset$Price)
cor(dataset$FrancePop,dataset$Price)
cor(dataset$Age,dataset$Price)
cor(dataset$HarvestRain,dataset$Price)
#linear Regression (one variable)
model1 = lm(Price~AGST,data=dataset, family = "binomial") 
summary(model1) 
plot(model1,lwd=5)
#sum of square errors
SSE=sum(model1$residuals^2)
SSE
#linear Regression (two variables)

model2 = lm(Price~AGST+WinterRain,data=dataset, family = "binomial") 
summary(model2)
plot(model2,lwd=5)
#sum of square errors

SSE=sum(model2$residuals^2)
SSE

#linear Regression (three variables)

model3 = lm(Price~AGST+WinterRain+HarvestRain,data=dataset, family = "binomial") 
summary(model3) 
plot(model3,lwd=5)
#sum of square errors

SSE=sum(model3$residuals^2)
SSE
#linear Regression (four variables)

model4 = lm(Price~AGST+WinterRain+HarvestRain+Age,data=dataset, family = "binomial") 
summary(model4) 
plot(model4,lwd=5)
#sum of square errors

SSE=sum(model4$residuals^2)
SSE

#linear Regression (all variables)
model5 = lm(Price~AGST+WinterRain+HarvestRain+Age+year+FrancePop,data=dataset, family = "binomial") 
summary(model5) 
plot(model5,lwd=5)
#sum of square errors

SSE=sum(model5$residuals^2)
SSE

testdata1=read.csv(file.choose(),head=TRUE) 
#make test set prediction

predictdata1=predict(model4,newdata=testdata1)
predictdata1
#quality analysis

testdata1$Price=predictdata1
p=length(predictdata1)

for (val in seq(1,p+1))
{
  if(predictdata1[val]>7.5)
  {
    predictdata1[val]="GOOD"
  } 
  else if(predictdata1[val]>6.5 & predictdata1[val]<=7.5)
  {
    predictdata1[val]="MEDIUM"
  }
  else
  {
    predictdata1[val]="POOR"
  }
  
}
predictdata1
#adding new column named quality

testdata1$Quality=predictdata1
testdata1  

write.csv(testdata1,file.choose())
testdata=read.csv(file.choose(),head=TRUE)
predictdata=predict(model4,newdata=testdata)
predictdata 

#compute R-squared 

SSE=sum((testdata$Price - predictdata)^2)
SSE
SST=sum((testdata$Price -mean(dataset$Price))^2)
SST
error=(SSE/SST)
error
R=1-error
R 








