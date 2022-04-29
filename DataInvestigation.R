setwd("~/School/Spring 2022/Applied Stats 2/Project")
library(readr)
library(tidyverse)
library(gridExtra)
library(ggResidpanel)
#library(rayshader)
insurance <- read_csv("Data/insurance.csv", 
                      col_types = cols(sex = col_factor(levels = c("male", "female")), 
                      smoker = col_factor(levels = c("yes", "no")),
                      region = col_factor(levels = c("northeast","southeast", "southwest", "northwest")),
                      children = col_factor(levels = c('0','1','2','3','4','5'))))
View(insurance)
summary(insurance)

#Summary and Investigation Plots
chargesBox <- ggplot(data=insurance) + geom_boxplot(aes(charges))
chargesBox

chargesHist <- ggplot(data=insurance) + geom_histogram(aes(charges),binwidth = 1000,fill="Light blue",color="Black") + 
  labs(title="Insurance Charges",x="Charges",y="Frequency")
chargesHist

chargesSmokerBox <- ggplot(data=insurance) + geom_boxplot(aes(charges,smoker))
chargesSmokerBox

bmiHist <- ggplot(data=insurance) + geom_histogram(aes(bmi),binwidth = 0.5,fill="Blue",color="Black") +
  labs(title="BMI",x="BMI",y="Frequency")
bmiHist

chargesBmi <- ggplot(data=insurance) + geom_point(aes(bmi,charges)) + labs(title="Charges and BMI",x="BMI",y="Charges")
chargesBmi

chargesChildren <- ggplot(data=insurance) + geom_boxplot(aes(charges,col=children)) + labs(title="Charges and children",x="charges",y="Children")
chargesChildren

chargesRegion <- ggplot(data=insurance) + geom_boxplot(aes(charges,col=region)) + labs(title="Charges and region",x="charges",y="region")
chargesRegion

chargesAge <- ggplot(data=insurance) + geom_point(aes(age,charges)) + labs(title="Charges and Age",x="Age",y="Charges")
chargesAge

chargesSexBox <- ggplot(data=insurance) + geom_boxplot(aes(charges,col=sex)) + labs(title="Charges and Sex")
chargesSexBox

grid.arrange(chargesBmi,chargesChildren,chargesRegion,chargesAge,chargesSmokerBox,chargesSexBox)

#Smoker Plots
nonsmokers <- subset(insurance,smoker=='no')
smokers <- subset(insurance,smoker == 'yes')
nonSmokerFit <- lm(charges~age,data=nonsmokers)
smokerFit <- lm(charges~age,data=smokers)
chargesAgeSmoker <- ggplot(data=insurance) + geom_point(aes(age,charges,col=smoker)) + labs(title="Charges and Age",x="Age",y="Charges") +
  geom_abline(slope=coef(nonSmokerFit)[["age"]] ,intercept = coef(nonSmokerFit)[["(Intercept)"]]) +
  geom_abline(slope=coef(smokerFit)[["age"]] ,intercept = coef(smokerFit)[["(Intercept)"]])  
chargesAgeSmoker
summary(nonSmokerFit)
summary(smokerFit)

logchargesAgeSmoker <- ggplot(data=insurance) + geom_point(aes(age,log(charges),col=smoker)) + labs(title="Log Charges and Age",x="Age",y="Charges")
logchargesAgeSmoker

chargesAgeSmoker <- ggplot(data=insurance) + geom_point(aes(age,charges,col=smoker)) + labs(title="Charges and Age",x="Age",y="Charges")
chargesAgeSmoker

#NonSmoker plot
smokerNoData <- subset(insurance,smoker == 'no')
chargesAgeNonSmokerBmi <- ggplot(data=smokerNoData) + geom_point(aes(age,charges,col=bmi)) + labs(title="Charges and Age and NonSmoker and BMI",x="Age",y="Charges")
chargesAgeNonSmokerBmi

chargesAgeNonSmokerRegion <- ggplot(data=smokerNoData) + geom_point(aes(age,charges,col=region)) + labs(title="Charges and Age and NonSmoker and Region",x="Age",y="Charges")
chargesAgeNonSmokerRegion

chargesAgeNonSmokerChildren <- ggplot(data=smokerNoData) + geom_point(aes(age,charges,col=children)) + labs(title="Charges and Age and NonSmoker and Children",x="Age",y="Charges")
chargesAgeNonSmokerChildren

charegesAgeNonSmokerSex <- ggplot(data=smokerNoData) + geom_point(aes(age,charges,col=sex)) + labs(title="Charges and Age and NonSmoker and Sex",x="Age",y="Charges")
charegesAgeNonSmokerSex

grid.arrange(charegesAgeNonSmokerSex,chargesAgeNonSmokerChildren,chargesAgeNonSmokerRegion,chargesAgeNonSmokerBmi,ncol=2,nrow=2)

#Smoker plots
smokerYesData <- subset(insurance,smoker == 'yes')
chargesAgeSmokerBmi <- ggplot(data=smokerYesData) + geom_point(aes(age,charges,col=bmi)) + labs(title="Charges and Age and Smoker and BMI",x="Age",y="Charges")
chargesAgeSmokerBmi

chargesAgeSmokerRegion <- ggplot(data=smokerYesData) + geom_point(aes(age,charges,col=region)) + labs(title="Charges and Age and Smoker and Region",x="Age",y="Charges")
chargesAgeSmokerRegion

chargesAgeSmokerChildren <- ggplot(data=smokerYesData) + geom_point(aes(age,charges,col=children)) + labs(title="Charges and Age and Smoker and Children",x="Age",y="Charges")
chargesAgeSmokerChildren

chargesAgeSmokerSex <- ggplot(data=smokerYesData) + geom_point(aes(age,charges,col=sex)) + labs(title="Charges and Age and Smoker and Sex",x="Age",y="Charges")
chargesAgeSmokerSex

grid.arrange(chargesAgeSmokerSex,chargesAgeSmokerChildren,chargesAgeSmokerRegion,chargesAgeSmokerBmi,ncol=2,nrow=2)

#3D plot
#plot_gg(chargesAgeSmokerBmi, width=3.5, multicore = TRUE, windowsize = c(1400,866), sunangle=225,
#        zoom = 0.60, phi = 30, theta = 45)

#Model Creation
modelSmoker <- lm(charges~age+bmi,data=smokerYesData)
summary(modelSmoker)

modelNoSmoker <- lm(charges~age+as.factor(children)+as.factor(sex),data=smokerNoData)
summary(modelNoSmoker)

modelFull1 <- lm(charges~age+smoker+bmi+as.factor(children),data=insurance)
summary(modelFull1)

modelFull2 <- lm(charges~smoker*(age+bmi) + age+as.factor(children)+as.factor(sex),data=insurance)
summary(modelFull2)

modelFull3 <- lm(charges~smoker*(age+bmi) + age+as.numeric(children)+as.factor(sex),data=insurance)
summary(modelFull3)

modelFull4 <- lm(charges~smoker*(age+I(age^2)+bmi) + age+I(age^2)+as.factor(children)+as.factor(sex),data=insurance)
summary(modelFull4)

modelFull5 <- lm(charges~(age+smoker+bmi+as.factor(children)+sex+region)^2,data=insurance)
summary(modelFull5)

modelFull6 <- lm(charges~smoker*(age+I(age^2)+bmi) + age+I(age^2)+as.factor(children)+as.factor(sex),data=insurance)
summary(modelFull6)

#Residual Plots
resid_panel(modelFull1)
resid_panel(modelFull2)
resid_panel(modelFull3)
resid_panel(modelFull4)

#Comparing Models
AIC(modelFull1,modelFull2,modelFull3,modelFull4)

runSexTTest <- function(){
  femaleData <- subset(insurance,sex=='female')
  maleData <- subset(insurance,sex=='male')
  logFemaleCharges <- log(femaleData$charges)
  logMaleCharges <- log(maleData$charges)
  
  mean(femaleData$charges)
  mean(maleData$charges)
  
  shapiro.test(femaleData$charges)
  shapiro.test(maleData$charges)
  
  shapiro.test(logFemaleCharges)
  shapiro.test(logMaleCharges)
  
  qqnorm(femaleData$charges)
  qqnorm(maleData$charges)
  
  qqnorm(logFemaleCharges)
  qqnorm(logMaleCharges)
  
  var.test(logFemaleCharges,logMaleCharges,alternative = "two.sided")
  
  t.test(logFemaleCharges,logMaleCharges,alternative = "two.sided",var.equal = FALSE)
  #Means are not different
}
runSexTTest()

runSmokerTTest <- function(){
  smokerYesData <- subset(insurance,smoker=='yes')
  smokerNoData <- subset(insurance,smoker=='no')
  #logFemaleCharges <- log(femaleData$charges)
  #logMaleCharges <- log(maleData$charges)
  
  mean(smokerYesData$charges)
  mean(smokerNoData$charges)
  
  shapiro.test(smokerYesData$charges)
  shapiro.test(smokerNoData$charges)
  
  qqnorm(smokerYesData$charges)
  qqnorm(smokerNoData$charges)
  
  #qqnorm(logFemaleCharges)
  #qqnorm(logMaleCharges)
  
  #var.test(logFemaleCharges,logMaleCharges,alternative = "two.sided")
  
  t.test(smokerYesData$charges,smokerNoData$charges,alternative = "two.sided",var.equal = FALSE)
  #Means are not different
}
runSmokerTTest()

#Anova for children
results <- aov(charges~children,data=insurance)
summary(results)
TukeyHSD(results)

#Anova for region
results <- aov(charges~region,data=insurance)
summary(results)
TukeyHSD(results)



#Model Testing
library(dplyr)
set.seed(101)
train<-sample_frac(insurance, 0.7)
sid<-as.numeric(rownames(train)) # because rownames() returns character
test<-insurance[-sid,]

trainModel <- lm(charges~smoker*(age+I(age^2)+bmi) + age+I(age^2)+as.factor(children)+as.factor(sex),data=train)
predictedVsActual <- data.frame(predicted = predict(trainModel,test),actual=test$charges)

ggplot(predictedVsActual, aes(x=predicted,actual)) +
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values')

#Sum of squared errors
sse <- sum((predictedVsActual$actual-predictedVsActual$predicted)^2)

dad <- data.frame(smoker="yes",age=46,bmi=24.1,children=5,sex="male",region="NA") #Non-smoker,age,bmi,children,sex probably spend 5000

predict(modelFull3,dad)
