#Group Assignment 3: Pilgrim Bank Case


##Preliminary Setup

#Load necessary packages and read the data
library(data.table)
library(dplyr)
library("pROC")

pilgrim.data <- fread(file.choose()) 

str(pilgrim.data) #View to ensure correct upload


##Question 1
sorted = sort(pilgrim.data$Profit99, decreasing = TRUE)
cum.Profit99 = (cumsum(sorted)/sum(sorted))*100
cum.Profit99
customerpct = (1:31634/31632)*100

plot(x=customerpct,y=cum.Profit99, xlab = 'Cumulative Customers', ylab = 'Cumulative Profit', main = 'Profitability Skew')

##Question 2
#Part a)

#Combine the cumulative percentages into a dataframe for analysis ease
cust.profitability = data.frame(customerpct, cum.Profit99, sorted)
cust.profitability

#Find the minimum percentage above 100
minpctabv100 = min(cust.profitability$customerpct[cust.profitability$cum.Profit99 >= 100]) #it's 21.53515%

#visualization of what has been found (For the presentation)
plot(x=customerpct,y=cum.Profit99, xlab = 'Cumulative Customers', ylab = 'Cumulative Profit', main = 'Profitability Skew')
abline(h=100, col = "blue")
abline(v=minpctabv100, col = "blue")

#Part b)

#The highest cumulative profit - found by all sorted profitability with positive values
maxcumprofit = max(cust.profitability$cum.Profit99[cust.profitability$sorted > 0]) 
maxcumprofit #It's 121.5755%

#The pct of customers at the highest cumulative profit
profitablecusts = max(cust.profitability$customerpct[cust.profitability$cum.Profit99 == max(cust.profitability$cum.Profit99[cust.profitability$sorted > 0])]) 
profitablecusts #It's 53.91376%

#Make a plot to visualize it
plot(x=customerpct,y=cum.Profit99, xlab = 'Cumulative Customers', ylab = 'Cumulative Profit', main = 'Profitability Skew')
abline(h=maxcumprofit, col = "red")
abline(v=profitablecusts, col = "red")


##Part c) is conceptual - not included in R

##Question 3

#Create the intercept-only model
int.only = lm(pilgrim.data$Profit99 ~ 1)
summary(int.only) #The estimate is 111.503

#Find the confidence intervals
confint(int.only)

##Question 4
#make a copy of the data to do this analysis so that we don't mess with the raw data
mydata<-data.frame(pilgrim.data)

#Create the dummy variables
AgeMean<-ifelse(is.na(mydata$Age99),mean(mydata$Age99,na.rm=T),mydata$Age99)
AgeMean
IncMean<-ifelse(is.na(mydata$Inc99),mean(mydata$Inc99,na.rm=T),mydata$Inc99)
IncMean
AgeZero<-ifelse(is.na(mydata$Age99),'0',mydata$Age99)
AgeZero
IncZero<-ifelse(is.na(mydata$Inc99),'0',mydata$Inc99)
IncZero
AgeMiss <- ifelse(is.na(mydata$Age99),'1','0')
AgeMiss
IncMiss <- ifelse(is.na(mydata$Inc99),'1','0')
IncMiss
mydata1 <- cbind(mydata,AgeMean,IncMean,AgeZero,IncZero,AgeMiss,IncMiss)
mydata1
Age99new <- ifelse(is.na(mydata$Age99),'0',mydata$Age99)
Age99new
Inc99new <- ifelse(is.na(mydata$Inc99),'0',mydata$Inc99)
Inc99new
IncZeronew <- log((as.numeric(IncZero) + 1))
IncZeronew
mydata2 <- cbind(mydata1, Age99new, Inc99new,IncZeronew)
mydata2

#Now let's create the models
Mod1<- lm(Profit99 ~ Online99 + Age99 + Inc99 + Tenure99 + as.factor(District99), data=mydata2)
summary(Mod1)

Mod2<- lm(Profit99 ~ Online99 + AgeMean + IncMean + Tenure99 + as.factor(District99),data=mydata2)
summary(Mod2)

Mod3<- lm(Profit99 ~ Online99 + AgeZero + IncZero + Tenure99 + as.factor(District99), data=mydata2)
summary(Mod3)

Mod4 <- lm(Profit99 ~ Online99 + AgeMiss + AgeMean + IncMiss + IncMean + Tenure99 + as.factor(District99), data=mydata2)
summary(Mod4)

Mod5<- lm(Profit99 ~ Online99 + AgeMiss + AgeZero  + IncMiss + IncZero + Tenure99 + as.factor(District99), data=mydata2)
summary(Mod5)

Mod6 <- lm(Profit99 ~ Online99 + AgeMiss + AgeZero  + IncMiss + IncZeronew + Tenure99 + as.factor(District99),data=mydata2)
summary(Mod6)

#Part a)
#Turn all values into a table, in order to view their impact on the model
Models <- list(Mod1, Mod2, Mod3, Mod4, Mod5, Mod6)
variableNames <- c()
for(i in seq_along(Models)){
  variableNames <- append(variableNames, names(coef(Models[[i]])))
}
UniqueVarNames <- unique(variableNames)
Results <- matrix(nrow=length(UniqueVarNames), ncol = 6)
colnames(Results) <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6")
rownames(Results) <- UniqueVarNames
for(i in seq_along(Models)){
  CurrentModel <- coef(Models[[i]])
  for(j in 1:length(CurrentModel)){
    variable <- names(CurrentModel[j])
    value <- unname(CurrentModel[j])
    Results[variable, paste("Model", i, sep = " ")]=value
  }
}
View(Results)


#Parts b) and c) are conceptual based on the information above, no further code needed for them


#Question 5 does not need further code either


#Question 6

pilgrim.data6 <- cbind(pilgrim.data,AgeMiss,IncMiss,Age99new,Inc99new)
pilgrim.data6

# Set value of zero for profit of customers who left in 2000
pilgrim.data6$profit00zero.na <- ifelse(is.na(pilgrim.data6$Profit00),0,pilgrim.data6$Profit00)
pilgrim.data6$profit00zero.na

#Define Profitability00 as 1 if profit > 0 
pilgrim.data6$profitablity00 <- ifelse(pilgrim.data6$profit00zero.na > 0 ,1,0)
pilgrim.data6$profitablity00

# using Regression model to predict the 2000 profitablity of customers who used online channel in 1999 
modelq6 = glm( profitablity00 ~ Online99 + AgeMiss + Age99new + IncMiss + Inc99new , data=pilgrim.data6, family=binomial(link="logit"))
summary(modelq6)

#Create response predictions
pilgrim.data6[ , Profit00Prob := predict(modelq6,type="response")]
head(pilgrim.data6)
summary(pilgrim.data6$Profit00Prob)

pilgrim.data6[, plot(sort(Profit00Prob,decreasing = T))]

#The hashed out code below would be the initial set with cutoff = 0.5, but the ROC threshold was used in analysis, so this is irrelevant, but worth keeping for posterity.
# define profit00prob to be 1 if probability >= 0.5
#pilgrim.data6[, Profit00Prob := ifelse(Profit00Prob>=0.5, 1, 0)]
#head(pilgrim.data6)
#pilgrim.data6[, table(profitablity00, Profit00Prob)]

pilgrim.data6.roc = pilgrim.data6[, roc(profitablity00, Profit00Prob, percent=T)]
auc(pilgrim.data6.roc)
plot(pilgrim.data6.roc,smooth=T)
coords(pilgrim.data6.roc,"best","specificity",transpose = F)
#     threshold  specificity  sensitivity
# 1   0.5011316    58.16961    66.43502

# define profit00prob to be 1 if probability >=  the threshold is = 0.5011316
pilgrim.data6[, Profit00Prob := ifelse(Profit00Prob>=0.5011316, 1, 0)]
head(pilgrim.data6)
pilgrim.data6[, table(profitablity00, Profit00Prob)]

# How to interpret this result?
#              Profit00Prob
# profitablity00     0     1
#     0             9445  6792
#     1             5168 10229

# hit rate: true positive /true positive + false negative
hitrate6 <- 10229/(5168+10229);hitrate6 #66.44%

#Question 7

# Define customer retained equal to 1 if there is value for profit column
pilgrim.data6$retained00  <- ifelse(is.na(pilgrim.data6$Profit00),0,1)

#logistic regression to find the model to predict retained probabilty based on channel use and other parameters
modelq7 = glm( retained00  ~  Online99 + AgeMiss + Age99new + IncMiss + Inc99new + as.factor(District99), data=pilgrim.data6, family=binomial(link="logit"))
summary(modelq7)

pilgrim.data6[ , retained00Prob := predict(modelq7,type="response")]

summary(pilgrim.data6$retained00Prob)
pilgrim.data6[, plot(sort(retained00Prob,decreasing = T))]

pilgrim.data6.roc7 = pilgrim.data6[, roc(retained00, retained00Prob, percent=T)]
auc(pilgrim.data6.roc7)
plot(pilgrim.data6.roc7,smooth=T)
coords(pilgrim.data6.roc7,"best","specificity",transpose = F)

#     threshold  specificity   sensitivity
# 1   0.7668316    65.7121      82.01243

# define profit00prob to be 1 if probability >=  the threshold = 0.7668316
pilgrim.data6[, retained00Prob := ifelse(retained00Prob>=0.7668316, 1, 0)]
head(pilgrim.data6)
pilgrim.data6[, table(retained00, retained00Prob)]


#        retained00Prob
#retained00     0     1
#   0         3442  1796
#   1         4748 21648

# hit rate: true positive /true positive + false negative
hitrate7 <- 21648/(3442+21648);hitrate7 # hit rate = 86.28%


