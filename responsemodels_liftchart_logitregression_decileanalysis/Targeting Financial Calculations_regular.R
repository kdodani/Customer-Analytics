### LECTURE 5 RESPONSE MODELS, RFM ANALYSIS, LOGIT REGRESSION AND ANALYSIS, ROC CURVE, CUMULATIVE LIFT CHART- NO DATA TABLE METHOD
rm(list=ls())
library(data.table)
#install.packages("dplyr")
library(dplyr)
#install.packages("pROC")
library("pROC")

data = read.csv("Targeting Financial Calculations.csv", header = T)

head(data)
str(data)

summary(data)

table(data$ItalCook+data$ItalAtlas+data$ItalArt+data$GeogBks+data$ArtBks==data$Related_Purchase)

# traditional RFM analysis by looking at the 
aggregate(data$Last_Purchase, by = list(data$Rcode), FUN = mean)
aggregate(data$Frequency, by = list(data$Fcode), FUN = mean)
aggregate(data$Amt_Purchased, by = list(data$Mcode), FUN = mean)


table(data$Mcode,data$Fcode,data$Rcode)


# using Logistic Regression to predict the purchase of "Florence"
mod1 = glm(Florence ~ Last_Purchase + Frequency + Amt_Purchased , data=data, family=binomial(link="logit"))
summary(mod1)
            
mod2 =  glm(Florence ~ Last_Purchase + Frequency + Amt_Purchased + Gender + ChildBks + YouthBks + CookBks + DIYBks + RefBks + ArtBks + GeogBks + ItalCook + ItalAtlas + ItalArt , data=data, family=binomial(link="logit")) 
summary(mod2)

data$FlorenceProb = predict(mod2, data=data, type="response")
head(data)
summary(data$FlorenceProb)
plot(sort(data$FlorenceProb,decreasing = T))

data$FlorencePred = ifelse(data$FlorenceProb>=0.5, 1, 0)
table(data$Florence, data$FlorencePred)


# ROC curve
# y-axis is the sensitivity: the probabilty that the modle predics a response when customers actually respond (positive responses)
# x-axis is (100%-specificity)
# specificity refers to the % of predicted non-responses contained in the group of customers not selected by the model
# thus, a good model is the one with high sensitivity and low false positive rate (100%-specifity)
data.roc =  roc(data$Florence, data$FlorenceProb, percent=T)
auc(data.roc)
plot(smooth(data.roc))
coords(data.roc,"best","specificity",transpose = F)

# use the optimal threshold as the logistic cutoff for prediction
data$FlorencePred = ifelse(data$FlorenceProb>=0.1048957, 1, 0)
table(data$Florence, data$FlorencePred)



# random selection vs. targeting

# randomly select 10%
overall.responserate = mean(data$Florence)
overall.responserate

# now let's see what is the top 10% response rate?
data$FlorenceProb_quantile = ntile(data$FlorenceProb,10)
table(data$FlorenceProb_quantile)


targeted = aggregate(data$FlorenceProb, by = list(data$FlorenceProb_quantile), FUN= mean)
colnames(targeted)<-c("quantile","rate_predicted")
targeted$n  = table(data$FlorenceProb_quantile)
targeted
plot(order(-targeted$quantile),targeted$rate,type="b")
abline(h=max(targeted$rate),col=2)


# code for cumulative lift chart
targeted<-targeted[order(-targeted$quantile),]

targeted$cum.n = cumsum(targeted$n)
targeted$n.retained = targeted$n * targeted$rate
targeted$cum.n.retained = cumsum(targeted$n.retained)
targeted$cumlift<-(targeted$cum.n.retained/targeted$cum.n)/mean(data$Florence)
targeted$cumcustomerpt<-targeted$cum.n/sum(targeted$n)
targeted

plot(targeted$cumcustomerpt,targeted$cumlift,type="b")

