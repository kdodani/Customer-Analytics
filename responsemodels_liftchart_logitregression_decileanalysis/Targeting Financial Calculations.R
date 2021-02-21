### LECTURE 5 RESPONSE MODELS, RFM ANALYSIS, LOGIT REGRESSION AND ANALYSIS, ROC CURVE, CUMULATIVE LIFT CHART- DATA TABLE METHOD
rm(list=ls())
library(data.table)
#install.packages("dplyr")
library(dplyr)
#install.packages("pROC")
library("pROC")

data = fread("Targeting Financial Calculations.csv")

# understand the data
head(data)
str(data)

summary(data)

# RFM variables
data[,list(mean(Last_Purchase)),by=.(Rcode)]
data[,list(mean(Frequency)),by=.(Fcode)]
data[,list(mean(Amt_Purchased)),by=.(Mcode)]

data[,table(Mcode,Fcode,Rcode)]

data[,table(ItalCook+ItalAtlas+ItalArt+GeogBks+ArtBks==Related_Purchase)]


# using Logistic Regression to predict the purchase of "Florence"
mod1 = glm(Florence ~ Last_Purchase + Frequency + Amt_Purchased , data=data, family=binomial(link="logit"))
summary(mod1) ##using only rfm method
            
mod2 =  glm(Florence ~ Last_Purchase + Frequency + Amt_Purchased + Gender + ChildBks + YouthBks + CookBks + DIYBks + RefBks + ArtBks + GeogBks + ItalCook + ItalAtlas + ItalArt , data=data, family=binomial(link="logit")) 
summary(mod2) ##using rfm and other predictor variables

data[, FlorenceProb := predict(mod2,type="response")] ##predict function using mod2, response predicts the probability of responding not log odds
head(data)
summary(data$FlorenceProb)

data[, plot(sort(FlorenceProb,decreasing = T))]

data[, FlorencePred := ifelse(FlorenceProb>=0.5, 1, 0)] #setting target for hit rate, if proba greatly than 50%, then we predict customer will respond, otherwise wont
head(data)
data[, table(Florence, FlorencePred)]

# ROC curve
# y-axis is the sensitivity: the probabilty that the modle predics a response when customers actually respond (positive responses)
# x-axis is (100%-specificity)
# specificity refers to the % of predcited non-responses contained in the group of customers not selected by the model
# thus, a good model is the one with high sensitivity and low false positive rate (100%-specifity)
data.roc = data[, roc(Florence, FlorenceProb, percent=T)]
auc(data.roc)
plot(data.roc,smooth=T)
coords(data.roc,"best","specificity",transpose = F) ##get best threshold to get a sense of a better hit ratee that captures more 1s

data[, FlorencePred := ifelse(FlorenceProb>=0.1048957, 1, 0)] #lowering the hit rate to 10% as peer threshold defined above
head(data)
data[, table(Florence, FlorencePred)]

# random selection vs. targeting
data[, FlorenceProb_quantile:=ntile(FlorenceProb,10)]
data[,table(FlorenceProb_quantile)]
targeted = data[, list(n.retained=sum(Florence),
                       n=length(Florence),
                       rate_observed=sum(Florence)/length(Florence),
                       rate_predicted=mean(FlorenceProb)),
                by=.(FlorenceProb_quantile)]
targeted
targeted = targeted[order(-FlorenceProb_quantile),]
targeted
targeted[,plot(order(-FlorenceProb_quantile),rate_predicted,type="b")]
targeted[,abline(h=max(rate_predicted),col=2)]


# cumulative lift chart
setorder(targeted,-FlorenceProb_quantile)
targeted
targeted[,cumlift:=(cumsum(n.retained)/cumsum(n))/(sum(n.retained)/sum(n))] 
targeted[,cumcustomerpt:=cumsum(n)/sum(n)]
targeted
targeted[, plot(cumcustomerpt,cumlift,type="b")]


#cumulative gain chart
setorder(targeted,-FlorenceProb_quantile)
targeted[,gains:=cumsum(n.retained)/sum(n.retained)]
targeted[,cumcustomerpt:=cumsum(n)/sum(n)]
targeted[, plot(cumcustomerpt,gains,type="b",ylim=c(0,1),xlim=c(0,1),main="gains chart")]; abline(0,1,col=2)
