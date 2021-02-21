
rm(list=ls())

#set working directory
# install.packages("data.table)
library(data.table)
# install.packages("dplyr")
library(dplyr)
library("pROC")


#Question 1
#load transaction data and retention data
data<-fread("book_transaction.csv")

# take a look at the column properties
str(data)

# specify the date format for DATE variable
data[, DATE:=as.Date(DATE,format="%m/%d/%Y")]  

str(data)
head(data)

# summary data
table(is.na(data))
summary(data)

data[, hist(BOOKS, breaks = 100)]
data[, hist(DOLLARS, breaks = 100)]
data[, hist(DOLLARS/BOOKS, breaks = 100)]


#split the data into calibration and validation samples at 9/30/17
data.calibration<-data[DATE<=as.Date(c("2017-09-30")),]
data.validation<-data[DATE>as.Date(c("2017-09-30")),]

# create new data frames that aggregate info at per ID level
# monetary = average spend
# frequency = number of separate orders
# lastpurchase = date of most recent purchase
# numBOOKS = average number of BOOKS ordered
new.calibration <- data.calibration[,list(monetary=mean(DOLLARS),
                                          frequency=length(DOLLARS),
                                          lastpurchase=as.Date(max(DATE)),
                                          recency=as.numeric(max(data.calibration$DATE)-max(DATE)),
                                          numbooks=sum(BOOKS)),
                                    by=.(ID)]

new.validation <- data.validation[,list(monetary=mean(DOLLARS),
                                        frequency=length(DOLLARS),
                                        lastpurchase=as.Date(max(DATE)),
                                        recency=as.numeric(max(data.validation$DATE)-max(DATE)),
                                        numbooks=sum(BOOKS)),
                                  by=.(ID)]


#Merge calibration and validation samples into wide format where NAs in the validation indicate not being retained
new<-merge(new.calibration, new.validation, by = c("ID"), all.x = TRUE)

#creates another column which returns a 1 if customer purchases in the validation period, 0 otherwise
new[,retained:=as.numeric(!is.na(monetary.y))]

View(new)


#Linear regression Question 2a
new.linearfit <-lm(retained ~ recency.x + frequency.x + monetary.x, data=new)
summary(new.linearfit)
new[, linearfit:=predict(new.linearfit, type="response")]

hist(new$linearfit)
View(new)

#Question 2b Impact of M on retention
monetaryprediction = expand.grid(recency.x=mean(new$recency.x),
                                 frequency.x=mean(new$frequency.x),
                                 monetary.x=seq(min(new$monetary.x),max(new$monetary.x)))
monetaryprediction$linearfit <- predict(new.linearfit, monetaryprediction,type="response")
plot(monetaryprediction$monetary.x,monetaryprediction$linearfit,col=3)


#Question 2b Impact of R on retention
recencyprediction = expand.grid(recency.x=seq(min(new$recency.x),max(new$recency.x)),
                                 frequency.x=mean(new$frequency.x),
                                 monetary.x=mean(new$monetary.x))
recencyprediction$linearfit <- predict(new.linearfit, recencyprediction,type="response")
plot(recencyprediction$recency.x,recencyprediction$linearfit,col=3)

#Question 2b Impact of F on Retention
frequencyprediction = expand.grid(recency.x=mean(new$recency.x),
                                frequency.x=seq(min(new$frequency.x),max(new$frequency.x)),
                                monetary.x=mean(new$monetary.x))
frequencyprediction$linearfit <- predict(new.linearfit, frequencyprediction,type="response")
plot(frequencyprediction$frequency.x,frequencyprediction$linearfit,col=3)


#Question 2c Logistic regression
new.logisticfit<- glm(retained ~ recency.x + frequency.x + monetary.x, data=new, family=binomial(link="logit"))
summary(new.logisticfit)

new[, logisticfit:=predict(new.logisticfit, type="response")]
new[, retainedpredict:=ifelse(logisticfit>=0.5,1,0)]
new[, table(retained,retainedpredict)]


#Question 2d marginal impacts
recencypredictionlogit = expand.grid(recency.x=seq(min(new$recency.x),max(new$recency.x)),
                                frequency.x=mean(new$frequency.x),
                                monetary.x=mean(new$monetary.x))
recencypredictionlogit$logisticfit <- predict(new.logisticfit, recencypredictionlogit,type="response")
plot(recencypredictionlogit$recency.x,recencypredictionlogit$logisticfit,col=3)


frequencypredictionlogit = expand.grid(recency.x=mean(new$recency.x),
                                  frequency.x=seq(min(new$frequency.x),max(new$frequency.x)),
                                  monetary.x=mean(new$monetary.x))
frequencypredictionlogit$logisticfit <- predict(new.logisticfit, frequencypredictionlogit,type="response")
plot(frequencypredictionlogit$frequency.x,frequencypredictionlogit$logisticfit,col=3)



#Question 3
#3. Decile analysis: For recency.x, frequency.x, and monetary.x in the restructured data, 
#create new columns that place each customer into a decile based on how they rank. 
#This will be easy to do with the package “dplyr” and the function “ntile.” 
#Calculate and plot the percent retention in decile of recency, each decile of frequency, 
#and each decile of monetary. (1 pt)each
new[, recency_quantile := ntile(recency.x,10)]
new[,table(recency_quantile)]
targetedrecency = new[, list(n.retained=sum(retained),
                       n=length(retained),
                       rate_observed=sum(retained)/length(retained),
                       rate_predicted=mean(logisticfit)),
                by=.(recency_quantile)]
targetedrecency
targetedrecency = targetedrecency[order(-recency_quantile),]
targetedrecency
targetedrecency[,plot(order(-recency_quantile),rate_predicted,type="b")]
targetedrecency[,abline(h=max(rate_predicted),col=2)]

new[, frequency_quantile:=ntile(frequency.x,10)]
new[,table(frequency_quantile)]
targetedfrequency = new[, list(n.retained=sum(retained),
                             n=length(retained),
                             rate_observed=sum(retained)/length(retained),
                             rate_predicted=mean(logisticfit)),
                      by=.(frequency_quantile)]
targetedfrequency
targetedfrequency = targetedfrequency[order(-frequency_quantile),]
targetedfrequency
targetedfrequency[,plot(order(-frequency_quantile),rate_predicted,type="b")]
targetedfrequency[,abline(h=max(rate_predicted),col=2)]

new[, monetary_quantile:=ntile(monetary.x,10)]
new[,table(monetary_quantile)]
targetedmonetary = new[, list(n.retained=sum(retained),
                               n=length(retained),
                               rate_observed=sum(retained)/length(retained),
                               rate_predicted=mean(logisticfit)),
                        by=.(monetary_quantile)]
targetedmonetary
targetedmonetary = targetedmonetary[order(-monetary_quantile),]
targetedmonetary
targetedmonetary[,plot(order(-monetary_quantile),rate_predicted,type="b")]
targetedmonetary[,abline(h=max(rate_predicted),col=2)]




#Question 4 a

setorder(targetedrecency,-recency_quantile)
targetedrecency
targetedrecency[,cumlift:=(cumsum(n.retained)/cumsum(n))/(sum(n.retained)/sum(n))] 
targetedrecency[,cumcustomerpt:=cumsum(n)/sum(n)]
targetedrecency
targetedrecency[, plot(cumcustomerpt,cumlift,type="b")]




#Question 4 b
new[, logisticfit_quantile:=ntile(logisticfit,10)]
new[,table(logisticfit_quantile)]
targeted1 = new[, list(n.retained=sum(retainedpredict),
                       n=length(retainedpredict),
                       rate_observed=sum(retainedpredict)/length(retainedpredict),
                       rate_predicted=mean(logisticfit)),
                by=.(logisticfit_quantile)]
targeted1
targeted1 = targeted1[order(-logisticfit_quantile),]
targeted1
targeted1[,plot(order(-logisticfit_quantile),rate_predicted,type="b")]
targeted1[,abline(h=max(rate_predicted),col=2)]


# cumulative lift chart
setorder(targeted1,-logisticfit_quantile)
targeted1
targeted1[,cumlift:=(cumsum(n.retained)/cumsum(n))/(sum(n.retained)/sum(n))] 
targeted1[,cumcustomerpt:=cumsum(n)/sum(n)]
targeted1
targeted1[, plot(cumcustomerpt,cumlift,type="b")]

#Question 4c
# randomly select 10%
overall.responserate = mean(new$retained)
overall.responserate


#Question 4d

new[, logisticfit_quantile:=ntile(logisticfit,10)]
new[,table(logisticfit_quantile)]
targeted2 = new[, list(n.retained=sum(retainedpredict),
                       n=length(retainedpredict),
                       rate_observed=sum(retainedpredict)/length(retainedpredict),
                       rate_predicted=mean(logisticfit)),
                by=.(logisticfit_quantile)]
targeted2
targeted2 = targeted2[order(-logisticfit_quantile),]
targeted2
targeted2[,plot(order(-logisticfit_quantile),rate_predicted,type="b")]
targeted2[,abline(h=max(rate_predicted),col=2)]

setorder(targeted2, -logisticfit_quantile)
targeted2[, gains:=cumsum(n.retained)/sum(n.retained)]
targeted2[, cumcustomerpt:=cumsum(n)/sum(n)]

targeted2[,plot(cumcustomerpt,gains,type="b",ylim=c(0,1),main="Gains Chart")]; abline(0,1,col=2)


#Question 4e 
setorder(targetedfrequency,-frequency_quantile)
targetedfrequency
targetedfrequency[, gains:=cumsum(n.retained)/sum(n.retained)]
targetedfrequency[, cumcustomerpt:=cumsum(n)/sum(n)]

targetedfrequency[,plot(cumcustomerpt,gains,type="b",ylim=c(0,1),main="Gains Chart against deciles based on frequency")]; abline(0,1,col=2)
