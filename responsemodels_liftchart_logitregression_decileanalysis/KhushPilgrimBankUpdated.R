#1
data <- read.csv("/Users/khush/Desktop/pilgrimBdata.csv")
mydata<-data.frame(data)
head(mydata)
profitsorted <- sort(data$Profit99, decreasing = TRUE)
profitsorted
profitcum<-cumsum(profitsorted)
profitcum
profitpercent<-(profitcum/sum(profitsorted))*100
#profitpercent1 <- round(profitcum/sum(profitsorted)*100, 2)
#profitpercent1
customerpercent = (1:31634/31634)*100
#customerpercent1<-paste(round(100*customerpercent, 2), "%", sep="")
plot(x=customerpercent,y=profitpercent, xlab = 'Cumulative Customers', ylab = 'Cumulative Profit', main = 'Profitability Skew', lwd =0.08)
grid(NULL,NULL,col="black", lty=3)


#2
table<-data.frame(customerpercent,profitpercent,profitsorted)
table
maxprofit = min(table$customerpercent[table$profitpercent = 100])
maxprofit




b = max(table$customerpercent[table$profitpercent == max(table$profitpercent[table$profitsorted > 0])])
b

# part c
#(Balance in deposit accounts)*(Net Interest Spread)+(Fees)+(Interest From Loans)-(Cost to Serve)
#net interest spread is the difference between the average yield that a financial institution receives from loans (including  interest-accruing activities) and average rate it pays on deposits and borrowings. The variability in profitability across customers is therefore explained by the balance in deposit accounts, net spread, interest and cost to serve- which aree variable in nature. For example the balance in deposit accounts could vary from customer to customer depending on various socio-demographic factors. Similarly, interest accrued from loans differ for each sutomer- as some may default in their payments. Cost to serve is also a variable cost to banks becasue some services are only offered as a value added benefit and are not available to the entire customer base. 


#4
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



