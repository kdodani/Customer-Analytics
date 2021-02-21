



# SET WORKING DIRECTORY
# READ DATA
maru.data<-read.csv("maru_data.csv")


#############################################################
# Answers to Part I
# CALCULATE ANNUAL MARGIN
maru.data$total.cost.per.hr<-maru.data$instructor.labor.cost.per.hr* maru.data$instructors.needed + maru.data$worker.labor.cost.per.hr* maru.data$workers.needed
maru.data$margin.hr<-maru.data$price.per.hr-maru.data$total.cost.per.hr
maru.data$annual.margin<-maru.data$margin.hr*maru.data$annual.hours


# answer to question 1 on page 5
# CALCULATE ACQUISITION COST
maru.data$acquisition.cost<-maru.data$contact.cost/maru.data$response.rate
maru.data$acquisition.cost
# Q1. : 10000 10000 60000 50000  2000

# answer to question 2
# BREAKEVEN WITHOUT DISCOUNTING
maxperiod = 6
period = seq(1,maxperiod)

cf = matrix(0,nrow=5,ncol=maxperiod)
npv = matrix(0,nrow=5,ncol=maxperiod)

for (t in 1:maxperiod){
  cf[,t] = maru.data$annual.margin*maru.data$retention.rate^(t-1)
  npv[,t] = rowSums(as.matrix(cf[,1:t])) - maru.data$acquisition.cost
}

# print out cash flow matrix 
cf
# print out npv to see since which period the npv>=0
npv
# you can try the following instead of eyeballing
apply(npv,1, function(x) which.max(x>=0))

# Q2. : 3 3 4 3 2

# PROBLEM 3: COMPUTE CLV (ASSUMING INFINITE TIME HORIZON)
# Net CLV = (M * ( (1+i) / (1 + i - R))) - AC Using formula #1
# Create a new column with CLV assuming numbers from case
maru.data$clv <- (maru.data$annual.margin* ((1+maru.data$interest.rate) / (1 + maru.data$interest.rate - maru.data$retention.rate))) - maru.data$acquisition.cost

maru.data$clv
# Q3. : 5714.286  1000.000  6000.000 16000.000   200.000

# Q4. : The most attractive customer segment for MBC is "Elite Ballplayers (Party)" , Net CLV = 16000

# PROBLEM 5: CHIYODA WARD
clv.littleleaguers.now<-maru.data[maru.data$X=="little leaguers",]$clv
clv.littleleaguers.chiyoda<-5000*(1+.1)/(1+.1-.65)-(600/.08)
clv.littleleaguers.now
clv.littleleaguers.chiyoda
# Chiyoda ward net CLV = 4722.222    Minato ward net CLV = 5714.286
# Q5. : MBC should not pursue the Chiyoda ward sponsership plan becasue it produces less net CLV than Minato ward.



clv.eliteballplayers.bat<-((7500-6000)*20)*(1+.1)/(1+.1-.6)-(12500/.29+10000)
clv.eliteballplayers.bat
clv.eliteballplayers.now
# PROBLEM 6: ELITE BALLPLAYERS DISCOUNT
clv.EliteBallPlayers_Party <- maru.data$clv[which(maru.data$X=="elite ballplayers (party)")]
clv.EliteBallPlayers_Partyafterdiscount <- (1000*20)*(1.1/(1.1-.75))+20*500-50000
max(clv.EliteBallPlayers_Party,clv.EliteBallPlayers_Partyafterdiscount)

#Q6. :MBC should offer this promotion because the net clv from promotion is larger than current clv for this segment


# PROBLEM 7: ELITE BALLPLAYERS BAT

clv.EliteBallPlayers_Partybat <- 1500*20*(1.1/(1.1-0.6))-12500/0.29-10000; clv.EliteBallPlayers_Partybat
max(clv.EliteBallPlayers_Party,clv.EliteBallPlayers_Partybat)

#Q.7 : MBC shouldn't offer the free bat offer because the net CLV after this promotion is going to be less than the original net CLV for this segment

##########################################################
# Answers to Part II
# SENSITIVITY ANALYSIS (pls follow the hints below)

# creates scenario values
# try use seq() to create a series of number for parameters AC, M, and R
ac <- seq(from = 40000, to = 60000, by = 5000)
am <- seq(from = 20000, to = 40000, by = 5000)
rr <- seq(from = 0.4, to = 0.8, by = 0.1)

# try use expand.grid to create a full matrix for all the possible combinations of AC, M, and R
values <- expand.grid(ac=ac,am=am,rr=rr)
values$i <- 0.10

# computes CLV for all scenarios
# visualization using scatter.smooth, for example
scatter.smooth(x=values$ac, y=values$nclv)
# you can use scatter.smoooth to plot out the relationships between, for example, AC and CLV

#computes CLV for all scenarios
values$CLV = (values$am*(1.1)/(1.1-values$rr))
values$CLV.minus.cost = values$CLV - values$ac
values

#How often is CLV below 0?

sum(values$CLV.minus.cost <= 0)/length(values$CLV.minus.cost) #around 1/4 of the time with variation

min(values$CLV)
values[values$CLV.minus.cost == min(values$CLV.minus.cost),]

#How plausible are the values?

neg.CLV = values[which(values$CLV.minus.cost <= 0),] #create a table of all negative CLVs
neg.CLV
#all variations of margin being 20,000, 25,000 with rr = 0.4
#Not likely at all if rr is higher than 0.6.  only 7 cases at rr>=0.6

sum(neg.CLV$rr == 0.4)/length(neg.CLV$rr) #47% of negative CLV has rr = 0.4
sum(values$rr == 0.4 & values$CLV.minus.cost <=0)/sum(values$rr == 0.4) #60% of rr = 0.4 have negative CLVs
sum(neg.CLV$rr == 0.5)/length(neg.CLV$rr) #47% of negative CLV has rr = 0.4

neg.CLV.hi.rr = neg.CLV[which(neg.CLV$rr != 0.4 & neg.CLV$rr != 0.5),] #To look at the values of negative CLV without rr=0.4 and rr=0.5
sum(neg.CLV$rr != 0.4 & neg.CLV$rr != 0.5)/length(neg.CLV$rr) #% of negative CLV without rr=0.4 and rr=0.5, 21.875%

sum(neg.CLV.hi.rr$am == 20000)/length(neg.CLV.hi.rr$am) #86% (6/7) cases have am of 20,000

#How often is this CLV lower than for Little Leaguers?

clv.littleleaguers = 15714.29
netclv.littleleaguers = 5714.29

sum(values$CLV <= clv.littleleaguers)/length(values$CLV.minus.cost) #% of cases where CLV is lower, 0%
sum(values$CLV.minus.cost <= netclv.littleleaguers)/length(values$CLV.minus.cost) #% of cases where net CLV is lower, 33.6%

values[which(values$CLV.minus.cost <= netclv.littleleaguers),] #display all cases where CLV was lower than for Little Leaguers
sum(values$CLV.minus.cost <= netclv.littleleaguers & values$CLV.minus.cost > 0)/sum(values$CLV.minus.cost <= netclv.littleleaguers)

values[which(values$CLV.minus.cost <= netclv.littleleaguers & values$CLV.minus.cost>0),]

#Changes in am, ceterus parabus
am.large = seq(from = 20000, to = 40000, by = 200)
am.large
CLV.M = ((am.large*(1.1))/(0.5)) - 50000
plot(am.large, CLV.M, xlab = "actual margin", ylab = "CLV", main = "actual margin vs. CLV")

(50000*0.5)/1.1 #Calculate the x-intercept

#Changes in rr, ceterus parabus
rr.large = seq(from = 0.4, to = 0.8, by = (0.4)/100)
rr.large
CLV.r = (30000*(1.1))/(1.1-rr.large) - 50000
plot(rr.large,CLV.r, xlab = "retention rate", ylab = "CLV", main = "retention rate vs. CLV")

(50000*1.1 - 30000*1.1)/50000 #Calculate the x-intercept

#Changes in ac, ceterus parabus
ac.large = seq(40000, to = 60000, by=200)
ac.large
CLV.c = (30000*(1.1))/(0.5) - ac.large
plot(ac.large,CLV.c, xlab = "acquisition cost", ylab = "CLV", main = "acquisition cost vs. CLV")

##########################################################
# Answers to Part III
elite.ballplayers<-maru.data[4 ,]
elite.ballplayers.subset<-subset(elite.ballplayers, select = c("acquisition.cost","annual.margin","retention.rate"))
elite.ballplayers.subset<-data.matrix(elite.ballplayers.subset)


#CLV for elite ballplayers calcuated using aggregate values
elite.ballplayers.clvaggregate<-maru.data$clv[4]


# simulate CLV for a bunch of elite ballplayers
# Set Seed for Random Number Generation
set.seed(123456)




# monte carlo exercises here
# I will show you the example of simulating the annual margins here
# load in customer margin data and analyze it
load("customers.rdata")
hist(customers)
load("customers.rdata")
hist(customers)
margin.mean<-mean(customers)
margin.sd<-sd(customers)


d<- 0.1
ac<-elite.ballplayers.subset[1]
m<-elite.ballplayers.subset[2]
r<-elite.ballplayers.subset[3]

num.samples=10000

d.vec<-rep(d, num.samples)
ac.vec<-rep(ac, num.samples)
m.vec<-rnorm(num.samples,margin.mean,margin.sd)
r.vec<-rep(r, num.samples)

netclv.vec=(m.vec* ((1+0.1)/(1+0.1-r.vec)))-ac.vec
mean(netclv.vec)
median(netclv.vec)
hist(netclv.vec, main = "CLV")

sorted = sort(netclv.vec, decreasing = TRUE)
cum.netclv.vec = cumsum(sorted)
customer = 1:num.samples
customerpercent = 100*customer/num.samples
netclv.vec.percent = sorted/sum(sorted)*100
cum.netclv.vec.percent = cumsum(netclv.vec.percent)
plot(x=customerpercent,y=cum.netclv.vec.percent, xlab = 'percentage of customers', ylab = 'cumulative percentage of aggregate clv', main = 'Whale chart for annual margin simulation')
abline(h=100, col="blue")
abline(v=80, col="blue")
# Next use Beta distribution to determine retention rate value
#setting parameter so average r.vec is around 0.6 from 10000 draws

r.vec = rbeta(num.samples, 10, 6.6666666666)
mean(r.vec)
#average is around 0.60
m.vec = rep(m, num.samples) 

# Now insert the simulated retention rate into the CLV formula 
netclv.vec=(m.vec* ((1+0.1)/(1+0.1-r.vec)))-ac.vec
mean(netclv.vec)
median(netclv.vec)
hist(netclv.vec, main = "CLV")

#Create "Whale Plot" to depict value concentration
# the key is to sort customers by their CLV
# use plot()
sorted = sort(netclv.vec, decreasing = TRUE)
cum.netclv.vec = cumsum(sorted)
customer = 1:num.samples
customerpercent = 100*customer/num.samples
netclv.vec.percent = sorted/sum(sorted)*100
cum.netclv.vec.percent = cumsum(netclv.vec.percent)
plot(x=customerpercent,y=cum.netclv.vec.percent, xlab = 'percentage of customers', ylab = 'cumulative percentage of aggregate clv', main = 'Whale chart for retention rate simulation')
abline(h=100, col="blue")
abline(v=80, col="blue")


