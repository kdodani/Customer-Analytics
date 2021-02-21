####CLV CALCULATION, NET CLV, DISCOUNTED CASH FLOW MATRIX, MONTE CARLO SIMULATION

  
# Example
# M=100
# R=.8
# AC=200
# d=.1 


M = 100 #profit margin
R = .8 #retention rate
AC = 200 #aacquisition cost
i = .1 #discoujnt rate

CLV = M*(1+i)/(1+i-R) #standard clv formula
CLV

NCLV = M*(1+i)/(1+i-R) - AC #net clv is clv minus acquisition cost
NCLV


# Let's verify by listing the infinite series
maxperiod = 100
period = seq(1,maxperiod)

# discounted cash flow matrix 
dcf = rep(0,maxperiod)
npv = rep(0,maxperiod)

for (t in 1:maxperiod){
  dcf[t] = M*R^(t-1)/(1+i)^(t-1)
  npv[t] = sum(dcf[1:t]) - AC
}
# print out the discounted cash flow matrix 
plot(dcf,type="b") #dcf will approximate 0 over a certain number of periods

# print out npv to see since which period the npv>=0
plot(npv,type="b") #net present value will be stable over a period

# an alternative to figure out since which period the npv>=0
which.max(npv>=0)


########### Monte Carlo simulation of  #############
set.seed(12345)

num.samples=10000


##defining random varibales for monte carlo simulation based on normal distribution 
m.mean = 100 
m.sd = 30
m.vec<-rnorm(num.samples,m.mean,m.sd) #gives teh different profit margins m you draw each time

i.vec<-rep(i, num.samples)
ac.vec<-rep(AC, num.samples)
r.vec<-rep(R, num.samples)

netclv.vec=(m.vec* ((1+i.vec)/(1+i.vec-r.vec)))-ac.vec  

mean(netclv.vec)
median(netclv.vec)
hist(netclv.vec, main = "CLV")






