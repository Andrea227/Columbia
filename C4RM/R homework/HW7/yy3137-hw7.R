######################### Q1:MLR

library(ggplot2)

# data(package = 'ggplot2')
names(mpg)

myfit0 = lm(formula = cty ~ 1, data = mpg)
myfit1 = lm(formula = cty ~ displ, data = mpg)
myfit2 = lm(formula = cty ~ displ + year, data = mpg)
myfit3 = lm(formula = cty ~ displ + year + cyl, data = mpg)
myfit4 = lm(formula = cty ~ displ + year + cyl + drv, data = mpg)
myfit5 = lm(formula = cty ~ displ + year + cyl + drv + manufacturer, data = mpg)
myfitn = lm(formula = cty ~ .,data = mpg)

# myfit = myfit3
# 
# summary(myfit)
# plot(myfit)

# #Tools
# # Stepwise Regression
# 
# scope = list(lower = formula(myfit0), upper = formula(myfitn))
# 
# # Stepwise AIC
# forwardAIC  = step(myfit0, scope, direction = "forward", k = 2)
# backwardAIC = step(myfitn, scope, direction = "backward", k = 2)
# 
# # Stepwise BIC
# n = myfit0$df.residual + 1
# scope = list(lower = formula(myfit0), upper = formula(myfitn))
# forwardBIC  = step(myfit0, scope, direction = "forward", k = log(n))
# backwardBIC = step(myfitn, scope, direction = "backward", k = log(n))


# Homework
MyStep = function(startModel, finishModel, backward_or_forward, AIC_or_BIC){
  if(AIC_or_BIC == 'AIC'){
    scope = list(lower = formula(startModel), upper = formula(finishModel))
    forwardAIC  = step(startModel, scope, direction = "forward", k = 2)
    backwardAIC = step(finishModel, scope, direction = "backward", k = 2)
    if(backward_or_forward == 'forward'){
      modelselected = forwardAIC
    } else {
      modelselected = backwardAIC
    }
  } else {
    n = startModel$df.residual + 1
    scope = list(lower = formula(startModel), upper = formula(finishModel))
    forwardBIC  = step(startModel, scope, direction = "forward", k = log(n))
    backwardBIC = step(finishModel, scope, direction = "backward", k = log(n))
    if(backward_or_forward == 'forward'){
      modelselected = forwardBIC
    } else {
      modelselected = backwardBIC
    }
  }
  return(modelselected)
}

# Unit Tests

myfit0 = lm(formula = cty ~ 1, data = mpg)
myfitn = lm(formula = cty ~ .,data = mpg)
x = MyStep(startModel = myfit0,
           finishModel = myfitn,
           backward_or_forward = 'backward',
           AIC_or_BIC = 'AIC')

round(x$coefficients[1],4) == 3.0366

######################### Q2: SLR_plot

PlotBetaFits = function(fit,mydata,nsim){
  plot(mydata[1:2])
  library(mvtnorm)
  sim = mvtnorm::rmvnorm(nsim,coefficients(fit),vcov(fit)) 
  for (i in 1:nsim) {
    abline(sim[i, 1], sim[i, 2])
  }
}

# # Tools: Multivariate simulation
# library(mvtnorm)
# mvtnorm::rmvnorm(nsim,coefficients(fit),vcov(fit))
# 
# # Tools: Coefficiencts of fit
# coefficients(fit)
# 
# # Tools: Variance covariance matrix of fit
# vcov(fit)

# Unit Tests
data(cars)
fit = lm(dist ~ speed,cars)
PlotBetaFits(fit = fit,mydata = cars,nsim = 100)

######################### Q3: TMat_Weighted

TMAT1= function(vec1,vec2,weights = 1){
  if (length(weights) == 1) {
    ratings = sort(unique(union(vec1,vec2)))
    n = length(ratings)
    tmat = matrix(0,n,n)
    for (r in 1:n){
      for (c in 1:n){
        tmat[r,c] = sum(vec1 == ratings[r] & vec2 == ratings[c])
      }
      tmat[r,] = tmat[r,]/sum(vec1 == ratings[r])
    }
  } else {
    ratings = sort(unique(union(vec1,vec2)))
    n = length(ratings)
    tmat = matrix(0,n,n)
    for (r in 1:n){
      for (c in 1:n){
        tmat[r,c] = sum((vec1 == ratings[r] & vec2 == ratings[c]) * weights)
      }
      tmat[r,] = tmat[r,]/sum((vec1 == ratings[r]) * weights)
    }
  }
  mat = tmat
  return(mat)
}

rLast = rep(c('A','B','C'),c(3,4,5))
rNow  = rep(c('A','B','C'),c(5,2,5))
weights = c(5:7,5:8,5:9)/10

# Unit Tests:

tmat = TMAT1(rLast,rNow,weights)
round(tmat[2,2],4) == .5769

######################### Q4: qGetReturns1

library(quantmod)
x = getSymbols('AAPL',auto.assign = F)[,6]
x = as.numeric(x)

getReturns = function(x,lag){
  n = length(x)
  ratiovec = x[(1+lag):n]/x[1:(n-lag)]
  returns =  ratiovec - 1
  return(returns)
}

# Unit Tests

x = c(10,20,50,100,1000)

rets1 = getReturns(x,1)
rets2 = getReturns(x,2)
rets3 = getReturns(x,3)

rets1[1] == 1
rets2[1] == 4
rets3[1] == 9

######################### Q5: qGetReturns2

# Display a histogram of returns with a second histogram of theoretical returns overlaid.


# You only need to install once
# install.packages("quantmod")

library(quantmod)
x = getSymbols('AAPL',auto.assign = F)[,6]
x = as.numeric(x)

# From previous question
rets1 = getReturns(x,1)
hist(rets1)

PlotReturns = function(r){
  n = length(r)
  xmean = mean(r)
  xsd = sd(r)
  rsims = rnorm(n,xmean,xsd)
  obj_hist = hist(r,breaks = 50)
  breaks = obj_hist$breaks
  hist(rsims,add = TRUE,breaks = breaks,col = 'blue') #  Add arguments
}

PlotReturns(rets1)

######################### Q6: qVaR

# Tools: percentile
# quantile(rnorm(10000),.9772)

PercentVaR = function(r,alpha){
  hist(r)
  a = quantile(r,alpha)
  return(a)
}

# Unit test 

r = rnorm(100000)
myalpha = .99
mypercentVaR = PercentVaR(r,1-myalpha)
round(mypercentVaR) == -2

######################### Q7: qES

# # Tools: percentile
# quantile(rnorm(10000),.9772)
# 
# # Tools: Indicator vector
# ind_gt_VaR = losses > VaR
# 
# # Tools: mean
# mean(lossess[ind_gt_VaR])

ES = function(losses, alpha){
  var = quantile(losses,alpha)
  ESc = losses[losses > var]
  out = mean(ESc)
  return(out)
}

# Unit test 

alpha = .90
set.seed(0)
losses = rnorm(1000,50000,1000)
hist(losses)
v = PercentVaR(losses, alpha)
abline(v = v,col='red')
es = ES(losses = losses, alpha)
abline(v = es,col='purple')

######################### Q8: Shiny




