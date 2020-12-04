rm(list=ls())
library(corrplot)
library(MASS)
library(ggplot2)
library(caret)
library(corrplot)
rm(list=ls())
#setwd('C:\\Users\\Priyanka\\Desktop\\bank\\bank-additional')
bank1<-read.csv("bank-additional-full.csv",sep=";", stringsAsFactors = FALSE)
#exporting the well-ordered datafile
write.csv(bank1,"bank1.csv")
dim(bank1) #41188 obs and 21 variables---20 independent variables (10 char,5 int, 5 num)
str(bank1) # there are
summary(bank1)
colnames(bank1)

# I dont want ID and zip code in the data; we cant analyze them

#setwd('C:\\Users\\Priyanka\\Desktop\\bank\\bank-additional')
#exporting the well-ordered datafile
write.csv(bank1,"bank1.csv")
dim(bank1) #41188 obs and 21 variables---20 independent variables (10 char,5 int, 5 num)
str(bank1) # there are
summary(bank1)
colnames(bank1)



bank2= as.data.frame(bank1)
# converting character variables to levels/factor
bank2$y= ifelse(bank2$y=='yes',1,0)
bank2$job=as.factor(bank2$job)
bank2$marital=as.factor(bank2$marital)

bank2$education=as.factor(bank2$education)
bank2$default=as.factor(bank2$default)
bank2$housing=as.factor(bank2$housing)
bank2$loan=as.factor(bank2$loan)
bank2$contact=as.factor(bank2$contact)
bank2$month=as.factor(bank2$month)
bank2$day_of_week=as.factor(bank2$day_of_week)
bank2$poutcome=as.factor(bank2$poutcome)
bank2$y=as.factor(bank2$y)


get.or = function(sobj, alpha=.05) {
  b = sobj$coef[-1, 'Estimate']
  se.b = sobj$coef[-1, 'Std. Error']
  pval = sobj$coef[-1, 'Pr(>|z|)']
  or = exp(b); se.or = exp(b)*se.b
  lb = b - qnorm(alpha/2)*se.b; lb.or = exp(lb)
  ub = b + qnorm(1-alpha/2)*se.b; ub.or = exp(ub)
  out = cbind(or, se.or, lb, ub, pval)
  colnames(out) = c('OR', 'SE', paste0((1-alpha)*100, '% CI, lower'),
                    paste0((1-alpha)*100, '% CI, upper'), 'p value')
  return(out)
}
get.or(summary(obj))

# OR(Income): The odds of accepting the loan offer increases by 5.2% if the income increases by one thousand dollars
# OR(Family): The odds of accepting the loan offer increases by 98.8% if the family size increases by one
# OR(Securities): The odds of accepting the loan offer for those who have securities account 
# with the bank is 0.296 times as the odds of accepting the loan offer for those who do not \
# have securities account with the bank

##############################################################
## what if we want to partition the data and evaluate model ##

# take 60% of data randomly as training
set.seed(1) # set a seed so that people get the same 60% next time they run the same code
id.train = sample(1:nrow(bank2), nrow(bank2)*.6) # ncol() gives number of columns
id.test = setdiff(1:nrow(bank2), id.train) # setdiff gives the set difference
dat.train = bank2[id.train,]
dat.test = bank2[id.test,]

min.model = glm(y~ 1, data = dat.train, family = 'binomial')
max.model = glm(y~ 1, data = dat.train, family = 'binomial')
max.formula = formula(max.model)

obj = step(min.model, direction='forward', scope=max.formula) # it will print out models in each step
summary(obj) # it will give you the final model

yhat = predict(obj, newdata = dat.test, type='response')
# if type = response, we get the probability.
hist(yhat)

dichotomize = function(yhat, cutoff=.4) {
  out = rep(0, length(yhat))
  out[yhat > cutoff] = 1
  out
}

yhat.class = dichotomize(yhat, .1)
err = mean(yhat.class != dat.test$y) # misclassification error rate
err

sen = function(ytrue, yhat) {
  ind.true1 = which(ytrue == 1)
  mean( ytrue[ind.true1] == yhat[ind.true1] )
}

spe = function(ytrue, yhat) {
  ind.true0 = which(ytrue == 0)
  mean( ytrue[ind.true0] == yhat[ind.true0] )
}

sen(dat.test$y, yhat.class)
spe(dat.test$y, yhat.class)

