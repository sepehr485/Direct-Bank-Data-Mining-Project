## When using the functions in this file,
## make sure that the outcome variable is converted to factor

rm(list=ls()); gc()
setwd('/Users/yikong/Dropbox (CSU Fullerton)/fullerton/aFullerton/R files/some data sets')
dat = read.csv('RidingMowers.csv', stringsAsFactors=T, head=T)
## If predictors are very different in scales,
## standardize them to have comparable scales.
## The code to do so is as follows:
## dat[,1:2] = scale(dat[,1:2])

set.seed(1)
n.train = floor( nrow(dat)*0.75 )
ind.train = sample(1:nrow(dat), n.train)
ind.test = setdiff(1:nrow(dat), ind.train)

## simple version; set k = 3
require(class)
Xtrain = dat[ind.train,1:2]
Xtest = dat[ind.test,1:2]
ytrain = dat[ind.train,3]

ypred = knn(Xtrain, Xtest, ytrain, k=3, prob=T)
# prob = T gives the probability of each observation in class 1

ytest = dat[ind.test,3]
table(ytest, ypred)

## what if we dont know which k to choose?
## we will go over a function that I wrote to choose the best k with the validation data
get.prob = function(x) {
  prob = attr(x, 'prob')
  cl = as.numeric(x)
  ind = which(cl == 1)
  prob[ind] = 1 - prob[ind]
  return(prob)
}

knn.bestK = function(train, test, y.train, y.test, k.grid = 1:20, ct = .5) {
  fun.tmp = function(x) {
    y.tmp = knn(train, test, y.train, k = x, prob=T) # run knn for each k in k.grid
    prob = get.prob(y.tmp)
    y.hat = as.numeric( prob > ct )
    return( sum(y.hat != as.numeric(y.test)) )
  }
  ## create a temporary function (fun.tmp) that we want to apply to each value in k.grid
  error = unlist(lapply(k.grid, fun.tmp))
  names(error) = paste0('k=', k.grid)
  ## it will return a list so I need to unlist it to make it to be a vector
  out = list(k.optimal = k.grid[which.min(error)], 
             error.min = min(error)/length(y.test),
             error.all = error/length(y.test))
  return(out)
}

obj1 = knn.bestK(Xtrain, Xtest, ytrain, ytest, seq(1, 18, 2), .5)
obj1

## rerun with the best k
ypred1 = knn(Xtrain, Xtest, ytrain, k=obj1$k.optimal, prob=T)
table(ytest, ypred1)
