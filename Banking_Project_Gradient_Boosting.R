bank.additional.full <- read.csv("bank-additional-full.csv",
                                 header=TRUE, sep=";")
#Loading the packages
library(MASS)
library(ggplot2)
library(caret)
library(corrplot)
library(e1071)
#Viewing the dataset
head(bank.additional.full,3)
str(bank.additional.full)
##preprocessing

##separating continuous and categorical variables for checking significance of the variables
bank_continous = subset(bank.additional.full, select = c(age,
                                                         duration,campaign,pdays,previous,emp.var.rate,euribor3m, cons.price.idx,cons.conf.idx,
                                                         nr.employed) )


bank_categorical=subset(bank.additional.full, select=-c(age,
                                                        duration,campaign,pdays,previous, emp.var.rate,euribor3m,
                                                        cons.price.idx,cons.conf.idx, nr.employed))



corr <- cor(bank_continous)
corrplot(corr = corr,method='square', type = 'full')
###nr.employed is correlated with euribor3m, emp.var.rate
##negative correlation btw previous and pdays
##we will remove euribor3m, emp.var.rate and previous
##cons.price.idx is correlated with euribor3m, emp.var.rate
bank_removed_correlations=bank.additional.full[,-c(14,16,17,19)]
#Goodness of fit for categorical variables
chisq.test(bank_categorical$housing,bank_categorical$y)
##remove housing p value is very less for both loan and housing
chisq.test(bank_categorical$job,bank_categorical$y)
chisq.test(bank_categorical$loan,bank_categorical$y)
##loan is not significant
chisq.test(bank_categorical$marital,bank_categorical$y)
chisq.test(bank_categorical$month,bank_categorical$y)
chisq.test(bank_categorical$contact,bank_categorical$y)
chisq.test(bank_categorical$day_of_week,bank_categorical$y)
chisq.test(bank_categorical$poutcome,bank_categorical$y)



bank_removed_correlations=bank_removed_correlations[,-c(6,7)]
str(bank_removed_correlations)


#Removing unknown rows
table(bank_removed_correlations$job)
unknownjob= which(bank_removed_correlations$job %in% c('unknown'))
bank_updated=bank_removed_correlations[-(unknownjob),]
table(bank_updated$job)


table(bank_removed_correlations$marital)
unknownmarital= which(bank_updated$marital %in% c('unknown'))
bank_updated=bank_updated[-(unknownmarital),]


table(bank_removed_correlations$education)
unknowneducation= which(bank_updated$education %in% c('unknown'))
bank_updated=bank_updated[-(unknowneducation),]


table(bank_removed_correlations$default)
unknowndefault= which(bank_updated$default%in% c('yes'))
##the people who have default cannot take bank term deposit
bank_updated=bank_updated[-(unknowndefault),]
table(bank_removed_correlations$contact)
table(bank_removed_correlations$month)
table(bank_removed_correlations$day_of_week)
table(bank_removed_correlations$poutcome)
#View(bank_updated)
######################################################################

set.seed(1)
##60% training and 40% test
training_size <- floor(0.60 * nrow(bank_updated))
train_ind <- sample(seq_len(nrow(bank_updated)), size = training_size)
training <- bank_updated[train_ind, ]
testing <- bank_updated[-train_ind, ]
##################################################################

library(gbm)
is.factor(bank_updated$y)
bank_updated$y=as.numeric(bank_updated$y)
bank_updated$y=factor(x=bank_updated$y, levels = c(2,1),labels = c("yes","no"))
bank_updated$y


?trainControl
outcomeName <- 'y'
predictorsNames <- names(bank_updated)[names(bank_updated) != outcomeName]
##performing cross validation for higher accuracy
objControl <- trainControl(method='cv', number=3, returnResamp='none',
                           summaryFunction = twoClassSummary, classProbs = TRUE, )

objModel <- train(training[,predictorsNames], training[,outcomeName],
                  method='gbm',
                  trControl=objControl,
                  metric = "ROC", preProc = c("center", "scale"))

?postResample
summary(objModel)
print(objModel)
predictions <- predict(object=objModel, testing[,predictorsNames], type="raw")
head(predictions)
print(postResample(pred=predictions, obs=as.factor(testing[,outcomeName])))
confusionMatrix(predictions, testing$y, positive='yes')

