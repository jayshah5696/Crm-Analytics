appetency_train=appetency_train[,-1]
appetency_valid=appetency_valid[,-1]



appetency_train$y_appetency=mapvalues(appetency_train$y_appetency,c('yes','no'),to=c('1','0'))
appetency_valid$y_appetency=mapvalues(appetency_valid$y_appetency,c('yes','no'),to=c('1','0'))



library(gbm)
#parellal computing
library(doParallel)
detectCores()
cl=makeCluster(3)
registerDoParallel(cl)
getDoParWorkers()
#stopCluster(cl)



set.seed (1)
boost.app =gbm(y_appetency~.,data=appetency_train ,distribution="bernoulli",n.trees =4000 ,interaction.depth =4,shrinkage = 0.2)

summary(boost.app)


#churn random forest
churn_train=churn_train[,-1]


#upselling svm
#install.packages("e1071")
library(e1071)
#
#Fit the support vector clssifier
#?svm
svmfit=svm(y_upselling~., data=upselling_train, kernel="linear", cost=10,scale=FALSE)#cost= 1/budget(c)
##"cost" is similar to tuning parameter C, but with opposite ##effects: small "cost", wide margin; large "cost", narrow margin 
