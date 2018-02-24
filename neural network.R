library(h2o)
localH2O = h2o.init()


#parellal computing
library(doParallel)
detectCores()
cl=makeCluster(3)
registerDoParallel(cl)
getDoParWorkers()
#stopCluster(cl)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Upsellling
#Import the dataset with Rstudio features
upselling_train <-read.table('upselling_train.csv',header=TRUE, sep=',',stringsAsFactors=TRUE)
upselling_valid <-read.table('upselling_valid.csv',header=TRUE, sep=',',stringsAsFactors=TRUE)
upselling_train=upselling_train[,-1]
upselling_valid=upselling_valid[,-1]



datx = colnames(upselling_train[2:59])
daty = colnames(upselling_train[1])
  trh=as.h2o(upselling_train,destination_frame = "trh")
  tth=as.h2o(upselling_valid,destination_frame="tth")

#training

  model <- 
  h2o.deeplearning(x = datx,  # column numbers for predictors
                   y = daty,   # column number for label
                   training_frame = "trh",# data in H2O format
                   activation = "RectifierWithDropout", # or 'Tanh'
                   input_dropout_ratio = 0.2, # % of inputs dropout
                   hidden_dropout_ratios = c(.2,.2,.2,.2), # % for nodes dropout
                   balance_classes = TRUE, 
                   hidden = c(58,32,16,8), # three layers of 50 nodes
                   epochs = 100, # max. no. of epochs
                   variable_importances=T,
                   stopping_rounds=2,
                   stopping_metric="misclassification", ## could be "MSE","logloss","r2"
                   stopping_tolerance=0.001,nfolds = 5,seed = 1,fold_assignment = 'Modulo',
                   validation_frame = 'tth',categorical_encoding ="OneHotInternal" )


#prediction
pr =h2o.predict(model,tth)
df_yhat_test <- as.data.frame(pr)
plot(model, 
     timestep = "epochs", 
     metric = "classification_error")
ups_pred_y = rep("no", length=5000) 
ups_pred_y[df_yhat_test[,3] > 0.2] = "yes"

table(df_yhat_test[,1],upselling_valid[,1])
(2181+1155)/5000
(2938+972)/5000

table(predict=ups_pred_y,truth=upselling_valid$y_upselling)
ups_pred_y = rep("no", length=5000) 
ups_pred_y[df_yhat_test[,3] > 0.2] = "yes"
(1222+2096)/5000

roc_auc <- function(probabilities,dataset){
  #Command - roc_auc(probabilities,dataset)
  #probabilities are those obtained from predict function
  #dataset is the actual data (0s and 1s)
  library(ROCR)   #Install ROCR library before running 
  pr=prediction(probabilities,dataset)
  prf=performance(pr,measure = "tpr", x.measure = "fpr")
  auc=performance(pr,measure = "auc")
  auc=auc@y.values[[1]]
  plot(prf,colorize=TRUE,main=paste("ROC curve with AUC=",auc))
}

roc_auc(df_yhat_test[,3],upselling_valid[,1])



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
#Appetency
#import dataset with r studio inbuilt function

appetency_train=appetency_train[,-1]
appetency_valid=appetency_valid[,-1]



datx = colnames(appetency_train[2:59])
daty = colnames(appetency_train[1])
trh=as.h2o(appetency_train,destination_frame = "trh")
tth=as.h2o(appetency_valid,destination_frame="tth")

#training
#
#
model <- 
  h2o.deeplearning(x = datx,  # column numbers for predictors
                   y = daty,   # column number for label
                   training_frame = "trh",# data in H2O format
                   activation = "RectifierWithDropout", # or 'Tanh'
                   input_dropout_ratio = 0.2, # % of inputs dropout
                   hidden_dropout_ratios = c(.2,.2,.2,.2), # % for nodes dropout
                   balance_classes = TRUE, 
                   hidden = c(58,32,16,8), # three layers of 50 nodes
                   epochs = 100, # max. no. of epochs
                   variable_importances=T,
                   stopping_rounds=2,
                   stopping_metric="misclassification", ## could be "MSE","logloss","r2"
                   stopping_tolerance=0.001,nfolds = 10,seed = 1,fold_assignment = 'Modulo',
                   validation_frame = 'tth',categorical_encoding ="OneHotInternal",sparse=T)


#prediction

pr =h2o.predict(model,tth)
df_yhat_test <- as.data.frame(pr)

table(df_yhat_test[,1],appetency_valid[,1])
(3189+232)/5000
(4205+146)/5000


roc_auc(df_yhat_test[,3],appetency_valid[,1])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Churn
#Import the dataset with Rstudio features
churn_train <-read.table('churn_train.csv',header=TRUE, sep=',',stringsAsFactors=TRUE)
churn_valid <-read.table('churn_valid.csv',header=TRUE, sep=',',stringsAsFactors=TRUE)
#import dataset with r studio inbuilt function

churn_train=churn_train[,-1]
churn_valid=churn_valid[,-1]



datx = colnames(churn_train[2:59])
daty = colnames(churn_train[1])
trh=as.h2o(churn_train,destination_frame = "trh")
tth=as.h2o(churn_valid,destination_frame="tth")

#training
model <- 
  h2o.deeplearning(x = datx,  # column numbers for predictors
                   y = daty,   # column number for label
                   training_frame = "trh",# data in H2O format
                   activation = "TanhWithDropout", # or 'Tanh'
                   input_dropout_ratio = .1, # % of inputs dropout
                   hidden_dropout_ratios = c(.2,.2,.1,.1,.1), # % for nodes dropout
                   balance_classes = TRUE, 
                   hidden = c(20,20,20,20,20), # three layers of 50 nodes
                   epochs = 50, # max. no. of epochs
                   variable_importances=T,
                   stopping_rounds=2,
                   stopping_metric="misclassification", ## could be "MSE","logloss","r2"
                   stopping_tolerance=0.001,nfolds = 5,seed = 1,fold_assignment = 'Modulo',
                   validation_frame = 'tth')


#prediction

pr =h2o.predict(model,tth)
df_yhat_test <- as.data.frame(pr)

table(df_yhat_test[,1],churn_valid[,1])
(1852+1110)/5000



roc_auc(df_yhat_test[,3],churn_valid[,1])








h2o.shutdown(prompt=FALSE)
stopCluster(cl)












