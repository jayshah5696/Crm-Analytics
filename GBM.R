setwd("~/Desktop/project_dataset")

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
#Import the dataset with Rstudio features
upselling_train <-read.table('upselling_train.csv',header=TRUE, sep=',',stringsAsFactors=TRUE)
upselling_valid <-read.table('upselling_valid.csv',header=TRUE, sep=',',stringsAsFactors=TRUE)

upselling_train=upselling_train[,-1]
upselling_valid=upselling_valid[,-1]



datx = colnames(upselling_train[2:59])
daty = colnames(upselling_train[1])
trh=as.h2o(upselling_train,destination_frame = "trh")
tth=as.h2o(upselling_valid,destination_frame="tth")


ups_rf=h2o.randomForest(x = datx,  # column numbers for predictors
                        y = daty,   # column number for label
                        training_frame = "trh",
                        validation_frame = 'tth',nfolds=5,seed=1)

model=h2o.gbm(x=datx,y=daty,training_frame="trh",
              nfolds=5,ntrees=1000,max_depth=2,
              distribution = "bernoulli",
              validation_frame = 'tth',
              seed=1,learn_rate=.2)



pr =h2o.predict(model,tth)
df_yhat_test <- as.data.frame(pr)

table(df_yhat_test[,1],upselling_valid[,1])
(894+3246)/5000

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
appetency_train <-read.table('appetency_train.csv',header=TRUE, sep=',',stringsAsFactors=TRUE)
appetency_valid <-read.table('appetency_valid.csv',header=TRUE, sep=',',stringsAsFactors=TRUE)

appetency_train=appetency_train[,-1]
appetency_valid=appetency_valid[,-1]



datx = colnames(appetency_train[2:59])
daty = colnames(appetency_train[1])
trh=as.h2o(appetency_train,destination_frame = "trh")
tth=as.h2o(appetency_valid,destination_frame="tth")

#training


#prediction
model=h2o.gbm(x=datx,y=daty,training_frame="trh",
              nfolds=5,ntrees=1000,max_depth=2,
              distribution = "bernoulli",
              validation_frame = 'tth',
              seed=1,learn_rate=.2)



pr =h2o.predict(model,tth)
df_yhat_test <- as.data.frame(pr)
table(df_yhat_test[,1],appetency_valid[,1])


app_pred_y = rep("no", length=5000) 
app_pred_y[df_yhat_test[,3] > .2] = "yes"
table(predict=app_pred_y,truth=appetency_valid$y_appetency)


(4367+123)/5000



roc_auc(df_yhat_test[,3],appetency_valid[,1])


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Churn
#Import the dataset with Rstudio features
churn_train <-read.table('churn_train.csv',header=TRUE, sep=',',stringsAsFactors=TRUE)
churn_valid <-read.table('churn_valid.csv',header=TRUE, sep=',',stringsAsFactors=TRUE)

churn_train=churn_train[,-1]
churn_valid=churn_valid[,-1]



datx = colnames(churn_train[2:59])
daty = colnames(churn_train[1])
trh=as.h2o(churn_train,destination_frame = "trh")
tth=as.h2o(churn_valid,destination_frame="tth")

#training

model=h2o.gbm(x=datx,y=daty,training_frame="trh",
              nfolds=5,ntrees=1000,max_depth=2,
              distribution = "bernoulli",
              validation_frame = 'tth',
              seed=1,learn_rate=.2)



#prediction

pr =h2o.predict(model,tth)
df_yhat_test <- as.data.frame(pr)

table(df_yhat_test[,1],churn_valid[,1])
(1072+2196)/5000




stopCluster(cl)
