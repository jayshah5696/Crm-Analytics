
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
                   hidden_dropout_ratios = c(.2,.2), # % for nodes dropout
                   balance_classes = TRUE, 
                   hidden = c(20,20), # three layers of 50 nodes
                   epochs = 100, # max. no. of epochs
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



model=h2o.randomForest(x=datx,y=daty,training_frame="trh",ntrees = 50,max_depth = 10,mtries = 15)

model=h2o.gbm(x=datx,y=daty,training_frame="trh",nfolds=5,ntrees=1000,max_depth=3,,seed=1,learn_rate=.2)
model=h2o.xgboost()


1328+1460
2788/5000
97/1425




with.y_train$appetency=mapvalues(with.y_train$appetency,c('yes','no'),to=c('1','0'))
with.y_valid$appetency=mapvalues(with.y_valid$appetency,c('yes','no'),to=c('1','0'))
with.y_train$churn=mapvalues(with.y_train$churn,c('yes','no'),to=c('1','0'))
with.y_valid$churn=mapvalues(with.y_valid$churn,c('yes','no'),to=c('1','0'))
