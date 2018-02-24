library(h2o)
localH2O = h2o.init()
datx = colnames(ups_train_dummy[2:72])
daty = colnames(ups_train_dummy[1])
trh=as.h2o(ups_train_dummy,destination_frame = "trh")
tth=as.h2o(ups_valid_dummy,destination_frame="tth")

ups_rf=h2o.randomForest(x = datx,  # column numbers for predictors
                        y = daty,   # column number for label
                        training_frame = "trh",
                        validation_frame = 'tth',nfolds=3,seed=1)
pr =h2o.predict(ups_rf,tth)


df_yhat_test <- as.data.frame(pr)

table(df_yhat_test[,1],upselling_valid[,1])
(roc_auc <- function(probabilities,dataset){
  #Command - roc_auc(probabilities,dataset)
  #probabilities are those obtained from predict function
  #dataset is the actual data (0s and 1s)
  library(ROCR)   #Install ROCR library before running 
  pr=prediction(probabilities,dataset)
  prf=performance(pr,measure = "tpr", x.measure = "fpr")
  auc=performance(pr,measure = "auc")
  auc=auc@y.values[[1]]
  plot(prf,colorize=TRUE,main=paste("ROC curve with AUC=",auc))}
  }
}
pr2=prediction(df_yhat_test[,3],upselling_valid[,1])
prf=performance(pr2,measure = "tpr", x.measure ="fpr")
plot(prf,colorize=TRUE,main=paste("ROC curve with AUC=",auc))
auc=performance(pr2,measure = "auc")
auc@y.values[[1]]
roc_auc(df_yhat_test[,3],upselling_valid[,1])










"Var126",
"Var28",
"Var57",
"Var113",
"Var153",
"Var81",
"Var13",
"Var125",
"Var73",
"Var6",
"Var133",
"Var140",
"Var134",
"Var38",
"Var119",
"Var76",
"Var163",
"Var149",
"Var74",
"Var160",
"Var25",
"Var123",
"Var112",
"Var218.1"
