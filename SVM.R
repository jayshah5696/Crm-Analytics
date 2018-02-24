#parellal computing
setwd("~/Desktop/project_dataset")
library(data.table)
library(doParallel)
detectCores()
cl=makeCluster(3)
registerDoParallel(cl)
getDoParWorkers()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Upsellling
#Import the dataset with Rstudio features
upselling_train <-read.table('upselling_train.csv',header=TRUE, sep=',',stringsAsFactors=TRUE)
upselling_valid <-read.table('upselling_valid.csv',header=TRUE, sep=',',stringsAsFactors=TRUE)
#write.csv(X,'traindata.csv')
upselling_train=upselling_train[,-1]
upselling_valid=upselling_valid[,-1]


library(dummies)

ups_train_dummy=dummy.data.frame(upselling_train,names=c("Var226.1","Var197.1","Var205.1","Var206.1"),sep="_")
ups_valid_dummy=dummy.data.frame(upselling_valid,names=c("Var226.1","Var197.1","Var205.1","Var206.1"),sep="_")
upselling_imp <-read.table('upselling_imp.csv',header=TRUE, sep=',',stringsAsFactors=F)
upselling_imp=c("Var126",
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
                "Var218.1")




x=ups_train_dummy[,upselling_imp]
y=ups_train_dummy[,1]


y_upselling=y
ups_train_dummy=cbind(y_upselling,x)

library(kernlab)
svm_ups=ksvm(y_upselling~.,data=ups_train_dummy,
             type='C-svc',scaled=T,kernel='rbfdot',
             cost=10,prob.model=T,kpar=list(sigma=.1))
ypred=predict(svm_ups,ups_valid_dummy,type="probabilities")


ups_pred_y = rep("no", length=5000) 
ups_pred_y[ypred[,2] > 0.2] = "yes"

table(predict=ups_pred_y,truth=ups_valid_dummy$y_upselling)


library(ROCR)
pred <- prediction(ypred[,2],upselling_valid[,1])
# Plot ROC curve
prf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc=performance(pred,measure = "auc")
auc=auc@y.values[[1]]
plot(prf,colorize=TRUE,main=paste("ROC curve with AUC=",auc))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
#churn

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Import the dataset with Rstudio features
churn_train <-read.table('churn_train.csv',header=TRUE, sep=',',stringsAsFactors=TRUE)
churn_valid <-read.table('churn_valid.csv',header=TRUE, sep=',',stringsAsFactors=TRUE)
#write.csv(X,'traindata.csv')
churn_train=churn_train[,-1]
churn_valid=churn_valid[,-1]




chu_train_dummy=dummy.data.frame(churn_train,names=c("Var226.1","Var197.1","Var205.1","Var206.1"),sep="_")
chu_valid_dummy=dummy.data.frame(churn_valid,names=c("Var226.1","Var197.1","Var205.1","Var206.1"),sep="_")
churn_imp <-read.table('churn_imp.csv',header=TRUE, sep=',',stringsAsFactors=F)
churn_imp=c("Var126",
            "Var113",
            "Var57",
            "Var73",
            "Var81",
            "Var13",
            "Var28",
            "Var6",
            "Var125",
            "Var153",
            "Var140",
            "Var133",
            "Var119",
            "Var134",
            "Var38",
            "Var76",
            "Var74",
            "Var163",
            "Var149",
            "Var160",
            "Var123",
            "Var25",
            "Var197.1_0",
            "Var197.1_1",
            "Var197.1_2",
            "Var197.1_3",
            "Var197.1_4",
            "Var197.1_5",
            "Var205.1_0",
            "Var205.1_1",
            "Var109",
            "Var112",
            "Var21",
            "Var83",
            "Var22",
            "Var226.1_0",
            "Var226.1_1",
            "Var226.1_2",
            "Var226.1_3",
            "Var85")




x=chu_train_dummy[,churn_imp]
y=chu_train_dummy[,1]



y_churn=y
chu_train_dummy=cbind(y_churn,x)


svm_chu=ksvm(y_churn~.,data=chu_train_dummy,
             type='C-svc',scaled=T,kernel='rbfdot',
             cost=10,prob.model=T,kpar=list(sigma=.1))
ypred=predict(svm_chu,chu_valid_dummy,type="probabilities")


chu_pred_y = rep("no", length=5000) 
chu_pred_y[ypred[,2] > 0.3] = "yes"

table(predict=chu_pred_y,truth=chu_valid_dummy$y_churn)



pred <- prediction(ypred[,2],churn_valid[,1])
# Plot ROC curve
prf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc=performance(pred,measure = "auc")
auc=auc@y.values[[1]]
plot(prf,colorize=TRUE,main=paste("ROC curve with AUC=",auc))


#appetency

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Import the dataset with Rstudio features
appetency_train <-read.table('appetency_train.csv',header=TRUE, sep=',',stringsAsFactors=TRUE)
appetency_valid <-read.table('appetency_valid.csv',header=TRUE, sep=',',stringsAsFactors=TRUE)
#write.csv(X,'traindata.csv')
appetency_train=appetency_train[,-1]
appetency_valid=appetency_valid[,-1]




app_train_dummy=dummy.data.frame(appetency_train,names=c("Var226.1","Var197.1","Var205.1","Var206.1"),sep="_")
app_valid_dummy=dummy.data.frame(appetency_valid,names=c("Var226.1","Var197.1","Var205.1","Var206.1"),sep="_")
appetency_imp <-read.table('appetency_imp.csv',header=TRUE, sep=',',stringsAsFactors=F)
appetency_imp=c("Var126",
                "Var113",
                "Var57",
                "Var73",
                "Var81",
                "Var13",
                "Var28",
                "Var6",
                "Var125",
                "Var153",
                "Var140",
                "Var133",
                "Var119",
                "Var134",
                "Var38",
                "Var76",
                "Var74",
                "Var163",
                "Var149",
                "Var160",
                "Var123",
                "Var25",
                "Var197.1_0",
                "Var197.1_1",
                "Var197.1_2",
                "Var197.1_3",
                "Var197.1_4",
                "Var197.1_5",
                "Var205.1_0",
                "Var205.1_1",
                "Var109",
                "Var112",
                "Var21",
                "Var83",
                "Var22",
                "Var226.1_0",
                "Var226.1_1",
                "Var226.1_2",
                "Var226.1_3",
                "Var85")




x=app_train_dummy[,appetency_imp]
y=app_train_dummy[,1]



y_appetency=y
app_train_dummy=cbind(y_appetency,x)


svm_app=ksvm(y_appetency~.,data=app_train_dummy,
             type='C-svc',scaled=T,kernel='rbfdot',
             cost=10,prob.model=T,kpar=list(sigma=.01))
ypred=predict(svm_app,app_valid_dummy,type="probabilities")


app_pred_y = rep("no", length=5000) 
app_pred_y[ypred[,2] > 0.07] = "yes"

table(predict=app_pred_y,truth=app_valid_dummy$y_appetency)



pred <- prediction(ypred[,2],appetency_valid[,1])
# Plot ROC curve
prf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc=performance(pred,measure = "auc")
auc=auc@y.values[[1]]
plot(prf,colorize=TRUE,main=paste("ROC curve with AUC=",auc))



stopCluster(cl)






















