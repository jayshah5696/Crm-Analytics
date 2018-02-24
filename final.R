#Loading Required Libraries
library(caret)
library(data.table)
library(doParallel)
library(dplyr)
library(AppliedPredictiveModeling)
library(randomForest)
library(ROCR)   
library(h2o)
library(tree)
library(dummies)
library(kernlab)
library(mlr)
library(plyr)

#Setting random seed for our future calculations
set.seed(1000)

#This Data has lots of missing values. So, it is important for us to address those first
#setting Na string
na_strings <- c(
  '',
  'na', 'n.a', 'n.a.',
  'nan', 'n.a.n', 'n.a.n.',
  'NA', 'N.A', 'N.A.',
  'NaN', 'N.a.N', 'N.a.N.',
  'NAN', 'N.A.N', 'N.A.N.',
  'nil', 'Nil', 'NIL',
  'null', 'Null', 'NULL')

X <-read.table('total.csv',header=TRUE, sep=',',stringsAsFactors=TRUE, na.strings=na_strings)
X=X[,-c(1,2)]


nb_input_features <- ncol(X)
input_feature_names <- names(X)
nb_samples <- nrow(X)

comp_x= X


#Size of Train and Test
nb_test_samples <- length(churn_test)
nb_train_samples <- length(churn_train)

#Getting Rid of Input Features $x$'s with Too Many Missing Values
# In order to get idea of missing vallue in each columns, we need to calculate missing values in each columns
#Here I have used function
input_features_missing_proportions <-  sapply(comp_x, function(col) sum(is.na(col))) / 15000
#Ploting histogram
hist(input_features_missing_proportions)


#Here we are only considering the features that has missing value percentge < 60%
#We can consider other columns as, they are not significant
input_feature_names <-  input_feature_names[input_features_missing_proportions <= .6]
nb_input_features <- length(input_feature_names)


#after that we are left with only 80 features
#all other are removed
comp_X <- comp_X[ , input_feature_names, (with=FALSE)]


#calculating the variance of each column
#Since it is not usefule to use near zero variance term.
d=nearZeroVar(traindata,saveMetrics = F)

#We're left with the following r nb_input_features input features $x$'s:
input_feature_names
#The classes of these remaining $x$'s are:
#The classes of these remaining $x$'s are:
input_feature_classes <- factor(sapply(comp_X, class))
input_feature_classes

# we have to devide our data into categorical and numerical data
# we have to preprocess this data individually

#dealing with numerical Features
numeric_input_feature_names <-input_feature_names[input_feature_classes != 'factor']
numeric_input_feature_names
numeric_input_feature_standard_deviations <-sapply(comp_X[ , numeric_input_feature_names, (with=FALSE)],function(col) sd(col, na.rm=TRUE))
numeric_input_feature_standard_deviations


#Imputing the missing values
# Let's fill up the missing values with the means of the respective columns:
numeric_input_feature_means <- sapply(comp_X[ , numeric_input_feature_names, (with=FALSE)],function(col) mean(col, na.rm=TRUE))

means1=rep(0,42)
counter=0
for (numeric_col in numeric_input_feature_names) {
  x <- comp_X[[numeric_col]]
  missing_value_row_yesno <- is.na(x)
  if (sum(missing_value_row_yesno) > 0) {
    comp_X[ , numeric_col]=as.numeric(x)
    mu <- numeric_input_feature_means[numeric_col]
    counter=counter+1
    means1[counter]=mu
    comp_X[missing_value_row_yesno, numeric_col]=mu
  }
}
#checking the cleaned Data
all.equal(numeric_input_feature_means,sapply(comp_X[ , numeric_input_feature_names, (with=FALSE)], mean))

#Cleaning Categorical Variables
categorical_input_feature_names <-input_feature_names[input_feature_classes == 'factor']

#checking the levels of Categorical Values
categorical_input_feature_nb_levels <-  sapply(comp_X[ , categorical_input_feature_names, (with=FALSE)],function(col) length(levels(col)))
categorical_input_feature_nb_levels

#here there are couple of categorical colums that has levels more than 50.
#Here we are not dealing with Text data.
#Taking the NLP approch would be not feasible for this project. So simply we are ignoring these varaiables
categorical_input_feature_names <-categorical_input_feature_names[categorical_input_feature_nb_levels <= 50]

#Categorical imputation with mode
#Here We are Imputing the categorical features with max occuring class (mode)
for (cat_col in categorical_input_feature_names) {
  missing_value_row_yesno <- is.na(comp_X[[cat_col]])
  if (sum(missing_value_row_yesno) > 0) {
    comp_X[missing_value_row_yesno, cat_col]=sort(comp_X[,cat_col])[1]
  }}


#combining Categorical and numerical Features
comp_X <-  comp_X[ , c(numeric_input_feature_names, categorical_input_feature_names), (with=FALSE)] #Those variables having over 500 categories are likely to be just text / character data. Let's get rid of them:


#Data Partition in train and test
#train_proportion <-0.80
#train_indices <- createDataPartition(y=comp_x$churn,p=train_proportion, list=FALSE)
#X_train1 <- X[train_indices, ]
#churn_train <- comp_x$churn[train_indices]
#appetency_train <- comp_x$appetency[train_indices]
#upselling_train <- comp_x$upselling[train_indices]



#write.csv(X_test,'testdata.csv')

#write.csv(X_train1,'traindata.csv')

input_feature_train <- c(names(X_train1))
aa=c('churn_valid','Var6', "Var7",        "Var13",       "Var21" ,      "Var22",       "Var24",       "Var25" ,      "Var28",       "Var35",      
     "Var38",       "Var44",       "Var57",       "Var65",       "Var72",       "Var73",       "Var74",       "Var76",       "Var78",       "Var81",      
     "Var83",       "Var85",       "Var94",      "Var109",      "Var112",      "Var113" ,     "Var119",      "Var123",      "Var125",      "Var126",     
     "Var132",      "Var133",      "Var134",      "Var140",      "Var143",      "Var144",      "Var149",    "Var153",      "Var160",      "Var163",     
     "Var173",      "Var181" ,     "Var189",      "Var195",      "Var196",      "Var203",      "Var205",      "Var206",      "Var207",      "Var208",     
     "Var210",      "Var211" ,     "Var218",      "Var219",      "Var221",      "Var223",      "Var225",      "Var226",      "Var227",      "Var228",     
     "Var229")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#after this we have implimented techniques in excel 
#and here we removed categorical variables column "Var222',"Var220","Var217","Var192","Var198","Var199","Var202","Var204","Var216".This variables having class more than 500 and also no significant class that we can encode. So, here we have removed those variables in excel.
#------------------------------------------------------------------------------------------------
total_cat=fread("cat.csv")
total_num=total[,numericinput_feature_names,(with=F)]
final_total=cbind(total_num,total_cat)
write.csv(final_total,'total_final.csv')
getwd()

final_total=final_total[,4:61]

y_churn=total[,70]
y_appetency=total[,71]
y_upselling=total[,73]


#deviding Data into multiple 3 data in order to create 3 multiple models on each predictor
Churn_total=cbind(y_churn,final_total)
upselling_total=cbind(y_upselling,final_total)
appetency_total=cbind(y_appetency,final_total)

#individual Data Partition
set.seed(1)
train=sample(15000,10000)
churn_train=Churn_total[train,]
churn_valid=Churn_total[-train,]

upselling_train=upselling_total[train,]
upselling_valid=upselling_total[-train,]

appetency_train=appetency_total[train,]
appetency_valid=appetency_total[-train,]



write.csv(churn_valid,'churn_valid.csv')
write.csv(churn_train,'churn_train.csv')
write.csv(appetency_valid,'appetency_valid.csv')
write.csv(appetency_train,'appetency_train.csv')


write.csv(upselling_valid,'upselling_valid.csv')

write.csv(upselling_train,'upselling_train.csv')



#datacleaning upselling
half_cat1=total_updated[,c(2,4,6,8,10,12,14,16,18,20)]
half_cat2=total_updated2[,c(2,4,6,8,10,12,14,16,18)]
total_cat=cbind(half_cat1,half_cat2)


#binding total with y

fianl.with.y=total_final.with.y[,5:65]
set.seed(1)
train=sample(15000,10000)

with.y_train=fianl.with.y[train,]
with.y_valid=fianl.with.y[-train,]


write.csv(with.y_train,'with.y_train.csv')
write.csv(with.y_valid,'with.y_valid.csv')





#From Here We are going to Implement different Classification Techniques on each 3 class predictors to, predict
#the 3 classes


#In order to asses the predictive power of differen models, we are using AUC as a metric
#this is because we have unbalanced dataset. and predictiveability in unbalanced data in terms of accuracy
#cant be assesed
#AUC is more robust metirc interms of assesing the predictive power of the models
#Auc Defined as When using normalized units, the area under the curve (AUC) is equal to
#the probability that a classifier will rank a randomly chosen positive  instance 
#higher than a randomly chosen negative one


roc_auc <- function(probabilities,dataset){
  #Command - roc_auc(probabilities,dataset)
  #probabilities are those obtained from predict function
  #dataset is the actual data (0s and 1s)
  pr=prediction(probabilities, dataset)
  prf=performance(pr,measure ="tpr",x.measure ="fpr")
  auc=performance(pr,measure ="auc")
  auc=auc@y.values[[1]]
  plot(prf,colorize=TRUE,main=paste("ROC curve with AUC=",auc))
}


#The model order for different Techniques are

#1)RandomForest
#1. Random Forest
#2. Bagging
#3. Boosting
#4. Support vector machine
#5. Logistic regression
#6. TREE
#7. Artificial Neural Networks (ANN)
#8. Multi label classification



#~~~~~~~~~~~~~~~~~~~RANDOMFOREST~~~~~~~~~~``

#Appetency
set.seed(1)

#trail Run on this data set
bag.ups =randomForest(y_appetency~.,data=appetency_train, mtry=52,importance =TRUE)


yhat=predict(bag.ups,appetency_valid, type = "prob")
y=appetency_valid$y_appetency
ypred=rep('no',5000)
ypred[yhat[,2] > 0.2] = "yes"
table(ypred,y)

#geting Important variables to use in the model
varImpPlot(bag.ups)
#plotting AUC roc
roc_auc(yhat[,2],y)

#finding best parameters in terms of AUC
auc=rep(0,10)
ntr=c(50,100,150,200,250,300,350,400,450,500)
mtr=c(20,24,28,32,36,40,44,48,52,56)
for (i in 1:10){
  bag.ups =randomForest(y_appetency~.,data=appetency_train, mtry=mtr,ntry=ntr[i],importance =TRUE)
  yhat=predict(bag.ups,appetency_valid, type = "prob")
  pr=prediction(yhat, appetency_valid$y_appetencyt)
  auc=performance(pr,measure = "auc")
  auc[i]=auc@y.values[[1]]
}
#selecting best parameter and generating roc curve
bag.ups =randomForest(y_appetency~.,data=appetency_train, mtry=52,ntree=250,importance =TRUE)
yhat=predict(bag.ups,appetency_valid, type = "prob")
y=appetency_valid$y_appetency
ypred=rep('no',5000)
ypred[yhat[,2] > 0.2] = "yes"
table(ypred,y)
varImpPlot(bag.ups)
roc_auc(yhat[,2],y)

#Churn--------------------------------------------------------
#getting the best predictiors of churn
#trail run
bag.churn =randomForest(y_churn~.,data=churn_train, mtry=48,importance =TRUE)
yhat=predict(bag.ups,churn_valid, type = "prob")
y=churn_valid$y_churn
ypred=rep('no',5000)
ypred[yhat[,2] > 0.2] = "yes"
table(ypred,y)
varImpPlot(bag.ups)
roc_auc(yhat[,2],y)


#Grid Search for Churn
auc=rep(0,10)
ntr=c(50,100,150,200,250,300,350,400,450,500)
mtr=c(20,24,28,32,36,40,44,48,52,56)
for (i in 1:10){
  bag.ups =randomForest(y_churn~.,data=churn_train, mtry=mtr[i],ntry=ntr[i],importance =TRUE)
  yhat=predict(bag.ups,churn_valid, type = "prob")
  pr=prediction(yhat, churn_valid$y_churnt)
  auc=performance(pr,measure = "auc")
  auc[i]=auc@y.values[[1]]
}


#selecting best parameter and generating roc curve
bag.ups =randomForest(y_churn~.,data=churn_train, mtry=48,ntree=300,importance =TRUE)
yhat=predict(bag.ups,churn_valid, type = "prob")
y=churn_valid$y_churn
ypred=rep('no',5000)
ypred[yhat[,2] > 0.2] = "yes"
table(ypred,y)
varImpPlot(bag.ups)
roc_auc(yhat[,2],y)



#Upselling
#Trial Run for Upselling
bag.ups =randomForest(y_upselling~.,data=upselling_train, mtry=58,importance =TRUE)
yhat=predict(bag.ups,upselling_valid, type = "prob")
y=upselling_valid$y_upselling
ypred=rep('no',5000)
ypred[yhat[,2] > 0.2] = "yes"
table(ypred,y)
varImpPlot(bag.ups)
roc_auc(yhat[,2],y)


auc=rep(0,10)
ntr=c(50,100,150,200,250,300,350,400,450,500)
mtr=c(20,24,28,32,36,40,44,48,52,58)

for( i in 1:10){
  bag.ups =randomForest(y_upselling~.,data=upselling_train, mtry=mtr,ntry=ntr[i],importance =TRUE)
  yhat=predict(bag.ups,upselling_valid, type = "prob")
  pr=prediction(yhat, upselling_valid$y_upsellingt)
  auc=performance(pr,measure = "auc")
  auc[i]=auc@y.values[[1]]
}
#selecting best parameter and generating roc curve
bag.ups =randomForest(y_upselling~.,data=upselling_train, mtry=58,ntree=200,importance =TRUE)
yhat=predict(bag.ups,upselling_valid, type = "prob")
y=upselling_valid$y_upselling
ypred=rep('no',5000)
ypred[yhat[,2] > 0.2] = "yes"
table(ypred,y)
varImpPlot(bag.ups)

roc_auc(yhat[,2],y)









~~~~~~~~~~~~~~~~~~BAGGING~~~~~~~~~~~~~~~~~~~

#appetency
set.seed (1)

bag.app =randomForest(y_appetency~.,data=appetency_train, mtry=58,importance =TRUE)
yhat=predict(bag.app,appetency_valid,type = "prob")
y=appetency_valid$y_appetency
ypred=rep('no',5000)
ypred[yhat[,2] > 0.2] = "yes"
table(ypred,y)
varImpPlot(bag.app)
roc_auc(yhat[,2],y)

auc=rep(0,10)
ntr=c(50,100,150,200,250,300,350,400,450,500)
for (i in 1:10){
  bag.ups =randomForest(y_appetency~.,data=appetency_train, mtry=58,ntry=ntr[i],importance =TRUE)
  yhat=predict(bag.ups,appetency_valid, type = "prob")
  
  pr=prediction(yhat, appetency_valid$y_appetencyt)
  auc=performance(pr,measure = "auc")
  auc[i]=auc@y.values[[1]]
}

#selecting best parameter and generating roc curve
bag.app =randomForest(y_appetency~.,data=appetency_train, mtry=58,ntree=400,importance =TRUE)
yhat=predict(bag.app,appetency_valid,type = "prob")
y=appetency_valid$y_appetency
ypred=rep('no',5000)
ypred[yhat[,2] > 0.2] = "yes"
table(ypred,y)
varImpPlot(bag.app)
roc_auc(yhat[,2],y)




#upselling
bag.ups =randomForest(y_upselling~.,data=upselling_train, mtry=58,importance =TRUE)
yhat=predict(bag.ups,upselling_valid, type = "prob")
y=upselling_valid$y_upselling
ypred=rep('no',5000)
ypred[yhat[,2] > 0.2] = "yes"
table(ypred,y)
varImpPlot(bag.ups)
roc_auc(yhat[,2],y)


auc=rep(0,10)
ntr=c(50,100,150,200,250,300,350,400,450,500)
for (i in 1:10){
  bag.ups =randomForest(y_upselling~.,data=upselling_train, mtry=58,ntry=ntr[i],importance =TRUE)
  yhat=predict(bag.ups,upselling_valid, type = "prob")
  
  pr=prediction(yhat, upselling_valid$y_upsellingt)
  auc=performance(pr,measure = "auc")
  auc[i]=auc@y.values[[1]]
}
#selecting best parameter and generating roc curve
bag.ups =randomForest(y_upselling~.,data=upselling_train, mtry=58,ntree=500,importance =TRUE)
yhat=predict(bag.ups,upselling_valid, type = "prob")
y=upselling_valid$y_upselling
ypred=rep('no',5000)
ypred[yhat[,2] > 0.2] = "yes"
table(ypred,y)
varImpPlot(bag.ups)
roc_auc(yhat[,2],y)






#churn------------------------
bag.churn =randomForest(y_churn~.,data=churn_train, mtry=58,importance =TRUE)
yhat=predict(bag.churn,churn_valid, type = "prob")
y=churn_valid$y_churn
ypred=rep('no',5000)
ypred[yhat[,2] > 0.2] = "yes"
table(ypred,y)
varImpPlot(bag.churn)
roc_auc(yhat[,2],y)

auc=rep(0,10)
ntr=c(50,100,150,200,250,300,350,400,450,500)

for (i in 1:10){
  bag.ups =randomForest(y_churn~.,data=churn_train, mtry=58,ntry=ntr[i],importance =TRUE)
  yhat=predict(bag.ups,churn_valid, type = "prob")
  
  pr=prediction(yhat, churn_valid$y_churnt)
  auc=performance(pr,measure = "auc")
  auc[i]=auc@y.values[[1]]
}
#selecting best parameter and generating roc curve
bag.churn =randomForest(y_churn~.,data=churn_train, mtry=58,nree=450,importance =TRUE)
yhat=predict(bag.churn,churn_valid, type = "prob")
y=churn_valid$y_churn
ypred=rep('no',5000)
ypred[yhat[,2] > 0.2] = "yes"
table(ypred,y)

roc_auc(yhat[,2],y)




#~~~~~~~~~~~~~`Boosting~~~~~~~~~~~~~~~`
#Here we are using h2o library
#We have implemented neural network through R library H2O. We have used this library
#because of computational disadvantage of normal personal computer to implement neural
#network on large data set. H2O is an open source, in-memory, distributed, fast, and scalable
#machine learning and predictive analytics platform that allows you to build machine learning
#models on big data and provides easy productionalization of those models in an enterprise
#environment. The speed, quality, ease-of-use, and model-deployment for the various cutting
#edge Supervised and Unsupervised algorithms like Deep Learning, Tree Ensembles, and
#GLRM make H2O a highly sought-after API for big data, data science.
localH2O = h2o.init()


#parellal computing

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
roc_auc(df_yhat_test[,3],churn_valid[,1])



stopCluster(cl)

h2o.shutdown(prompt=FALSE)


#~~~~~~~~~~~tree~~~~~~~~~~~~



#upselling
tree.upselling=tree(y_upselling~.,upselling_train)
summary(tree.upselling)
y=upselling_valid$y_upselling
plot(tree.upselling)
text(tree.upselling,pretty=0)
tree.upselling
tree.pred=predict(tree.upselling,upselling_valid,type="vector")
ypred=rep('no',5000)
ypred[tree.pred[,2] > 0.2] = "yes"
table(ypred,y)

roc_auc(tree.pred[,2],y)

cv.ups=cv.tree(tree.upselling)
plot(cv.ups$size,cv.ups$dev,type='b')

prune.ups=prune.tree(tree.ups,best=10)
plot(prune.ups)
text(prune.ups,pretty=0)

tree.pred=predict(prune.ups,upselling_valid,type="vector")
ypred=rep('no',5000)
ypred[tree.pred[,2] > 0.2] = "yes"
table(ypred,y)




#appetency

tree.appetency=tree(y_appetency~.,appetency_train)
summary(tree.appetency)
plot(tree.appetency)
text(tree.appetency,pretty=0)
tree.appetency
y=appetency_valid$y_appetency
tree.pred=predict(tree.appetency,appetency_valid,type="vector")
ypred=rep('no',5000)
ypred[tree.pred[,2] > 0.2] = "yes"
table(ypred,y)
roc_auc(tree.pred[,2],y)



cv.app=cv.tree(tree.appetency)
plot(cv.app$size,cv.app$dev,type='b')

prune.app=prune.tree(tree.app,best=8)
plot(prune.app)
text(prune.app,pretty=0)

tree.pred=predict(prune.app,appetency_valid,type="vector")
ypred=rep('no',5000)
ypred[tree.pred[,2] > 0.2] = "yes"
table(ypred,y)
roc_auc(tree.pred[,2],y)




#churn
tree.churn=tree(y_churn~.,churn_train)
summary(tree.churn)
plot(tree.churn)
text(tree.churn,pretty=0)
tree.churn
y=churn_valid$y_churn
tree.pred=predict(tree.churn,churn_valid,type="vector")
ypred=rep('no',5000)
ypred[tree.pred[,2] > 0.2] = "yes"
table(ypred,y)
roc_auc(tree.pred[,2],y)

cv.churn=cv.tree(tree.churn)
plot(cv.churn$size,cv.churn$dev,type='b')

prune.churn=prune.tree(tree.churn,best=9)
plot(prune.churn)
text(prune.churn,pretty=0)


tree.pred=predict(prune.churn,churn_valid,type="vector")
ypred=rep('no',5000)
ypred[tree.pred[,2] > 0.2] = "yes"
table(ypred,y)
roc_auc(tree.pred[,2],y)




#~~~~~~~~~~~~Logistic~~~~~~~~~~~
#
#appetency
library(ISLR)
training_data = appetency_train
testing_data = appetency_valid
testing_y = appetency_valid$y_appetency


#Using the Important Predictors Generated from the RandomForest
logistic_model = glm(y_appetency ~ Var126+Var113+Var218.1+Var81+Var28+Var153+Var133+Var73+Var38+Var125+Var134+Var6+Var13+Var119, data = training_data, family = "binomial")
logistic_probs = predict(logistic_model, testing_data, type =    "response")
head(logistic_probs)
logistic_pred_y = rep("no", length(testing_y)) 
logistic_pred_y[logistic_probs > 0.05] = "yes"
table(logistic_pred_y, testing_y)
mean(logistic_pred_y != testing_y)


roc_auc(logistic_probs,testing_y)



#churn
training_data = churn_train
testing_data = churn_valid 
testing_y = churn_valid$y_churn



logistic_model = glm(y_churn ~Var126+Var73+Var81+Var28+Var153+Var140+Var133+Var134+Var38, data = training_data, family = "binomial")
logistic_probs = predict(logistic_model, testing_data, type =    "response")
head(logistic_probs)
logistic_pred_y = rep("no", length(testing_y)) 
logistic_pred_y[logistic_probs > 0.3] = "yes"
table(logistic_pred_y, testing_y)
mean(logistic_pred_y != testing_y)

roc_auc(logistic_probs,testing_y)


#upselling
training_data = upselling_train
testing_data = upselling_valid
testing_y = upselling_valid$y_upselling
logistic_model = glm(y_upselling ~Var126+Var28+Var13+Var125+Var73+Var134+Var38, data = training_data, family = "binomial")
logistic_probs = predict(logistic_model, testing_data, type =    "response")
head(logistic_probs)
logistic_pred_y = rep("no", length(testing_y)) 
logistic_pred_y[logistic_probs > 0.3] = "yes"
table(logistic_pred_y, testing_y)
mean(logistic_pred_y != testing_y)

roc_auc(logistic_probs,testing_y)




#~~~~~~~~~~SVM~~~~~~~~~~~~~~~~`
#
#UsinG important predictors
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Upsellling
#using parellal computing
detectCores()
cl=makeCluster(3)
registerDoParallel(cl)
getDoParWorkers()



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


svm_ups=ksvm(y_upselling~.,data=ups_train_dummy,
             type='C-svc',scaled=T,kernel='rbfdot',
             cost=10,prob.model=T,kpar=list(sigma=.1))
ypred=predict(svm_ups,ups_valid_dummy,type="probabilities")


ups_pred_y = rep("no", length=5000) 
ups_pred_y[ypred[,2] > 0.2] = "yes"

table(predict=ups_pred_y,truth=ups_valid_dummy$y_upselling)



pred <- prediction(ypred[,2],upselling_valid[,1])
# Plot ROC curve
prf <- performance(pred, measure ="tpr",x.measure ="fpr")
auc=performance(pred,measure = "auc")
auc=auc@y.values[[1]]
plot(prf,colorize=TRUE,main=paste("ROC curve with AUC=",auc))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
#churn

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
prf <- performance(pred, measure ="tpr", x.measure ="fpr")
auc=performance(pred,measure = "auc")
auc=auc@y.values[[1]]
plot(prf,colorize=TRUE,main=paste("ROC curve with AUC=",auc))


#appetency

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


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
prf <- performance(pred, measure ="tpr", x.measure ="fpr")
auc=performance(pred,measure = "auc")
auc=auc@y.values[[1]]
plot(prf,colorize=TRUE,main=paste("ROC curve with AUC=",auc))


stopCluster(cl)







#~~~~~~~~~~~~~~~ANN~~~~~~~~~~~~~~~~~~~~
#H2O Deep Learning models have many input parameters, many of which are only accessible
#via the expert mode. Here we mostly used default in many parameters. And concentrated on
#important parameters like hidden layer nodes, dropout ratio, and activation function.
#For deep learning we used parallel computing to decrease the computational time. Using the
#code,
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
#In order to train models with the H2O engine, I need to link the datasets to the H2O cluster
#first. There are many ways to do it. In this case, I linked a data frame (Upselling train) using
#the following code.


datx = colnames(upselling_train[2:59])
daty = colnames(upselling_train[1])
trh=as.h2o(upselling_train,destination_frame = "trh")
tth=as.h2o(upselling_valid,destination_frame="tth")

#training
#Early stopping (stop training before the specified number of epochs is completed to prevent
#over fitting) is enabled by default. If a validation frame is given, or if cross-validation is used
#(nfolds > 1), it will use validation error to determine the early stopping point. If just a training
#frame is given (and no CV), it will use the training set to perform early stopping. More on that
#below
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



roc_auc(df_yhat_test[,3],upselling_valid[,1])



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
#Appetency
#import dataset with r studio inbuilt function



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



#~~~~~~~~~~~~~Multilabel~~~~~~~~~~~~~~~

#We have used mlr library, which is a powerful and modularized toolbox for machine learning
#in R. The package offers a unified interface to more than a hundred learners from the areas
#classification, regression, cluster analysis and survival analysis.

# Furthermore, the package provides functions and tools that facilitate complex workflows such as hyper parameter and
#feature selection that can now also be applied to the multi label classification methods. In the
#following, we list the algorithm adaptation methods and problem transformation methods that
#are currently available in mlr

#The first task of multi label classification is to create a task. Learning tasks allow us to
#encapsulate the data set and specify information about a machine learning task. The first thing
#you have to do for multi label classification in mlr is to get your data in the right format. You
#need a data. Frame which consists of the features and a logical vector for each label which
#indicates if the label is present in the observation or not. This is important step as in real
#scenario classification problem have response as a factor but, here we are using logical
#argument as a response.

#Import dataset
total_final.with.y <-read.table('total_final with y.csv',header=TRUE, sep=',',stringsAsFactors=TRUE)
fianl.with.y=total_final.with.y[,5:65]

z=fianl.with.y
#converting output to true false
fianl.with.y$appetency=mapvalues(fianl.with.y$appetency,c('yes','no'),to=c('TRUE','FALSE'))
fianl.with.y$churn=mapvalues(fianl.with.y$churn,c('yes','no'),to=c('TRUE','FALSE'))
fianl.with.y$upselling=mapvalues(fianl.with.y$upselling,c('yes','no'),to=c('TRUE','FALSE'))

#converting factor into logical argument
fianl.with.y$appetency=as.logical(fianl.with.y$appetency)
fianl.with.y$churn=as.logical(fianl.with.y$churn)
fianl.with.y$upselling=as.logical(fianl.with.y$upselling)





#making task

set.seed(1)

#CRP = getTaskData(fianl.with.y)
labels = colnames(fianl.with.y)[59:61]
crp.task = makeMultilabelTask(id = "multi", data = fianl.with.y, target = labels)

#1=classifier chain approach together with a decision tree for the binary classification problems
binary.learner = makeLearner("classif.rpart", predict.type = "prob")
lrncc = makeMultilabelClassifierChainsWrapper(binary.learner)

fianl.with.y=z[1:10000,]
n = 10000
train.set = seq(1, n, by = 2)
test.set = seq(2, n, by = 2)
crp.mod.cc = train(lrncc, crp.task,subset=train.set)
crp.pred.cc = predict(crp.mod.cc, task = crp.task, subset=test.set)
listMeasures("multilabel")
performance(crp.pred.cc, measures = list(multilabel.f1, multilabel.acc))
getMultilabelBinaryPerformances(crp.pred.cc, measures = list(acc, mmce, auc))



#2 multilabel binary classifier
lrncc = makeMultilabelClassifierChainsWrapper(binary.learner)
crp.mod.cc = train(lrncc, crp.task,subset=train.set)
crp.pred.cc = predict(crp.mod.cc, task = crp.task, subset=test.set)
performance(crp.pred.cc, measures = list(multilabel.hamloss, multilabel.subset01, multilabel.f1, multilabel.acc))
getMultilabelBinaryPerformances(crp.pred.cc, measures = list(acc, mmce, auc))


#3 multilabel random forest learner with package randomForestSRC
set.see(1)
lrn.rfsrc = makeLearner("multilabel.randomForestSRC", predict.type = "prob")
crp.mod.cc = train(lrn.rfsrc, crp.task,subset=train.set)
crp.pred.cc = predict(crp.mod.cc, task = crp.task, subset=test.set)
performance(crp.pred.cc, measures = list(multilabel.hamloss, multilabel.subset01, multilabel.f1, multilabel.acc))
getMultilabelBinaryPerformances(crp.pred.cc, measures = list(acc, mmce, auc,tpr,fpr))


#4 multilabel random Forest with rferns package
lrn.rFerns = makeLearner("multilabel.rFerns")
crp.mod.cc = train(lrn.rFerns, crp.task,subset=train.set)
crp.pred.cc = predict(crp.mod.cc, task = crp.task, subset=test.set)
performance(crp.pred.cc, measures = list(multilabel.hamloss, multilabel.subset01, multilabel.f1, multilabel.acc))
getMultilabelBinaryPerformances(crp.pred.cc, measures = list(acc, mmce, auc))


#5Multilabel classification
lrn.br = makeLearner("classif.rpart", predict.type = "prob")
lrn.br = makeMultilabelBinaryRelevanceWrapper(lrn.br)
crp.mod.cc = train(lrn.br, crp.task,subset=train.set)
crp.pred.cc = predict(crp.mod.cc, task = crp.task, subset=test.set)
performance(crp.pred.cc, measures = list(multilabel.f1, multilabel.acc))
getMultilabelBinaryPerformances(crp.pred.cc, measures = list(acc, mmce, auc))









