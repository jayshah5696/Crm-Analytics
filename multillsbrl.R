
library(mlr)
library(plyr)
#mImport dataset
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
performance(crp.pred.cc, measures = list(multilabel.f1, multilabel.acc,multilabel.tpr))
getMultilabelBinaryPerformances(crp.pred.cc, measures = list(acc, mmce, auc))


roc=calculateROCMeasures(crp.pred.cc)

#2 multilabel binary classifier
lrncc = makeMultilabelBinaryRelevanceWrapper(binary.learner)
crp.mod.cc = train(lrncc, crp.task,subset=train.set)
crp.pred.cc = predict(crp.mod.cc, task = crp.task, subset=test.set)
performance(crp.pred.cc, measures = list(multilabel.hamloss, multilabel.subset01, multilabel.f1, multilabel.acc))
getMultilabelBinaryPerformances(crp.pred.cc, measures = list(acc, mmce, auc))


#3 multilabel random forest learner with package randomForestSRC
lrn.rfsrc = makeLearner("multilabel.randomForestSRC", predict.type = "prob")
crp.mod.cc = train(lrn.rFerns, crp.task,subset=train.set)
crp.pred.cc = predict(crp.mod.cc, task = crp.task, subset=test.set)
performance(crp.pred.cc, measures = list(multilabel.hamloss, multilabel.subset01, multilabel.f1, multilabel.acc))



#4 multilabel random Forest with rferns package
lrn.rFerns = makeLearner("multilabel.rFerns")
crp.mod.cc = train(lrn.rfsrc, crp.task,subset=train.set)
crp.pred.cc = predict(crp.mod.cc, task = crp.task, subset=test.set)
performance(crp.pred.cc, measures = list(multilabel.hamloss, multilabel.subset01, multilabel.f1, multilabel.acc))
getMultilabelBinaryPerformances(crp.pred.cc, measures = list(acc, mmce, auc))


#5Multilabel classification using svm
lrn.br = makeLearner("classif.svm", predict.type = "prob")
lrn.br = makeMultilabelNestedStackingWrapper(lrn.br,cv.folds = 10)
crp.mod.cc = train(lrn.br, crp.task,subset=train.set)
crp.pred.cc = predict(crp.mod.cc, task = crp.task, subset=test.set)
performance(crp.pred.cc, measures = list(multilabel.f1, multilabel.acc))
getMultilabelBinaryPerformances(crp.pred.cc, measures = list(acc, mmce, auc))

