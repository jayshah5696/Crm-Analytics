library(caret)
rf.ups =randomForest(y_appetency~.,data=appetency_train, mtry=52,importance =TRUE)
yhat=predict(rf.ups,appetency_valid)
y=appetency_valid$y_appetency
table(yhat,y)
rf.ups$importance
varImpPlot(rf.ups)

rf.ups =randomForest(y_churn~.,data=churn_train, mtry=48,importance =TRUE)
yhat=predict(rf.ups,churn_valid)
y=churn_valid$y_churn
table(yhat,y)
rf.ups$importance
varImpPlot(rf.ups)

rf.ups =randomForest(y_upselling~.,data=upselling_train, mtry=48,importance =TRUE)
yhat=predict(rf.ups,upselling_valid)
y=upselling_valid$y_upselling
table(yhat,y)
rf.ups$importance
varImpPlot(rf.ups)