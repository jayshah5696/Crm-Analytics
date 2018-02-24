library(h2o)
demo(h20)
localH2O = h2o.init()
h2o.init(nthreads=-1, max_mem_size="2G")
localH2O = h2o.init(ip = "127.0.0.1", port = 54321, startH2O = TRUE)
h2o.removeAll()
args(h2o.deeplearning)
help(h2o.deeplearning)
example(h2o.deeplearning)
demo(h2o.deeplearning)
<Return>

#While H2O Deep Learning has many parameters, it was designed to be just as easy to use as the other supervised training methods in H2O. Early stopping, automatic data standardization and handling of categorical variables and missing values and adaptive learning rates (per weight) reduce the amount of parameters the user has to specify. Often, it's just the number and sizes of hidden layers, the number of epochs and the activation function and maybe some regularization techniques
rm(list = ls())
localH2O <- h2o.init(max_mem_size = '1g')
library(ISLR)
attach(Default)


ids = sample(1:nrow(Default),8000)
train= Default[ids, ]
test= Default[-ids, ]

upselling_train=upselling_train[,-1]
upselling_valid=upselling_valid[,-1]



datx = colnames(upselling_train[2:59])
daty = colnames(upselling_train[1])
trh=as.h2o(upselling_train,destination_frame = "trh")
tth=as.h2o(upselling_valid,destination_frame="tth")



as.h2o(localH2O,test,key='tes',destination_frame = "validation")
train_hex <- h2o.importFile(localH2O,trh20)
test_hex <- h2o.importFile(localH2O, path = path_test)
valt <- as.h2o(localH2O, test)
?as.h2o
getwd()
write.csv(test, "test.csv")
path2 = paste0("C:/Users/Lenovo/test.csv")
tth2 = h2o.importFile(path=path2)

model <- 
  h2o.deeplearning(x = datx,  # column numbers for predictors
                   y = daty,   # column number for label
                   training_frame = "trh",# data in H2O format
                   activation = "TanhWithDropout", # or 'Tanh'
                   input_dropout_ratio = 0.2, # % of inputs dropout
                   hidden_dropout_ratios = c(0.5,0.5,0.5), # % for nodes dropout
                   balance_classes = TRUE, 
                   hidden = c(10,10), # three layers of 50 nodes
                   epochs = 1000, # max. no. of epochs
                   variable_importances=T,
                   stopping_rounds=2,
                   stopping_metric="misclassification", ## could be "MSE","logloss","r2"
                   stopping_tolerance=0.01)

print(model)



pr =h2o.predict(model,tth)
df_yhat_test <- as.data.frame(pr)
summary(df_yhat_test)
table(df_yhat_test[,1],upselling_valid[,1])


# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
if (! ("methods" %in% rownames(installed.packages()))) { install.packages("methods") }
if (! ("statmod" %in% rownames(installed.packages()))) { install.packages("statmod") }
if (! ("stats" %in% rownames(installed.packages()))) { install.packages("stats") }
if (! ("graphics" %in% rownames(installed.packages()))) { install.packages("graphics") }
if (! ("RCurl" %in% rownames(installed.packages()))) { install.packages("RCurl") }
if (! ("rjson" %in% rownames(installed.packages()))) { install.packages("rjson") }
if (! ("tools" %in% rownames(installed.packages()))) { install.packages("tools") }
if (! ("utils" %in% rownames(installed.packages()))) { install.packages("utils") }


# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/master/3092/R")))
library(h2o)
localH2O = h2o.init()
