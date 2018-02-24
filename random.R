library(ISLR)
names(Default)
dim(Default)
install.packages("MXNet")
library(MXNet)
library(h2o)
library(nnet)
library(deepnet)
h2o.init(nthreads=-1, max_mem_size="2G")
attach(Default)
names(Default)
?Credit
install.packages("neuralnet")
library(neuralnet)
set.seed(123)

fix(df)
?neuralnet
fix(Credit)
fix(Default)
df=Default
df$default<-as.numeric(as.factor(Default$default))
df$student<-as.numeric(Default$student)
def <- as.data.frame(scale(df[,2:4]),na.rm=T)
fix(def2)
def2=data.frame(CA,def)
formula= default~.
nn=neuralnet(dd~student+balance+income,hidden = 1,threshold = 0.01,err.fct ="ce" ,data=def2,linear.output = F,stepmax = 1e+05, algorithm = "backprop",learningrate = 1)
nn=neuralnet(dd~student+balance+income,hidden = 2,threshold = 0.01,err.fct ="ce" ,data=def2,linear.output = F,stepmax = 1e+05,rep=10)
plot(nn)
nn$result.matrix
library(dplyr)

library(tidyr)
Default$default %>%
  mutate(value = 1,
         c = paste0("Is", c)) %>%
  spread(c, value, fill = 0)

CA <- ifelse(Default$default=="Yes", 0, 1)
