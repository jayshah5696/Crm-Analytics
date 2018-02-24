# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(format(utils::object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

# shorthand
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

lsos()




df=mice(train_60, imputationMethod   = c("norm","logreg","polyreg","pmm","mean","logreg2","lda","sample"))

df=mice(train_60, imputationMethod   = c("norm","logreg","polyreg","pmm","mean","logreg2","lda","sample"))

df=mice(train_60,method ="norm",maxit=1,seed=1 )
gc()
#install.packages("HotDeckImputation")
library(HotDeckImputation)
x2=data.matrix(X, rownames.force = NA)
x1=impute.CPS_SEQ_HD(DATA = x2,covariates = NULL)
x2=impute.NN_HD(DATA = X, distance = "man", weights = "range", attributes = "sim",
                comp = "rseq", donor_limit = Inf, optimal_donor = "no",
                list_donors_recipients = NULL, diagnose = NULL)
warnings()
?impute.mean
input_feature_names_train = names(train_60)

x3=impute.mean(X[,num,with=F])
z=as.numeric(X)
num<- sapply(X,is.numeric)
input_feature_classes <- factor(sapply(train_60, class))
numeric_input_feature_names <-  input_feature_names_train[input_feature_classes != 'factor']
numeric_input_feature_names

numeric_input_feature_standard_deviations <-
  sapply(train_60[ , numeric_input_feature_names, with=FALSE],
         function(col) sd(col, na.rm=TRUE))
numeric_input_feature_standard_deviations

z=train_60[ , numeric_input_feature_names, with=FALSE]
tr=mice(z)
library(DMwR)
tr=knnImputation(z)
tr=impute.NN_HD(z)
?impute.NN_HD
