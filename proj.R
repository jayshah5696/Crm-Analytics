#Importing Dataset and lables
train_path = "F:/JAY/COURSES/ISEN 613/Project"
library(data.table)
nastrings <- c(
  '',
  'nan', 'n.a.n', 'n.a.n.',
  'NAN', 'N.A.N', 'N.A.N.',
  'NA', 'N.A', 'N.A.',
  'NaN', 'N.a.N', 'N.a.N.',
  'na', 'n.a', 'n.a.',
  'nil', 'Nil', 'NIL',
  'null', 'Null', 'NULL')
#Matrix x-------------------------------------------
X <- as.data.table(read.table(
  file.path(train_path, 'orange_small_train.data'),
  header=TRUE, sep='\t', stringsAsFactors=TRUE, na.strings=nastrings))

#Feature names--------------------------------
input_feature_names = names(X)

#importinglabel Churn ------------------------
churn <- factor(
  read.table(
    file.path(train_path, 'orange_small_train_churn.labels'),
    header=FALSE, sep='\t')[[1]],
  levels=c(-1, 1),
  labels=c('no', 'yes'))

#importing label upsell---------------------------
upsell <- factor(
  read.table(
    file.path(train_path, 'orange_small_train_upselling.labels'),
    header=FALSE, sep='\t')[[1]],
  levels=c(-1, 1),
  labels=c('no', 'yes'))

#importing label apptency--------------------------
appetency <- factor(
  read.table(
    file.path(train_path, 'orange_small_train_appetency.labels'),
    header=FALSE, sep='\t')[[1]],
  levels=c(-1, 1),
  labels=c('no', 'yes'))

#Finding missing value percentage in matrix X
input_features_missing_percentage =  sapply(X, function(col) sum(is.na(col))) /50000

hist(input_features_missing_proportions,col = "blue")


#dealing with missing data

missing_features =sapply(X, function(col) sum(is.na(col)))
#Removing Features having missing values more than 50%
ab=input_features_missing_proportions<0.5
true= input_feature_names[input_features_missing_proportions<0.5]
length(true)
train_60= X[ ,true, with=F]

#Finding categorical column
input_feature_classes = factor(sapply(X_train, class))
input_feature_classes

#imputing continuous column with mean
numeric_input_feature_names =  input_feature_names[input_feature_classes != 'factor']
numeric_input_feature_names
