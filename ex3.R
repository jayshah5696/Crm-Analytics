
library(caret)
library(data.table)
na_strings <- c(
  '',
  'na', 'n.a', 'n.a.',
  'nan', 'n.a.n', 'n.a.n.',
  'NA', 'N.A', 'N.A.',
  'NaN', 'N.a.N', 'N.a.N.',
  'NAN', 'N.A.N', 'N.A.N.',
  'nil', 'Nil', 'NIL',
  'null', 'Null', 'NULL')
X <-read.table('orange_small_train1.data',header=TRUE, sep='\t',stringsAsFactors=TRUE, na.strings=na_strings)

nb_input_features <- ncol(X)
input_feature_names <- names(X)
nb_samples <- nrow(X)

# read y labels
churn <- factor(read.table('orange_small_train_churn.labels',header=FALSE, sep='\t')[[1]],levels=c(-1, 1),labels=c('no', 'yes'))
appetency <- factor(read.table('orange_small_train_appetency.labels',header=FALSE, sep='\t')[[1]],levels=c(-1, 1),labels=c('no', 'yes'))
upselling <- factor(read.table('orange_small_train_upselling.labels',header=FALSE, sep='\t')[[1]],levels=c(-1, 1),labels=c('no', 'yes'))


comp_x= cbind.data.frame(X,churn,appetency)


train_proportion <- .4
train_indices <- createDataPartition(
  y=comp_x$churn,
  p=train_proportion,
  list=FALSE)
X_train <- X[train_indices, ]
X_test <- X[-train_indices, ]
churn_train <- comp_x$churn[train_indices]
churn_test <- comp_x$churn[-train_indices]
nb_test_samples <- length(churn_test)

valid_proportion <- .25
valid_indices <- createDataPartition(
  y=churn_train,
  p=valid_proportion,
  list=FALSE)

X_valid <- X_train[valid_indices, ]
X_train <- X_train[-valid_indices, ]
churn_valid <- churn_train[valid_indices]
churn_train <- churn_train[-valid_indices]
nb_train_samples <- length(churn_train)
nb_valid_samples <- length(churn_valid)

#Getting Rid of Input Features $x$'s with Too Many Missing Values
input_features_missing_proportions <-
  sapply(X_train, function(col) sum(is.na(col))) / nb_train_samples
hist(input_features_missing_proportions)
input_feature_names <-
  input_feature_names[input_features_missing_proportions <= .6]
nb_input_features <- length(input_feature_names)
X_train <- X_train[ , input_feature_names, (with=FALSE)]

#We're left with the following r nb_input_features input features $x$'s:
input_feature_names
#The classes of these remaining $x$'s are:
#The classes of these remaining $x$'s are:
input_feature_classes <- factor(sapply(X_train, class))
input_feature_classes
numeric_input_feature_names <-input_feature_names[input_feature_classes != 'factor']
numeric_input_feature_names
numeric_input_feature_standard_deviations <-sapply(X_train[ , numeric_input_feature_names, (with=FALSE)],function(col) sd(col, na.rm=TRUE))
numeric_input_feature_standard_deviations

# Let's fill up the missing values with the means of the respective columns:
numeric_input_feature_means <-
  sapply(X_train[ , numeric_input_feature_names, (with=FALSE)],
         function(col) mean(col, na.rm=TRUE))
means1=rep(0,42)
counter=0
for (numeric_col in numeric_input_feature_names) {
  x <- X_train[[numeric_col]]
  missing_value_row_yesno <- is.na(x)
  if (sum(missing_value_row_yesno) > 0) {
    X_train[ , numeric_col]=as.numeric(x)
    mu <- numeric_input_feature_means[numeric_col]
    counter=counter+1
    means1[counter]=mu
    X_train[missing_value_row_yesno, numeric_col]=mu
  }
}
all.equal(
  numeric_input_feature_means,
  sapply(X_train[ , numeric_input_feature_names, (with=FALSE)], mean))

#Cleaning Categorical Variables
categorical_input_feature_names <-
  input_feature_names[input_feature_classes == 'factor']

categorical_input_feature_nb_levels <-
  sapply(X_train[ , categorical_input_feature_names, (with=FALSE)],
         function(col) length(levels(col)))
categorical_input_feature_nb_levels


categorical_input_feature_names <-
  categorical_input_feature_names[categorical_input_feature_nb_levels <= 500]

X_train <-
  X_train[ , c(numeric_input_feature_names, categorical_input_feature_names), (with=FALSE)] #Those variables having over 500 categories are likely to be just text / character data. Let's get rid of them:

for (cat_col in categorical_input_feature_names) {
  missing_value_row_yesno <- is.na(X_train[[cat_col]])
  #if (sum(missing_value_row_yesno) > 0) {
  #X_train[missing_value_row_yesno, cat_col]='xxxx'
}