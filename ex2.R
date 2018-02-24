

categorical_input_feature_names <-
  input_feature_names[input_feature_classes == 'factor']
categorical_input_feature_nb_levels <-


categorical_input_feature_names <-
  categorical_input_feature_names[categorical_input_feature_nb_levels <= 500]
X_train <-
  X_train[ , c(numeric_input_feature_names, categorical_input_feature_names), with=FALSE]


collapsed_categories <- list()
for (cat_col in categorical_input_feature_names) {
  missing_value_row_yesno <- is.na(X_train[[cat_col]])
  if (sum(missing_value_row_yesno) > 0) {
    X_train[missing_value_row_yesno, cat_col := 'zzzMISSING', with=FALSE]
  }
  x <- X_train[[cat_col]]
  for (cat in levels(x)) {
    cat_rows_yesno <- x == cat
    if (sum(cat_rows_yesno) < .05 * nb_train_samples) {
      if (!(cat_col %in% names(collapsed_categories))) {
        collapsed_categories[[cat_col]] <- character()
      }
      collapsed_categories[[cat_col]] <- c(collapsed_categories[[cat_col]], cat)
      X_train[cat_rows_yesno, cat_col := 'zzzOTHER', with=FALSE]
      levels(X_train[[cat_col]])[levels(X_train[[cat_col]]) == cat] <- NA
    }
  }
  cats <- levels(X_train[[cat_col]])
  if ((length(cats) == 1) ||
      (length(cats[(cats != 'zzzMISSING') & (cats != 'zzzOTHER')]) < 2)) {
    categorical_input_feature_names <- setdiff(categorical_input_feature_names, cat_col)
  }
}``