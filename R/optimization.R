library(xgboost)
library(randomForest)
library(rBayesianOptimization)
library(pROC)

#' An internal function used to fit the XGB model for some given set
#' of hyperparameters defiend as arguments. This is not used directly,
#' and is a subroutine of `optimize_XGB`
xgb_cv_bayes <- function(n_estimators, eta, max_depth, subsample, colsample_bytree) {
  cv <- xgb.cv(params = list(
    n_estimators = n_estimators,
    eta = eta,
    max_depth = max_depth,
    subsample = subsample,
    colsample_bytree = colsample_bytree,
    objective = "binary:logistic",
    eval_metric = "auc"
  ), data = dtrain, nrounds=100, folds=cv_folds, prediction = TRUE, showsd=TRUE, early_stopping_rounds = 5, maximize=TRUE, verbose=1)

  cv_eval <- list(Score = cv$evaluation_log[cv$best_iteration, "test_auc_mean"],
       Pred = cv$pred)
  return(cv_eval)
}

#' Optimizes the XGB configuration.
#'
#' @param thin_frame A dataframe of lidar covariates and a column that specifies
#' THIN or NO THIN categories. Typically made with the `assign_plots` function
#' @param n_folds The number of cross-validation folds to use. Default is 10
#' @return A list containing the best hyperparameter set (see `BayesianOptimization` documentation)
optimize_XGB <- function(thin_frame, nfolds = 10) {
  # These have to be saved to parent scope, since I cannot pass arbitrary arguments to through `BayesianOptimization`
  dtrain   <<- xgb.DMatrix(as.matrix(thin_frame[,colnames(thin_frame) != "thinnable"]), label = thin_frame$thinnable)
  cv_folds <<- KFold(thin_frame$thinnable, nfolds = nfolds, stratified = TRUE)

  bo <- BayesianOptimization(xgb_cv_bayes, bounds=list(n_estimators = c(1L, 101L),
                       eta = c(0.01, 0.3),
                       max_depth = c(1L, 31L),
                       subsample = c(0.1, 1),
                       colsample_bytree = c(0.1, 1)),
                       init_grid_dt = NULL, init_points = 10, n_iter = 20, acq = "ucb", kappa = 2.576, eps = 0.0, verbose = TRUE)

  xgb <- xgboost(data = dtrain, n_estimators = bo$Best_Par[[1]], eta = bo$Best_Par[[2]],
                 max_depth = bo$Best_Par[[3]], subsample = bo$Best_Par[[4]], colsample_bytree = bo$Best_Par[[5]],
                 objective = "binary:logistic", eval="auc", nrounds=bo$Best_Par[[1]])

  xgb$mod_type <- "xgb"

  return(xgb)
}

#' Unlike XGB, RF does not come with a pre-baked CV method, so we need our own.
#' This is used internally by the `rf_cv_bayes` function only.
#'
#' @return A list containing a vector of the cross-validated predictions
#' (Preds), and cross-validated aucs (aucs)
rf.cv <- function(mtry, nodesize, ntree) {
  pred_df <- data.frame(pred = rep(0, nrow(thin_frame)))
  aucs <- c()

  for(fold in cv_folds) {
    test <- thin_frame[fold,]
    train <- thin_frame[-fold,]

    x_train <- train[,colnames(train) != "thinnable"]
    y_train <- as.factor(train[,"thinnable"])

    x_test <- test[,colnames(test) != "thinnable"]
    y_test <- as.factor(test[,"thinnable"])

    rf <- randomForest(x_train, y_train, mtry=mtry)
    pred <- predict(rf, x_test, type="prob")[,2]
    ix <- as.numeric(names(pred))
    pred_df[ix, 1] <- pred

    fold_roc <- roc(y_test, pred, levels = c(0,1), direction = "<")
    fold_auc <- as.numeric(fold_roc$auc)
    aucs <- c(aucs, fold_auc)
  }
  return(list(Preds = pred_df[,1], aucs=aucs))
}

#' An internal function used to fit the XGB model for some given set
#' of hyperparameters defiend as arguments. This is not used directly,
#' and is a subroutine of `optimize_RF`
rf_cv_bayes <- function(mtry, nodesize) {
  cv <- rf.cv(mtry, nodesize, 1000)
  cv_eval <- list(Score = mean(cv$aucs), Pred = cv$Preds)
  return(cv_eval)
}

#' Optimizes the XGB configuration.
#'
#' @param thin_frame A dataframe of lidar covariates and a column that specifies
#' THIN or NO THIN categories. Typically made with the `assign_plots` function
#' @param n_folds The number of cross-validation folds to use. Default is 10
#' @return A random forest model, trained on all available data, using the best hyperparameter set.
optimize_RF <- function(thin_frame, nfolds = 10) {
  thin_frame <<- thin_frame
  cv_folds <<- KFold(thin_frame$thinnable, nfolds = nfolds, stratified = TRUE)

  bo <- BayesianOptimization(rf_cv_bayes, bounds=list(mtry=c(1L, 10L), nodesize=c(6L, 101L)),
                       init_grid_dt = NULL, init_points = 10, n_iter = 20,
                       acq = "ucb", kappa = 2.576, eps = 0.0, verbose = TRUE)

  rf <- randomForest(thin_frame[, colnames(thin_frame) != "thinnable"], y = as.factor(thin_frame$thinnable), mtry=bo$Best_Par[[1]],
                     nodesize=bo$Best_Par[[2]])
  rf$mod_type <- "rf"

  return(rf)
}

#' Selects the optimum threshold for producing binary predictions such
#' that kappa is maximized.
#'
#' @param model Either and RF or XGB model.
select_threshold <- function(thin_frame, model) {
  response <- thin_frame$thinnable

  if (model$mod_type == "rf") { # if the model is RF
    pred <- as.numeric(predict(model, type='prob')[,2])
  } else { # the model is XGB
    dmat <- xgb.DMatrix(as.matrix(thin_frame[,colnames(thin_frame) != "thinnable"]))
    pred <- predict(model, newdata=dmat)
  }

  n <- nrow(thin_frame)
  best_thresh <- 0
  best_kappa <- 0

  for (t in seq(0, 1, by = 0.01)) {
    pred_bool <- pred > t
    confusion_mat <- table(pred_bool, response)

    if(dim(confusion_mat)[[1]] == 2) {
      tn  <- confusion_mat[[1]]
      tp <- confusion_mat[[4]]
      fp <- confusion_mat[[2]]
      fn <- confusion_mat[[3]]

      p_0 <- (tp + tn) / n
      p_e <- (((tn + fn)/ n) * ((tn + fp)/ n)) + (((fp + tp)/n) * ((fn + tp) /n))

      kappa <- (p_0 - p_e) / (1 - p_e)
      if(kappa > best_kappa) {
        best_kappa <- kappa
        best_thresh <- t
      }
    }
  }
  return(best_thresh)
}
