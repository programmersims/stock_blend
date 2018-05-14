evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  preds[preds<0] <- 0.01
  err <- -log(xor1(preds, as.numeric(labels)))
  err <- mean(err)/sd(err)
  return(list(metric = "error", value = err))
}

get_xgb <- function(data, target, depth=c(4,5,6,7,8), alpha=c(0.05, 0.1, 0.2), subsamp=c(0.5,0.7),  min_weight=c(0,100)) {
  preds_xgb_best <- rexp(n=length(target))
  n <- 1
  full_models <- list()
  for(i in depth)
    for(j in alpha)
      for(k in subsamp)
        for(h in min_weight){
          
          data_sparse <- sparse.model.matrix(~-1 + ., data=as.data.frame(data))
          dtrain <-xgb.DMatrix(data=data_sparse,label=(target))
          
          model_gradient_tree <- xgb.cv(data = dtrain,
                                        nfold=10,
                                        nthread = 3,
                                        nrounds = 50000,
                                        max_depth = i,
                                        alpha = j,
                                        eta = 0.005,
                                        subsample = k,
                                        colsample_bytree = 0.70,
                                        booster = "gbtree",
                                        #feval = evalerror,
                                        eval_metric = 'mae',
                                        #metrics = list("mae"),
                                        maximize = FALSE,
                                        objective = "reg:linear",
                                        print.every.n = 1000,
                                        verbose = TRUE,
                                        min_child_weight=h,
                                        prediction=TRUE,
                                        early_stopping_rounds=20,
                                        watchlist = list(train = dtrain))
          
          l <- list()
          l[[1]] <- model_gradient_tree$pred
          l[[2]] <- colnames(data)
          l[[3]] <- i
          l[[4]] <- j
          l[[5]] <- k
          l[[6]] <- h
          l[[7]] <- model_gradient_tree$niter
          full_models[[n]] <- l
          n <- n+1
        }
  return(full_models)
}


#@description: remove bad covariates until cross-validated risk increases: returns vector of best covartiates

#@param data - dataframe of covariates
#@param target - target vector
#@param selecting_fetures - number of features to remove every loop

#@return  vector of best covartiates

get_xgb_predictors <- function(data, target, selecting_features=10) {
  preds_xgb_best <- rexp(n=length(target))
  set.seed(1)
  folds <- cvFolds(n = length(target), K = 10, type = "random")
  old <- colnames(data)
  new <- colnames(data)
  best <- new
  model_feat <- list()
  i <- 0
  j <- 0
  
  while(((length(new)-selecting_features) > 1) & i <5){
    
    data_sparse <- sparse.model.matrix(~-1 + ., data=as.data.frame(data[,new]))
    dtrain <-xgb.DMatrix(data=data_sparse,label=(target))
    
    model_gradient_tree <- xgb.cv(data = dtrain,
                                  nfold=10,
                                  nthread = 3,
                                  nrounds = 50000,
                                  max_depth = 7,
                                  alpha = 0.2,
                                  eta = 0.005,
                                  subsample = .5,
                                  colsample_bytree = 0.70,
                                  booster = "gbtree",
                                  #feval = evalerror,
                                  eval_metric = 'mae',
                                  #metrics = list("mae"),
                                  maximize = FALSE,
                                  objective = "reg:linear",
                                  print.every.n = 1000,
                                  verbose = TRUE,
                                  min_child_weight=100,
                                  prediction=TRUE,
                                  early_stopping_rounds=20,
                                  watchlist = list(train = dtrain))
    
    preds_new <- model_gradient_tree$pred
    preds_xgb_best <- compare_models(preds_xgb_best, preds_new, target)
    if(all(preds_xgb_best==preds_new)){
      i == 0
      best <- new
    } else {
      i <- i+1
    }
    
    model_gradient <- xgb.train(data = dtrain,
                                nfold=10,
                                nthread = 3,
                                nrounds = model_gradient_tree$niter,
                                max_depth = 7,
                                alpha = 0.2,
                                eta = 0.005,
                                subsample = .5,
                                colsample_bytree = 0.70,
                                booster = "gbtree",
                                #feval = evalerror,
                                eval_metric = 'mae',
                                #metrics = list("mae"),
                                maximize = FALSE,
                                objective = "reg:linear",
                                print.every.n = 1000,
                                verbose = TRUE,
                                min_child_weight=100,
                                prediction=TRUE,
                                watchlist = list(train = dtrain))
    
    imp <- xgb.importance(colnames(dtrain), model = model_gradient)
    if(length(new) > 100) {
      if(length(imp$Feature)>100){
        new <- imp$Feature[1:100]
        selecting_features <- 10
      } else{
        new <- imp$Feature
        selecting_features <- selecting_features <- 5
        new <- imp$Feature[1:(length(imp$Feature)-selecting_features)]
      }
    } else{
      selecting_features <- 5
      new <- imp$Feature[1:(length(imp$Feature)-selecting_features)]
    }
  }
  return(best)
}
