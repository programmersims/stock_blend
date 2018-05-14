#@param data - dataframe of predictors
#@param target - vector of target

#return - list of cross validated predicted values, covariates used, size, decay, and max iterations

get_lasso_ridge <- function(data, target){
  full_list <- list()
  preds_lasso <- numeric(length(target))
  mod_lasso1 <- cv.glmnet(x = as.matrix(data), y = target)
  set.seed(1)
  folds <- cvFolds(n = length(target), K = 10, type = "random")
  for(i in 1:10){
    invisible(capture.output(mod_lasso <- glmnet(x = as.matrix(data[folds$which!=i, ]), y=(target[folds$which!=i]), lambda=mod_lasso1$lambda.min)))
    preds_lasso[folds$which==i] <- predict(mod_lasso, as.matrix(data[folds$which==i,] ))
  }
  print(paste('Correlation of Lasso:'))
  corr <- cor(preds_lasso, target)
  print(corr)
  print(paste('Annual Sharp Ratio of Lasso:'))
  l <- log(preds_lasso) * log(target)
  sharp <- sqrt(250) * mean(l)/sd(l)
  print(sharp)
  l1 <- list()
  l1[[1]] <- preds_lasso
  l1[[2]] <- mod_lasso1$lambda.min
  l1[[3]] <- 'Lasso'
  full_list[[1]] <- l1
  
  preds_lasso <- numeric(length(target))
  mod_lasso1 <- cv.glmnet(x = as.matrix(data), y = target, alpha=0)
  set.seed(1)
  folds <- cvFolds(n = length(target), K = 10, type = "random")
  for(i in 1:10){
    invisible(capture.output(mod_lasso <- glmnet(x = as.matrix(data[folds$which!=i, ]), y=(target[folds$which!=i]), lambda=mod_lasso1$lambda.min, alpha=0)))
    preds_lasso[folds$which==i] <- predict(mod_lasso, as.matrix(data[folds$which==i,] ))
  }
  print(paste('Correlation of Ridge:'))
  corr <- cor(preds_lasso, target)
  print(corr)
  print(paste('Annual Sharp Ratio of Ridge:'))
  l <- log(preds_lasso) * log(target)
  sharp <- sqrt(250) * mean(l)/sd(l)
  print(sharp)
  l1 <- list()
  l1[[1]] <- preds_lasso
  l1[[2]] <- mod_lasso1$lambda.min
  l1[[3]] <- 'Ridge'
  full_list[[2]] <- l1
  return(full_list)
}
