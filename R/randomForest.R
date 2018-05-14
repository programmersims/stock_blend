get_rf <- function(data, target, covs, mtry=c(5), tree=150, cores=7) {
  full_models <- list()
  n <- 1
  #covs[[1]] <- colnames(data)
  cl <-makeCluster(cores, type="SOCK")
  registerDoSNOW(makeCluster(cores, type="SOCK"))
  for(i in mtry)
    for(j in 1:length(covs)) {
      
      preds_rf <- numeric(length(target))
      set.seed(1)
      folds <- cvFolds(n = length(target), K = 10, type = "random")
      for(k in 1:10){
        mod_rf <- foreach(ntree = rep(floor(tree/cores), cores), .combine = combine, .packages = "randomForest") %dopar%
          randomForest(x = as.matrix(data[folds$which!=k, covs[[j]]]), y=(target[folds$which!=k]), ntree = tree, mtry=i)
        #mod_rf <- randomForest(x = as.matrix(data[folds$which!=i, covs[[j]]]), y=(target[folds$which!=i]), mtry=i, ntree=tree)
        preds_rf[folds$which==k] <- predict(mod_rf, as.matrix(data[folds$which==k, covs[[j]]] ))
      }
      print(paste('Correlation of RF',i))
      corr <- cor(preds_rf, target)
      print(corr)
      print(paste('Annual Sharp Ratio of RF:',i))
      l <- log(preds_rf) * log(target)
      sharp <- sqrt(250) * mean(l)/sd(l)
      print(sharp)
      
      l <- list()
      l[[1]] <- preds_rf
      l[[2]] <- covs[[j]]
      l[[3]] <- i
      l[[4]] <- cores
      full_models[[n]] <- l
      n <- n+1
    }
  return(full_models)
}
