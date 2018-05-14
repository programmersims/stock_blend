the_threads <- 1
get_strong_models_full_day_seq <- function(stocks, target_name, p_values = c(0.05,0.01), proportion = 0.8,
                                       depth=c(6,7), alpha=c(0.05, 0.1, 0.15), subsamp=c(0.5,0.7),  min_weight=c(10, 50, 100),
                                       C=c(0.01,0.02,0.05,0.1,0.4,1), gamma = c(0.0001,0.001,0.01,0.1), epsilon=c(0.1,0.05,0.15),
                                       decay=c(0.5, 0.3, 0.1), maxit=200, size=c(5,10,25), neurons = c(1, 2),
                                      tree=1000, try=c(2,5,10), weighting=0,
                                       samp=0.6, iter=10, pca=FALSE, variation_to_keep=1, abs_v=0.0001, full=FALSE, kernel=c('linear'), weight_exp=0, clusters=10,
                                       sigma=c(1),degree=c(1),order=c(1), bag=5, sqrt_weights=TRUE){

  target <- stocks[30:nrow(stocks), target_name]
  ind <- stocks[29:(nrow(stocks)-1), ]
  ind$Time <- 1:length(ind$Time)
  #k <- unlist(kmeans(ind, centers=clusters)[1])
  #for(i in 1:clusters){
  #  ind[,paste('cluster_',i,sep='')] <- ifelse(k==i,1,0)
  #}


  ind2 <- ind[(floor(length(target)*proportion)+1):length(target),]
  target2 <- target[(floor(length(target)*proportion)+1):length(target)]

  ind <- ind[1:floor(length(target)*proportion),]
  target <- target[1:floor(length(target)*proportion)]

  for(i in 1:ncol(ind)){
    ind2[,i] <- (ind2[,i]-mean(ind[,i]))/sd(ind[,i])
    ind[,i] <- (ind[,i]-mean(ind[,i]))/sd(ind[,i])
  }

  print(dim(ind))
  prin <- princomp(ind)
  prin_loadings2 <- prin$loadings
  for(i in 1:ncol(as.matrix(prin_loadings2))){
    prin_loadings2[abs(prin_loadings2[,i])<abs_v, i] <- 0
  }
  prin_ind2 <- as.matrix(ind2) %*% as.matrix(prin_loadings2)
  prin_ind <-  as.matrix(ind) %*% as.matrix(prin_loadings2)
  colnames(prin_ind2) <- colnames(prin$scores)
  colnames(prin_ind) <- colnames(prin$scores)
  prin_ind2 <- prin_ind2[, prin$sdev > variation_to_keep]
  prin_ind <- prin_ind[,prin$sdev > variation_to_keep]

  vect_sds <- numeric(ncol(prin_ind))
  for(i in 1:ncol(prin_ind)){
    vect_sds[i] <- sd(prin_ind[,i])
    prin_ind2[,i] <- (prin_ind2[,i])/sd(prin_ind[,i])
    prin_ind[,i] <- (prin_ind[,i])/sd(prin_ind[,i])
  }

  print(colnames(prin_ind))
  prin_list <- list()
  prin_list[[1]] <- prin_loadings2
  prin_list[[2]] <- prin$sdev
  prin_list[[3]] <- variation_to_keep
  prin_list[[4]] <- vect_sds
  prin_list[[5]] <- colnames(prin$scores)
  prin_list[[6]] <- pca

  if(pca){
    ind <- as.data.frame(prin_ind)
    ind2 <- as.data.frame(prin_ind2)
  }

  seq_list <- list()
  iter2 <- 1/iter #ceiling(seq(1, (nrow(ind)*(1-samp)), length.out = (nrow(ind)*(1-samp))))
  the_vals_train <- ceiling(seq(nrow(ind)*(1-samp), nrow(ind), length.out = (iter+1) ))
  vals <- list()
  targ <- list()
  vals2 <- list()
  for(i in 1:iter){
    vals[[i]] <- ind[1:the_vals_train[i],]
    targ[[i]] <- target[1:the_vals_train[i]]
    vals2[[i]] <- ind[(the_vals_train[i]+1):(the_vals_train[i+1]),]
  }
  seq_list[[1]] <- vals
  seq_list[[2]] <- targ
  seq_list[[3]] <- vals2
  seq_list[[4]] <- target[(the_vals_train[1]+1):length(target)]

  if(!full){
    ll_covs <- get_ls_seq(seq_list=seq_list, p_values = p_values)
    ll_covs[[(length(ll_covs)+1)]] <- get_lasso_covs_seq(seq_list = seq_list)
    invisible(capture.output(feat <- get_xgb_predictors(data = ind, target = target)))
    ll_covs[[(length(ll_covs)+1)]] <- feat
  } else{
    ll_covs <- list()
    ll_covs[[1]] <- colnames(ind)
  }
  #rf1 <- get_cart(seq_list=seq_list, covs = ll_covs, weighting=weighting, weight_exp=weight_exp)
  #detach("package:doParallel", unload=TRUE)
  library(doSNOW)
  rf1 <- get_rf_seq2(seq_list=seq_list, covs = ll_covs, mtry = try, tree = tree, cores = the_threads, weighting=weighting, weight_exp=weight_exp, full=full, sqrt_weights=sqrt_weights)
  #detach("package:doSNOW", unload=TRUE)
  #library(doParallel)
  #registerDoParallel(cores=8)
  bn1 <- get_brnn_seq(seq_list=seq_list, covs = ll_covs, neurons = neurons, bag=1)
  s1 <- get_svm_seq2(seq_list=seq_list, covs=ll_covs, C = C, gamma = gamma, epsilon = epsilon, kernel=kernel,sigma=sigma,order=order,degree=degree)
  nn1 <- get_nnet_seq(seq_list=seq_list, covs = ll_covs, pca_string = "", decay = decay, maxit = maxit, size = size, sqrt_weights=sqrt_weights)
  invisible(capture.output(xgb1 <- get_xgb_seq(seq_list=seq_list, depth = depth, alpha = alpha, subsamp = subsamp, min_weight = min_weight, weighting=weighting, sqrt_weights=sqrt_weights)))
  

   mods <- list()
   mods[[1]] <- xgb1
   mods[[2]] <- bn1
   mods[[3]] <- nn1
   mods[[4]] <- s1
   mods[[5]] <- rf1

   fin_list <- list()
   fin_list[[1]] <- mods
   fin_list[[2]] <- target
   fin_list[[3]] <- target2
   fin_list[[4]] <- ind
   fin_list[[5]] <- ind2
   fin_list[[6]] <- target_name
   fin_list[[7]] <- seq_list
   fin_list[[8]] <- prin_list
   return(fin_list)
}

get_ls_seq <- function(seq_list=seq_list, p_values = c(0.1, 0.05)){
  train <- seq_list[[1]]
  targ <- seq_list[[2]]
  test <- seq_list[[3]]
  targ_test <- seq_list[[4]]

  int_train <- train[[1]]
  int_targ <- targ[[1]]

  covs_list <- list()
  j <- 1
  for(i in p_values){
     mod_lm1 <- lm(int_targ ~., data=int_train)
     covs <- names(summary(mod_lm1)$coef[summary(mod_lm1)$coef[,4] <= i  , 4])[-1]
     print(paste(paste('P_values', i, ':')))
     print(covs)
     covs_list[[j]] <- covs
     j <- j+1
  }
  preds_lm <- numeric(length(targ_test))
  for(i in 1:length(covs_list)){
    temp <- 1
    for(j in 1:length(train)){
     mod_lm <- lm(targ[[j]] ~. , data=as.data.frame(train[[j]])[,covs_list[[i]]])  
     preds_lm[temp:(nrow(test[[j]])+temp-1)] <- predict(mod_lm, as.data.frame(test[[j]])[,covs_list[[i]]])
     temp <- (nrow(test[[j]])+temp)
    } 
    print(paste('Correlation of Sparse (P=', as.character(p_values[i]),') LS:'))
    #print(preds_lm)
    corr <- cor(preds_lm, targ_test)
    print(corr)
    print(paste('Annual Sharp Ratio of Sparse (P=',  as.character(p_values[i]),') LS:'))
    l <- log(preds_lm) * log(targ_test)
    sharp <- sqrt(250) * mean(l)/sd(l)
    print(sharp)
  }

return(covs_list)
}

get_brnn_seq <- function(seq_list=seq_list, covs=covs, neurons = c(1, 2), bag=5){
  full_list <- list()

  train <- seq_list[[1]]
  targ <- seq_list[[2]]
  test <- seq_list[[3]]
  targ_test <- seq_list[[4]]

  preds_brnn <- numeric(length(targ_test))
  a <- 1
  print(covs)
  for(j in 1:length(covs)){
    for(k in neurons){
      for(p in 1:bag) {
      temp <- 1
      for(i in 1:length(train)){
        invisible(capture.output(mod_brnn <- brnn(x = as.matrix(train[[i]][,covs[[j]]]), y=(targ[[i]]), neurons=k, cores=the_threads)))
        preds_brnn[temp:(nrow(test[[i]])+temp-1)] <-preds_brnn[temp:(nrow(test[[i]])+temp-1)] + predict(mod_brnn, as.matrix(test[[i]][, covs[[j]]]))*1/bag
        temp <- (nrow(test[[i]])+temp)
      }
    }
      print(paste('Correlation of', k ,'Neuron BNN:'))
      corr <- cor(preds_brnn, targ_test)
      print(corr)
      print(paste('Annual Sharp Ratio of',  k,'Neuron BNN:'))
      l <- log(preds_brnn) * log(targ_test)
      sharp <- sqrt(250) * mean(l)/sd(l)
      print(sharp)
      l1 <- list()
      l1[[1]] <- preds_brnn
      l1[[2]] <- covs[[j]]
      l1[[3]] <- k
      full_list[[a]] <- l1
      a <- a+1
      preds_brnn <- numeric(length(targ_test))
    }
  }
  return(full_list)
}

get_lasso_covs_seq <- function(seq_list = seq_list){
  train <- seq_list[[1]]
  targ <- seq_list[[2]]
  int_train <- train[[1]]
  int_targ <- targ[[1]]
  las <- glmnet(x = as.matrix(int_train), y = int_targ, lambda=0.00025)
  covs <- colnames(int_train)[which(coef(las, s = "lambda")[2:length(coef(las, s = "lambda.min")),1] != 0)]
  print(covs)
  return(covs)
}

get_nnet_seq <- function(seq_list=seq_list, covs=covs, pca_string="", decay=c(0.1,0.01,0.001), maxit=100, size=c(5,10,20,50), sqrt_weights=TRUE) {
  full_list <- list()

  train <- seq_list[[1]]
  targ <- seq_list[[2]]
  test <- seq_list[[3]]
  targ_test <- seq_list[[4]]

  preds_nnet <- numeric(length(targ_test))
  set.seed(1)
  a <- 1
  for(j in 1:length(covs)){
    for(k in size){
      for(h in decay){
        temp <- 1
        for(i in 1:length(train)){
          #invisible(capture.output(mod_nnet <- nnet(x = as.matrix(train[[i]])[, covs[[j]]], y=(targ[[i]]), size=k, maxit = maxit, linout = TRUE, decay = h, MaxNWts=100000)))
          #preds_nnet[temp:(nrow(test[[i]])+temp-1)] <- predict(mod_nnet, as.matrix(test[[i]])[,covs[[j]]])
          #temp <- (nrow(test[[i]])+temp)
          #net <- foreach(u=1:8, .combine=cbind)  %dopar% {
            if(sqrt_weights){
              weights <- get_weights(nrow(train[[i]]))
            } else {
              weights <- 1+1.5*seq(0,1, length.out = nrow(train[[i]]))
            }
            invisible(capture.output(mod_nnet <- nnet(x = as.matrix(train[[i]])[, covs[[j]]], y=(targ[[i]]), weights=weights, size=k, maxit = maxit, linout = TRUE, decay = h, MaxNWts=100000)))
            preds_nnet1 <- predict(mod_nnet, as.matrix(test[[i]])[,covs[[j]]])
          #}
         #net <- as.data.frame(net)
         preds_nnet[temp:(nrow(test[[i]])+temp-1)] <- preds_nnet1 #apply(net, 1, mean)
         temp <- (nrow(test[[i]])+temp)          
        }
        print(paste('Correlation of', k ,'Neurons', pca_string,'Neural Net','decay=', h,':'))
        corr <- cor(preds_nnet, targ_test)
        print(corr)
        print(paste('Annual Sharp Ratio of',  k,'Neurons', pca_string,'Neural Net', 'decay=', h,':'))
        l <- log(preds_nnet) * log(targ_test)
        sharp <- sqrt(250) * mean(l)/sd(l)
        print(sharp)
        l1 <- list()
        l1[[1]] <- preds_nnet
        l1[[2]] <- covs[[j]]
        l1[[3]] <- k
        l1[[4]] <- h
        l1[[5]] <- maxit
        l1[[6]] <- sqrt_weights
        full_list[[a]] <- l1
        a <- a+1
      }
    }
  }
  return(full_list)
}

get_svm_seq <- function(seq_list=seq_list, covs=covs,kernel='linear', C=c(0.001,0.01,0.05,0.1,0.2,0.4,1), gamma = c(0.0001,0.001,0.01,0.1), epsilon=c(0.1,0.05,0.15)) {
  full_models <- list()

  train <- seq_list[[1]]
  targ <- seq_list[[2]]
  test <- seq_list[[3]]
  targ_test <- seq_list[[4]]


  n <- 1
  preds_svm <- numeric(length(targ_test))
  for(f in C)
    for(j in gamma)
      for(k in epsilon)
        for(h in 1:length(covs)) {
          temp <- 1
          for(i in 1:length(train)){
            invisible(capture.output(mod_svm <- svm(x = as.matrix(train[[i]][,covs[[h]]]), y=(targ[[i]]), C=f, gamma=j, epsilon=k, kernel=kernel)))
            preds_svm[temp:(nrow(test[[i]])+temp-1)] <- predict(mod_svm, as.matrix(test[[i]][,covs[[h]]] ))
            temp <- (nrow(test[[i]])+temp)
          }
        print(paste('Correlation of SVM:'))
        corr <- cor(preds_svm, targ_test)
        print(corr)
        print(paste('Annual Sharp Ratio of SVM:'))
        l <- log(preds_svm) * log(targ_test)
        sharp <- sqrt(250) * mean(l)/sd(l)
        print(sharp)
          l <- list()
          l[[1]] <- preds_svm
          l[[2]] <- covs[[h]]
          l[[3]] <- f
          l[[4]] <- j
          l[[5]] <- k
          l[[6]] <- kernel
          full_models[[n]] <- l
          n <- n+1
        }
  return(full_models)
}

get_svm_seq2 <- function(seq_list=seq_list, covs=covs,kernel=c('linear'), C=c(0.001,0.01,0.05,0.1,0.2,0.4,1), gamma = c(0.0001,0.001,0.01,0.1), epsilon=c(0.1,0.05,0.15), order=c(1), degree=c(1),sigma=c(1)) {
  full_models <- list()

  train <- seq_list[[1]]
  targ <- seq_list[[2]]
  test <- seq_list[[3]]
  targ_test <- seq_list[[4]]


  n <- 1
  preds_svm <- numeric(length(targ_test))
  for(o in kernel)
  if('besseldot' != o){
  for(f in C)
    for(j in gamma)
      for(k in epsilon)
        for(h in 1:length(covs)) {
          temp <- 1
          for(i in 1:length(train)){
            invisible(capture.output(mod_svm <- ksvm(x = as.matrix(train[[i]][,covs[[h]]]), y=(targ[[i]]), C=f, gamma=j, epsilon=k, kernel=o)))
            preds_svm[temp:(nrow(test[[i]])+temp-1)] <- predict(mod_svm, as.matrix(test[[i]][,covs[[h]]] ))
            temp <- (nrow(test[[i]])+temp)
          }
        #print(c)
        print(paste('Correlation of SVM:', o))
        corr <- cor(preds_svm, targ_test)
        print(corr)
        print(paste('Annual Sharp Ratio of SVM:'))
        l <- log(preds_svm) * log(targ_test)
        sharp <- sqrt(250) * mean(l)/sd(l)
        print(sharp)
          l <- list()
          l[[1]] <- preds_svm
          l[[2]] <- covs[[h]]
          l[[3]] <- f
          l[[4]] <- j
          l[[5]] <- k
          l[[6]] <- o
          l[[7]] <- 1
          l[[8]] <- 1
          l[[9]] <- 1
          full_models[[n]] <- l
          n <- n+1
        } }
  if('besseldot' %in% kernel){
    for(p in order)
    for(m in sigma)
    for(b in degree)
    for(f in C)
    for(j in gamma)
      for(k in epsilon)
        for(h in 1:length(covs)) {
          temp <- 1
          for(i in 1:length(train)){
            invisible(capture.output(mod_svm <- ksvm(x = as.matrix(train[[i]][,covs[[h]]]), y=(targ[[i]]), C=f, gamma=j, epsilon=k, kernel='besseldot',kpar=list(order=p,sigma=m,degree=b))))
            preds_svm[temp:(nrow(test[[i]])+temp-1)] <- predict(mod_svm, as.matrix(test[[i]][,covs[[h]]] ))
            temp <- (nrow(test[[i]])+temp)
          }
        #print(c)
        print(paste('Correlation of SVM:', o))
        corr <- cor(preds_svm, targ_test)
        print(corr)
        print(paste('Annual Sharp Ratio of SVM:'))
        l <- log(preds_svm) * log(targ_test)
        sharp <- sqrt(250) * mean(l)/sd(l)
        print(sharp)
          l <- list()
          l[[1]] <- preds_svm
          l[[2]] <- covs[[h]]
          l[[3]] <- f
          l[[4]] <- j
          l[[5]] <- k
          l[[6]] <- 'besseldot'
          l[[7]] <- p
          l[[8]] <- m
          l[[9]] <- b
          full_models[[n]] <- l
          n <- n+1
  } }
  return(full_models)
}

get_rf_seq <- function(seq_list=seq_list, covs=covs, mtry=c(5), tree=150, cores=7) {
  full_models <- list()

  train <- seq_list[[1]]
  targ <- seq_list[[2]]
  test <- seq_list[[3]]
  targ_test <- seq_list[[4]]

  n <- 1
  cl <-makeCluster(cores, type="SOCK")
  registerDoSNOW(makeCluster(cores, type="SOCK"))
  for(i in mtry)
    for(j in 1:length(covs)) {
      preds_rf <- numeric(length(targ_test))
      temp <- 1
      for(k in 1:length(train)){
        mod_rf <- foreach(ntree = rep(floor(tree/cores), cores), .combine = combine, .packages = "randomForest") %dopar%
          randomForest(x = as.matrix(train[[k]][, covs[[j]]]), y=(targ[[k]]), ntree = tree, mtry=i)
        #mod_rf <- randomForest(x = as.matrix(data[folds$which!=i, covs[[j]]]), y=(target[folds$which!=i]), mtry=i, ntree=tree)
        preds_rf[temp:(nrow(test[[k]])+temp-1)] <- predict(mod_rf, as.matrix(test[[k]][, covs[[j]]] ))
        temp <- (nrow(test[[k]])+temp)
      }
      print(paste('Correlation of RF',i))
      corr <- cor(preds_rf, targ_test)
      print(corr)
      print(paste('Annual Sharp Ratio of RF:',i))
      l <- log(preds_rf) * log(targ_test)
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

get_rf_seq2 <- function(seq_list=seq_list, covs=covs, weighting=0, mtry=c(5), tree=150, cores=7, weight_exp=0, full=FALSE, sqrt_weights=TRUE) {
  full_models <- list()

  train <- seq_list[[1]]
  targ <- seq_list[[2]]
  test <- seq_list[[3]]
  targ_test <- seq_list[[4]]
  if(!full){
    covs[[length(covs)+1]] <- colnames(train[[1]])
  }

  n <- 1
  #cl <-makeCluster(cores, type="SOCK")
  #registerDoSNOW(makeCluster(cores, type="SOCK"))
  for(i in mtry)
    for(j in 1:length(covs)) {
      preds_rf <- numeric(length(targ_test))
      temp <- 1
      for(k in 1:length(train)){
        data <- as.data.frame(train[[k]][, covs[[j]]])
        data$Target <- as.numeric(targ[[k]])
        if(sqrt_weights){
          weights <- get_weights(nrow(data))
        } else {
         if(weight_exp!=0){
           weights <- 1+weighting*qexp(seq(0.01,.99, length.out = nrow(data)), rate=weight_exp)
           } else{
           weights <- 1+weighting*seq(0,1, length.out = nrow(data))
         }
       }
        mod_rf <- ranger(Target~., data = data, num.trees = tree, mtry=i, write.forest=TRUE, case.weights=weights)
        preds_rf[temp:(nrow(test[[k]])+temp-1)] <- unlist(predict(mod_rf, as.data.frame(test[[k]][, covs[[j]]] ))[1])
        temp <- (nrow(test[[k]])+temp)
      }
      print(paste('Correlation of RF',i))
      corr <- cor(preds_rf, targ_test)
      print(corr)
      print(paste('Annual Sharp Ratio of RF:',i))
      l <- log(preds_rf) * log(targ_test)
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

get_cart <- function(seq_list=seq_list, covs=covs, weighting=0, weight_exp=0) {
  full_models <- list()

  train <- seq_list[[1]]
  targ <- seq_list[[2]]
  test <- seq_list[[3]]
  targ_test <- seq_list[[4]]
  covs[[length(covs)+1]] <- colnames(train[[1]])

  n <- 1
    for(j in 1:length(covs)) {
      preds_rf <- numeric(length(targ_test))
      temp <- 1
      for(k in 1:length(train)){
        data <- as.data.frame(train[[k]][, covs[[j]]])
        data$Target <- as.numeric(targ[[k]])
        if(weight_exp!=0){
          weights <- 1+weighting*qexp(seq(0.01,.99, length.out = nrow(data)), rate=weight_exp)
          } else{
          weights <- 1+weighting*seq(0,1, length.out = nrow(data))
        }
        mod_rf <- rpart(Target~., data = data, weights=weights, control=rpart.control(min_split=5, cp=0))
        preds_rf[temp:(nrow(test[[k]])+temp-1)] <- unlist(predict(mod_rf, as.data.frame(test[[k]][, covs[[j]]] ))[1])
        temp <- (nrow(test[[k]])+temp)
      }
      print('Correlation of CART')
      corr <- cor(preds_rf, targ_test)
      print(corr)
      print('Annual Sharp Ratio of CART:')
      l <- log(preds_rf) * log(targ_test)
      sharp <- sqrt(250) * mean(l)/sd(l)
      print(sharp)
      
      l <- list()
      l[[1]] <- preds_rf
      l[[2]] <- covs[[j]]
      l[[3]] <- j
      l[[4]] <- j
      full_models[[n]] <- l
      n <- n+1
    }
  return(full_models)
}


get_xgb_seq <- function(seq_list=seq_list, weighting=1, depth=c(4,5,6,7,8), alpha=c(0.05, 0.1, 0.2), subsamp=c(0.5,0.7),  min_weight=c(0,100), sqrt_weights=TRUE) {

  train <- seq_list[[1]]
  targ <- seq_list[[2]]
  test <- seq_list[[3]]
  targ_test <- seq_list[[4]]


  preds <- numeric(length(targ_test))
  n <- 1
  full_models <- list()
  for(i in depth)
    for(j in alpha)
      for(k in subsamp)
        for(h in min_weight){
        temp <- 1
          for(m in 1:length(train)){
            if(sqrt_weights){
              weights <- get_weights(nrow(train[[m]]))
            } else {
              weights <- 1+weighting*seq(0,1, length.out = nrow(train[[m]]))
            }
            data_sparse <- sparse.model.matrix(~-1 + ., data=as.data.frame(train[[m]]))
            dtrain <-xgb.DMatrix(data=data_sparse,label=(targ[[m]]), weight=weights)
            data_sparse2 <- sparse.model.matrix(~-1 + ., data=as.data.frame(test[[m]]))
            dtest <-xgb.DMatrix(data=data_sparse2)
          
            model_gradient_tree <- xgb.train(data = dtrain,
                                          nthread = the_threads,
                                          nrounds = 1750,
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

            preds[temp:(nrow(test[[m]])+temp-1)] <- predict(model_gradient_tree, dtest)
            temp <- (nrow(test[[m]])+temp)
          }
            
            l <- list()
            l[[1]] <- preds
            l[[2]] <- colnames(train[[m]])
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

fit_models_full_day_seq <- function(full_list,name,str="", length_samp=0.1, bias_proportion=0, weighting=0, corr=FALSE, mm =FALSE, cust=TRUE){

  mods <- full_list[[1]]
  targ <- full_list[[2]]
  targ2 <- full_list[[3]]
  ind <- full_list[[4]]
  ind2 <- full_list[[5]]
  seq_list <- full_list[[7]]
  targ_seq <- seq_list[[4]]

  corr <<- corr
  mm <<- mm
  cust <<- cust
  grad1 <- mods[[1]]
  bnn1 <- mods[[2]]
  net1 <- mods[[3]]
  svm1 <- mods[[4]]
  rf1 <- mods[[5]]

  pos_grad1 <- get_best_model(grad1, targ_seq)[[2]]
  pos_brnn1 <- get_best_model(bnn1, targ_seq)[[2]]
  pos_net1 <- get_best_model(net1, targ_seq)[[2]]
  pos_svm1 <- get_best_model(svm1, targ_seq)[[2]]
  pos_rf1 <- get_best_model(rf1, targ_seq)[[2]]

  pgrad1 <- grad1[[pos_grad1]]
  pbnn1 <- bnn1[[pos_brnn1]]
  pnet1 <- net1[[pos_net1]]
  psvm1 <- svm1[[pos_svm1]]
  prf1 <- rf1[[pos_rf1]]

  mods2 <- list()
  mods2[[1]] <- pgrad1
  mods2[[2]] <- pbnn1
  mods2[[3]] <- pnet1
  mods2[[4]] <- psvm1
  mods2[[5]] <- prf1

  p_list <<- list()

  p_list[[1]] <<- pgrad1[[1]]
  p_list[[2]] <<- pbnn1[[1]]
  p_list[[3]] <<- pnet1[[1]]
  p_list[[4]] <<- psvm1[[1]]
  p_list[[5]] <<- prf1[[1]]


  full_list2 <- list()
  full_list2[[1]] <- full_list

  target <<- targ_seq
  ratios <<- optim(par = rep(1, (length(mods2)-1)), fn = opt, lower = 0.0, upper = 100000, method = 'L-BFGS-B', )$par
  #ratios <<- ratios + rep(0.5, (length(mods2)-1))
  full_list2[[2]] <- ratios
  full_list2[[3]] <- mods2
  full_list2[[4]] <- weighting

  print('Ratios of models')
  print(ratios)

  fin_sharp <- log(fin_preds(p_list, ratios)) * log(targ_seq)
  fin_sharp  <- mean(fin_sharp)/sd(fin_sharp)*sqrt(252)
  print('Sharp of in sample:')
  print(fin_sharp)
  print('Corr of in sample:')
  print(cor(targ_seq, fin_preds(p_list, ratios)))
  c <- cor(targ_seq, fin_preds(p_list, ratios))

  x1 <- paste(full_list[[6]], 'Sharp1:',fin_sharp, 'Corr1:',c)

  output_predictions <- numeric(length(targ2))
  output_predictions1 <- numeric(length(targ2))
  output_predictions2 <- numeric(length(targ2))
  output_predictions3 <- numeric(length(targ2))
  output_predictions4 <- numeric(length(targ2))
  output_predictions5 <- numeric(length(targ2))
  the_vals <- ceiling(seq(1, length(targ2), length.out = ceiling(length(targ2)*length_samp)))
  the_vals <- the_vals[2:length(the_vals)]
  temp <- 1

  bias <- (1-mean(fin_preds(p_list, ratios)))

  for(i in 1:ncol(ind2)){
    ind2[is.na(ind2[,i]),i] <- median(ind2[,i], na.rm = TRUE)
  }

  pca_list <- list()
  for(i in the_vals){
    if(temp > 1){
      data <- rbind(ind, ind2[1:temp,])
      targ3 <- c(targ, targ2[1:temp])
      to_be_fitted <- ind2[(temp+1):i,]
    }
    else{
      data <- ind
      targ3 <- targ
      to_be_fitted <- ind2[1:i,]
    }
    if(pnet1[[6]]){
      weights <- get_weights(nrow(data))
    } else{
      weights <- 1+weighting*seq(0,1, length.out = nrow(data))
    }

    data_sparse <- sparse.model.matrix(~-1 + ., data=as.data.frame(data[,pgrad1[[2]]]))
    dtrain <-xgb.DMatrix(data=data_sparse,label=(targ3), weight=weights)
    data_sparse2 <- sparse.model.matrix(~-1 + ., data=as.data.frame(to_be_fitted[,pgrad1[[2]]]))
    dtest <-xgb.DMatrix(data=data_sparse2)


    model_gradient <- xgb.train(data = dtrain,
                                nfold=10,
                                nthread = 1,
                                nrounds = 1750,
                                max_depth = pgrad1[[3]],
                                alpha = pgrad1[[4]],
                                eta = 0.005,
                                subsample = pgrad1[[5]],
                                colsample_bytree = 0.70,
                                booster = "gbtree",
                                eval_metric = 'mae',
                                #metrics = list("mae"),
                                maximize = FALSE,
                                objective = "reg:linear",
                                print.every.n = 1000,
                                verbose = TRUE,
                                min_child_weight=pgrad1[[6]],
                                prediction=TRUE,
                                watchlist = list(train = dtrain))

    preds_grad1 <- predict(model_gradient, dtest)

    invisible(capture.output(mod_brnn <- brnn(x = as.matrix(data[,pbnn1[[2]]]), y=(targ3), neurons=pbnn1[[3]], cores=8)))
    preds_brnn1 <- predict(mod_brnn, as.matrix(to_be_fitted[, pbnn1[[2]]]))
    #net <- foreach(u=1:8, .combine=cbind)  %dopar% {
      invisible(capture.output(mod_nnet <- nnet(x = as.matrix(data[, pnet1[[2]]]), weights=weights, y=(targ3), size=pnet1[[3]], maxit = pnet1[[5]], linout = TRUE, decay = pnet1[[4]], MaxNWts=100000)))
      preds_nnet1 <- predict(mod_nnet, as.matrix(to_be_fitted[, pnet1[[2]]]))
    #}
    #net <- as.data.frame(net)
    #preds_nnet1 <- apply(net, 1, mean)
    if('besseldot' != psvm1[[6]]){
      invisible(capture.output(mod_svm <- ksvm(x = as.matrix(data[,psvm1[[2]]]), y=(targ3), C=psvm1[[3]], gamma=psvm1[[4]], epsilon=psvm1[[5]], kernel=psvm1[[6]])))
      preds_svm1 <- predict(mod_svm, as.matrix(to_be_fitted[, psvm1[[2]]]))
    } else{
      invisible(capture.output(mod_svm <- ksvm(x = as.matrix(data[,psvm1[[2]]]), y=(targ3), C=psvm1[[3]], gamma=psvm1[[4]], epsilon=psvm1[[5]], kernel=psvm1[[6]], kpar=list(order=psvm1[[7]], sigma=psvm1[[8]],degree=psvm1[[9]]))))
      preds_svm1 <- predict(mod_svm, as.matrix(to_be_fitted[, psvm1[[2]]]))
    }
    data2 <- as.data.frame(data[, prf1[[2]]])
    data2$Target <- (targ3)
    mod_rf <- ranger(Target~., data=data2, num.trees = 1000, mtry=prf1[[3]], write.forest=TRUE, case.weights=weights)
    preds_rf1 <- unlist(predict(mod_rf, as.data.frame(to_be_fitted[, prf1[[2]]]))[1])


    pca_list[[1]] <- preds_grad1
    pca_list[[2]] <- preds_brnn1
    pca_list[[3]] <- preds_nnet1
    pca_list[[4]] <- preds_svm1
    pca_list[[5]] <- preds_rf1

    if(temp > 1){
      output_predictions[(temp+1):i] <- fin_preds(pca_list, ratios)
      output_predictions1[(temp+1):i] <- preds_grad1
      output_predictions2[(temp+1):i] <- preds_brnn1
      output_predictions3[(temp+1):i] <- preds_nnet1
      output_predictions4[(temp+1):i] <- preds_svm1
      output_predictions5[(temp+1):i] <- preds_rf1

       p_list[[1]] <<- c(p_list[[1]], preds_grad1)
       p_list[[2]] <<- c(p_list[[2]], preds_brnn1)
       p_list[[3]] <<- c(p_list[[3]], preds_nnet1)
       p_list[[4]] <<- c(p_list[[4]], preds_svm1)
       p_list[[5]] <<- c(p_list[[5]], preds_rf1)

      target <<- c(target, targ2[(temp+1):i])
      ratios <<- optim(par = rep(1, (length(mods2)-1)), fn = opt, lower = 0, upper = 100000, method = 'L-BFGS-B')$par
      #print(ratios)
    } else{
      output_predictions[1:i] <- fin_preds(pca_list, ratios)
      output_predictions1[(1):i] <- preds_grad1
      output_predictions2[(1):i] <- preds_brnn1
      output_predictions3[(1):i] <- preds_nnet1
      output_predictions4[(1):i] <- preds_svm1
      output_predictions5[(1):i] <- preds_rf1

       p_list[[1]] <<- c(p_list[[1]], preds_grad1)
       p_list[[2]] <<- c(p_list[[2]], preds_brnn1)
       p_list[[3]] <<- c(p_list[[3]], preds_nnet1)
       p_list[[4]] <<- c(p_list[[4]], preds_svm1)
       p_list[[5]] <<- c(p_list[[5]], preds_rf1)

      target <<- c(target, targ2[1:i])
      ratios <<- optim(par = rep(1, (length(mods2)-1)), fn = opt, lower = 0, upper = 100000, method = 'L-BFGS-B')$par
    }
    temp <- i
  }
  full_list2[[2]] <- ratios
  print(ratios)
  output_predictions <- output_predictions + bias*bias_proportion
  #full_list2[[10]] <- ratios
  full_list2[[5]] <- sd(output_predictions)
  full_list2[[6]] <- (output_predictions)
  fin_sharp <- log(output_predictions) * log(targ2)
  fin_sharp <- mean(fin_sharp)/sd(fin_sharp) * sqrt(252)
  print('Sharp on out of sample:')
  print(fin_sharp)
  print('Correlation out of sample:')
  c <- cor(output_predictions, targ2)
  print(cor(output_predictions, targ2))

  iter1 <- abs(output_predictions-1) > 1*sd(output_predictions)

  fin_sharp1 <- log(output_predictions[iter1]) * log(targ2[iter1])
  fin_sharp1 <- mean(fin_sharp1)/sd(fin_sharp1) * sqrt(252 * sum(iter1)/length(targ2))
  print('Sharp on out of sample (1 sd movements):')
  print(fin_sharp1)
  print('Correlation out of sample (1 sd movements):')
  c1 <- cor(output_predictions[iter1], targ2[iter1])
  print(c1)
  print('Proportion of Trades 1 sd:')
  print(sum(iter1)/length(targ2))

  iter2 <- abs(output_predictions-1) > 1.5*sd(output_predictions)

  fin_sharp2 <- log(output_predictions[iter2]) * log(targ2[iter2])
  fin_sharp2 <- mean(fin_sharp2)/sd(fin_sharp2) * sqrt(252 * sum(iter2)/length(targ2))
  print('Sharp on out of sample (1.5 sd movements):')
  print(fin_sharp2)
  print('Correlation out of sample (1.5 sd movements):')
  c2 <- cor(output_predictions[iter2], targ2[iter2])
  print(c2)
  print('Proportion of Trades 1.5 sd:')
  print(sum(iter2)/length(targ2))
 
  x <- paste(x1, 'Sharp2:',fin_sharp, 'Corr2:',c, 'Sharp2 (1 sd):', fin_sharp1,'Corr 2 1(sd)', c1, 'Sharp2 (1.5 sd):', fin_sharp2, 'Corr 2 (1.5 sd)', c2)
 
  saveRDS(x, paste(full_list[[6]],str, '.txt', sep=""))
  saveRDS(full_list2, paste(name,str,'.rds',sep=""))

  fin_sharp <- log(output_predictions1) * log(targ2)
  fin_sharp <- mean(fin_sharp)/sd(fin_sharp) * sqrt(252)
  print('Sharp on out of sample XGB:')
  print(fin_sharp)
  print('Correlation out of sample XGB:')
  c <- cor(output_predictions1, targ2)
  print(cor(output_predictions1, targ2))

  fin_sharp <- log(output_predictions2) * log(targ2)
  fin_sharp <- mean(fin_sharp)/sd(fin_sharp) * sqrt(252)
  print('Sharp on out of sample BRNN:')
  print(fin_sharp)
  print('Correlation out of sample BRNN:')
  c <- cor(output_predictions2, targ2)
  print(cor(output_predictions2, targ2))

  fin_sharp <- log(output_predictions3) * log(targ2)
  fin_sharp <- mean(fin_sharp)/sd(fin_sharp) * sqrt(252)
  print('Sharp on out of sample NNET:')
  print(fin_sharp)
  print('Correlation out of sample NNET:')
  c <- cor(output_predictions3, targ2)
  print(cor(output_predictions3, targ2))

  fin_sharp <- log(output_predictions4) * log(targ2)
  fin_sharp <- mean(fin_sharp)/sd(fin_sharp) * sqrt(252)
  print('Sharp on out of sample SVM:')
  print(fin_sharp)
  print('Correlation out of sample SVM:')
  c <- cor(output_predictions4, targ2)
  print(cor(output_predictions4, targ2))

  fin_sharp <- log(output_predictions5) * log(targ2)
  fin_sharp <- mean(fin_sharp)/sd(fin_sharp) * sqrt(252)
  print('Sharp on out of sample RF:')
  print(fin_sharp)
  print('Correlation out of sample RF:')
  c <- cor(output_predictions5, targ2)
  print(cor(output_predictions5, targ2))

  return(output_predictions)
}

get_weights <- function(rows){
  if(rows < 1001){
    weights <- sqrt(seq(1:rows))+10
  } else{
    weights <- rep(3, rows-1000)
    weights <- c(weights, sqrt(seq(1:1000))+3)
  }
  return(weights)
}

get_new_script <- function(name,str){
  x <-
paste("
library(Stocks)
library(xgboost)
library(e1071)
library(ranger)
library(Matrix)
library(Metrics)
library(nnet)
library(cvTools)
library(TTR)
library(brnn)
library(mailR)
library(kernlab)

full_lists <- readRDS('", name,str,".rds')
stocks <- get_data_full_day(name=c('",name,"'),  ma_days=c(3,5,10), ema=FALSE, days_close=c(1:5,10,15,20))
stocks$Time <- 1:nrow(stocks)
target <- stocks[50:nrow(stocks), '", name, '_change',"']
ind <- stocks[49:(nrow(stocks)), ]

for(i in 1:ncol(ind)){
  ind[,i] <- (ind[,i]-mean(ind[,i]))/sd(ind[,i])
}
tom <-  ind[(nrow(ind)-1):nrow(ind), ]
ind <- ind[1:(nrow(ind)-1), ]

mods <- full_lists[[3]]
weighting <- full_lists[[4]]

pgrad1 <- mods[[1]]
pbnn1 <- mods[[2]]
pnet1 <- mods[[3]]
psvm1 <- mods[[4]]
prf1 <- mods[[5]]

weights <- 1+weighting*seq(0,1, length.out = nrow(ind))
preds_final <- 0
for(iterator in 1:5){
data_sparse <- sparse.model.matrix(~-1 + ., data=as.data.frame(ind[,pgrad1[[2]]]))
dtrain <-xgb.DMatrix(data=data_sparse,label=(target)  , weight=weights)
data_sparse2 <- sparse.model.matrix(~-1 + ., data=as.data.frame(tom[,pgrad1[[2]]]))
dtest <-xgb.DMatrix(data=data_sparse2)

  model_gradient <- xgb.train(data = dtrain,
  nfold=10,
  nthread = 3,
  nrounds = pgrad1[[7]],
  max_depth = pgrad1[[3]],
  alpha = pgrad1[[4]],
  eta = 0.005,
  subsample = pgrad1[[5]],
  colsample_bytree = 0.70,
  booster = 'gbtree',
  eval_metric = 'mae',
  #metrics = list('mae'),
  maximize = FALSE,
  objective = 'reg:linear',
  print.every.n = 1000,
  verbose = TRUE,
  min_child_weight=pgrad1[[6]],
  prediction=TRUE,
  watchlist = list(train = dtrain))

  preds_grad1 <- predict(model_gradient, dtest)
  
  invisible(capture.output(mod_brnn <- brnn(x = as.matrix(ind[,pbnn1[[2]]]), y=(target),cores=8, neurons=pbnn1[[3]])))
  preds_brnn1 <- predict(mod_brnn, as.matrix(tom[, pbnn1[[2]]]))
  data2 <- as.data.frame(ind[, prf1[[2]]])
  data2$Target <- (target)
  mod_rf <- ranger(Target~., data=data2, num.trees = 2500, mtry=prf1[[3]], write.forest=TRUE, case.weights=weights)
  preds_rf1 <- unlist(predict(mod_rf, as.data.frame(tom[, prf1[[2]]]))[1])
  library(doParallel)
  library(foreach)
  registerDoParallel(cores=8)
  net <- foreach(u=1:8, .combine=cbind)  %dopar% {
      invisible(capture.output(mod_nnet <- nnet(x = as.matrix(ind[, pnet1[[2]]]), y=(target), weights=weights, size=pnet1[[3]], maxit = pnet1[[5]], linout = TRUE, decay = pnet1[[4]], MaxNWts=100000)))
      preds_nnet1 <- predict(mod_nnet, as.matrix(tom[, pnet1[[2]]]))
  }
  net <- as.data.frame(net)
  preds_nnet1 <- apply(net, 1, mean)

  invisible(capture.output(mod_svm <- ksvm(x = as.matrix(ind[,psvm1[[2]]]), y=(target), C=psvm1[[3]], gamma=psvm1[[4]], epsilon=psvm1[[5]], kernel=psvm1[[6]])))
  preds_svm1 <- predict(mod_svm, as.matrix(tom[, psvm1[[2]]]))

p_list <- list()
p_list[[1]] <- preds_grad1
p_list[[2]] <- preds_brnn1
p_list[[3]] <- preds_nnet1
p_list[[4]] <- preds_svm1
p_list[[5]] <- preds_rf1

preds <- fin_preds(p_list, full_lists[[2]])
preds <- preds[2]
preds_final <- preds*1/5 + preds_final
}
bod <- get_bod(preds_final, '",name,"', c(2*full_lists[[5]], full_lists[[5]]))

write(bod, paste('",name,"', '_email.txt',sep=''))
", sep="")

write(x,  paste(name, '_script.R', sep=''))
}

# get_nnet_cv <- function(seq_list=seq_list, samp=0.8, iter=50, gm=FALSE) {
 
#   full_list <- list()

#   train <- seq_list[[1]]
#   targ <- seq_list[[2]]  
  
#   grid1 <- list()
  
#   grid1[[1]] <- list(0.1, 100, 3)
#   grid1[[2]] <- list(0.1, 100, 4)
#   grid1[[3]] <- list(0.1, 100, 5)
#   grid1[[4]] <- list(0.025, 100, 6)
#   grid1[[5]] <- list(0.5, 100, 7)
#   grid1[[6]] <- list(0.05, 50, 8)
#   grid1[[7]] <- list(0.7, 50, 9)
#   grid1[[8]] <- list(0.7, 50, 10)
#   grid1[[9]] <- list(0.7, 50, 11)
#   grid1[[10]] <- list(0.1, 50, 3)
#   grid1[[11]] <- list(0.01, 150, 4)
#   grid1[[12]] <- list(0.1, 150, 5)
#   grid1[[13]] <- list(0.5, 150, 6)
#   grid1[[14]] <- list(0.2, 250, 7)
#   grid1[[15]] <- list(0.5, 200, 8)
#   grid1[[16]] <- list(0.2, 200, 9)
#   grid1[[17]] <- list(0.8, 200, 10)
#   grid1[[18]] <- list(0.9, 200, 11)
  
#   nnet_tune <- function(decay, maxit, size, samp_size){
#     samp <- sample(1:nrow(stocks_train), (samp_size*nrow(stocks_train)))
#     train1 <- stocks_train[samp,]
#     train2 <- stocks_train[-samp,]
#     targ1 <- target_train[samp]
#     targ2 <- target_train[-samp]
    
#     mod <- nnet(x = train1, y = targ1, decay=decay, maxit = maxit, size = size, MaxNWts=10000000000)
#     preds <- predict(mod, train2)
#     loss <- mae(preds, targ2)
#     return(loss)
#   }
  
#   tune_nnet <- function(start_grid, f){
#     for(i in 1:length(start_grid)){
#       g <- start_grid[[i]]
#       l <- f(g[[1]], g[[2]], g[[3]], 0.8)
#       start_grid[[i]][[(length(g)+1)]] <- l
#     }
#     return(start_grid)
#   }
  
#   m <- tune_nnet(grid1, nnet_tune)
#   df <- data.frame(matrix(unlist(m), nrow=18, byrow=T))
#   colnames(df) <- c('a', 'b', 'c', 'y')
  
#   grid_to_search_size <- 2:35
#   grid_to_search_decay <- seq(0.001, 1, length.out = 30)
#   grid_to_search_maxit <- seq(50, 1000, length.out = 40)
  
#   to_search <- expand.grid(grid_to_search_decay, grid_to_search_maxit, grid_to_search_size)
#   colnames(to_search) <- c('a', 'b', 'c')
  
#   for(i in 1:iter){
#     if(gm){
#       mod_nnet1 <- gam(y~s(a)+s(b)+s(c), data=df)
#     } else{
#       mod_nnet1 <- nnet(df[,1:3], df[,4], size=3, decay=0.0, , MaxNWts=1000000000)
#     }
#     preds_1 <- predict(mod_nnet1, to_search)
#     r <- to_search[which.min(preds_1), ]
#     df[18+i,4] <- nnet_tune(r[,1], r[,2], r[,3], 0.8)
#     print(r)
#     print(df[18+i, 4])
#     df[18+i,1] <- r[,1]
#     df[18+i,2] <- r[,2]
#     df[18+i,3] <- r[,3]
#   }
  
#   mod_nnet1 <- nnet(stocks_train, target_train, size=df[iter+18,3], decay=df[iter+18,1], maxit=df[iter+18,2], , MaxNWts=1000000000)
#   preds <- predict(mod_nnet1, stocks_test)
#   print(df)
#   print(cor(preds, target_test))
# }
