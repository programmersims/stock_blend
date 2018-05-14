get_strong_models <- function(stocks, target_name, p_values = c(0.05,0.01), proportion = 0.8, abs_v=0.5, variation_to_keep = 1, pca=FALSE, intra=FALSE,
                              depth=c(6,7), alpha=c(0.05, 0.1, 0.15), subsamp=c(0.5,0.7),  min_weight=c(10, 50, 100),
                              C=c(0.01,0.02,0.05,0.1,0.4), gamma = c(0.0001,0.001,0.01,0.1), epsilon=c(0.1,0.05,0.15),
                              decay=c(0.5, 0.3, 0.1), maxit=200, size=c(5,10,25), neurons = c(1, 2), tree=150, try=c(2,5,10)){

  if(!intra){
    target <- stocks[30:nrow(stocks), target_name]
    ind <- stocks[29:(nrow(stocks)-1), ]
    ind$Time <- NULL
  } else {
    target <- stocks[30:nrow(stocks), 'Target']
    ind <- stocks[30:(nrow(stocks)),]
    ind$Target <- NULL
    ind$Date <- NULL
    ind$Time_hour <- NULL
    ind$Time <- NULL
  }

  ind2 <- ind[(floor(length(target)*proportion)+1):length(target),]
  target2 <- target[(floor(length(target)*proportion)+1):length(target)]

  ind <- ind[1:floor(length(target)*proportion),]
  target <- target[1:floor(length(target)*proportion)]

  for(i in 1:ncol(ind)){
    ind2[,i] <- (ind2[,i]-mean(ind[,i]))/sd(ind[,i])
    ind[,i] <- (ind[,i]-mean(ind[,i]))/sd(ind[,i])
  }
  if(!pca){
    ll <- get_ls(data=ind, target = target, p_values = p_values)
    ll_covs <- sapply(ll, "[[", 2)
    ll_covs[[1]] <- NULL
  }
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

  l2 <- get_ls(data=as.data.frame(prin_ind), target = target, p_values = p_values, 'PCA')
  l2_covs <- sapply(l2, "[[", 2)
  l2_covs[[1]] <- colnames(prin_ind)[1:20]



  prin_feat <- get_xgb_predictors(data = prin_ind, target = target)
  l2_covs[[(length(l2_covs)+1)]] <- prin_feat
  #rf <- get_rf(data = prin_ind, target = target, covs = l2_covs, mtry=try, tree=tree)
  bn2 <- get_brnn(data = prin_ind, target = target, covs = l2_covs, neurons = neurons, pca_string = "PCA")
  nn2 <- get_nnet(data = prin_ind, target = target, covs = l2_covs, pca_string = "", decay = decay, maxit = maxit, size = size)

  if(!pca){
    feat <- get_xgb_predictors(data = ind, target = target)
    ll_covs[[(length(ll_covs)+1)]] <- feat
    bn1 <- get_brnn(data = ind, target = target, covs = ll_covs, neurons = neurons, pca_string = "")
    nn1 <- get_nnet(data = ind, target = target, covs = ll_covs, pca_string = "", decay = decay, maxit = maxit, size = size)
  }

  xgb2 <- get_xgb(data = prin_ind[,prin_feat], target = target, depth = depth, alpha = alpha, subsamp = subsamp, min_weight = min_weight)

  s2 <- get_svm(data=prin_ind, target = target, covs = l2_covs, C = C, gamma = gamma, epsilon = epsilon)

  #lr <- get_lasso_ridge_summaries(data=ind, target = target)

  mods <- list()
  mods[[1]] <- xgb2
  mods[[2]] <- bn2
  mods[[3]] <- nn2
  mods[[4]] <- s2

  if(!pca){
    feat <- get_xgb_predictors(data = ind, target = target)
    xgb1 <- get_xgb(data = ind[,feat], target = target, depth = depth, alpha = alpha, subsamp = subsamp, min_weight = min_weight)
    ll_covs[[(length(ll_covs)+1)]] <- feat

    s1 <- get_svm(data=ind, target = target, covs = ll_covs, C = C, gamma = gamma, epsilon = epsilon)
    bn1 <- get_brnn(data = ind, target = target, covs = ll_covs, neurons = neurons, pca_string = "")
    nn1 <- get_nnet(data = ind, target = target, covs = ll_covs, pca_string = "", decay = decay, maxit = maxit, size = size)

    mods[[5]] <- xgb1
    mods[[6]] <- bn1
    mods[[7]] <- nn1
    mods[[8]] <- s1
  }

  fin_list <- list()
  fin_list[[1]] <- mods
  fin_list[[2]] <- prin_ind
  fin_list[[3]] <- target
  fin_list[[4]] <- prin_ind2
  fin_list[[5]] <- target2
  fin_list[[6]] <- ind
  fin_list[[7]] <- ind2
  fin_list[[8]] <- prin_list
  fin_list[[9]] <- target_name
  #fit_models(mods, prin_ind, target, prin_ind2, target2)
  return(fin_list)
}

get_strong_models_full_day <- function(stocks, target_name, p_values = c(0.05,0.01), proportion = 0.8,
                                       depth=c(6,7), alpha=c(0.05, 0.1, 0.15), subsamp=c(0.5,0.7),  min_weight=c(10, 50, 100),
                                       C=c(0.01,0.02,0.05,0.1,0.4), gamma = c(0.0001,0.001,0.01,0.1), epsilon=c(0.1,0.05,0.15),
                                       decay=c(0.5, 0.3, 0.1), maxit=200, size=c(5,10,25), neurons = c(1, 2), tree=150, try=c(2,5,10)){

  target <- stocks[30:nrow(stocks), target_name]
  ind <- stocks[29:(nrow(stocks)-1), ]
  ind$Time <- NULL

  ind2 <- ind[(floor(length(target)*proportion)+1):length(target),]
  target2 <- target[(floor(length(target)*proportion)+1):length(target)]

  ind <- ind[1:floor(length(target)*proportion),]
  target <- target[1:floor(length(target)*proportion)]

  for(i in 1:ncol(ind)){
    ind2[,i] <- (ind2[,i]-mean(ind[,i]))/sd(ind[,i])
    ind[,i] <- (ind[,i]-mean(ind[,i]))/sd(ind[,i])
  }

  ll <- get_ls(data=ind, target = target, p_values = p_values)
  ll_covs <- sapply(ll, "[[", 2)
  ll_covs[[1]] <- NULL

  feat <- get_xgb_predictors(data = ind, target = target)
  ll_covs[[(length(ll_covs)+1)]] <- feat
  bn1 <- get_brnn(data = ind, target = target, covs = ll_covs, neurons = neurons, pca_string = "")
  nn1 <- get_nnet(data = ind, target = target, covs = ll_covs, pca_string = "", decay = decay, maxit = maxit, size = size)
  xgb1 <- get_xgb(data = ind[,feat], target = target, depth = depth, alpha = alpha, subsamp = subsamp, min_weight = min_weight)
  s1 <- get_svm(data=ind, target = target, covs = ll_covs, C = C, gamma = gamma, epsilon = epsilon)
  rf1 <- get_rf(data = ind, target = target, covs = ll_covs, mtry = try, tree = tree, cores = 7)

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
  return(fin_list)
}

fit_models_full_day <- function(full_list, bias_proportion=0, weighting=0, corr=FALSE, mm =TRUE){

  mods <- full_list[[1]]
  targ <- full_list[[2]]
  targ2 <- full_list[[3]]
  ind <- full_list[[4]]
  ind2 <- full_list[[5]]

  corr <<- corr
  mm <<- mm
  grad1 <- mods[[1]]
  bnn1 <- mods[[2]]
  net1 <- mods[[3]]
  svm1 <- mods[[4]]
  rf1 <- mods[[5]]

  pos_grad1 <- get_best_model(grad1, targ)[[2]]
  pos_brnn1 <- get_best_model(bnn1, targ)[[2]]
  pos_net1 <- get_best_model(net1, targ)[[2]]
  pos_svm1 <- get_best_model(svm1, targ)[[2]]
  pos_rf1 <- get_best_model(rf1, targ)[[2]]

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
  full_list2[[1]] <- mods2
  full_list2[2:6] <- full_list[2:6]

  target <<- targ
  ratios <<- optim(par = rep(1, (length(mods2)-1)), fn = opt, lower = 0, upper = 100, method = 'L-BFGS-B')$par

  print('Ratios of models')
  print(ratios)

  fin_sharp <- log(fin_preds(p_list, ratios)) * log(targ)
  fin_sharp  <- mean(fin_sharp)/sd(fin_sharp)*sqrt(252)
  print('Sharp of in sample:')
  print(fin_sharp)
  print('Corr of in sample:')
  print(cor(targ, fin_preds(p_list, ratios)))

  output_predictions <- numeric(length(targ2))
  the_vals <- ceiling(seq(1, length(targ2), length.out = ceiling(length(targ2)*0.1)))
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

    weights <- 1+weighting*seq(0,1, length.out = nrow(data))
    data_sparse <- sparse.model.matrix(~-1 + ., data=as.data.frame(data[,pgrad1[[2]]]))
    dtrain <-xgb.DMatrix(data=data_sparse,label=(targ3), weight=weights)
    data_sparse2 <- sparse.model.matrix(~-1 + ., data=as.data.frame(to_be_fitted[,pgrad1[[2]]]))
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

    invisible(capture.output(mod_brnn <- brnn(x = as.matrix(data[,pbnn1[[2]]]), y=(targ3), neurons=pbnn1[[3]])))
    preds_brnn1 <- predict(mod_brnn, as.matrix(to_be_fitted[, pbnn1[[2]]]))
    invisible(capture.output(mod_nnet <- nnet(x = as.matrix(data[, pnet1[[2]]]), y=(targ3), size=pnet1[[3]], maxit = pnet1[[5]], linout = TRUE, decay = pnet1[[4]], MaxNWts=100000)))
    preds_nnet1 <- predict(mod_nnet, as.matrix(to_be_fitted[, pnet1[[2]]]))
    invisible(capture.output(mod_svm <- svm(x = as.matrix(data[,psvm1[[2]]]), y=(targ3), C=psvm1[[3]], gamma=psvm1[[4]], epsilon=psvm1[[5]])))
    preds_svm1 <- predict(mod_svm, as.matrix(to_be_fitted[, psvm1[[2]]]))
    mod_rf <- foreach(ntree = rep(floor(500/prf1[[4]]), prf1[[4]]), .combine = combine, .packages = "randomForest") %dopar%
      randomForest(x = as.matrix(data[, prf1[[2]]]), y=(targ3), ntree = 500, mtry=prf1[[3]])
    preds_rf1 <- predict(mod_rf, as.matrix(to_be_fitted[, prf1[[2]]]))


    pca_list[[1]] <- preds_grad1
    pca_list[[2]] <- preds_brnn1
    pca_list[[3]] <- preds_nnet1
    pca_list[[4]] <- preds_svm1
    pca_list[[5]] <- preds_rf1

    if(temp > 1){
      output_predictions[(temp+1):i] <- fin_preds(pca_list, ratios)
    } else{
      output_predictions[1:i] <- fin_preds(pca_list, ratios)
    }
    temp <- i
  }
  output_predictions <- output_predictions + bias*bias_proportion
  #full_list2[[10]] <- ratios
  #full_list2[[11]] <- sd(output_predictions)
  fin_sharp <- log(output_predictions) * log(targ2)
  fin_sharp <- mean(fin_sharp)/sd(fin_sharp) * sqrt(252)
  print('Sharp on out of sample:')
  print(fin_sharp)
  print('Correlation out of sample:')
  print(cor(output_predictions, targ2))
  saveRDS(full_list2, paste(full_list[[9]], '.rds', sep=""))
  return(output_predictions)
}

fit_models <- function(full_list, bias_proportion=0, weighting=0, corr=TRUE, mm =FALSE){

  mods <- full_list[[1]]
  prin <- full_list[[2]]
  targ <- full_list[[3]]
  prin2 <- full_list[[4]]
  targ2 <- full_list[[5]]
  ind <- full_list[[6]]
  ind2 <- full_list[[7]]
  prin_list <- full_list[[8]]
  pca <- prin_list[[6]]

  corr <<- corr
  mm <<- mm
  grad2 <- mods[[1]]
  bnn2 <- mods[[2]]
  net2 <- mods[[3]]
  svm2 <- mods[[4]]

  pos_grad2 <- get_best_model(grad2, targ)[[2]]
  pos_brnn2 <- get_best_model(bnn2, targ)[[2]]
  pos_net2 <- get_best_model(net2, targ)[[2]]
  pos_svm2 <- get_best_model(svm2, targ)[[2]]

  pgrad2 <- grad2[[pos_grad2]]
  pbnn2 <- bnn2[[pos_brnn2]]
  pnet2 <- net2[[pos_net2]]
  psvm2 <- svm2[[pos_svm2]]

  mods2 <- list()
  mods2[[1]] <- pgrad2
  mods2[[2]] <- pbnn2
  mods2[[3]] <- pnet2
  mods2[[4]] <- psvm2

  p_list <<- list()

  p_list[[1]] <<- pgrad2[[1]]
  p_list[[2]] <<- pbnn2[[1]]
  p_list[[3]] <<- pnet2[[1]]
  p_list[[4]] <<- psvm2[[1]]

  if(!pca){
    grad1 <- mods[[5]]
    bnn1 <- mods[[6]]
    net1 <- mods[[7]]
    svm1 <- mods[[8]]

    pos_grad1 <- get_best_model(grad1, targ)[[2]]
    pos_brnn1 <- get_best_model(bnn1, targ)[[2]]
    pos_net1 <- get_best_model(net1, targ)[[2]]
    pos_svm1 <- get_best_model(svm1, targ)[[2]]

    pgrad1 <- grad1[[pos_grad1]]
    pbnn1 <- bnn1[[pos_brnn1]]
    pnet1 <- net1[[pos_net1]]
    psvm1 <- svm1[[pos_svm1]]

    mods2[[5]] <- pgrad1
    mods2[[6]] <- pbnn1
    mods2[[7]] <- pnet1
    mods2[[8]] <- psvm1

    p_list[[5]] <<- pgrad1[[1]]
    p_list[[6]] <<- pbnn1[[1]]
    p_list[[7]] <<- pnet1[[1]]
    p_list[[8]] <<- psvm1[[1]]
  }

  full_list2 <- list()
  full_list2[[1]] <- mods2
  full_list2[2:9] <- full_list[2:9]

  target <<- targ
  if(!pca){
    ratios <<- optim(par = c(1,1,1,1,1,1,1), fn = opt, lower = 0, upper = 100, method = 'L-BFGS-B')$par
  } else{
    ratios <<- optim(par = c(1.25,1.25,1.25), fn = opt, lower = 0, upper = 100, method = 'L-BFGS-B')$par
    #ratios <<-  c(0.5,0.5,0.5) + ratios
  }
  print('Ratios of models')
  print(ratios)

  fin_sharp <- log(fin_preds(p_list, ratios)) * log(targ)
  fin_sharp  <- mean(fin_sharp)/sd(fin_sharp)*sqrt(252)
  print('Sharp of in sample:')
  print(fin_sharp)

  output_predictions <- numeric(length(targ2))
  the_vals <- ceiling(seq(1, length(targ2), length.out = ceiling(length(targ2)*0.1)))
  the_vals <- the_vals[2:length(the_vals)]
  temp <- 1

  bias <- (1-mean(fin_preds(p_list, ratios)))

  for(i in 1:ncol(ind2)){
    ind2[is.na(ind2[,i]),i] <- median(ind2[,i], na.rm = TRUE)
  }
  for(i in 1:ncol(prin2)){
    prin2[is.na(prin2[,i]),i] <- 0
  }

  for(i in the_vals){
    if(temp > 1){
      data <- rbind(ind, ind2[1:temp,])
      prin_data <- rbind(prin, prin2[1:temp,])
      targ3 <- c(targ, targ2[1:temp])
      to_be_fitted <- ind2[(temp+1):i,]
      p_to_be_fitted <- prin2[(temp+1):i,]
    }
    else{
      data <- ind
      prin_data <- prin
      targ3 <- targ
      to_be_fitted <- ind2[1:i,]
      p_to_be_fitted <- prin2[1:i,]
    }

    weights <- 1+weighting*seq(0,1, length.out = nrow(data))
    pca_list <- list()
    if(!pca){
      data_sparse <- sparse.model.matrix(~-1 + ., data=as.data.frame(data[,pgrad1[[2]]]))
      dtrain <-xgb.DMatrix(data=data_sparse,label=(targ3))  #, weight=weights)
      data_sparse2 <- sparse.model.matrix(~-1 + ., data=as.data.frame(to_be_fitted[,pgrad1[[2]]]))
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

      invisible(capture.output(mod_brnn <- brnn(x = as.matrix(data[,pbnn1[[2]]]), y=(targ3), neurons=pbnn1[[3]])))
      preds_brnn1 <- predict(mod_brnn, as.matrix(to_be_fitted[, pbnn1[[2]]]))
      invisible(capture.output(mod_nnet <- nnet(x = as.matrix(data[, pnet1[[2]]]), y=(targ3), size=pnet1[[3]], maxit = pnet1[[5]], linout = TRUE, decay = pnet1[[4]], MaxNWts=100000)))
      preds_nnet1 <- predict(mod_nnet, as.matrix(to_be_fitted[, pnet1[[2]]]))
      invisible(capture.output(mod_svm <- svm(x = as.matrix(data[,psvm1[[2]]]), y=(targ3), C=psvm1[[3]], gamma=psvm1[[4]], epsilon=psvm1[[5]])))
      preds_svm1 <- predict(mod_svm, as.matrix(to_be_fitted[, psvm1[[2]]]))
    }
    weights <- 1+weighting*seq(0,1, length.out = nrow(data))
    data_sparse <- sparse.model.matrix(~-1 + ., data=as.data.frame(prin_data[,pgrad2[[2]]]))
    dtrain <-xgb.DMatrix(data=data_sparse,label=(targ3) , weight=weights)
    data_sparse2 <- sparse.model.matrix(~-1 + ., data=as.data.frame(p_to_be_fitted[,pgrad2[[2]]]))
    dtest <-xgb.DMatrix(data=data_sparse2)


    model_gradient <- xgb.train(data = dtrain,
                                nfold=10,
                                nthread = 3,
                                nrounds = pgrad2[[7]],
                                max_depth = pgrad2[[3]],
                                alpha = pgrad2[[4]],
                                eta = 0.005,
                                subsample = pgrad2[[5]],
                                colsample_bytree = 0.70,
                                booster = "gbtree",
                                eval_metric = 'mae',
                                #metrics = list("mae"),
                                maximize = FALSE,
                                objective = "reg:linear",
                                print.every.n = 1000,
                                verbose = TRUE,
                                min_child_weight=pgrad2[[6]],
                                prediction=TRUE,
                                watchlist = list(train = dtrain))

    preds_grad2 <- predict(model_gradient, dtest)

    invisible(capture.output(mod_brnn <- brnn(x = as.matrix(prin_data[,pbnn2[[2]]]), y=(targ3), neurons=pbnn2[[3]])))
    preds_brnn2 <- predict(mod_brnn, as.matrix(p_to_be_fitted[, pbnn2[[2]]]))
    invisible(capture.output(mod_nnet <- nnet(x = as.matrix(prin_data[, pnet2[[2]]]), y=(targ3), size=pnet2[[3]], maxit = pnet2[[5]], linout = TRUE, decay = pnet2[[4]], MaxNWts=100000)))
    preds_nnet2 <- predict(mod_nnet, as.matrix(p_to_be_fitted[, pnet2[[2]]]))
    invisible(capture.output(mod_svm <- svm(x = as.matrix(prin_data[,psvm2[[2]]]), y=(targ3), C=psvm2[[3]], gamma=psvm2[[4]], epsilon=psvm2[[5]],  kernel='linear')))
    preds_svm2 <- predict(mod_svm, as.matrix(p_to_be_fitted[, psvm2[[2]]]))


    pca_list[[1]] <- preds_grad2
    pca_list[[2]] <- preds_brnn2
    pca_list[[3]] <- preds_nnet2
    pca_list[[4]] <- preds_svm2
    if(!pca){
      pca_list[[5]] <- preds_grad1
      pca_list[[6]] <- preds_brnn1
      pca_list[[7]] <- preds_nnet1
      pca_list[[8]] <- preds_svm1
    }
    if(temp > 1){
      output_predictions[(temp+1):i] <- fin_preds(pca_list, ratios)
    } else{
      output_predictions[1:i] <- fin_preds(pca_list, ratios)
    }
    temp <- i
  }
  output_predictions <- output_predictions + bias*bias_proportion
  full_list2[[10]] <- ratios
  full_list2[[11]] <- sd(output_predictions)
  fin_sharp <- log(output_predictions) * log(targ2)
  fin_sharp <- mean(fin_sharp)/sd(fin_sharp) * sqrt(252)
  print('Sharp on out of sample:')
  print(fin_sharp)
  print('Correlation out of sample:')
  print(cor(output_predictions, targ2))
  saveRDS(full_list2, paste(full_list[[9]], '.rds', sep=""))
  return(output_predictions)
}

