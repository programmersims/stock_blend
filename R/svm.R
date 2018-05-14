get_svm <- function(data, target, covs, C=c(0.001,0.01,0.05,0.1,0.2,0.4,1), gamma = c(0.0001,0.001,0.01,0.1), epsilon=c(0.1,0.05,0.15)) {
  full_models <- list()
  n <- 1
  for(i in C)
    for(j in gamma)
      for(k in epsilon)
        for(h in 1:length(covs)) {
          preds_svm <- numeric(length(target))
          set.seed(1)
          folds <- cvFolds(n = length(target), K = 10, type = "random")
          for(i in 1:10){
            invisible(capture.output(mod_svm <- svm(x = as.matrix(data[folds$which!=i, covs[[h]]]), y=(target[folds$which!=i]), C=i, gamma=j, epsilon=k, kernel='linear')))
            preds_svm[folds$which==i] <- predict(mod_svm, as.matrix(data[folds$which==i, covs[[h]]] ))
          }
          
          l <- list()
          l[[1]] <- preds_svm
          l[[2]] <- covs[[h]]
          l[[3]] <- i
          l[[4]] <- j
          l[[5]] <- k
          full_models[[n]] <- l
          n <- n+1
        }
  return(full_models)
}

get_svm3 <- function(stocks, target, samp=0.8, iter=50, prin=TRUE, var_to_keep=1, gm=FALSE, k='linear') {
  
  stocks$Time <-NULL
  target <- stocks[2:nrow(stocks),target]
  for(i in 1:ncol(stocks)){
    stocks[,i] <- (stocks[,i]- mean(stocks[,i], na.rm=TRUE))/(sd(stocks[,i], na.rm = TRUE)+0.01)
  }
  if(prin){
    stocks1 <- princomp(stocks)
    stocks <- stocks1$scores[,stocks1$sdev > var_to_keep]
    for(i in 1:ncol(stocks)){
      stocks[,i] <- (stocks[,i]- mean(stocks[,i], na.rm=TRUE))/(sd(stocks[,i], na.rm = TRUE)+0.01)
    }
  }
  for(i in 1:ncol(stocks)){
    stocks[is.na(stocks[,i]),i] <- 0
  }
  stocks <- stocks[1:(nrow(stocks)-1),]
  
  target_train <<- target[1:floor(nrow(stocks)*samp)]
  stocks_train <<- stocks[1:floor(nrow(stocks)*samp),]
  target_test <- target[(floor(nrow(stocks)*samp)+1):nrow(stocks)]
  stocks_test <- stocks[(floor(nrow(stocks)*samp)+1):nrow(stocks),]
  
  
  grid1 <- list()
  
  grid1[[1]] <- list(0.1, 0.01, 0.1)
  grid1[[2]] <- list(0.1, 0.1, 0.5)
  grid1[[3]] <- list(0.1, 0.001, 1)
  grid1[[4]] <- list(0.025, 0.0001, 0.1)
  grid1[[5]] <- list(0.5, 0.00001, 0.5)
  grid1[[6]] <- list(0.05, 0.01, 1)
  grid1[[7]] <- list(0.7, 0.1, 2)
  grid1[[8]] <- list(0.7, 0.05, 5)
  grid1[[9]] <- list(0.7, 0.0001, 0.1)
  grid1[[10]] <- list(0.1, 0.001, 0.2)
  grid1[[11]] <- list(0.01, 0.001, 2)
  grid1[[12]] <- list(0.1, 0.0001, 1)
  grid1[[13]] <- list(0.5, 0.001, 1)
  grid1[[14]] <- list(0.2, 0.01, 0.5)
  grid1[[15]] <- list(0.5, 0.01, 0.05)
  grid1[[16]] <- list(0.2, 0.003, 0.1)
  grid1[[17]] <- list(0.8, 0.002, 2)
  grid1[[18]] <- list(0.9, 0.02, 3)
  
  svm_tune <- function(eps, nu, cost, samp_size){
    samp <- sample(1:nrow(stocks_train), (samp_size*nrow(stocks_train)))
    train1 <- stocks_train[samp,]
    train2 <- stocks_train[-samp,]
    targ1 <- target_train[samp]
    targ2 <- target_train[-samp]
    
    mod <- svm(x = train1, y = targ1, kernel=k, epsilon=eps, nu=nu, cost = cost)
    preds <- predict(mod, train2)
    loss <- mae(preds, targ2)
    print(loss)
    return(loss)
  }
  
  tune_svm <- function(start_grid, f){
    for(i in 1:length(start_grid)){
      g <- start_grid[[i]]
      l <- f(g[[1]], g[[2]], g[[3]], 0.8)
      start_grid[[i]][[(length(g)+1)]] <- l
    }
    return(start_grid)
  }
  
  m <- tune_svm(grid1, svm_tune)
  df <- data.frame(matrix(unlist(m), nrow=18, byrow=T))
  colnames(df) <- c('a', 'b', 'c', 'y')
  
  grid_to_search_eps <- seq(0.0001, 0.25, length.out = 30)
  grid_to_search_nu <- seq(0.0001, 0.1, length.out = 100)
  grid_to_search_cost <- seq(0.01, 10, length.out = 40)
  
  to_search <- expand.grid(grid_to_search_eps, grid_to_search_nu, grid_to_search_cost)
  colnames(to_search) <- c('a', 'b', 'c')
  
  for(i in 1:iter){
    if(gm){
      mod_svm1 <- gam(y~s(a)+s(b)+s(c), data=df)
    } else{
      mod_svm1 <- nnet(x=df[,1:3], y=df[,4], size=4)
    }
    preds_1 <- predict(mod_svm1, to_search)
    r <- to_search[which.min(preds_1), ]
    df[18+i,4] <- svm_tune(r[,1], r[,2], r[,3], 0.8)
    print(r)
    print(df[18+i, 4])
    df[18+i,1] <- r[,1]
    df[18+i,2] <- r[,2]
    df[18+i,3] <- r[,3]
  }
  
  mod_svm1 <- svm(x=stocks_train, y=target_train,kernel=k, cost=df[iter+18,3], eps=df[iter+18,1], nu=df[iter+18,2])
  preds <- predict(mod_svm1, stocks_test)
  print(df)
  print(cor(preds, target_test))
}