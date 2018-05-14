#@param data - dataframe of predictors
#@param target - vector of target
#@param covs - list of covariates to iterate through when fiting cross-validated neural networks
#@param decay - vector of decay values to try when fitting cross-validated neural networks
#@param size - vector of size values to try when fitting cross-validated neural networks
#@param maxit - max iterations of neural network
#@param pca_string - string to use that prints if using principle components
#prints cross validated least squares correlation and sharp ratio or neural net

#return - list of cross validated predicted values, covariates used, size, decay, and max iterations

get_nnet <- function(data, target, covs, pca_string="", decay=c(0.1,0.01,0.001), maxit=100, size=c(5,10,20,50)) {
  full_list <- list()
  preds_nnet <- numeric(length(target))
  set.seed(1)
  folds <- cvFolds(n = length(target), K = 10, type = "random")
  a <- 1
  for(j in 1:length(covs)){
    for(k in size){
      for(h in decay){
        for(i in 1:10){
          invisible(capture.output(mod_nnet <- nnet(x = as.matrix(data[folds$which!=i, covs[[j]]]), y=(target[folds$which!=i]), size=k, maxit = maxit, linout = TRUE, decay = h, MaxNWts=100000)))
          preds_nnet[folds$which==i] <- predict(mod_nnet, as.matrix(data[folds$which==i, covs[[j]]]))
        }
        print(paste('Correlation of', k ,'Neurons', pca_string,'Neural Net','decay=', h,':'))
        corr <- cor(preds_nnet, target)
        print(corr)
        print(paste('Annual Sharp Ratio of',  k,'Neurons', pca_string,'Neural Net', 'decay=', h,':'))
        l <- log(preds_nnet) * log(target)
        sharp <- sqrt(250) * mean(l)/sd(l)
        print(sharp)
        l1 <- list()
        l1[[1]] <- preds_nnet
        l1[[2]] <- covs[[j]]
        l1[[3]] <- k
        l1[[4]] <- h
        l1[[5]] <- maxit
        full_list[[a]] <- l1
        a <- a+1
      }
    }
  }
  return(full_list)
}


get_nnet2 <- function(stocks, target, samp=0.8, iter=50, prin=TRUE, var_to_keep=1, gm=FALSE) {
  
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
  
  grid1[[1]] <- list(0.1, 100, 3)
  grid1[[2]] <- list(0.1, 100, 4)
  grid1[[3]] <- list(0.1, 100, 5)
  grid1[[4]] <- list(0.025, 100, 6)
  grid1[[5]] <- list(0.5, 100, 7)
  grid1[[6]] <- list(0.05, 50, 8)
  grid1[[7]] <- list(0.7, 50, 9)
  grid1[[8]] <- list(0.7, 50, 10)
  grid1[[9]] <- list(0.7, 50, 11)
  grid1[[10]] <- list(0.1, 50, 3)
  grid1[[11]] <- list(0.01, 150, 4)
  grid1[[12]] <- list(0.1, 150, 5)
  grid1[[13]] <- list(0.5, 150, 6)
  grid1[[14]] <- list(0.2, 250, 7)
  grid1[[15]] <- list(0.5, 200, 8)
  grid1[[16]] <- list(0.2, 200, 9)
  grid1[[17]] <- list(0.8, 200, 10)
  grid1[[18]] <- list(0.9, 200, 11)
  
  nnet_tune <- function(decay, maxit, size, samp_size){
    samp <- sample(1:nrow(stocks_train), (samp_size*nrow(stocks_train)))
    train1 <- stocks_train[samp,]
    train2 <- stocks_train[-samp,]
    targ1 <- target_train[samp]
    targ2 <- target_train[-samp]
    
    mod <- nnet(x = train1, y = targ1, decay=decay, maxit = maxit, size = size, MaxNWts=10000000000)
    preds <- predict(mod, train2)
    loss <- mae(preds, targ2)
    return(loss)
  }
  
  tune_nnet <- function(start_grid, f){
    for(i in 1:length(start_grid)){
      g <- start_grid[[i]]
      l <- f(g[[1]], g[[2]], g[[3]], 0.8)
      start_grid[[i]][[(length(g)+1)]] <- l
    }
    return(start_grid)
  }
  
  m <- tune_nnet(grid1, nnet_tune)
  df <- data.frame(matrix(unlist(m), nrow=18, byrow=T))
  colnames(df) <- c('a', 'b', 'c', 'y')
  
  grid_to_search_size <- 2:35
  grid_to_search_decay <- seq(0.001, 1, length.out = 30)
  grid_to_search_maxit <- seq(50, 1000, length.out = 40)
  
  to_search <- expand.grid(grid_to_search_decay, grid_to_search_maxit, grid_to_search_size)
  colnames(to_search) <- c('a', 'b', 'c')
  
  for(i in 1:iter){
    if(gm){
      mod_nnet1 <- gam(y~s(a)+s(b)+s(c), data=df)
    } else{
      mod_nnet1 <- nnet(df[,1:3], df[,4], size=3, decay=0.0, , MaxNWts=1000000000)
    }
    preds_1 <- predict(mod_nnet1, to_search)
    r <- to_search[which.min(preds_1), ]
    df[18+i,4] <- nnet_tune(r[,1], r[,2], r[,3], 0.8)
    print(r)
    print(df[18+i, 4])
    df[18+i,1] <- r[,1]
    df[18+i,2] <- r[,2]
    df[18+i,3] <- r[,3]
  }
  
  mod_nnet1 <- nnet(stocks_train, target_train, size=df[iter+18,3], decay=df[iter+18,1], maxit=df[iter+18,2], , MaxNWts=1000000000)
  preds <- predict(mod_nnet1, stocks_test)
  print(df)
  print(cor(preds, target_test))
}
