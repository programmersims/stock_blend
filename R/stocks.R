# library(xgboost)
# library(e1071)
# library(Matrix)
# library(Metrics)
# library(brnn)
# library(cvTools)
# library(TTR)
# library(brnn)
# library(mailR)
# 
# #@param x - prediction vector
# #@param y - target vector
# 
# #@return - return stream vector
# 
# xor1 <- function(x,y){
#   if(y > 1 & x > 1){
#     return(y)
#   }
#   if(y < 1 & x < 1){
#     return((1+abs(1-y)))
#   }
#   else{
#     return((1-abs(1-y)))
#   }
# }
# 
# #@param stream - stream of stock percents
# #@param size - size of volatility stream
# 
# #descrpition: This function returns the standard deviation of the previous SIZE days for a stream of stocks percents
# 
# volatility_days <- function(stream, size){
#   new_stream <- numeric(length(stream))
#   for(i in (size+1):length(stream)){
#     new_stream[i] <- sd(stream[(i-size):i])
#   }
#   new_stream[new_stream==0] <- NA
#   return(new_stream)
# }
# 
# corr_days <- function(stocks, target_stock, days=c(20,50, 100), interaction=list(c(10,50)), method='spearman'){
#   names <- unlist(unique(lapply((strsplit(colnames(stocks),split = '_')), '[[', 1)))
#   names <- names[names!='Time']
#   for(i in 1:length(names)){
#     names[i] <- paste(names[i], 'change', sep = '_')
#   }
#   print(names)
#   for(i in names){
#     if(i != target_stock){
#       for(j in days){
#        for(k in (j+1):nrow(stocks)){
#           stocks[k, paste(i, j,'cor', target_stock,sep='_')] <- cor(stocks[(k-j):k,i], stocks[(k-j):k,target_stock], method=method)
#         }
#       }
#     }
#   }
#   return(stocks)
# }
# 
# corr_days2 <- function(stocks, target_stock, days=c(100), method='spearman', d=5){
#   target_stock <- paste(target_stock, 'close_divided_', d, 'close', sep = '_')
#   names <- unlist(unique(lapply((strsplit(colnames(stocks),split = '_')), '[[', 1)))
#   names <- names[names!='Time']
#   for(i in 1:length(names)){
#     names[i] <- paste(names[i], 'close_divided_',d,'close', sep = '_')
#   }
#   for(i in names){
#     if(i != target_stock){
#       for(j in days){
#         for(k in seq((j+1),nrow(stocks), by = 1)){
#           stocks[k, paste(i, j,'cor', target_stock,sep='_')] <- cor(stocks[seq((k-j),k,by=d),i], stocks[seq((k-j),k,by=d), target_stock], method=method)
#         }
#       }
#     }
#   }
#   return(stocks)
# }
# 
# 
# relationship_vars <- function(stocks, target_stock, days=c(50), method='spearman'){
#   names <- unlist(unique(lapply((strsplit(colnames(stocks),split = '_')), '[[', 1)))
#   names <- names[names!='Time']
#   for(i in 1:length(names)){
#     names[i] <- paste(names[i], 'change', sep = '_')
#   }
#   for(i in names){
#     if(i != target_stock){
#       for(j in days){
#         for(k in (j+1):nrow(stocks)){
#           stocks[k, paste(i, j,'rel', target_stock,sep='_')] <- cor(stocks[(k-j):k,i], stocks[(k-j):k,target_stock], method=method) * (stocks[k,i]-1)/sd(stocks[(k-j):k, i] )
#         }
#       }
#     }
#   }
#   return(stocks)
# }
# 
# relationship_vars2 <- function(stocks, target_stock, days=c(50), method='spearman'){
#   names <- unlist(unique(lapply((strsplit(colnames(stocks),split = '_')), '[[', 1)))
#   names <- names[names!='Time']
#   names2 <- names
#   for(i in 1:length(names)){
#     names2[i] <- paste(names[i], 'SMA_percRange_9', sep = '_')
#     names[i] <- paste(names[i], 'change', sep = '_')
#   }
#   d <- 0
#   for(i in names){
#     d <- d + 1
#     if(i != target_stock){
#       for(j in days){
#         for(k in (j+1):nrow(stocks)){
#           stocks[k, paste(i, j,'rel', target_stock,sep='_')] <- (cor(stocks[(k-j):k,i], stocks[(k-j):k,target_stock])*0.5+cor(stocks[(k-j):k,i], stocks[(k-j):k,target_stock], method='spearman')*0.5) *
#             (stocks[k,i]-1)/(((stocks[k,names2[d]])+0.00001)*sd(stocks[(k-j):k, i] ))
#         }
#       }
#     }
#   }
#   return(stocks)
# }
# 
# #@param fin - final prediction
# #@param name - name of target
# #@param nums - vector (usually of standard deviations)  to tell how large movement is
# 
# #return - body to be used in crontab email
# 
# get_bod <- function(fin, name, nums){
#   if(fin > (1+nums[1])){
#     bod = paste("This is a strong upward", name, "movement predicted at:", fin)
#   }
#   if((fin > (1+nums[2])) & (fin < (1+nums[1]))){
#     bod = paste("This is a moderately strong upward", name, "movement  predicted at:", fin)
#   }
#   if((fin > 1) & (fin < (1+nums[2]))){
#     bod = paste("This is a weak upward", name, "movement  predicted at:", fin)
#   }
#   if((fin > (1-nums[2])) & (fin < 1)){
#     bod = paste("This is a weak downward",name ,"movement  predicted at:", fin)
#   }
#   if((fin > (1-nums[1])) & (fin < (1-nums[2]))){
#     bod = paste("This is a moderate downward", name, "movement  predicted at:", fin)
#   }
#   if(fin < (1-nums[1])){
#     bod = paste("This is a strong downward", name, "movement  predicted at:", fin)
#   }
#   return(bod)
# }
# 
# 
# get_data_full_day <- function(name, ma_days=c(), adx_days=c(), sd_days=c(), sd_days_interaction=list(), macd=FALSE, ema=FALSE, days_close=c(5)){
# 
#   time <- "390"
#   str <- "5"
#   command <- paste("python vxx_daily", str, ".py --lista", sep="")
# 
#   for(i in name){
#     command <- paste(command, i)
#   }
#   command <- paste(command, "--time_in_minutes", time)
#   system(command = command)
# 
#   stocks <- list()
#   j <- 1
#   for(i in name){
#     s <- read.csv(paste(i, '.csv', sep=""), header = FALSE)
#     stocks[[j]] <- s
#     j <- j+1
#   }
#   for(i in 1:length(stocks)){
#     colnames(stocks[[i]]) <- c('Time', 'High', 'Low', 'Open', 'Close', 't1', 't2', 't3')
#   }
# 
#   for(i in 1:length(stocks)){
#     stocks[[i]] <- get_good_columns_full_day(stocks[[i]], ma_days = ma_days, adx_days=adx_days, sd_days=sd_days, sd_days_interaction=sd_days_interaction, macd=macd, ema=ema, days_close = days_close)
#     colnames(stocks[[i]])[2:ncol(stocks[[i]])] <- paste(name[i], colnames(stocks[[i]])[2:ncol(stocks[[i]])] , sep="_")
#   }
# 
#   stocks <-Reduce(merge, stocks)
#   return(stocks)
# }
# 
# get_good_columns_full_day <- function(x, ma_days=c(), adx_days=c(), sd_days=c(), sd_days_interaction=list(), macd=FALSE, ema=FALSE, days_close=c(5)){
#   x <- x[nrow(x):1,]
# 
#   x$percRange<-x$High/x$Low
#   #x$highOpen<-x$High/x$Open
#   #x$highClose<-x$High/x$Close
#   #x$lowOpen<-x$Low/x$Open
#   #x$lowClose<-x$Low/x$Close
#   x$change<-x$Close/x$Open
#   x$changeRange<-x$change/x$percRange
#   x$t3 <- NULL
# 
#   for(i in 2:ncol(x)){
#     x[!is.finite(x[,i]), i] <- mean(x[is.finite(x[,i]),i], na.rm=TRUE)
#     x[!is.numeric(x[,i]),i] <- mean(x[is.numeric(x[,i]),i], na.rm=TRUE)
#   }
# 
#   for(i in ma_days){
#     x[,paste('SMA_percRange', i, sep='_')] <- SMA(x$percRange, i)
#     x[,paste('SMA_change', i, sep='_')] <- SMA(x$change, i)
#     x[,paste('SMA_changeRange', i, sep='_')] <- SMA(x$changeRange, i)
#     if(ema) {
#       x[,paste('EMA_change', i, sep='_')] <- EMA(x$change, i)
#       x[,paste('EMA_percRange', i, sep='_')] <- EMA(x$percRange, i)
#       x[,paste('EMA_changeRange', i, sep='_')] <- EMA(x$changeRange, i)
#     }
#   }
#   #elimates redundancy in smas
#   for(i in length(ma_days):2){
#     x[,paste('SMA_percRange', ma_days[i], sep='_')] <- ((x[,paste('SMA_percRange', ma_days[i], sep='_')]) - (ma_days[i-1]/ma_days[i]) * (x[,paste('SMA_percRange', ma_days[i-1], sep='_')]))*ma_days[i]/(ma_days[i]-ma_days[i-1])
#     x[,paste('SMA_change', ma_days[i], sep='_')] <- ((x[,paste('SMA_change', ma_days[i], sep='_')]) - (ma_days[i-1]/ma_days[i]) * (x[,paste('SMA_change', ma_days[i-1], sep='_')]))*ma_days[i]/(ma_days[i]-ma_days[i-1])
#     x[,paste('SMA_changeRange', ma_days[i], sep='_')] <- ((x[,paste('SMA_changeRange', ma_days[i], sep='_')]) - (ma_days[i-1]/ma_days[i]) * (x[,paste('SMA_changeRange', ma_days[i-1], sep='_')]))*ma_days[i]/(ma_days[i]-ma_days[i-1])
#   }
# 
#   if(macd){
#     x[,paste('MACD_Sig_r', i, sep='_')] <- MACD(x[,'percRange'])[,2]
#     x[,paste('MACD_Sig_c', i, sep='_')] <- MACD(x[,'change'])[,2]
#   }
# 
#   for(i in adx_days){
#     x[,c(paste('ADX', i,'DIP', sep='_'), paste('ADX', i,'DIn', sep='_'), paste('ADX', i,'DX', sep='_'), paste('ADX', i,'ADXI', sep='_'))] <- ADX(x[,c('High','Low', 'Close')], n=i)
#   }
# 
#   for(i in sd_days){
#     x[,paste('sd_percRange', i, sep='_')] <- volatility_days(x$percRange, i)
#     x[,paste('sd_change', i, sep='_')] <- volatility_days(x$change, i)
#   }
#   for(i in sd_days_interaction){
#     x[,paste('sd_percRange', i[1],i[2], sep='_')] <- volatility_days(x$percRange, i[1])/volatility_days(x$percRange, i[2])
#     x[,paste('sd_change', i[1],i[2], sep='_')] <- volatility_days(x$change, i[1])/volatility_days(x$change, i[2])
#     x[,paste('sd_volume', i[1],i[2], sep='_')] <- volatility_days(x$t1, i[1])/volatility_days(x$t1, i[2])
#   }
#   for(i in days_close){
#     for(j in (i+1):nrow(x)){
#       x[j, paste('close_divided_',i,'close', sep="_")] <- x$Close[j]/x$Close[j-i]
#     }
#   }
# 
#   x$Open <- NULL
#   x$High <- NULL
#   x$Low <- NULL
#   x$Close <- NULL
#   x$t1 <- NULL
#   x$t2 <- NULL
# 
#   return(x)
# }
# 
# get_ls <- function(data, target, p_values = c(0.1, 0.05), pca_string=""){
#   full_list <- list()
#   mod_lm1 <- lm(target ~., data=data)
#   print(paste('Best', pca_string, 'LS Predictors:'))
#   decent <- names(summary(mod_lm1)$coef[summary(mod_lm1)$coef[,4] <= 0.1  , 4])[-1]
#   print(decent)
#   set.seed(1)
#   folds <- cvFolds(n = length(target), K = 10, type = "random")
#   preds_lm <- numeric(length(target))
#   for(i in 1:10){
#     mod_lm <- lm(target[folds$which!=i] ~. , data=as.data.frame(data[folds$which!=i, ]))
#     preds_lm[folds$which==i] <- predict(mod_lm, as.data.frame(data[folds$which==i, ]))
#   }
#   l1 <- list()
#   l1[[1]] <- preds_lm
#   l1[[2]] <- colnames(data)
#   full_list[[1]] <- l1
#   print('Correlation of Full LS:')
#   corr <- cor(preds_lm, target)
#   print(corr)
# 
#   for(j in 1:length(p_values)){
#     l1 <- list()
#     decent <- names(summary(mod_lm1)$coef[summary(mod_lm1)$coef[,4] <= p_values[j], 4])[-1]
#     for(i in 1:10){
#       mod_lm <- lm(target[folds$which!=i] ~., data=as.data.frame(data[folds$which!=i, decent]))
#       preds_lm[folds$which==i] <- predict(mod_lm, as.data.frame(data[folds$which==i, decent]))
#     }
#     l1[[1]] <- preds_lm
#     l1[[2]] <- decent
#     full_list[[j+1]] <- l1
#     print(paste('Correlation of Sparse (P=', as.character(p_values[j]),')' , pca_string,'LS:'))
#     corr <- cor(preds_lm, target)
#     print(corr)
#     print(paste('Annual Sharp Ratio of Sparse (P=',  as.character(p_values[j]),')' , pca_string,'LS:'))
#     l <- log(preds_lm) * log(target)
#     sharp <- sqrt(250) * mean(l)/sd(l)
#     print(sharp)
#   }
#   return(full_list)
# }
# 
# #@param data - dataframe of predictors
# #@param target - vector of target
# #@param covs - list of covariates to iterate through when fiting cross-validated bayesian neural networks
# #@param neurons - vector of neurons to try when fitting cross-validated bayesian neural networks
# #@param pca - string to use that prints if using principle components
# #prints cross validated least squares correlation and sharp ratio or bayiesien neural net
# 
# #return - list of cross validated predicted values and covariates used, and neurons used
# 
# get_brnn <- function(data, target, covs, neurons = c(1, 2), pca_string=""){
#   full_list <- list()
#   preds_brnn <- numeric(length(target))
#   set.seed(1)
#   folds <- cvFolds(n = length(target), K = 10, type = "random")
#   a <- 1
#   print(covs)
#   for(j in 1:length(covs)){
#     for(k in neurons){
#       for(i in 1:10){
#         invisible(capture.output(mod_brnn <- brnn(x = as.matrix(data[folds$which!=i, covs[[j]]]), y=(target[folds$which!=i]), neurons=k)))
#         preds_brnn[folds$which==i] <- predict(mod_brnn, as.matrix(data[folds$which==i, covs[[j]]]))
#       }
#       print(paste('Correlation of', k ,'Neuron', pca_string,'BNN'))
#       corr <- cor(preds_brnn, target)
#       print(corr)
#       print(paste('Annual Sharp Ratio of',  k,'Neuron', pca_string,'BNN:'))
#       l <- log(preds_brnn) * log(target)
#       sharp <- sqrt(250) * mean(l)/sd(l)
#       print(sharp)
#       l1 <- list()
#       l1[[1]] <- preds_brnn
#       l1[[2]] <- covs[[j]]
#       l1[[3]] <- k
#       full_list[[a]] <- l1
#       a <- a+1
#     }
#   }
#   return(full_list)
# }
# 
# #@param data - dataframe of predictors
# #@param target - vector of target
# #@param covs - list of covariates to iterate through when fiting cross-validated neural networks
# #@param decay - vector of decay values to try when fitting cross-validated neural networks
# #@param size - vector of size values to try when fitting cross-validated neural networks
# #@param maxit - max iterations of neural network
# #@param pca_string - string to use that prints if using principle components
# #prints cross validated least squares correlation and sharp ratio or neural net
# 
# #return - list of cross validated predicted values, covariates used, size, decay, and max iterations
# 
# get_nnet <- function(data, target, covs, pca_string="", decay=c(0.1,0.01,0.001), maxit=100, size=c(5,10,20,50)) {
#   full_list <- list()
#   preds_nnet <- numeric(length(target))
#   set.seed(1)
#   folds <- cvFolds(n = length(target), K = 10, type = "random")
#   a <- 1
#   for(j in 1:length(covs)){
#     for(k in size){
#       for(h in decay){
#        for(i in 1:10){
#          invisible(capture.output(mod_nnet <- nnet(x = as.matrix(data[folds$which!=i, covs[[j]]]), y=(target[folds$which!=i]), size=k, maxit = maxit, linout = TRUE, decay = h, MaxNWts=100000)))
#          preds_nnet[folds$which==i] <- predict(mod_nnet, as.matrix(data[folds$which==i, covs[[j]]]))
#        }
#        print(paste('Correlation of', k ,'Neurons', pca_string,'Neural Net','decay=', h,':'))
#        corr <- cor(preds_nnet, target)
#        print(corr)
#        print(paste('Annual Sharp Ratio of',  k,'Neurons', pca_string,'Neural Net', 'decay=', h,':'))
#        l <- log(preds_nnet) * log(target)
#        sharp <- sqrt(250) * mean(l)/sd(l)
#        print(sharp)
#        l1 <- list()
#        l1[[1]] <- preds_nnet
#        l1[[2]] <- covs[[j]]
#        l1[[3]] <- k
#        l1[[4]] <- h
#        l1[[5]] <- maxit
#        full_list[[a]] <- l1
#        a <- a+1
#       }
#     }
#   }
#   return(full_list)
# }
# 
# #@param data - dataframe of predictors
# #@param target - vector of target
# 
# #return - list of cross validated predicted values, covariates used, size, decay, and max iterations
# 
# get_lasso_ridge <- function(data, target){
#   full_list <- list()
#   preds_lasso <- numeric(length(target))
#   mod_lasso1 <- cv.glmnet(x = as.matrix(data), y = target)
#   set.seed(1)
#   folds <- cvFolds(n = length(target), K = 10, type = "random")
#   for(i in 1:10){
#     invisible(capture.output(mod_lasso <- glmnet(x = as.matrix(data[folds$which!=i, ]), y=(target[folds$which!=i]), lambda=mod_lasso1$lambda.min)))
#     preds_lasso[folds$which==i] <- predict(mod_lasso, as.matrix(data[folds$which==i,] ))
#   }
#   print(paste('Correlation of Lasso:'))
#   corr <- cor(preds_lasso, target)
#   print(corr)
#   print(paste('Annual Sharp Ratio of Lasso:'))
#   l <- log(preds_lasso) * log(target)
#   sharp <- sqrt(250) * mean(l)/sd(l)
#   print(sharp)
#   l1 <- list()
#   l1[[1]] <- preds_lasso
#   l1[[2]] <- mod_lasso1$lambda.min
#   l1[[3]] <- 'Lasso'
#   full_list[[1]] <- l1
# 
#   preds_lasso <- numeric(length(target))
#   mod_lasso1 <- cv.glmnet(x = as.matrix(data), y = target, alpha=0)
#   set.seed(1)
#   folds <- cvFolds(n = length(target), K = 10, type = "random")
#   for(i in 1:10){
#     invisible(capture.output(mod_lasso <- glmnet(x = as.matrix(data[folds$which!=i, ]), y=(target[folds$which!=i]), lambda=mod_lasso1$lambda.min, alpha=0)))
#     preds_lasso[folds$which==i] <- predict(mod_lasso, as.matrix(data[folds$which==i,] ))
#   }
#   print(paste('Correlation of Ridge:'))
#   corr <- cor(preds_lasso, target)
#   print(corr)
#   print(paste('Annual Sharp Ratio of Ridge:'))
#   l <- log(preds_lasso) * log(target)
#   sharp <- sqrt(250) * mean(l)/sd(l)
#   print(sharp)
#   l1 <- list()
#   l1[[1]] <- preds_lasso
#   l1[[2]] <- mod_lasso1$lambda.min
#   l1[[3]] <- 'Ridge'
#   full_list[[2]] <- l1
#   return(full_list)
# }
# 
# #@param a- vector of cross validated predictions from some model
# #@param b- vector of cross validated predictions from some model
# #@param target- vector of target values
# 
# #@return prediction vector with lowest 2/3 mae/correlation/sharp ratio
# 
# library(Metrics)
# compare_models <- function(a, b, target){
#   if(anyNA(b) | any(b<0)){
#     return(a)
#   }
#   s1 <- log(a)*log(target)
#   s2 <- log(b)*log(target)
#   m11 <- mean(s1)/sd(s1)
#   m12  <- mean(s2)/sd(s2)
#   m21 <- cor(a, target)
#   m22 <- cor(b, target)
#   m31 <- mae(target, a)
#   m32 <- mae(target, b)
#   s <- (m11 > m12) + (m21 > m22) + (m31 < m32)
#   if(s > 1){
#     return(a)
#   }
#   else{
#     return(b)
#   }
# }
# 
# #@param l1 - list of models containing prediction vector in first spot
# #@param target- target vector
# 
# #@return - list with best predictions and spot in spot of best predictions in l1
# 
# get_best_model <- function(l1, target){
#   best <- unlist(l1[[1]][1])
#   temp <- best
#   x <- 1
#   for(i in 1:length(l1)){
#     best <- compare_models(unlist(best), unlist(l1[[i]][1]), target)
#     if(all(best!=temp)){
#       x <- i
#       temp <- best
#     }
#   }
#   print('Corralation of best model:')
#   print(cor(best, target))
#   print('Sharp of best model:')
#   l <- log(best)*log(target)
#   print(mean(l)/sd(l)* sqrt(252))
#   l <- list()
#   l[[1]] <- best
#   l[[2]] <- x
#   return(l)
# }
# 
# #@description: remove bad covariates until cross-validated risk increases: returns vector of best covartiates
# 
# #@param data - dataframe of covariates
# #@param target - target vector
# #@param selecting_fetures - number of features to remove every loop
# 
# #@return  vector of best covartiates
# 
# get_xgb_predictors <- function(data, target, selecting_features=10) {
#   preds_xgb_best <- rexp(n=length(target))
#   set.seed(1)
#   folds <- cvFolds(n = length(target), K = 10, type = "random")
#   old <- colnames(data)
#   new <- colnames(data)
#   best <- new
#   model_feat <- list()
#   i <- 0
#   j <- 0
# 
#   while(((length(new)-selecting_features) > 1) & i <5){
# 
#     data_sparse <- sparse.model.matrix(~-1 + ., data=as.data.frame(data[,new]))
#     dtrain <-xgb.DMatrix(data=data_sparse,label=(target))
# 
#     model_gradient_tree <- xgb.cv(data = dtrain,
#                                   nfold=10,
#                                   nthread = 3,
#                                   nrounds = 50000,
#                                   max_depth = 7,
#                                   alpha = 0.2,
#                                   eta = 0.005,
#                                   subsample = .5,
#                                   colsample_bytree = 0.70,
#                                   booster = "gbtree",
#                                   #feval = evalerror,
#                                   eval_metric = 'mae',
#                                   #metrics = list("mae"),
#                                   maximize = FALSE,
#                                   objective = "reg:linear",
#                                   print.every.n = 1000,
#                                   verbose = TRUE,
#                                   min_child_weight=100,
#                                   prediction=TRUE,
#                                   early_stopping_rounds=20,
#                                   watchlist = list(train = dtrain))
# 
#     preds_new <- model_gradient_tree$pred
#     preds_xgb_best <- compare_models(preds_xgb_best, preds_new, target)
#     if(all(preds_xgb_best==preds_new)){
#       i == 0
#       best <- new
#     } else {
#       i <- i+1
#     }
# 
#     model_gradient <- xgb.train(data = dtrain,
#                                   nfold=10,
#                                   nthread = 3,
#                                   nrounds = model_gradient_tree$niter,
#                                   max_depth = 7,
#                                   alpha = 0.2,
#                                   eta = 0.005,
#                                   subsample = .5,
#                                   colsample_bytree = 0.70,
#                                   booster = "gbtree",
#                                   #feval = evalerror,
#                                   eval_metric = 'mae',
#                                   #metrics = list("mae"),
#                                   maximize = FALSE,
#                                   objective = "reg:linear",
#                                   print.every.n = 1000,
#                                   verbose = TRUE,
#                                   min_child_weight=100,
#                                   prediction=TRUE,
#                                   watchlist = list(train = dtrain))
# 
#     imp <- xgb.importance(colnames(dtrain), model = model_gradient)
#     if(length(new) > 100) {
#       if(length(imp$Feature)>100){
#           new <- imp$Feature[1:100]
#           selecting_features <- 10
#       } else{
#         new <- imp$Feature
#         selecting_features <- selecting_features <- 5
#         new <- imp$Feature[1:(length(imp$Feature)-selecting_features)]
#       }
#     } else{
#       selecting_features <- 5
#       new <- imp$Feature[1:(length(imp$Feature)-selecting_features)]
#     }
#   }
#   return(best)
# }
# 
# get_rf <- function(data, target, covs, mtry=c(5), tree=150, cores=7) {
#   full_models <- list()
#   n <- 1
#   #covs[[1]] <- colnames(data)
#   cl <-makeCluster(cores, type="SOCK")
#   registerDoSNOW(makeCluster(cores, type="SOCK"))
#   for(i in mtry)
#     for(j in 1:length(covs)) {
# 
#       preds_rf <- numeric(length(target))
#       set.seed(1)
#       folds <- cvFolds(n = length(target), K = 10, type = "random")
#       for(k in 1:10){
#         mod_rf <- foreach(ntree = rep(floor(tree/cores), cores), .combine = combine, .packages = "randomForest") %dopar%
#           randomForest(x = as.matrix(data[folds$which!=i, covs[[j]]]), y=(target[folds$which!=i]), ntree = tree, mtry=i)
#         #mod_rf <- randomForest(x = as.matrix(data[folds$which!=i, covs[[j]]]), y=(target[folds$which!=i]), mtry=i, ntree=tree)
#         preds_rf[folds$which==k] <- predict(mod_rf, as.matrix(data[folds$which==i, covs[[j]]] ))
#       }
#       print(paste('Correlation of RF',i))
#       corr <- cor(preds_rf, target)
#       print(corr)
#       print(paste('Annual Sharp Ratio of RF:',i))
#       l <- log(preds_rf) * log(target)
#       sharp <- sqrt(250) * mean(l)/sd(l)
#       print(sharp)
# 
#       l <- list()
#       l[[1]] <- preds_rf
#       l[[2]] <- covs[[j]]
#       l[[3]] <- i
#       l[[4]] <- cores
#       full_models[[n]] <- l
#       n <- n+1
#     }
#   return(full_models)
# }
# 
# get_svm <- function(data, target, covs, C=c(0.001,0.01,0.05,0.1,0.2,0.4,1), gamma = c(0.0001,0.001,0.01,0.1), epsilon=c(0.1,0.05,0.15)) {
#   full_models <- list()
#   n <- 1
#   for(i in C)
#     for(j in gamma)
#       for(k in epsilon)
#         for(h in 1:length(covs)) {
#         preds_svm <- numeric(length(target))
#         set.seed(1)
#         folds <- cvFolds(n = length(target), K = 10, type = "random")
#         for(i in 1:10){
#           invisible(capture.output(mod_svm <- svm(x = as.matrix(data[folds$which!=i, covs[[h]]]), y=(target[folds$which!=i]), C=i, gamma=j, epsilon=k, kernel='linear')))
#           preds_svm[folds$which==i] <- predict(mod_svm, as.matrix(data[folds$which==i, covs[[h]]] ))
#         }
# 
#           l <- list()
#           l[[1]] <- preds_svm
#           l[[2]] <- covs[[h]]
#           l[[3]] <- i
#           l[[4]] <- j
#           l[[5]] <- k
#           full_models[[n]] <- l
#           n <- n+1
#         }
#   return(full_models)
# }
# 
# evalerror <- function(preds, dtrain) {
#   labels <- getinfo(dtrain, "label")
#   preds[preds<0] <- 0.01
#   err <- -log(xor1(preds, as.numeric(labels)))
#   err <- mean(err)/sd(err)
#   return(list(metric = "error", value = err))
# }
# 
# get_xgb <- function(data, target, depth=c(4,5,6,7,8), alpha=c(0.05, 0.1, 0.2), subsamp=c(0.5,0.7),  min_weight=c(0,100)) {
#   preds_xgb_best <- rexp(n=length(target))
#   n <- 1
#   full_models <- list()
#   for(i in depth)
#     for(j in alpha)
#       for(k in subsamp)
#         for(h in min_weight){
# 
#           data_sparse <- sparse.model.matrix(~-1 + ., data=as.data.frame(data))
#           dtrain <-xgb.DMatrix(data=data_sparse,label=(target))
# 
#            model_gradient_tree <- xgb.cv(data = dtrain,
#                                       nfold=10,
#                                       nthread = 3,
#                                       nrounds = 50000,
#                                       max_depth = i,
#                                       alpha = j,
#                                       eta = 0.005,
#                                       subsample = k,
#                                       colsample_bytree = 0.70,
#                                       booster = "gbtree",
#                                       #feval = evalerror,
#                                       eval_metric = 'mae',
#                                       #metrics = list("mae"),
#                                       maximize = FALSE,
#                                       objective = "reg:linear",
#                                       print.every.n = 1000,
#                                       verbose = TRUE,
#                                      min_child_weight=h,
#                                      prediction=TRUE,
#                                      early_stopping_rounds=20,
#                                      watchlist = list(train = dtrain))
# 
#            l <- list()
#            l[[1]] <- model_gradient_tree$pred
#            l[[2]] <- colnames(data)
#            l[[3]] <- i
#            l[[4]] <- j
#            l[[5]] <- k
#            l[[6]] <- h
#            l[[7]] <- model_gradient_tree$niter
#            full_models[[n]] <- l
#            n <- n+1
#         }
#   return(full_models)
# }
# 
# 
# get_strong_models <- function(stocks, target_name, p_values = c(0.05,0.01), proportion = 0.8, abs_v=0.5, variation_to_keep = 1, pca=FALSE, intra=FALSE,
#                               depth=c(6,7), alpha=c(0.05, 0.1, 0.15), subsamp=c(0.5,0.7),  min_weight=c(10, 50, 100),
#                               C=c(0.01,0.02,0.05,0.1,0.4), gamma = c(0.0001,0.001,0.01,0.1), epsilon=c(0.1,0.05,0.15),
#                               decay=c(0.5, 0.3, 0.1), maxit=200, size=c(5,10,25), neurons = c(1, 2), tree=150, try=c(2,5,10)){
# 
#   if(!intra){
#     target <- stocks[30:nrow(stocks), target_name]
#     ind <- stocks[29:(nrow(stocks)-1), ]
#     ind$Time <- NULL
#   } else {
#     target <- stocks[30:nrow(stocks), 'Target']
#     ind <- stocks[30:(nrow(stocks)),]
#     ind$Target <- NULL
#     ind$Date <- NULL
#     ind$Time_hour <- NULL
#     ind$Time <- NULL
#   }
# 
#   ind2 <- ind[(floor(length(target)*proportion)+1):length(target),]
#   target2 <- target[(floor(length(target)*proportion)+1):length(target)]
# 
#   ind <- ind[1:floor(length(target)*proportion),]
#   target <- target[1:floor(length(target)*proportion)]
# 
#   for(i in 1:ncol(ind)){
#     ind2[,i] <- (ind2[,i]-mean(ind[,i]))/sd(ind[,i])
#     ind[,i] <- (ind[,i]-mean(ind[,i]))/sd(ind[,i])
#   }
#   if(!pca){
#     ll <- get_ls(data=ind, target = target, p_values = p_values)
#     ll_covs <- sapply(ll, "[[", 2)
#     ll_covs[[1]] <- NULL
#   }
#   prin <- princomp(ind)
#   prin_loadings2 <- prin$loadings
#   for(i in 1:ncol(as.matrix(prin_loadings2))){
#     prin_loadings2[abs(prin_loadings2[,i])<abs_v, i] <- 0
#   }
#   prin_ind2 <- as.matrix(ind2) %*% as.matrix(prin_loadings2)
#   prin_ind <-  as.matrix(ind) %*% as.matrix(prin_loadings2)
#   colnames(prin_ind2) <- colnames(prin$scores)
#   colnames(prin_ind) <- colnames(prin$scores)
#   prin_ind2 <- prin_ind2[, prin$sdev > variation_to_keep]
#   prin_ind <- prin_ind[,prin$sdev > variation_to_keep]
# 
#   vect_sds <- numeric(ncol(prin_ind))
#   for(i in 1:ncol(prin_ind)){
#     vect_sds[i] <- sd(prin_ind[,i])
#     prin_ind2[,i] <- (prin_ind2[,i])/sd(prin_ind[,i])
#     prin_ind[,i] <- (prin_ind[,i])/sd(prin_ind[,i])
#   }
# 
#   print(colnames(prin_ind))
#   prin_list <- list()
#   prin_list[[1]] <- prin_loadings2
#   prin_list[[2]] <- prin$sdev
#   prin_list[[3]] <- variation_to_keep
#   prin_list[[4]] <- vect_sds
#   prin_list[[5]] <- colnames(prin$scores)
#   prin_list[[6]] <- pca
# 
#    l2 <- get_ls(data=as.data.frame(prin_ind), target = target, p_values = p_values, 'PCA')
#    l2_covs <- sapply(l2, "[[", 2)
#    l2_covs[[1]] <- colnames(prin_ind)[1:20]
# 
# 
# 
#   prin_feat <- get_xgb_predictors(data = prin_ind, target = target)
#   l2_covs[[(length(l2_covs)+1)]] <- prin_feat
#   #rf <- get_rf(data = prin_ind, target = target, covs = l2_covs, mtry=try, tree=tree)
#   bn2 <- get_brnn(data = prin_ind, target = target, covs = l2_covs, neurons = neurons, pca_string = "PCA")
#   nn2 <- get_nnet(data = prin_ind, target = target, covs = l2_covs, pca_string = "", decay = decay, maxit = maxit, size = size)
# 
#   if(!pca){
#     feat <- get_xgb_predictors(data = ind, target = target)
#     ll_covs[[(length(ll_covs)+1)]] <- feat
#     bn1 <- get_brnn(data = ind, target = target, covs = ll_covs, neurons = neurons, pca_string = "")
#     nn1 <- get_nnet(data = ind, target = target, covs = ll_covs, pca_string = "", decay = decay, maxit = maxit, size = size)
#   }
# 
#   xgb2 <- get_xgb(data = prin_ind[,prin_feat], target = target, depth = depth, alpha = alpha, subsamp = subsamp, min_weight = min_weight)
# 
#   s2 <- get_svm(data=prin_ind, target = target, covs = l2_covs, C = C, gamma = gamma, epsilon = epsilon)
# 
#   #lr <- get_lasso_ridge_summaries(data=ind, target = target)
# 
#   mods <- list()
#   mods[[1]] <- xgb2
#   mods[[2]] <- bn2
#   mods[[3]] <- nn2
#   mods[[4]] <- s2
# 
#   if(!pca){
#     feat <- get_xgb_predictors(data = ind, target = target)
#     xgb1 <- get_xgb(data = ind[,feat], target = target, depth = depth, alpha = alpha, subsamp = subsamp, min_weight = min_weight)
#     ll_covs[[(length(ll_covs)+1)]] <- feat
# 
#     s1 <- get_svm(data=ind, target = target, covs = ll_covs, C = C, gamma = gamma, epsilon = epsilon)
#     bn1 <- get_brnn(data = ind, target = target, covs = ll_covs, neurons = neurons, pca_string = "")
#     nn1 <- get_nnet(data = ind, target = target, covs = ll_covs, pca_string = "", decay = decay, maxit = maxit, size = size)
# 
#     mods[[5]] <- xgb1
#     mods[[6]] <- bn1
#     mods[[7]] <- nn1
#     mods[[8]] <- s1
#   }
# 
#   fin_list <- list()
#   fin_list[[1]] <- mods
#   fin_list[[2]] <- prin_ind
#   fin_list[[3]] <- target
#   fin_list[[4]] <- prin_ind2
#   fin_list[[5]] <- target2
#   fin_list[[6]] <- ind
#   fin_list[[7]] <- ind2
#   fin_list[[8]] <- prin_list
#   fin_list[[9]] <- target_name
#   #fit_models(mods, prin_ind, target, prin_ind2, target2)
#   return(fin_list)
# }
# 
# get_strong_models_full_day <- function(stocks, target_name, p_values = c(0.05,0.01), proportion = 0.8,
#                               depth=c(6,7), alpha=c(0.05, 0.1, 0.15), subsamp=c(0.5,0.7),  min_weight=c(10, 50, 100),
#                               C=c(0.01,0.02,0.05,0.1,0.4), gamma = c(0.0001,0.001,0.01,0.1), epsilon=c(0.1,0.05,0.15),
#                               decay=c(0.5, 0.3, 0.1), maxit=200, size=c(5,10,25), neurons = c(1, 2), tree=150, try=c(2,5,10)){
# 
#   target <- stocks[30:nrow(stocks), target_name]
#   ind <- stocks[29:(nrow(stocks)-1), ]
#   ind$Time <- NULL
# 
#   ind2 <- ind[(floor(length(target)*proportion)+1):length(target),]
#   target2 <- target[(floor(length(target)*proportion)+1):length(target)]
# 
#   ind <- ind[1:floor(length(target)*proportion),]
#   target <- target[1:floor(length(target)*proportion)]
# 
#   for(i in 1:ncol(ind)){
#     ind2[,i] <- (ind2[,i]-mean(ind[,i]))/sd(ind[,i])
#     ind[,i] <- (ind[,i]-mean(ind[,i]))/sd(ind[,i])
#   }
#   ll <- get_ls(data=ind, target = target, p_values = p_values)
#   ll_covs <- sapply(ll, "[[", 2)
#   ll_covs[[1]] <- NULL
# 
#   feat <- get_xgb_predictors(data = ind, target = target)
#   ll_covs[[(length(ll_covs)+1)]] <- feat
#   bn1 <- get_brnn(data = ind, target = target, covs = ll_covs, neurons = neurons, pca_string = "")
#   nn1 <- get_nnet(data = ind, target = target, covs = ll_covs, pca_string = "", decay = decay, maxit = maxit, size = size)
#   xgb1 <- get_xgb(data = ind[,feat], target = target, depth = depth, alpha = alpha, subsamp = subsamp, min_weight = min_weight)
#   s1 <- get_svm(data=ind, target = target, covs = ll_covs, C = C, gamma = gamma, epsilon = epsilon)
#   rf1 <- get_rf(data = ind, target = target, covs = ll_covs, mtry = try, tree = tree, cores = 7)
# 
#   mods <- list()
#   mods[[1]] <- xgb1
#   mods[[2]] <- bn1
#   mods[[3]] <- nn1
#   mods[[4]] <- s1
#   mods[[5]] <- rf1
# 
#   fin_list <- list()
#   fin_list[[1]] <- mods
#   fin_list[[2]] <- target
#   fin_list[[3]] <- target2
#   fin_list[[4]] <- ind
#   fin_list[[5]] <- ind2
#   fin_list[[6]] <- target_name
#   return(fin_list)
# }
# 
# fit_models_full_day <- function(full_list, bias_proportion=0, weighting=0, corr=FALSE, mm =TRUE){
# 
#   mods <- full_list[[1]]
#   targ <- full_list[[2]]
#   targ2 <- full_list[[3]]
#   ind <- full_list[[4]]
#   ind2 <- full_list[[5]]
# 
#   corr <<- corr
#   mm <<- mm
#   grad1 <- mods[[1]]
#   bnn1 <- mods[[2]]
#   net1 <- mods[[3]]
#   svm1 <- mods[[4]]
#   rf1 <- mods[[5]]
# 
#   pos_grad1 <- get_best_model(grad1, targ)[[2]]
#   pos_brnn1 <- get_best_model(bnn1, targ)[[2]]
#   pos_net1 <- get_best_model(net1, targ)[[2]]
#   pos_svm1 <- get_best_model(svm1, targ)[[2]]
#   pos_rf1 <- get_best_model(rf1, targ)[[2]]
# 
#   pgrad1 <- grad1[[pos_grad1]]
#   pbnn1 <- bnn1[[pos_brnn1]]
#   pnet1 <- net1[[pos_net1]]
#   psvm1 <- svm1[[pos_svm1]]
#   prf1 <- rf1[[pos_rf1]]
# 
#   mods2 <- list()
#   mods2[[1]] <- pgrad1
#   mods2[[2]] <- pbnn1
#   mods2[[3]] <- pnet1
#   mods2[[4]] <- psvm1
#   mods2[[5]] <- prf1
# 
#   p_list <<- list()
# 
#   p_list[[1]] <<- pgrad1[[1]]
#   p_list[[2]] <<- pbnn1[[1]]
#   p_list[[3]] <<- pnet1[[1]]
#   p_list[[4]] <<- psvm1[[1]]
#   p_list[[5]] <<- prf1[[1]]
# 
# 
#   full_list2 <- list()
#   full_list2[[1]] <- mods2
#   full_list2[2:6] <- full_list[2:6]
# 
#   target <<- targ
#   ratios <<- optim(par = rep(1, (length(mods2)-1)), fn = opt, lower = 0, upper = 100, method = 'L-BFGS-B')$par
# 
#   print('Ratios of models')
#   print(ratios)
# 
#   fin_sharp <- log(fin_preds(p_list, ratios)) * log(targ)
#   fin_sharp  <- mean(fin_sharp)/sd(fin_sharp)*sqrt(252)
#   print('Sharp of in sample:')
#   print(fin_sharp)
#   print('Corr of in sample:')
#   print(cor(targ, fin_preds(p_list, ratios)))
# 
#   output_predictions <- numeric(length(targ2))
#   the_vals <- ceiling(seq(1, length(targ2), length.out = ceiling(length(targ2)*0.1)))
#   the_vals <- the_vals[2:length(the_vals)]
#   temp <- 1
# 
#   bias <- (1-mean(fin_preds(p_list, ratios)))
# 
#   for(i in 1:ncol(ind2)){
#     ind2[is.na(ind2[,i]),i] <- median(ind2[,i], na.rm = TRUE)
#   }
# 
#   pca_list <- list()
#   for(i in the_vals){
#     if(temp > 1){
#       data <- rbind(ind, ind2[1:temp,])
#       targ3 <- c(targ, targ2[1:temp])
#       to_be_fitted <- ind2[(temp+1):i,]
#     }
#     else{
#       data <- ind
#       targ3 <- targ
#       to_be_fitted <- ind2[1:i,]
#     }
# 
#     weights <- 1+weighting*seq(0,1, length.out = nrow(data))
#     data_sparse <- sparse.model.matrix(~-1 + ., data=as.data.frame(data[,pgrad1[[2]]]))
#     dtrain <-xgb.DMatrix(data=data_sparse,label=(targ3), weight=weights)
#     data_sparse2 <- sparse.model.matrix(~-1 + ., data=as.data.frame(to_be_fitted[,pgrad1[[2]]]))
#     dtest <-xgb.DMatrix(data=data_sparse2)
# 
# 
#     model_gradient <- xgb.train(data = dtrain,
#                                 nfold=10,
#                                 nthread = 3,
#                                 nrounds = pgrad1[[7]],
#                                 max_depth = pgrad1[[3]],
#                                 alpha = pgrad1[[4]],
#                                 eta = 0.005,
#                                 subsample = pgrad1[[5]],
#                                 colsample_bytree = 0.70,
#                                 booster = "gbtree",
#                                 eval_metric = 'mae',
#                                 #metrics = list("mae"),
#                                 maximize = FALSE,
#                                 objective = "reg:linear",
#                                 print.every.n = 1000,
#                                 verbose = TRUE,
#                                 min_child_weight=pgrad1[[6]],
#                                 prediction=TRUE,
#                                 watchlist = list(train = dtrain))
# 
#     preds_grad1 <- predict(model_gradient, dtest)
# 
#     invisible(capture.output(mod_brnn <- brnn(x = as.matrix(data[,pbnn1[[2]]]), y=(targ3), neurons=pbnn1[[3]])))
#     preds_brnn1 <- predict(mod_brnn, as.matrix(to_be_fitted[, pbnn1[[2]]]))
#     invisible(capture.output(mod_nnet <- nnet(x = as.matrix(data[, pnet1[[2]]]), y=(targ3), size=pnet1[[3]], maxit = pnet1[[5]], linout = TRUE, decay = pnet1[[4]], MaxNWts=100000)))
#     preds_nnet1 <- predict(mod_nnet, as.matrix(to_be_fitted[, pnet1[[2]]]))
#     invisible(capture.output(mod_svm <- svm(x = as.matrix(data[,psvm1[[2]]]), y=(targ3), C=psvm1[[3]], gamma=psvm1[[4]], epsilon=psvm1[[5]])))
#     preds_svm1 <- predict(mod_svm, as.matrix(to_be_fitted[, psvm1[[2]]]))
#     mod_rf <- foreach(ntree = rep(floor(500/prf1[[4]]), prf1[[4]]), .combine = combine, .packages = "randomForest") %dopar%
#       randomForest(x = as.matrix(data[, prf1[[2]]]), y=(targ3), ntree = 500, mtry=prf1[[3]])
#     preds_rf1 <- predict(mod_rf, as.matrix(to_be_fitted[, prf1[[2]]]))
# 
# 
#     pca_list[[1]] <- preds_grad1
#     pca_list[[2]] <- preds_brnn1
#     pca_list[[3]] <- preds_nnet1
#     pca_list[[4]] <- preds_svm1
#     pca_list[[5]] <- preds_rf1
# 
#     if(temp > 1){
#       output_predictions[(temp+1):i] <- fin_preds(pca_list, ratios)
#     } else{
#       output_predictions[1:i] <- fin_preds(pca_list, ratios)
#     }
#     temp <- i
#   }
#   output_predictions <- output_predictions + bias*bias_proportion
#   #full_list2[[10]] <- ratios
#   #full_list2[[11]] <- sd(output_predictions)
#   fin_sharp <- log(output_predictions) * log(targ2)
#   fin_sharp <- mean(fin_sharp)/sd(fin_sharp) * sqrt(252)
#   print('Sharp on out of sample:')
#   print(fin_sharp)
#   print('Correlation out of sample:')
#   print(cor(output_predictions, targ2))
#   saveRDS(full_list2, paste(full_list[[9]], '.rds', sep=""))
#   return(output_predictions)
# }
# 
# 
# 
# opt <- function(rat) {
#   x <- p_list[[1]]
#   for(i in 1:(length(rat))) {
#     x <- x + p_list[[i+1]]*rat[i]
#   }
#   x <- x/(1+sum(rat))
#   y <- -log(target) * log(x)
#   if(mm){
#     return(mae(target, x))
#   }
#   if(corr){
#     return(-cor(x, target))
#   } else{
#     return(mean(y)/sd(y))
#   }
# }
# 
# fin_preds <- function(p_list, ratios){
#   x <- p_list[[1]]
#   for(i in 1:(length(ratios))) {
#     x <- x + p_list[[i+1]]*ratios[i]
#   }
#   x <- x/(1+sum(ratios))
#   return(x)
# }
# 
# fit_models <- function(full_list, bias_proportion=0, weighting=0, corr=TRUE, mm =FALSE){
# 
#   mods <- full_list[[1]]
#   prin <- full_list[[2]]
#   targ <- full_list[[3]]
#   prin2 <- full_list[[4]]
#   targ2 <- full_list[[5]]
#   ind <- full_list[[6]]
#   ind2 <- full_list[[7]]
#   prin_list <- full_list[[8]]
#   pca <- prin_list[[6]]
# 
#   corr <<- corr
#   mm <<- mm
#   grad2 <- mods[[1]]
#   bnn2 <- mods[[2]]
#   net2 <- mods[[3]]
#   svm2 <- mods[[4]]
# 
#   pos_grad2 <- get_best_model(grad2, targ)[[2]]
#   pos_brnn2 <- get_best_model(bnn2, targ)[[2]]
#   pos_net2 <- get_best_model(net2, targ)[[2]]
#   pos_svm2 <- get_best_model(svm2, targ)[[2]]
# 
#   pgrad2 <- grad2[[pos_grad2]]
#   pbnn2 <- bnn2[[pos_brnn2]]
#   pnet2 <- net2[[pos_net2]]
#   psvm2 <- svm2[[pos_svm2]]
# 
#   mods2 <- list()
#   mods2[[1]] <- pgrad2
#   mods2[[2]] <- pbnn2
#   mods2[[3]] <- pnet2
#   mods2[[4]] <- psvm2
# 
#   p_list <<- list()
# 
#   p_list[[1]] <<- pgrad2[[1]]
#   p_list[[2]] <<- pbnn2[[1]]
#   p_list[[3]] <<- pnet2[[1]]
#   p_list[[4]] <<- psvm2[[1]]
# 
#   if(!pca){
#     grad1 <- mods[[5]]
#     bnn1 <- mods[[6]]
#     net1 <- mods[[7]]
#     svm1 <- mods[[8]]
# 
#     pos_grad1 <- get_best_model(grad1, targ)[[2]]
#     pos_brnn1 <- get_best_model(bnn1, targ)[[2]]
#     pos_net1 <- get_best_model(net1, targ)[[2]]
#     pos_svm1 <- get_best_model(svm1, targ)[[2]]
# 
#     pgrad1 <- grad1[[pos_grad1]]
#     pbnn1 <- bnn1[[pos_brnn1]]
#     pnet1 <- net1[[pos_net1]]
#     psvm1 <- svm1[[pos_svm1]]
# 
#     mods2[[5]] <- pgrad1
#     mods2[[6]] <- pbnn1
#     mods2[[7]] <- pnet1
#     mods2[[8]] <- psvm1
# 
#     p_list[[5]] <<- pgrad1[[1]]
#     p_list[[6]] <<- pbnn1[[1]]
#     p_list[[7]] <<- pnet1[[1]]
#     p_list[[8]] <<- psvm1[[1]]
#   }
# 
#   full_list2 <- list()
#   full_list2[[1]] <- mods2
#   full_list2[2:9] <- full_list[2:9]
# 
#   target <<- targ
#   if(!pca){
#     ratios <<- optim(par = c(1,1,1,1,1,1,1), fn = opt, lower = 0, upper = 100, method = 'L-BFGS-B')$par
#   } else{
#     ratios <<- optim(par = c(1.25,1.25,1.25), fn = opt, lower = 0, upper = 100, method = 'L-BFGS-B')$par
#     #ratios <<-  c(0.5,0.5,0.5) + ratios
#   }
#   print('Ratios of models')
#   print(ratios)
# 
#   fin_sharp <- log(fin_preds(p_list, ratios)) * log(targ)
#   fin_sharp  <- mean(fin_sharp)/sd(fin_sharp)*sqrt(252)
#   print('Sharp of in sample:')
#   print(fin_sharp)
# 
#   output_predictions <- numeric(length(targ2))
#   the_vals <- ceiling(seq(1, length(targ2), length.out = ceiling(length(targ2)*0.1)))
#   the_vals <- the_vals[2:length(the_vals)]
#   temp <- 1
# 
#   bias <- (1-mean(fin_preds(p_list, ratios)))
# 
#   for(i in 1:ncol(ind2)){
#     ind2[is.na(ind2[,i]),i] <- median(ind2[,i], na.rm = TRUE)
#   }
#   for(i in 1:ncol(prin2)){
#     prin2[is.na(prin2[,i]),i] <- 0
#   }
# 
#   for(i in the_vals){
#    if(temp > 1){
#      data <- rbind(ind, ind2[1:temp,])
#      prin_data <- rbind(prin, prin2[1:temp,])
#      targ3 <- c(targ, targ2[1:temp])
#      to_be_fitted <- ind2[(temp+1):i,]
#      p_to_be_fitted <- prin2[(temp+1):i,]
#    }
#    else{
#      data <- ind
#      prin_data <- prin
#      targ3 <- targ
#      to_be_fitted <- ind2[1:i,]
#      p_to_be_fitted <- prin2[1:i,]
#    }
# 
#     weights <- 1+weighting*seq(0,1, length.out = nrow(data))
#     pca_list <- list()
#     if(!pca){
#       data_sparse <- sparse.model.matrix(~-1 + ., data=as.data.frame(data[,pgrad1[[2]]]))
#       dtrain <-xgb.DMatrix(data=data_sparse,label=(targ3))  #, weight=weights)
#       data_sparse2 <- sparse.model.matrix(~-1 + ., data=as.data.frame(to_be_fitted[,pgrad1[[2]]]))
#       dtest <-xgb.DMatrix(data=data_sparse2)
# 
# 
#       model_gradient <- xgb.train(data = dtrain,
#                                   nfold=10,
#                                   nthread = 3,
#                                   nrounds = pgrad1[[7]],
#                                   max_depth = pgrad1[[3]],
#                                   alpha = pgrad1[[4]],
#                                   eta = 0.005,
#                                   subsample = pgrad1[[5]],
#                                   colsample_bytree = 0.70,
#                                   booster = "gbtree",
#                                   eval_metric = 'mae',
#                                   #metrics = list("mae"),
#                                   maximize = FALSE,
#                                   objective = "reg:linear",
#                                   print.every.n = 1000,
#                                   verbose = TRUE,
#                                   min_child_weight=pgrad1[[6]],
#                                   prediction=TRUE,
#                                   watchlist = list(train = dtrain))
# 
#       preds_grad1 <- predict(model_gradient, dtest)
# 
#       invisible(capture.output(mod_brnn <- brnn(x = as.matrix(data[,pbnn1[[2]]]), y=(targ3), neurons=pbnn1[[3]])))
#       preds_brnn1 <- predict(mod_brnn, as.matrix(to_be_fitted[, pbnn1[[2]]]))
#       invisible(capture.output(mod_nnet <- nnet(x = as.matrix(data[, pnet1[[2]]]), y=(targ3), size=pnet1[[3]], maxit = pnet1[[5]], linout = TRUE, decay = pnet1[[4]], MaxNWts=100000)))
#       preds_nnet1 <- predict(mod_nnet, as.matrix(to_be_fitted[, pnet1[[2]]]))
#       invisible(capture.output(mod_svm <- svm(x = as.matrix(data[,psvm1[[2]]]), y=(targ3), C=psvm1[[3]], gamma=psvm1[[4]], epsilon=psvm1[[5]])))
#       preds_svm1 <- predict(mod_svm, as.matrix(to_be_fitted[, psvm1[[2]]]))
#     }
#    weights <- 1+weighting*seq(0,1, length.out = nrow(data))
#    data_sparse <- sparse.model.matrix(~-1 + ., data=as.data.frame(prin_data[,pgrad2[[2]]]))
#    dtrain <-xgb.DMatrix(data=data_sparse,label=(targ3) , weight=weights)
#    data_sparse2 <- sparse.model.matrix(~-1 + ., data=as.data.frame(p_to_be_fitted[,pgrad2[[2]]]))
#    dtest <-xgb.DMatrix(data=data_sparse2)
# 
# 
#    model_gradient <- xgb.train(data = dtrain,
#                                  nfold=10,
#                                  nthread = 3,
#                                  nrounds = pgrad2[[7]],
#                                  max_depth = pgrad2[[3]],
#                                  alpha = pgrad2[[4]],
#                                  eta = 0.005,
#                                  subsample = pgrad2[[5]],
#                                  colsample_bytree = 0.70,
#                                  booster = "gbtree",
#                                  eval_metric = 'mae',
#                                  #metrics = list("mae"),
#                                  maximize = FALSE,
#                                  objective = "reg:linear",
#                                  print.every.n = 1000,
#                                  verbose = TRUE,
#                                  min_child_weight=pgrad2[[6]],
#                                  prediction=TRUE,
#                                  watchlist = list(train = dtrain))
# 
#    preds_grad2 <- predict(model_gradient, dtest)
# 
#    invisible(capture.output(mod_brnn <- brnn(x = as.matrix(prin_data[,pbnn2[[2]]]), y=(targ3), neurons=pbnn2[[3]])))
#    preds_brnn2 <- predict(mod_brnn, as.matrix(p_to_be_fitted[, pbnn2[[2]]]))
#    invisible(capture.output(mod_nnet <- nnet(x = as.matrix(prin_data[, pnet2[[2]]]), y=(targ3), size=pnet2[[3]], maxit = pnet2[[5]], linout = TRUE, decay = pnet2[[4]], MaxNWts=100000)))
#    preds_nnet2 <- predict(mod_nnet, as.matrix(p_to_be_fitted[, pnet2[[2]]]))
#    invisible(capture.output(mod_svm <- svm(x = as.matrix(prin_data[,psvm2[[2]]]), y=(targ3), C=psvm2[[3]], gamma=psvm2[[4]], epsilon=psvm2[[5]],  kernel='linear')))
#    preds_svm2 <- predict(mod_svm, as.matrix(p_to_be_fitted[, psvm2[[2]]]))
# 
# 
#   pca_list[[1]] <- preds_grad2
#   pca_list[[2]] <- preds_brnn2
#   pca_list[[3]] <- preds_nnet2
#   pca_list[[4]] <- preds_svm2
#   if(!pca){
#     pca_list[[5]] <- preds_grad1
#     pca_list[[6]] <- preds_brnn1
#     pca_list[[7]] <- preds_nnet1
#     pca_list[[8]] <- preds_svm1
#   }
#   if(temp > 1){
#     output_predictions[(temp+1):i] <- fin_preds(pca_list, ratios)
#   } else{
#     output_predictions[1:i] <- fin_preds(pca_list, ratios)
#   }
#   temp <- i
#   }
#   output_predictions <- output_predictions + bias*bias_proportion
#   full_list2[[10]] <- ratios
#   full_list2[[11]] <- sd(output_predictions)
#   fin_sharp <- log(output_predictions) * log(targ2)
#   fin_sharp <- mean(fin_sharp)/sd(fin_sharp) * sqrt(252)
#   print('Sharp on out of sample:')
#   print(fin_sharp)
#   print('Correlation out of sample:')
#   print(cor(output_predictions, targ2))
#   saveRDS(full_list2, paste(full_list[[9]], '.rds', sep=""))
#   return(output_predictions)
# }
# 
# get_new_script <- function(target_name, names){
#   x <-
# paste("
# library(Stocks)
# library(xgboost)
# library(e1071)
# library(Matrix)
# library(Metrics)
# library(brnn)
# library(cvTools)
# library(TTR)
# library(brnn)
# library(mailR)
# 
# full_lists <- readRDS(", target_name,".rds)
# stocks <-  <- get_data(", names ,")
# stocks$Time <- NULL
# target <- stocks[30:nrow(stocks), full_lists[[9]]]
# ind <- stocks[29:(nrow(stocks)), ]
# 
# for(i in 1:ncol(ind)){
#   ind[,i] <- (ind[,i]-mean(ind[,i]))/sd(ind[,i])
# }
# prin_list <- full_lists[[8]]
# prin_loadings <- prin_list[[1]]
# pca <- prin_list[[6]]
# prin_ind <- as.matrix(ind) %*% as.matrix(prin_loadings)
# 
# colnames(prin_ind) <- prin_list[[5]]
# vect_sds <- prin_list[[4]]
# for(i in 1:length(vect_sds)){
#   prin_ind[,i] <- prin_ind[,i]/vect_sds[i]
# }
# 
# tom <-  ind[(nrow(ind)-1):nrow(ind), ]
# prin_tom <- prin_ind[(nrow(prin_ind)-1):nrow(prin_ind), ]
# prin_ind <- prin_ind[1:(nrow(prin_ind)-1),]
# ind <- ind[1:(nrow(ind)-1), ]
# 
# mods <- full_lists[[1]]
# 
# pgrad2 <- mods[[1]]
# pbnn2 <- mods[[2]]
# pnet2 <- mods[[3]]
# psvm2 <- mods[[4]]
# 
# if(!pca){
#   pgrad1 <- mods[[5]]
#   pbnn1 <- mods[[6]]
#   pnet1 <- mods[[7]]
#   psvm1 <- mods[[8]]
# 
#   data_sparse <- sparse.model.matrix(~-1 + ., data=as.data.frame(ind[,pgrad1[[2]]]))
#   dtrain <-xgb.DMatrix(data=data_sparse,label=(target))  #, weight=weights)
#   data_sparse2 <- sparse.model.matrix(~-1 + ., data=as.data.frame(tom[,pgrad1[[2]]]))
#   dtest <-xgb.DMatrix(data=data_sparse2)
# 
# 
#   model_gradient <- xgb.train(data = dtrain,
#   nfold=10,
#   nthread = 3,
#   nrounds = pgrad1[[7]],
#   max_depth = pgrad1[[3]],
#   alpha = pgrad1[[4]],
#   eta = 0.005,
#   subsample = pgrad1[[5]],
#   colsample_bytree = 0.70,
#   booster = 'gbtree',
#   eval_metric = 'mae',
#   #metrics = list('mae'),
#   maximize = FALSE,
#   objective = 'reg:linear',
#   print.every.n = 1000,
#   verbose = TRUE,
#   min_child_weight=pgrad1[[6]],
#   prediction=TRUE,
#   watchlist = list(train = dtrain))
# 
#   preds_grad1 <- predict(model_gradient, dtest)
# 
#   invisible(capture.output(mod_brnn <- brnn(x = as.matrix(ind[,pbnn1[[2]]]), y=(target), neurons=pbnn1[[3]])))
#   preds_brnn1 <- predict(mod_brnn, as.matrix(tom[, pbnn1[[2]]]))
#   invisible(capture.output(mod_nnet <- nnet(x = as.matrix(ind[, pnet1[[2]]]), y=(target), size=pnet1[[3]], maxit = pnet1[[5]], linout = TRUE, decay = pnet1[[4]], MaxNWts=100000)))
#   preds_nnet1 <- predict(mod_nnet, as.matrix(tom[, pnet1[[2]]]))
#   invisible(capture.output(mod_svm <- svm(x = as.matrix(data[,psvm1[[2]]]), y=(target), C=psvm1[[3]], gamma=psvm1[[4]], epsilon=psvm1[[5]])))
#   preds_svm1 <- predict(mod_svm, as.matrix(to_be_fitted[, psvm1[[2]]]))
# 
# }
# 
# #weights <- 0.5+seq(0,1, length.out = nrow(data))
# data_sparse <- sparse.model.matrix(~-1 + ., data=as.data.frame(prin_ind[,pgrad2[[2]]]))
# dtrain <-xgb.DMatrix(data=data_sparse,label=(target))  #, weight=weights)
# data_sparse2 <- sparse.model.matrix(~-1 + ., data=as.data.frame(prin_tom[,pgrad2[[2]]]))
# dtest <-xgb.DMatrix(data=data_sparse2)
# 
# 
# model_gradient <- xgb.train(data = dtrain,
# nfold=10,
# nthread = 3,
# nrounds = pgrad2[[7]],
# max_depth = pgrad2[[3]],
# alpha = pgrad2[[4]],
# eta = 0.005,
# subsample = pgrad2[[5]],
# colsample_bytree = 0.70,
# booster = 'gbtree',
# eval_metric = 'mae',
# #metrics = list('mae'),
# maximize = FALSE,
# objective = 'reg:linear',
# print.every.n = 1000,
# verbose = TRUE,
# min_child_weight=pgrad2[[6]],
# prediction=TRUE,
# watchlist = list(train = dtrain))
# 
# preds_grad2 <- predict(model_gradient, dtest)
# 
# invisible(capture.output(mod_brnn <- brnn(x = as.matrix(prin_ind[,pbnn2[[2]]]), y=(target), neurons=pbnn2[[3]])))
# preds_brnn2 <- predict(mod_brnn, as.matrix(prin_tom[, pbnn2[[2]]]))
# invisible(capture.output(mod_nnet <- nnet(x = as.matrix(prin_ind[, pnet2[[2]]]), y=(target), size=pnet2[[3]], maxit = pnet2[[5]], linout = TRUE, decay = pnet2[[4]], MaxNWts=100000)))
# preds_nnet2 <- predict(mod_nnet, as.matrix(prin_tom[, pnet2[[2]]]))
# invisible(capture.output(mod_svm <- svm(x = as.matrix(prin_data[,psvm2[[2]]]), y=(target), C=psvm2[[3]], gamma=psvm2[[4]], epsilon=psvm2[[5]])))
# preds_svm2 <- predict(mod_svm, as.matrix(prin_tom[, psvm2[[2]]]))
# 
# p_list <- list()
# p_list[[1]] <- preds_grad2
# p_list[[2]] <- preds_brnn2
# p_list[[3]] <- preds_nnet2
# p_list[[4]] <- preds_svm2
# 
# if(!pca){
#   p_list[[5]] <- preds_grad1
#   p_list[[6]] <- preds_brnn1
#   p_list[[7]] <- preds_nnet1
#   p_list[[8]] <- preds_svm1
# }
# 
# preds <- fin_preds(p_list, full_lists[[10]])
# preds <- preds[2]
# bod <- get_bod(preds, target_name, c(full_lists[[11]], 2*full_lists[[11]]))
# 
# write(bod, paste(target_name, '_email.txt',sep=''))
# write(as.character(preds), paste(target_name, '_prediction.txt',sep=''))
# ", sep="")
# 
# write(x,  paste(target_name, '_script.R'))
# }
# 
# 
# 
# get_nnet2 <- function(stocks, target, samp=0.8, iter=50, prin=TRUE, var_to_keep=1, gm=FALSE) {
# 
#   stocks$Time <-NULL
#   target <- stocks[2:nrow(stocks),target]
#   for(i in 1:ncol(stocks)){
#     stocks[,i] <- (stocks[,i]- mean(stocks[,i], na.rm=TRUE))/(sd(stocks[,i], na.rm = TRUE)+0.01)
#   }
#   if(prin){
#     stocks1 <- princomp(stocks)
#     stocks <- stocks1$scores[,stocks1$sdev > var_to_keep]
#     for(i in 1:ncol(stocks)){
#       stocks[,i] <- (stocks[,i]- mean(stocks[,i], na.rm=TRUE))/(sd(stocks[,i], na.rm = TRUE)+0.01)
#     }
#   }
#   for(i in 1:ncol(stocks)){
#     stocks[is.na(stocks[,i]),i] <- 0
#   }
#   stocks <- stocks[1:(nrow(stocks)-1),]
# 
#   target_train <<- target[1:floor(nrow(stocks)*samp)]
#   stocks_train <<- stocks[1:floor(nrow(stocks)*samp),]
#   target_test <- target[(floor(nrow(stocks)*samp)+1):nrow(stocks)]
#   stocks_test <- stocks[(floor(nrow(stocks)*samp)+1):nrow(stocks),]
# 
# 
#   grid1 <- list()
# 
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
# 
#   nnet_tune <- function(decay, maxit, size, samp_size){
#     samp <- sample(1:nrow(stocks_train), (samp_size*nrow(stocks_train)))
#     train1 <- stocks_train[samp,]
#     train2 <- stocks_train[-samp,]
#     targ1 <- target_train[samp]
#     targ2 <- target_train[-samp]
# 
#     mod <- nnet(x = train1, y = targ1, decay=decay, maxit = maxit, size = size, MaxNWts=10000000000)
#     preds <- predict(mod, train2)
#     loss <- mae(preds, targ2)
#     return(loss)
#   }
# 
#   tune_nnet <- function(start_grid, f){
#     for(i in 1:length(start_grid)){
#       g <- start_grid[[i]]
#       l <- f(g[[1]], g[[2]], g[[3]], 0.8)
#       start_grid[[i]][[(length(g)+1)]] <- l
#     }
#     return(start_grid)
#   }
# 
#   m <- tune_nnet(grid1, nnet_tune)
#   df <- data.frame(matrix(unlist(m), nrow=18, byrow=T))
#   colnames(df) <- c('a', 'b', 'c', 'y')
# 
#   grid_to_search_size <- 2:35
#   grid_to_search_decay <- seq(0.001, 1, length.out = 30)
#   grid_to_search_maxit <- seq(50, 1000, length.out = 40)
# 
#   to_search <- expand.grid(grid_to_search_decay, grid_to_search_maxit, grid_to_search_size)
#   colnames(to_search) <- c('a', 'b', 'c')
# 
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
# 
#   mod_nnet1 <- nnet(stocks_train, target_train, size=df[iter+18,3], decay=df[iter+18,1], maxit=df[iter+18,2], , MaxNWts=1000000000)
#   preds <- predict(mod_nnet1, stocks_test)
#   print(df)
#   print(cor(preds, target_test))
# }
# 
# get_svm3 <- function(stocks, target, samp=0.8, iter=50, prin=TRUE, var_to_keep=1, gm=FALSE, k='linear') {
# 
#   stocks$Time <-NULL
#   target <- stocks[2:nrow(stocks),target]
#   for(i in 1:ncol(stocks)){
#     stocks[,i] <- (stocks[,i]- mean(stocks[,i], na.rm=TRUE))/(sd(stocks[,i], na.rm = TRUE)+0.01)
#   }
#   if(prin){
#     stocks1 <- princomp(stocks)
#     stocks <- stocks1$scores[,stocks1$sdev > var_to_keep]
#     for(i in 1:ncol(stocks)){
#       stocks[,i] <- (stocks[,i]- mean(stocks[,i], na.rm=TRUE))/(sd(stocks[,i], na.rm = TRUE)+0.01)
#     }
#   }
#   for(i in 1:ncol(stocks)){
#     stocks[is.na(stocks[,i]),i] <- 0
#   }
#   stocks <- stocks[1:(nrow(stocks)-1),]
# 
#   target_train <<- target[1:floor(nrow(stocks)*samp)]
#   stocks_train <<- stocks[1:floor(nrow(stocks)*samp),]
#   target_test <- target[(floor(nrow(stocks)*samp)+1):nrow(stocks)]
#   stocks_test <- stocks[(floor(nrow(stocks)*samp)+1):nrow(stocks),]
# 
# 
#   grid1 <- list()
# 
#   grid1[[1]] <- list(0.1, 0.01, 0.1)
#   grid1[[2]] <- list(0.1, 0.1, 0.5)
#   grid1[[3]] <- list(0.1, 0.001, 1)
#   grid1[[4]] <- list(0.025, 0.0001, 0.1)
#   grid1[[5]] <- list(0.5, 0.00001, 0.5)
#   grid1[[6]] <- list(0.05, 0.01, 1)
#   grid1[[7]] <- list(0.7, 0.1, 2)
#   grid1[[8]] <- list(0.7, 0.05, 5)
#   grid1[[9]] <- list(0.7, 0.0001, 0.1)
#   grid1[[10]] <- list(0.1, 0.001, 0.2)
#   grid1[[11]] <- list(0.01, 0.001, 2)
#   grid1[[12]] <- list(0.1, 0.0001, 1)
#   grid1[[13]] <- list(0.5, 0.001, 1)
#   grid1[[14]] <- list(0.2, 0.01, 0.5)
#   grid1[[15]] <- list(0.5, 0.01, 0.05)
#   grid1[[16]] <- list(0.2, 0.003, 0.1)
#   grid1[[17]] <- list(0.8, 0.002, 2)
#   grid1[[18]] <- list(0.9, 0.02, 3)
# 
#   svm_tune <- function(eps, nu, cost, samp_size){
#     samp <- sample(1:nrow(stocks_train), (samp_size*nrow(stocks_train)))
#     train1 <- stocks_train[samp,]
#     train2 <- stocks_train[-samp,]
#     targ1 <- target_train[samp]
#     targ2 <- target_train[-samp]
# 
#     mod <- svm(x = train1, y = targ1, kernel=k, epsilon=eps, nu=nu, cost = cost)
#     preds <- predict(mod, train2)
#     loss <- mae(preds, targ2)
#     print(loss)
#     return(loss)
#   }
# 
#   tune_svm <- function(start_grid, f){
#     for(i in 1:length(start_grid)){
#       g <- start_grid[[i]]
#       l <- f(g[[1]], g[[2]], g[[3]], 0.8)
#       start_grid[[i]][[(length(g)+1)]] <- l
#     }
#     return(start_grid)
#   }
# 
#   m <- tune_svm(grid1, svm_tune)
#   df <- data.frame(matrix(unlist(m), nrow=18, byrow=T))
#   colnames(df) <- c('a', 'b', 'c', 'y')
# 
#   grid_to_search_eps <- seq(0.0001, 0.25, length.out = 30)
#   grid_to_search_nu <- seq(0.0001, 0.1, length.out = 100)
#   grid_to_search_cost <- seq(0.01, 10, length.out = 40)
# 
#   to_search <- expand.grid(grid_to_search_eps, grid_to_search_nu, grid_to_search_cost)
#   colnames(to_search) <- c('a', 'b', 'c')
# 
#   for(i in 1:iter){
#     if(gm){
#       mod_svm1 <- gam(y~s(a)+s(b)+s(c), data=df)
#     } else{
#       mod_svm1 <- nnet(x=df[,1:3], y=df[,4], size=4)
#     }
#     preds_1 <- predict(mod_svm1, to_search)
#     r <- to_search[which.min(preds_1), ]
#     df[18+i,4] <- svm_tune(r[,1], r[,2], r[,3], 0.8)
#     print(r)
#     print(df[18+i, 4])
#     df[18+i,1] <- r[,1]
#     df[18+i,2] <- r[,2]
#     df[18+i,3] <- r[,3]
#   }
# 
#   mod_svm1 <- svm(x=stocks_train, y=target_train,kernel=k, cost=df[iter+18,3], eps=df[iter+18,1], nu=df[iter+18,2])
#   preds <- predict(mod_svm1, stocks_test)
#   print(df)
#   print(cor(preds, target_test))
# }
# 







# #Gets summary stats
# get_weak_models <- function(stocks, target_name){
#   target <- stocks[30:nrow(stocks), target_name]
#   ind <- stocks[29:(nrow(stocks)-1), ]
#   ind$Time <- NULL
#
#   for(i in 1:ncol(ind)){
#     ind[,i] <- (ind[,i]-mean(ind[,i]))/sd(ind[,i])
#   }
#
#   mod_lm <- lm(target ~., data=ind)
#   summary(mod_lm)
#   print('Best LS Predictors:')
#   decent <- names(summary(mod_lm)$coef[summary(mod_lm)$coef[,4] <= 0.05, 4])[-1]
#   print(decent)
#   decent2 <- names(summary(mod_lm)$coef[summary(mod_lm)$coef[,4] <= 0.01, 4])[-1]
#
#   set.seed(1)
#   folds <- cvFolds(n = length(target), K = 10, type = "random")
#   preds_lm <- numeric(length(target))
#   for(i in 1:10){
#     mod_lm <- lm(target[folds$which!=i] ~. , data=as.data.frame(ind[folds$which!=i, ]))
#     preds_lm[folds$which==i] <- predict(mod_lm, as.data.frame(ind[folds$which==i, ]))
#   }
#   print('Correlation of Full LS:')
#   corr <- cor(preds_lm, target)
#   print(corr)
#   #print('Sharp Ratio of Full LS:')
#   #l <- log(preds_lm) * log(target)
#   #sharp <- mean(l)/sd(l)
#   #print(sharp)
#
#   for(i in 1:10){
#     mod_lm <- lm(target[folds$which!=i] ~. , data=as.data.frame(ind[folds$which!=i, decent]))
#     preds_lm[folds$which==i] <- predict(mod_lm, as.data.frame(ind[folds$which==i, decent]))
#   }
#   print('Correlation of Sparse (P=0.05) LS:')
#   corr <- cor(preds_lm, target)
#   print(corr)
#   #print('Sharp Ratio of Sparse (P=0.05) LS:')
#   #l <- log(preds_lm) * log(target)
#   #sharp <- mean(l)/sd(l)
#   #print(sharp)
#
#   for(i in 1:10){
#     mod_lm <- lm(target[folds$which!=i] ~. , data=as.data.frame(ind[folds$which!=i, decent2]))
#     preds_lm[folds$which==i] <- predict(mod_lm, as.data.frame(ind[folds$which==i, decent2]))
#   }
#   print('Correlation of Sparse (P=0.01) LS:')
#   corr <- cor(preds_lm, target)
#   print(corr)
#   print('Annual Sharp Ratio of Sparse (P=0.01) LS:')
#   l <- log(preds_lm) * log(target)
#   sharp <- sqrt(250) *mean(l)/sd(l)
#   print(sharp)
#
#   prin <- princomp(ind)
#   mod_lm <- lm(target ~., data=as.data.frame(prin$scores))
#   summary(mod_lm)
#   print('Best Principle Components LS Predictors:')
#   decent3 <- names(summary(mod_lm)$coef[summary(mod_lm)$coef[,4] <= 0.05, 4])[-1]
#   print(decent3)
#   decent4 <- names(summary(mod_lm)$coef[summary(mod_lm)$coef[,4] <= 0.01, 4])[-1]
#
#   for(i in 1:10){
#     mod_lm <- lm(target[folds$which!=i] ~. , data=as.data.frame(prin$scores[folds$which!=i,] ))
#     preds_lm[folds$which==i] <- predict(mod_lm, as.data.frame(prin$scores[folds$which==i,] ))
#   }
#   print('Correlation of Full PCA LS:')
#   corr <- cor(preds_lm, target)
#   print(corr)
#   print('Annual Sharp Ratio of Full PCA LS:')
#   l <- log(preds_lm) * log(target)
#   sharp <- sqrt(250) *mean(l)/sd(l)
#   print(sharp)
#
#   for(i in 1:10){
#     mod_lm <- lm(target[folds$which!=i] ~. , data=as.data.frame(prin$scores[folds$which!=i, decent3]))
#     preds_lm[folds$which==i] <- predict(mod_lm, as.data.frame(prin$scores[folds$which==i, decent3]))
#   }
#   print('Correlation of Sparse PCA (P=0.05) LS:')
#   corr <- cor(preds_lm, target)
#   print(corr)
#   print('Annual Sharp Ratio of Sparse PCA (P=0.05) LS:')
#   l <- log(preds_lm) * log(target)
#   sharp <- sqrt(250) *mean(l)/sd(l)
#   print(sharp)
#
#   for(i in 1:10){
#     mod_lm <- lm(target[folds$which!=i] ~. , data=as.data.frame(prin$scores[folds$which!=i, decent4]))
#     preds_lm[folds$which==i] <- predict(mod_lm, as.data.frame(prin$scores[folds$which==i, decent4]))
#   }
#   print('Correlation of Sparse PCA (P=0.01) LS:')
#   corr <- cor(preds_lm, target)
#   print(corr)
#   print('Annual Sharp Ratio of Sparse PCA (P=0.01) LS:')
#   l <- log(preds_lm) * log(target)
#   sharp <- sqrt(250) *mean(l)/sd(l)
#   print(sharp)
#
#   preds_brnn <- numeric(length(target))
#   for(i in 1:10){
#     invisible(capture.output(mod_brnn <- brnn(x = as.matrix(ind[folds$which!=i, decent]), y=(target[folds$which!=i]), neurons=1, verbose=FALSE)))
#     preds_brnn[folds$which==i] <- predict(mod_brnn, as.matrix(ind[folds$which==i,decent]))
#   }
#   print('Correlation of 1 Neuron BNN (P=0.05):')
#   corr <- cor(preds_brnn, target)
#   print(corr)
#   print('Annual Sharp Ratio of 1 Neuron BNN (P=0.05):')
#   l <- log(preds_brnn) * log(target)
#   sharp <- sqrt(250) *mean(l)/sd(l)
#   print(sharp)
#
#   for(i in 1:10){
#     invisible(capture.output(mod_brnn <- brnn(x = as.matrix(ind[folds$which!=i, decent]), y=(target[folds$which!=i]), neurons=2, verbose=FALSE)))
#     preds_brnn[folds$which==i] <- predict(mod_brnn, as.matrix(ind[folds$which==i,decent]))
#   }
#   print('Correlation of 2 Neuron BNN (P=0.05):')
#   corr <- cor(preds_brnn, target)
#   print(corr)
#   print('Annual Sharp Ratio of 2 Neuron BNN (P=0.05):')
#   l <- log(preds_brnn) * log(target)
#   sharp <- sqrt(250) *mean(l)/sd(l)
#   print(sharp)
#
#   for(i in 1:10){
#     invisible(capture.output(mod_brnn <- brnn(x = as.matrix(ind[folds$which!=i, decent2]), y=(target[folds$which!=i]), neurons=1)))
#     preds_brnn[folds$which==i] <- predict(mod_brnn, as.matrix(ind[folds$which==i,decent2]))
#   }
#   print('Correlation of 1 Neuron BNN (P=0.01):')
#   corr <- cor(preds_brnn, target)
#   print(corr)
#   print('Annual Sharp Ratio of 1 Neuron BNN (P=0.01):')
#   l <- log(preds_brnn) * log(target)
#   sharp <- sqrt(250) *mean(l)/sd(l)
#   print(sharp)
#
#   for(i in 1:10){
#     invisible(capture.output(mod_brnn <- brnn(x = as.matrix(ind[folds$which!=i, decent2]), y=(target[folds$which!=i]), neurons=2)))
#     preds_brnn[folds$which==i] <- predict(mod_brnn, as.matrix(ind[folds$which==i,decent2]))
#   }
#   print('Correlation of 2 Neuron BNN (P=0.05):')
#   corr <- cor(preds_lm, target)
#   print(corr)
#   print('Annual Sharp Ratio of 2 Neuron BNN (P=0.05):')
#   l <- log(preds_brnn) * log(target)
#   sharp <- sqrt(250) *mean(l)/sd(l)
#   print(sharp)
#
#   for(i in 1:10){
#     invisible(capture.output(mod_brnn <- brnn(x = as.matrix(prin$scores[folds$which!=i, decent3]), y=(target[folds$which!=i]), neurons=1)))
#     preds_brnn[folds$which==i] <- predict(mod_brnn, as.matrix(prin$scores[folds$which==i,decent3]))
#   }
#   print('Correlation of PCA 1 Neuron BNN (P=0.05):')
#   corr <- cor(preds_brnn, target)
#   print(corr)
#   print('Annual Sharp Ratio of PCA 1 Neuron BNN (P=0.05):')
#   l <- log(preds_brnn) * log(target)
#   sharp <- sqrt(250) *mean(l)/sd(l)
#   print(sharp)
#
#   for(i in 1:10){
#     invisible(capture.output(mod_brnn <- brnn(x = as.matrix(prin$scores[folds$which!=i, decent3]), y=(target[folds$which!=i]), neurons=2)))
#     preds_brnn[folds$which==i] <- predict(mod_brnn, as.matrix(prin$scores[folds$which==i,decent3]))
#   }
#   print('Correlation of PCA 2 Neuron BNN (P=0.05):')
#   corr <- cor(preds_brnn, target)
#   print(corr)
#   print('Annual Sharp Ratio of 2 Neuron BNN (P=0.05):')
#   l <- log(preds_brnn) * log(target)
#   sharp <-sqrt(250) *mean(l)/sd(l)
#   print(sharp)
#
#   for(i in 1:10){
#     invisible(capture.output(mod_brnn <- brnn(x = as.matrix(prin$scores[folds$which!=i, decent4]), y=(target[folds$which!=i]), neurons=1)))
#     preds_brnn[folds$which==i] <- predict(mod_brnn, as.matrix(prin$scores[folds$which==i,decent4]))
#   }
#   print('Correlation of PCA 1 Neuron BNN (P=0.01):')
#   corr <- cor(preds_brnn, target)
#   print(corr)
#   print('Annual Sharp Ratio of PCA 1 Neuron BNN (P=0.01):')
#   l <- log(preds_brnn) * log(target)
#   sharp <- sqrt(250) * mean(l)/sd(l)
#   print(sharp)
#
#   for(i in 1:10){
#     invisible(capture.output(mod_brnn <- brnn(x = as.matrix(prin$scores[folds$which!=i, decent4]), y=(target[folds$which!=i]), neurons=2)))
#     preds_brnn[folds$which==i] <- predict(mod_brnn, as.matrix(prin$scores[folds$which==i,decent4]))
#   }
#   print('Correlation of PCA 2 Neuron BNN (P=0.05):')
#   corr <- cor(preds_brnn, target)
#   print(corr)
#   print('Annual Sharp Ratio of PCA 2 Neuron BNN (P=0.05):')
#   l <- log(preds_brnn) * log(target)
#   sharp <- sqrt(250) * mean(l)/sd(l)
#   print(sharp)
#
#
# }



# get_good_columns <- function(x, ma_days=c(3,5,10,20), adx=c(9,14), macd=c(12,26), vol_days=c(10,30,60), ){
#   x <- x[nrow(x):1,]
#
#   x$percRange<-x$High/x$Low
#   x$highOpen<-x$High/x$Open
#   x$highClose<-x$High/x$Close
#   x$lowOpen<-x$Low/x$Open
#   x$lowClose<-x$Low/x$Close
#   x$change<-x$Close/x$Open
#   x$changeRange<-x$change/x$percRange
#   x$t3 <- NULL
#
#   for(i in 2:ncol(x)){
#     x[!is.finite(x[,i]), i] <- mean(x[is.finite(x[,i]),i], na.rm=TRUE)
#     x[!is.numeric(x[,i]),i] <- mean(x[is.numeric(x[,i]),i], na.rm=TRUE)
#   }
#
#   for(i in ma_days){
#     x[,paste('SMA_percRange', i, sep='_')] <- SMA(x$percRange, i)
#     x[,paste('EMA_percRange', i, sep='_')] <- EMA(x$percRange, i)
#     x[,paste('SMA_change', i, sep='_')] <- SMA(x$change, i)
#     x[,paste('EMA_change', i, sep='_')] <- EMA(x$change, i)
#     x[,paste('SMA_changeRange', i, sep='_')] <- SMA(x$changeRange, i)
#     x[,paste('EMA_changeRange', i, sep='_')] <- EMA(x$changeRange, i)
#   }
#
#   x[,paste('MACD_Sig_r', i, sep='_')] <- MACD(x[,'percRange'])[,2]
#   x[,paste('MACD_Sig_c', i, sep='_')] <- MACD(x[,'change'])[,2]
#
#   for(i in adx){
#     x[,c(paste('ADX', i,'DIP', sep='_'), paste('ADX', i,'DIn', sep='_'), paste('ADX', i,'DX', sep='_'), paste('ADX', i,'ADXI', sep='_'))] <- ADX(x[,c('High','Low', 'Close')], n=i)
#   }
#
#   #elimates redundancy in smas
#   for(i in length(ma_days):2){
#     x[,paste('SMA_percRange', ma_days[i], sep='_')] <- ((x[,paste('SMA_percRange', ma_days[i], sep='_')]) - (ma_days[i-1]/ma_days[i]) * (x[,paste('SMA_percRange', ma_days[i-1], sep='_')]))*ma_days[i]/(ma_days[i]-ma_days[i-1])
#     x[,paste('SMA_change', ma_days[i], sep='_')] <- ((x[,paste('SMA_change', ma_days[i], sep='_')]) - (ma_days[i-1]/ma_days[i]) * (x[,paste('SMA_change', ma_days[i-1], sep='_')]))*ma_days[i]/(ma_days[i]-ma_days[i-1])
#     x[,paste('SMA_changeRange', ma_days[i], sep='_')] <- ((x[,paste('SMA_changeRange', ma_days[i], sep='_')]) - (ma_days[i-1]/ma_days[i]) * (x[,paste('SMA_changeRange', ma_days[i-1], sep='_')]))*ma_days[i]/(ma_days[i]-ma_days[i-1])
#   }
#
#   x$vol1 <- SMA(x$t1,10)/(SMA(x$t1, 20)+0.00001)
#   x$vol2 <- SMA(x$t1,5)/(SMA(x$t1, 10)+0.00001)
#   x$vol3 <- x$t1/(SMA(x$t1, 5)+0.00001)
#   x$vol4 <- SMA(x$t1,3)/(SMA(x$t1, 5)+0.00001)
#
#   for(i in vol_days){
#     for(j in length(ma_days)){
#       x[,paste('vol_percRange', i, j, sep='_')] <- volatility_days(x[,paste('SMA_percRange', ma_days[j], sep='_')], i)
#       x[,paste('vol_change', i, j, sep='_')] <- volatility_days(x[,paste('SMA_change', ma_days[j], sep='_')], i)
#     }
#   }
#
#   x$Open <- NULL
#   x$High <- NULL
#   x$Low <- NULL
#   x$Close <- NULL
#   x$t1 <- NULL
#   x$t2 <- NULL
#
#   return(x)
# }
#
# get_data <- function(name, ma_days=c(3,5,10,20), adx=c(9,14), macd=c(12,26), vol_days=c(10,30,60),  time="390", str="5", int=FALSE, target_name, hour, time2){
#
#   command <- paste("python vxx_daily", str, ".py --lista", sep="")
#
#   for(i in name){
#     command <- paste(command, i)
#   }
#   command <- paste(command, "--time_in_minutes", time)
#   system(command = command)
#
#   stocks <- list()
#   j <- 1
#   for(i in name){
#     s <- read.csv(paste(i, '.csv', sep=""), header = FALSE)
#     stocks[[j]] <- s
#     j <- j+1
#   }
#   for(i in 1:length(stocks)){
#     colnames(stocks[[i]]) <- c('Time', 'High', 'Low', 'Open', 'Close', 't1', 't2', 't3')
#   }
#
#   for(i in 1:length(stocks)){
#     stocks[[i]] <- get_good_columns(stocks[[i]], ma_days = ma_days, adx=adx, macd = macd, vol_days = vol_days)
#     colnames(stocks[[i]])[2:ncol(stocks[[i]])] <- paste(name[i], colnames(stocks[[i]])[2:ncol(stocks[[i]])] , sep="_")
#   }
#
#   stocks <-Reduce(merge, stocks)
#   if(int){
#     stocks$Date <- substr(as.character(stocks$Time),0,10)
#     stocks$Time_hour <- substr(as.character(stocks$Time),12,20)
#
#     bars <- as.integer(time2)/as.integer(time)
#     stocks$Target <- NA
#     for(i in (bars+1):nrow(stocks)){
#       stocks$Target[(i-bars)] <- prod(stocks[(i-bars+1):(i), target_name])
#     }
#
#     stocks <- stocks[stocks$Time_hour==hour, ]
#   }
#   return(stocks)
# }
#
#

#calculates columns for stock data frame with IQFEED data
#@param x - dataframe from get_data or get_intra_data
#@param ma_days- vector of number of days to use in moving averages (must be in increasing order)

#@return - dataframe with clean columns

# get_good_columns <- function(x, ma_days=c(3,5,10,20), adx=c(9,14), macd=c(12,26), vol_days=c(10,30,60), int=FALSE, target_name, hour, b1, b2){
#   x <- x[nrow(x):1,]
#
#   x$percRange<-x$High/x$Low
#   x$highOpen<-x$High/x$Open
#   x$highClose<-x$High/x$Close
#   x$lowOpen<-x$Low/x$Open
#   x$lowClose<-x$Low/x$Close
#   x$change<-x$Close/x$Open
#   x$changeRange<-x$change/x$percRange
#   x$t3 <- NULL
#
#   for(i in 2:ncol(x)){
#     x[!is.finite(x[,i]), i] <- mean(x[is.finite(x[,i]),i], na.rm=TRUE)
#     x[!is.numeric(x[,i]),i] <- mean(x[is.numeric(x[,i]),i], na.rm=TRUE)
#   }
#
#   for(i in ma_days){
#     x[,paste('SMA_percRange', i, sep='_')] <- SMA(x$percRange, i)
#     x[,paste('EMA_percRange', i, sep='_')] <- EMA(x$percRange, i)
#     x[,paste('SMA_change', i, sep='_')] <- SMA(x$change, i)
#     x[,paste('EMA_change', i, sep='_')] <- EMA(x$change, i)
#     x[,paste('SMA_changeRange', i, sep='_')] <- SMA(x$changeRange, i)
#     x[,paste('EMA_changeRange', i, sep='_')] <- EMA(x$changeRange, i)
#   }
#
#   x[,paste('MACD_Sig_r', i, sep='_')] <- MACD(x[,'percRange'])[,2]
#   x[,paste('MACD_Sig_c', i, sep='_')] <- MACD(x[,'change'])[,2]
#
#   for(i in adx){
#     x[,c(paste('ADX', i,'DIP', sep='_'), paste('ADX', i,'DIn', sep='_'), paste('ADX', i,'DX', sep='_'), paste('ADX', i,'ADXI', sep='_'))] <- ADX(x[,c('High','Low', 'Close')], n=i)
#   }
#
#   #elimates redundancy in smas
#   for(i in length(ma_days):2){
#     x[,paste('SMA_percRange', ma_days[i], sep='_')] <- ((x[,paste('SMA_percRange', ma_days[i], sep='_')]) - (ma_days[i-1]/ma_days[i]) * (x[,paste('SMA_percRange', ma_days[i-1], sep='_')]))*ma_days[i]/(ma_days[i]-ma_days[i-1])
#     x[,paste('SMA_change', ma_days[i], sep='_')] <- ((x[,paste('SMA_change', ma_days[i], sep='_')]) - (ma_days[i-1]/ma_days[i]) * (x[,paste('SMA_change', ma_days[i-1], sep='_')]))*ma_days[i]/(ma_days[i]-ma_days[i-1])
#     x[,paste('SMA_changeRange', ma_days[i], sep='_')] <- ((x[,paste('SMA_changeRange', ma_days[i], sep='_')]) - (ma_days[i-1]/ma_days[i]) * (x[,paste('SMA_changeRange', ma_days[i-1], sep='_')]))*ma_days[i]/(ma_days[i]-ma_days[i-1])
#   }
#
#   x$vol1 <- SMA(x$t1,10)/(SMA(x$t1, 20)+0.00001)
#   x$vol2 <- SMA(x$t1,5)/(SMA(x$t1, 10)+0.00001)
#   x$vol3 <- x$t1/(SMA(x$t1, 5)+0.00001)
#   x$vol4 <- SMA(x$t1,3)/(SMA(x$t1, 5)+0.00001)
#
#   for(i in vol_days){
#     x[,paste('vol_percRange', i, sep='_')] <- volatility_days(x$percRange, i)
#     x[,paste('vol_change', i, sep='_')] <- volatility_days(x$change, i)
#   }
#
#   if(int){
#     bars <- as.integer(b2)/as.integer(b1)
#     x$Target <- NA
#     for(i in (bars+1):nrow(x)){
#       x$Target[i] <- cumprod(stocks[(i-bars):(i), target_name])[length(bars)]
#     }
#   }
#
#   x$Open <- NULL
#   x$High <- NULL
#   x$Low <- NULL
#   x$Close <- NULL
#   x$t1 <- NULL
#   x$t2 <- NULL
#
#   return(x)
# }


#Gets IQFEED data using python script
#@param name - vector names of stocks to get
#@param time - time in minutes of block (390 minutes in one trading day default)
#@param ma_days- vector of number of days to use in moving averages (must be in increasing order)

#@return - dataframe of stocks

# get_data <- function(name, ma_days=c(3,5,10,20), adx=c(9,14), time="390", str="5"){
#
#   command <- paste("python vxx_daily", str, ".py --lista", sep="")
#
#   for(i in name){
#     command <- paste(command, i)
#   }
#   command <- paste(command, "--time_in_minutes", time)
#   system(command = command)
#
#   stocks <- list()
#   j <- 1
#   for(i in name){
#     s <- read.csv(paste(i, '.csv', sep=""), header = FALSE)
#     stocks[[j]] <- s
#     j <- j+1
#   }
#   for(i in 1:length(stocks)){
#     colnames(stocks[[i]]) <- c('Time', 'High', 'Low', 'Open', 'Close', 't1', 't2', 't3')
#   }
#
#   for(i in 1:length(stocks)){
#     stocks[[i]] <- get_good_columns(stocks[[i]], ma_days = ma_days, adx=adx)
#     colnames(stocks[[i]])[2:ncol(stocks[[i]])] <- paste(name[i], colnames(stocks[[i]])[2:ncol(stocks[[i]])] , sep="_")
#   }
#
#   stocks <-Reduce(merge, stocks)
#   return(stocks)
# }

#@param name - vector names of stocks to get
#@param target_name - varianble to precdict (i.e. "STOCKNAME_change" )
#@param b1 - time in minutes of blocks in predictor dataframe
#@param b2 - time in minutes of blocks in target dataframe

#@return - dataframe of stocks with moving averages (from get_good_columns) from first dataframe with target column of block in second dataframe

# get_intra_data <- function(names, target_name, ma_days=c(3,5,10,20), adx=c(9,14), b1="10", b2="180", hour='13:00:00', hour2='16:00:00'){
#   stocks_30 <- get_data(name=names,  ma_days=ma_days, adx=adx, time=b1, str="5")
#   stocks_3 <- get_data(name=names,  ma_days=ma_days, adx=adx,time=b2, str="4")
#   stocks_3 <- stocks_3[,c('Time', target_name)]
#   colnames(stocks_3)[2] <- paste(target_name, "3hours", sep='_')
#   stocks <- list()
#   stocks[[1]] <- stocks_30
#   stocks[[2]] <- stocks_3
#
#   stocks <-Reduce(merge, stocks)
#
#   stocks$Date <- substr(as.character(stocks$Time),0,10)
#   stocks$Time_hour <- substr(as.character(stocks$Time),12,20)
#   stocks <- stocks[,(!grepl('//d', colnames(stocks)))]
#
#   mat <- stocks[stocks$Time_hour==hour2,c('Date', paste(target_name, "3hours", sep='_'))]
#   print(head(stocks, 20))
#   stocks2 <- stocks[stocks$Time_hour==hour,]
#   print(head(stocks2))
#   colnames(mat)[2] <- 'Target'
#   stocks <- list()
#   stocks[[1]] <- mat
#   stocks[[2]] <- stocks2
#   stocks <-Reduce(merge, stocks)
#
#   return(stocks)
# }

#@param data - dataframe of predictors
#@param target - vector of target
#@param p_values - vector of p_values to get covariates at
#@param pca - string to use that prints if using principle components
#prints cross validated least squares correlation and sharp ratio

#return - list of cross validated predicted values and covariates selected using p_values vector

#
# get_strong_prin_models <- function(stocks, target_name, p_values = c(0.05,0.01), proportion = 0.8, variation_to_keep = 1.5,
#                                    depth=c(6,7), alpha=c(0.05, 0.1, 0.15), subsamp=c(0.5,0.7),  min_weight=c(10, 50,100),
#                                    C=c(0.01,0.02,0.05,0.1,0.4), gamma = c(0.0001,0.001,0.01,0.1), epsilon=c(0.1,0.05,0.15),
#                                    decay=c(0.5, 0.3, 0.1), maxit=200, size=c(5,10,25), neurons = c(1, 2)){
#
#   target <- stocks[30:nrow(stocks), target_name]
#   ind <- stocks[29:(nrow(stocks)-1), ]
#   ind$Time <- NULL
#
#   ind2 <- ind[(floor(length(target)*proportion)+1):length(target),]
#   target2 <- target[(floor(length(target)*proportion)+1):length(target)]
#
#   ind <- ind[1:floor(length(target)*proportion),]
#   target <- target[1:floor(length(target)*proportion)]
#
#   for(i in 1:ncol(ind)){
#     ind2[,i] <- (ind2[,i]-mean(ind[,i]))/sd(ind[,i])
#     ind[,i] <- (ind[,i]-mean(ind[,i]))/sd(ind[,i])
#   }
#   #ll <- get_ls(data=ind, target = target, p_values = p_values)
#   #ll_covs <- sapply(ll, "[[", 2)
#   #ll_covs[[1]] <- NULL
#   prin <- princomp(ind)
#   prin_ind2 <- as.matrix(ind2) %*% as.matrix(prin$loadings)
#   colnames(prin_ind2) <- colnames(prin$scores)
#   prin_ind2 <- prin_ind2[, prin$sdev > variation_to_keep]
#   prin_ind <- prin$scores[,prin$sdev > variation_to_keep]
#
#   for(i in 1:ncol(prin_ind)){
#     prin_ind2[,i] <- (prin_ind2[,i])/sd(prin_ind[,i])
#     prin_ind[,i] <- (prin_ind[,i])/sd(prin_ind[,i])
#   }
#
#   prin_list <- list()
#   prin_list[[1]] <- prin$loading
#   prin_list[[2]] <- prin$sdev
#   prin_list[[3]] <- variation_to_keep
#
#   l2_covs <- list()
#   l2_covs[[1]] <- colnames(prin_ind)
#   xgb2 <- get_xgb(data = prin_ind, target = target, depth = depth, alpha = alpha, subsamp = subsamp, min_weight = min_weight)
#   #l2_covs[[(length(l2_covs)+1)]] <- prin_feat
#
#   s2 <- get_svm(data=prin_ind, target = target, covs = l2_covs, C = C, gamma = gamma, epsilon = epsilon)
#
#   bn2 <- get_brnn(data = prin_ind, target = target, covs = l2_covs, neurons = neurons, p_values = p_values, pca_string = "PCA")
#   nn2 <- get_nnet(data = prin_ind, target = target, covs = l2_covs, p_values = p_values, pca_string = "", decay = decay, maxit = maxit, size = size)
#
#   mods <- list()
#   mods[[1]] <- xgb2
#   mods[[2]] <- bn2
#   mods[[3]] <- nn2
#   mods[[4]] <- s2
#   fin_list <- list()
#   fin_list[[1]] <- mods
#   fin_list[[2]] <- prin_ind
#   fin_list[[3]] <- target
#   fin_list[[4]] <- prin_ind2
#   fin_list[[5]] <- target2
#   fin_list[[6]] <- ind
#   fin_list[[7]] <- ind2
#   fin_list[[8]] <- prin_list
#   #fit_models(mods, prin_ind, target, prin_ind2, target2)
#   return(fin_list)
# }

# opt_old <- function(a){
#   x <- -log(target) * log((p1+a[1]*p2+a[2]*p3+a[2]*p3+a[3]*p4+a[4]*p5+a[5]*p6+a[6]*p7+a[7]*p8)/(1+sum(a)))
#   return(mean(x)/sd(x)* sqrt(252))
# }
#
# fin_preds_old <- function(p1, p2, p3, p4, p5, p6, p7, p8, ratios){
#   x <- (p1+ratios[1]*p2 + ratios[2]*p3+ ratios[3]*p4+ ratios[4]*p5+ ratios[5]*p6+ ratios[6]*p7+ ratios[7]*p8)/(1+sum(ratios))
#   return(x)
# }
