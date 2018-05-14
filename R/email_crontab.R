#@param fin - final prediction
#@param name - name of target
#@param nums - vector (usually of standard deviations)  to tell how large movement is

#return - body to be used in crontab email

get_bod <- function(fin, name, nums){
  if(fin > (1+nums[1])){
    bod = paste("This is a strong upward", name, "movement predicted at:", fin)
  }
  if((fin > (1+nums[2])) & (fin < (1+nums[1]))){
    bod = paste("This is a moderately strong upward", name, "movement  predicted at:", fin)
  }
  if((fin > 1) & (fin < (1+nums[2]))){
    bod = paste("This is a weak upward", name, "movement  predicted at:", fin)
  }
  if((fin > (1-nums[2])) & (fin < 1)){
    bod = paste("This is a weak downward",name ,"movement  predicted at:", fin)
  }
  if((fin > (1-nums[1])) & (fin < (1-nums[2]))){
    bod = paste("This is a moderate downward", name, "movement  predicted at:", fin)
  }
  if(fin < (1-nums[1])){
    bod = paste("This is a strong downward", name, "movement  predicted at:", fin)
  }
  return(bod)
}

get_new_script <- function(target_name, names){
  x <-
    paste("
          library(Stocks)
          library(xgboost)
          library(e1071)
          library(Matrix)
          library(Metrics)
          library(brnn)
          library(cvTools)
          library(TTR)
          library(brnn)
          library(mailR)
          
          full_lists <- readRDS(", target_name,".rds)
          stocks <-  <- get_data(", names ,")
          stocks$Time <- NULL
          target <- stocks[30:nrow(stocks), full_lists[[9]]]
          ind <- stocks[29:(nrow(stocks)), ]
          
          for(i in 1:ncol(ind)){
          ind[,i] <- (ind[,i]-mean(ind[,i]))/sd(ind[,i])
          }
          prin_list <- full_lists[[8]]
          prin_loadings <- prin_list[[1]]
          pca <- prin_list[[6]]
          prin_ind <- as.matrix(ind) %*% as.matrix(prin_loadings)
          
          colnames(prin_ind) <- prin_list[[5]]
          vect_sds <- prin_list[[4]]
          for(i in 1:length(vect_sds)){
          prin_ind[,i] <- prin_ind[,i]/vect_sds[i]
          }
          
          tom <-  ind[(nrow(ind)-1):nrow(ind), ]
          prin_tom <- prin_ind[(nrow(prin_ind)-1):nrow(prin_ind), ]
          prin_ind <- prin_ind[1:(nrow(prin_ind)-1),]
          ind <- ind[1:(nrow(ind)-1), ]
          
          mods <- full_lists[[1]]
          
          pgrad2 <- mods[[1]]
          pbnn2 <- mods[[2]]
          pnet2 <- mods[[3]]
          psvm2 <- mods[[4]]
          
          if(!pca){
          pgrad1 <- mods[[5]]
          pbnn1 <- mods[[6]]
          pnet1 <- mods[[7]]
          psvm1 <- mods[[8]]
          
          data_sparse <- sparse.model.matrix(~-1 + ., data=as.data.frame(ind[,pgrad1[[2]]]))
          dtrain <-xgb.DMatrix(data=data_sparse,label=(target))  #, weight=weights)
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
          
          invisible(capture.output(mod_brnn <- brnn(x = as.matrix(ind[,pbnn1[[2]]]), y=(target), neurons=pbnn1[[3]])))
          preds_brnn1 <- predict(mod_brnn, as.matrix(tom[, pbnn1[[2]]]))
          invisible(capture.output(mod_nnet <- nnet(x = as.matrix(ind[, pnet1[[2]]]), y=(target), size=pnet1[[3]], maxit = pnet1[[5]], linout = TRUE, decay = pnet1[[4]], MaxNWts=100000)))
          preds_nnet1 <- predict(mod_nnet, as.matrix(tom[, pnet1[[2]]]))
          invisible(capture.output(mod_svm <- svm(x = as.matrix(data[,psvm1[[2]]]), y=(target), C=psvm1[[3]], gamma=psvm1[[4]], epsilon=psvm1[[5]])))
          preds_svm1 <- predict(mod_svm, as.matrix(to_be_fitted[, psvm1[[2]]]))
          
          }
          
          #weights <- 0.5+seq(0,1, length.out = nrow(data))
          data_sparse <- sparse.model.matrix(~-1 + ., data=as.data.frame(prin_ind[,pgrad2[[2]]]))
          dtrain <-xgb.DMatrix(data=data_sparse,label=(target))  #, weight=weights)
          data_sparse2 <- sparse.model.matrix(~-1 + ., data=as.data.frame(prin_tom[,pgrad2[[2]]]))
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
          booster = 'gbtree',
          eval_metric = 'mae',
          #metrics = list('mae'),
          maximize = FALSE,
          objective = 'reg:linear',
          print.every.n = 1000,
          verbose = TRUE,
          min_child_weight=pgrad2[[6]],
          prediction=TRUE,
          watchlist = list(train = dtrain))
          
          preds_grad2 <- predict(model_gradient, dtest)
          
          invisible(capture.output(mod_brnn <- brnn(x = as.matrix(prin_ind[,pbnn2[[2]]]), y=(target), neurons=pbnn2[[3]])))
          preds_brnn2 <- predict(mod_brnn, as.matrix(prin_tom[, pbnn2[[2]]]))
          invisible(capture.output(mod_nnet <- nnet(x = as.matrix(prin_ind[, pnet2[[2]]]), y=(target), size=pnet2[[3]], maxit = pnet2[[5]], linout = TRUE, decay = pnet2[[4]], MaxNWts=100000)))
          preds_nnet2 <- predict(mod_nnet, as.matrix(prin_tom[, pnet2[[2]]]))
          invisible(capture.output(mod_svm <- svm(x = as.matrix(prin_data[,psvm2[[2]]]), y=(target), C=psvm2[[3]], gamma=psvm2[[4]], epsilon=psvm2[[5]])))
          preds_svm2 <- predict(mod_svm, as.matrix(prin_tom[, psvm2[[2]]]))
          
          p_list <- list()
          p_list[[1]] <- preds_grad2
          p_list[[2]] <- preds_brnn2
          p_list[[3]] <- preds_nnet2
          p_list[[4]] <- preds_svm2
          
          if(!pca){
          p_list[[5]] <- preds_grad1
          p_list[[6]] <- preds_brnn1
          p_list[[7]] <- preds_nnet1
          p_list[[8]] <- preds_svm1
          }
          
          preds <- fin_preds(p_list, full_lists[[10]])
          preds <- preds[2]
          bod <- get_bod(preds, target_name, c(full_lists[[11]], 2*full_lists[[11]]))
          
          write(bod, paste(target_name, '_email.txt',sep=''))
          write(as.character(preds), paste(target_name, '_prediction.txt',sep=''))
          ", sep="")

write(x,  paste(target_name, '_script.R'))
}