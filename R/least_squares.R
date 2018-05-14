get_ls <- function(data, target, p_values = c(0.1, 0.05), pca_string="", cv_list){
  full_list <- list()
  mod_lm1 <- lm(target ~., data=data)
  print(paste('Best', pca_string, 'LS Predictors:'))
  decent <- names(summary(mod_lm1)$coef[summary(mod_lm1)$coef[,4] <= 0.1  , 4])[-1]
  print(decent)
  set.seed(1)
  if(cv_list[[1]]){
    preds_lm <- numeric(length(target))
    folds <- cvFolds(n = length(target), K = 10, type = "random")
    for(i in 1:10){
      mod_lm <- lm(target[folds$which!=i] ~. , data=as.data.frame(data[folds$which!=i, ]))
      preds_lm[folds$which==i] <- predict(mod_lm, as.data.frame(data[folds$which==i, ]))
    }
    l1 <- list()
    l1[[1]] <- preds_lm
    l1[[2]] <- colnames(data)
    full_list[[1]] <- l1
    print('Correlation of Full LS:')
    corr <- cor(preds_lm, target)
    print(corr)

    for(j in 1:length(p_values)){
     l1 <- list()
     decent <- names(summary(mod_lm1)$coef[summary(mod_lm1)$coef[,4] <= p_values[j], 4])[-1]
     for(i in 1:10){
       mod_lm <- lm(target[folds$which!=i] ~., data=as.data.frame(data[folds$which!=i, decent]))
       preds_lm[folds$which==i] <- predict(mod_lm, as.data.frame(data[folds$which==i, decent]))
     }
      l1[[1]] <- preds_lm
      l1[[2]] <- decent
      full_list[[j+1]] <- l1
      print(paste('Correlation of Sparse (P=', as.character(p_values[j]),')' , pca_string,'LS:'))
      corr <- cor(preds_lm, target)
      print(corr)
      print(paste('Annual Sharp Ratio of Sparse (P=',  as.character(p_values[j]),')' , pca_string,'LS:'))
      l <- log(preds_lm) * log(target)
      sharp <- sqrt(250) * mean(l)/sd(l)
      print(sharp)
   }
}
else{
    temp_train <- cv_list[[2]]
    temp_targ <- cv_list[[3]]
    temp_test <- cv_list[[4]]
    targ <- cv_list[[5]]
    j <- 1
    for(i in 1:length(temp_train)){
      mod_lm <- lm(target[i[1]:i[2]] ~. , data=as.data.frame(data[i[1]:i[2],]))
      
      l <- vals_test[[j]][2]-vals_test[[j]][1]
      t1 <- vals_test[[j]][1] - it
      t2 <- vals_test[[j]][2] - it
      
      preds_lm[t1:t2] <- predict(mod_lm, as.data.frame(data[(t1+it):(t2+it), ]))
      j <- j+1
    }
    
  }
  return(full_list)
}








