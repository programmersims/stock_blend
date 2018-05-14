#@param x - prediction vector
#@param y - target vector

#@return - return stream vector

xor1 <- function(x,y){
  if(y > 1 & x > 1){
    return(y)
  }
  if(y < 1 & x < 1){
    return((1+abs(1-y)))
  }
  else{
    return((1-abs(1-y)))
  }
}

#@param a- vector of cross validated predictions from some model
#@param b- vector of cross validated predictions from some model
#@param target- vector of target values

#@return prediction vector with lowest 2/3 mae/correlation/sharp ratio

library(Metrics)
compare_models <- function(a, b, target){
  if(anyNA(b) | any(b<0)){
    return(a)
  }
  s1 <- log(a)*log(target)
  s2 <- log(b)*log(target)
  m11 <- mean(s1)/sd(s1)
  m12  <- mean(s2)/sd(s2)
  m21 <- cor(a, target)
  m22 <- cor(b, target)
  m31 <- mae(target, a)
  m32 <- mae(target, b)
  s <- (m11 > m12) + (m21 > m22) + (m31 < m32)
  if(s > 1){
    return(a)
  }
  else{
    return(b)
  }
}

#@param l1 - list of models containing prediction vector in first spot
#@param target- target vector

#@return - list with best predictions and spot in spot of best predictions in l1

get_best_model <- function(l1, target){
  best <- unlist(l1[[1]][1])
  temp <- best
  x <- 1
  for(i in 1:length(l1)){
    best <- compare_models(unlist(best), unlist(l1[[i]][1]), target)
    if(all(best!=temp)){
      x <- i
      temp <- best
    }
  }
  print('Corralation of best model:')
  print(cor(best, target))
  print('Sharp of best model:')
  l <- log(best)*log(target)
  print(mean(l)/sd(l)* sqrt(252))
  l <- list()
  l[[1]] <- best
  l[[2]] <- x
  return(l)
}

opt <- function(rat) {
  x <- p_list[[1]]
  for(i in 1:(length(rat))) {
    x <- x + p_list[[i+1]]*rat[i]
  }
  x <- x/(1+sum(rat))
  y <- -log(target) * log(x)
  if(cust){
    #target <- 0.5*target+(seq(0, 1,length.out=length(target)))*target
    #x <- 0.5*x+(seq(0,1,length.out=length(target)))*x
    return(-cor(x, target)*50+0.25*mean(y)/sd(y)+0.25*mean(y[floor(length(y)/2):length(y)])/sd(y[floor(length(y)/2):length(y)])+0.25*
      0.25*mean(y[floor(3.25*length(y)/4):length(y)])/sd(y[floor(3.25*length(y)/4):length(y)]) +
      0.25*mean(y[floor(9*length(y)/10):length(y)])/sd(y[floor(9*length(y)/10):length(y)]))
  }
  if(mm){
    target <- 0.5*target+(seq(0, 1,length.out=length(target)))*target
    x <- 0.5*x+(seq(0,1,length.out=length(target)))*x
    return(mae((target*100), (x*100)))
  }
  if(corr){
    return(-cor(x, target))
  } else{
    return(mean(y)/sd(y))
  }
}

fin_preds <- function(p_list, ratios){
  x <- p_list[[1]]
  for(i in 1:(length(ratios))) {
    x <- x + p_list[[i+1]]*ratios[i]
  }
  x <- x/(1+sum(ratios))
  return(x)
}
