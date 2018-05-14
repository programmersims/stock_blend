#@param data - dataframe of predictors
#@param target - vector of target
#@param covs - list of covariates to iterate through when fiting cross-validated bayesian neural networks
#@param neurons - vector of neurons to try when fitting cross-validated bayesian neural networks
#@param pca - string to use that prints if using principle components
#prints cross validated least squares correlation and sharp ratio or bayiesien neural net

#return - list of cross validated predicted values and covariates used, and neurons used

get_brnn <- function(data, target, covs, neurons = c(1, 2), pca_string=""){
  full_list <- list()
  preds_brnn <- numeric(length(target))
  set.seed(1)
  folds <- cvFolds(n = length(target), K = 10, type = "random")
  a <- 1
  print(covs)
  for(j in 1:length(covs)){
    for(k in neurons){
      for(i in 1:10){
        invisible(capture.output(mod_brnn <- brnn(x = as.matrix(data[folds$which!=i, covs[[j]]]), y=(target[folds$which!=i]), neurons=k)))
        preds_brnn[folds$which==i] <- predict(mod_brnn, as.matrix(data[folds$which==i, covs[[j]]]))
      }
      print(paste('Correlation of', k ,'Neuron', pca_string,'BNN'))
      corr <- cor(preds_brnn, target)
      print(corr)
      print(paste('Annual Sharp Ratio of',  k,'Neuron', pca_string,'BNN:'))
      l <- log(preds_brnn) * log(target)
      sharp <- sqrt(250) * mean(l)/sd(l)
      print(sharp)
      l1 <- list()
      l1[[1]] <- preds_brnn
      l1[[2]] <- covs[[j]]
      l1[[3]] <- k
      full_list[[a]] <- l1
      a <- a+1
    }
  }
  return(full_list)
}

function (HLC, n = 14, maType, ...) 
{
    HLC <- try.xts(HLC, error = as.matrix)
    dH <- momentum(HLC[, 1])
    dL <- -momentum(HLC[, 2])
    DMIp <- ifelse(dH == dL | (dH < 0 & dL < 0), 0, ifelse(dH > 
        dL, dH, 0))
    DMIn <- ifelse(dH == dL | (dH < 0 & dL < 0), 0, ifelse(dH < 
        dL, dL, 0))
    TR <- ATR(HLC)[, "tr"]
    TRsum <- wilderSum(TR, n = n)
    DIp <- 100 * wilderSum(DMIp, n = n)/TRsum
    DIn <- 100 * wilderSum(DMIn, n = n)/TRsum
    DX <- 100 * (abs(DIp - DIn)/(DIp + DIn))
    maArgs <- list(n = n, ...)
    if (missing(maType)) {
        maType <- "EMA"
        maArgs$wilder <- TRUE
    }
    ADX <- do.call(maType, c(list(DX), maArgs))
    result <- cbind(DIp, DIn, DX, ADX)
    colnames(result) <- c("DIp", "DIn", "DX", "ADX")
    reclass(result, HLC)
}

function (x, n = 10) 
{
    x <- try.xts(x, error = as.matrix)
    if (n < 1 || n > NROW(x)) 
        stop(sprintf("n = %d is outside valid range: [1, %d]", 
            n, NROW(x)))
    if (NCOL(x) > 1) {
        stop("ncol(x) > 1. wilderSum only supports univariate 'x'")
    }
    x.na <- naCheck(x, n)
    result <- .Call("wilderSum", x, n, PACKAGE = "TTR")
    reclass(result, x)
}

function (x, n = 1, na.pad = TRUE) 
{
    x <- try.xts(x, error = as.matrix)
    if (is.xts(x)) {
        mom <- diff(x, n, na.pad = na.pad)
    }
    else {
        NAs <- NULL
        if (na.pad) {
            NAs <- rep(NA, n)
        }
        mom <- c(NAs, diff(x, n))
    }
    reclass(mom, x)
}


