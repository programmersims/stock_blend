get_data_full_day <- function(name, ma_days=c(), adx_days=c(), sd_days=c(), sd_days_interaction=list(), macd=FALSE, ema=FALSE, days_close=c(5), reversal=FALSE){
  
  time <- "390"
  str <- "5"
  command <- paste("python vxx_daily", str, ".py --lista", sep="")
  
  for(i in name){
    command <- paste(command, i)
  }
  command <- paste(command, "--time_in_minutes", time)
  system(command = command)
  
  stocks <- list()
  j <- 1
  for(i in name){
    s <- read.csv(paste(i, '.csv', sep=""), header = FALSE)
    stocks[[j]] <- s
    j <- j+1
  }
  for(i in 1:length(stocks)){
    colnames(stocks[[i]]) <- c('Time', 'High', 'Low', 'Open', 'Close', 't1', 't2', 't3')
  }
  
  for(i in 1:length(stocks)){
    stocks[[i]] <- get_good_columns_full_day(stocks[[i]], ma_days = ma_days, adx_days=adx_days, sd_days=sd_days, sd_days_interaction=sd_days_interaction, macd=macd, ema=ema, days_close = days_close, reversal=reversal)
    colnames(stocks[[i]])[2:ncol(stocks[[i]])] <- paste(name[i], colnames(stocks[[i]])[2:ncol(stocks[[i]])] , sep="_")
  }
  
  stocks <-Reduce(merge, stocks)
  return(stocks)
}

get_data_full_day_seq <- function(name, ma_days=c(), adx_days=c(), sd_days=c(), sd_days_interaction=list(), macd=FALSE, ema=FALSE, days_close=c(5), reversal=FALSE){
  
  time <- "390"
  str <- "5"
  command <- paste("python vxx_daily", str, ".py --lista", sep="")
  
  for(i in name){
    command <- paste(command, i)
  }
  command <- paste(command, "--time_in_minutes", time)
  #system(command = command)
  
  stocks <- list()
  j <- 1
  for(i in name){
    s <- read.csv(paste(i, '.csv', sep=""), header = FALSE)
    stocks[[j]] <- s
    j <- j+1
  }
  for(i in 1:length(stocks)){
    colnames(stocks[[i]]) <- c('Time', 'High', 'Low', 'Open', 'Close', 't1', 't2', 't3')
  }
  
  for(i in 1:length(stocks)){
    stocks[[i]] <- get_good_columns_full_day(stocks[[i]], ma_days = ma_days, adx_days=adx_days, sd_days=sd_days, sd_days_interaction=sd_days_interaction, macd=macd, ema=ema, days_close = days_close, reversal=reversal)
    colnames(stocks[[i]])[2:ncol(stocks[[i]])] <- paste(name[i], colnames(stocks[[i]])[2:ncol(stocks[[i]])] , sep="_")
  }
  
  stocks <-Reduce(merge, stocks)
  return(stocks)
}

get_good_columns_full_day <- function(x, ma_days=c(), adx_days=c(), sd_days=c(), sd_days_interaction=list(), macd=FALSE, ema=FALSE, days_close=c(5), reversal=FALSE){
  #x <- x[nrow(x):1,]
  
  x$percRange<-x$High/x$Low
  x$highOpen<-x$High/x$Open
  x$highClose<-x$High/x$Close
  x$lowOpen<-x$Low/x$Open
  x$lowClose<-x$Low/x$Close
  x$change<-x$Close/x$Open
  x$changeRange<-x$change/x$percRange
  x$t3 <- NULL
  
  for(i in 2:ncol(x)){
    x[!is.finite(x[,i]), i] <- mean(x[is.finite(x[,i]),i], na.rm=TRUE)
    x[!is.numeric(x[,i]),i] <- mean(x[is.numeric(x[,i]),i], na.rm=TRUE)
  }
  
  for(i in ma_days){
    x[,paste('SMA_percRange', i, sep='_')] <- SMA(x$percRange, i)
    x[,paste('SMA_change', i, sep='_')] <- SMA(x$change, i)
    x[,paste('SMA_changeRange', i, sep='_')] <- SMA(x$changeRange, i)
    if(ema) {
      x[,paste('EMA_change', i, sep='_')] <- EMA(x$change, i)
      x[,paste('EMA_percRange', i, sep='_')] <- EMA(x$percRange, i)
      x[,paste('EMA_changeRange', i, sep='_')] <- EMA(x$changeRange, i)

      x[,paste('DEMA_change', i, sep='_')] <- DEMA(x$change, i)
      x[,paste('DEMA_percRange', i, sep='_')] <- DEMA(x$percRange, i)
      x[,paste('DEMA_changeRange', i, sep='_')] <- DEMA(x$changeRange, i)

      
    }
  }

  if(reversal){
    outside <- numeric(nrow(stocks))
    for(i in 2:nrow(stocks)){
      if((stocks$High[i] > stocks$High[i-1]) & (stocks$Low[i] < stocks$Low[i-1]))
        outside[i] <- 1
    }
    stocks$outside_reversal <- outside
  }

  #elimates redundancy in smas
  #for(i in length(ma_days):2){
  #  x[,paste('SMA_percRange', ma_days[i], sep='_')] <- ((x[,paste('SMA_percRange', ma_days[i], sep='_')]) - (ma_days[i-1]/ma_days[i]) * (x[,paste('SMA_percRange', ma_days[i-1], sep='_')]))*ma_days[i]/(ma_days[i]-ma_days[i-1])
  #  x[,paste('SMA_change', ma_days[i], sep='_')] <- ((x[,paste('SMA_change', ma_days[i], sep='_')]) - (ma_days[i-1]/ma_days[i]) * (x[,paste('SMA_change', ma_days[i-1], sep='_')]))*ma_days[i]/(ma_days[i]-ma_days[i-1])
  #  x[,paste('SMA_changeRange', ma_days[i], sep='_')] <- ((x[,paste('SMA_changeRange', ma_days[i], sep='_')]) - (ma_days[i-1]/ma_days[i]) * (x[,paste('SMA_changeRange', ma_days[i-1], sep='_')]))*ma_days[i]/(ma_days[i]-ma_days[i-1])
  #}
  
  if(macd){
    x[,paste('MACD_Sig_r', i, sep='_')] <- MACD(x[,'percRange'])[,2]
    x[,paste('MACD_Sig_c', i, sep='_')] <- MACD(x[,'change'])[,2]
  }
  
  for(i in adx_days){
    x[,c(paste('ADX', i,'DIP', sep='_'), paste('ADX', i,'DIn', sep='_'), paste('ADX', i,'DX', sep='_'), paste('ADX', i,'ADXI', sep='_'))] <- ADX(x[,c('High','Low', 'Close')], n=i)
  }
  
  for(i in sd_days){
    x[,paste('sd_percRange', i, sep='_')] <- volatility_days(x$percRange, i)
    x[,paste('sd_change', i, sep='_')] <- volatility_days(x$change, i)
  }
  for(i in sd_days_interaction){
    x[,paste('sd_percRange', i[1],i[2], sep='_')] <- volatility_days(x$percRange, i[1])/volatility_days(x$percRange, i[2])
    x[,paste('sd_change', i[1],i[2], sep='_')] <- volatility_days(x$change, i[1])/volatility_days(x$change, i[2])
    x[,paste('sd_volume', i[1],i[2], sep='_')] <- volatility_days(x$t1, i[1])/volatility_days(x$t1, i[2])
  }
  for(i in days_close){
    for(j in (i+1):nrow(x)){
      x[j, paste('close_divided_',i,'close', sep="_")] <- x$Close[j]/x$Close[j-i]
    }
  }
  
  x$Open <- NULL
  x$High <- NULL
  x$Low <- NULL
  x$Close <- NULL
  x$t1 <- NULL
  x$t2 <- NULL
  
  return(x)
}
