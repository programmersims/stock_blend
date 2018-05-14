#@param stream - stream of stock percents
#@param size - size of volatility stream

#descrpition: This function returns the standard deviation of the previous SIZE days for a stream of stocks percents

volatility_days <- function(stream, size){
  new_stream <- numeric(length(stream))
  for(i in (size+1):length(stream)){
    new_stream[i] <- sd(stream[(i-size):i])
  }
  new_stream[new_stream==0] <- NA
  return(new_stream)
}

corr_days <- function(stocks, target_stock, days=c(20,50, 100), interaction=list(c(10,50)), method='spearman'){
  names <- unlist(unique(lapply((strsplit(colnames(stocks),split = '_')), '[[', 1)))
  names <- names[names!='Time']
  for(i in 1:length(names)){
    names[i] <- paste(names[i], 'change', sep = '_')
  }
  print(names)
  for(i in names){
    if(i != target_stock){
      for(j in days){
       for(k in (j+1):nrow(stocks)){
          stocks[k, paste(i, j,'cor', target_stock,sep='_')] <- cor(stocks[(k-j):k,i], stocks[(k-j):k,target_stock], method=method)
        }
      }
    }
  }
  return(stocks)
}

corr_days2 <- function(stocks, target_stock, days=c(100), method='spearman', d=5){
  target_stock <- paste(target_stock, 'close_divided_', d, 'close', sep = '_')
  names <- unlist(unique(lapply((strsplit(colnames(stocks),split = '_')), '[[', 1)))
  names <- names[names!='Time']
  for(i in 1:length(names)){
    names[i] <- paste(names[i], 'close_divided_',d,'close', sep = '_')
  }
  for(i in names){
    if(i != target_stock){
      for(j in days){
        for(k in seq((j+1),nrow(stocks), by = 1)){
          stocks[k, paste(i, j,'cor', target_stock,sep='_')] <- cor(stocks[seq((k-j),k,by=d),i], stocks[seq((k-j),k,by=d), target_stock], method=method)
        }
      }
    }
  }
  return(stocks)
}


relationship_vars <- function(stocks, target_stock, days=c(50), method='spearman'){
  names <- unlist(unique(lapply((strsplit(colnames(stocks),split = '_')), '[[', 1)))
  names <- names[names!='Time']
  for(i in 1:length(names)){
    names[i] <- paste(names[i], 'change', sep = '_')
  }
  for(i in names){
    if(i != target_stock){
      for(j in days){
        for(k in (j+1):nrow(stocks)){
          stocks[k, paste(i, j,'rel', target_stock,sep='_')] <- cor(stocks[(k-j):k,i], stocks[(k-j):k,target_stock], method=method) * (stocks[k,i]-1)/sd(stocks[(k-j):k, i] )
        }
      }
    }
  }
  return(stocks)
}

relationship_vars2 <- function(stocks, target_stock, days=c(50), method='spearman'){
  names <- unlist(unique(lapply((strsplit(colnames(stocks),split = '_')), '[[', 1)))
  names <- names[names!='Time']
  names2 <- names
  for(i in 1:length(names)){
    names2[i] <- paste(names[i], 'SMA_percRange_9', sep = '_')
    names[i] <- paste(names[i], 'change', sep = '_')
  }
  d <- 0
  for(i in names){
    d <- d + 1
    if(i != target_stock){
      for(j in days){
        for(k in (j+1):nrow(stocks)){
          stocks[k, paste(i, j,'rel', target_stock,sep='_')] <- (cor(stocks[(k-j):k,i], stocks[(k-j):k,target_stock])*0.5+cor(stocks[(k-j):k,i], stocks[(k-j):k,target_stock], method='spearman')*0.5) *
            (stocks[k,i]-1)/(((stocks[k,names2[d]])+0.00001)*sd(stocks[(k-j):k, i] ))
        }
      }
    }
  }
  return(stocks)
}

