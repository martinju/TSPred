#Subclass LT
LT <- function(base = exp(1)){ #::TSPred
  processing(prep_func=LogT, prep_par=list(base=base), 
             postp_func=LogT.rev, postp_par=list(base=base),
             method="Logarithmic transform", subclass="LT")
}
summary.LT <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  cat("\tBase: ",obj$prep$par$base,"\n")
}


#Subclass BCT
BCT <- function(lambda=NULL,prep_par=NULL,postp_par=NULL,...){
  processing(prep_func = TSPred::BCT, prep_par = c(list(lambda=lambda),prep_par),
             postp_func = TSPred::BCT.rev, postp_par = c(list(lambda=lambda),postp_par),
             method = "Box-Cox transform",..., subclass ="BCT")
}
preprocess.BCT <- function(obj,...){
  results <- NextMethod()
  
  #if preprocessing with undefined lambda, update value of computed lambda parameter in the BCT object(s)
  if(is.null(obj$prep$par$lambda)) results <- updt(results, par="lambda")
  
  return(results)
}
summary.BCT <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  cat("\tLambda: ",obj$prep$par$lambda,"\n")
  if(length(obj$prep$par)>1){
    cat("\nOther parameters:\n")
    print(obj$prep$par[-1])
  }
}


#Subclass WT  #::TSPred
WT <- function(level=NULL,filter=NULL,boundary="periodic",prep_par=NULL,postp_par=NULL,...){
  processing(prep_func = WaveletT, prep_par = c(list(level=level,filter=filter,boundary=boundary),prep_par),
             postp_func = WaveletT.rev, postp_par = c(list(wt_obj=NULL),postp_par),
             method = "Wavelet transform",..., subclass ="WT")
}
preprocess.WT <- function(obj,...){
  results <- NextMethod()
  
  #if preprocessing with undefined parameters, update computed values of parameters in the WT object(s)
  if(is.null(obj$postp$par$wt_obj)) results <- updt(results, par="wt_obj")
  if(is.null(obj$prep$par$level)||is.null(obj$prep$par$filter)){
    results <- updt(results, par="level", value=results[[1]][[1]]$obj$postp$par$wt_obj@level) 
    results <- updt(results, par="filter", value=results[[1]][[1]]$obj$postp$par$wt_obj@filter@wt.name)
  }
  
  return(results)
}
postprocess.WT <- function(obj,...){
  NextMethod(obj,...,map=FALSE)
}
summary.WT <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  cat("\tLevel: ",obj$prep$par$level,"\n")
  cat("\tFilter: ",obj$prep$par$filter,"\n")
  cat("\tBoundary: ",obj$prep$par$boundary,"\n")
  if(length(obj$prep$par)>3){
    cat("\nOther parameters:\n")
    print(obj$prep$par[-(1:3)])
  }
}


#Subclass subset
subsetting <- function(train_perc=0.8, test_len=NULL){
  processing(prep_func = train_test_subset, prep_par = list(train_perc=train_perc,test_len=test_len),
             postp_func = NULL, postp_par = NULL,
             method = "Subsetting data into training and testing sets", subclass ="subsetting")
}
summary.subsetting <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  if(!is.null(obj$prep$par$test_len))
    cat("\tTesting set length: ",obj$prep$par$test_len,"\n")
  else
    cat("\tTraining set percentage: ",obj$prep$par$train_perc,"\n")
}


#Subclass sw
SW <- function(window_len=NULL){
  processing(prep_func = sw, prep_par = list(k=window_len),
             postp_func = NULL, postp_par = NULL,
             method = "Sliding windows", subclass ="SW")
}
preprocess.SW <- function(obj,data,...,map=TRUE){
  if(attr(data,"subset") == "test")
    data[[1]] <- c( tail(attr(data,"train_data"),obj$prep$par$k-1), data[[1]] )
  
  NextMethod(obj,data,...,map=map)
}
is.SW <- function(sw_obj){
  is(sw_obj,"SW")
}
summary.SW <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par))  cat("Parameters:\n")
  cat("\tWindow length: ",obj$prep$par$k,"\n")
}


#Subclass NAS
NAS <- function(na.action=na.omit,prep_par=NULL){
  processing(prep_func = na.action, prep_par = c(list(prep_par)),
             postp_func = NULL, postp_par = NULL,
             method = "Missing values treatment", subclass ="NAS")
}
summary.NAS <- function(obj,...){
  NextMethod()
  cat("\tFunction: ",as.character(substitute(obj$prep$func)),"\n")
  if(!is.null(obj$prep$par) && (length(obj$prep$par)>0)){
    cat("Parameters:\n")
    print(obj$prep$par)
  }
}


#Subclass minmax
MinMax <- function(min=NULL,max=NULL,byRow=TRUE){
  processing(prep_func = minmax, prep_par = list(min=min,max=max,byRow=byRow),
             postp_func = minmax.rev, postp_par = list(min=min,max=max),
             method = "MinMax normalization", subclass ="MinMax")
}
preprocess.MinMax <- function(obj,...){
  if(obj$prep$par$byRow) obj$prep$par$min <- obj$prep$par$max <- NA
  
  results <- NextMethod()
  
  if(is.null(obj$prep$par$min) || obj$prep$par$byRow) results <- updt(results, par="min")
  if(is.null(obj$prep$par$max) || obj$prep$par$byRow) results <- updt(results, par="max")
  
  return(results)
}
summary.MinMax <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  cat("\tMin: ",obj$prep$par$min,"\n")
  cat("\tMax: ",obj$prep$par$max,"\n")
}


#Subclass AN
AN <- function(min=NULL,max=NULL,alpha=1.5,byRow=TRUE){
  processing(prep_func = an, prep_par = list(min=min,max=max,alpha=alpha,byRow=byRow),
             postp_func = an.rev, postp_par = list(min=min,max=max,an=NULL),
             method = "Adaptive normalization", subclass ="AN")
}
preprocess.AN <- function(obj,...){
  if(obj$prep$par$byRow) obj$prep$par$min <- obj$prep$par$max <- NA
  
  results <- NextMethod()
  
  if(is.null(obj$prep$par$min) || obj$prep$par$byRow) results <- updt(results, par="min")
  if(is.null(obj$prep$par$max) || obj$prep$par$byRow) results <- updt(results, par="max")
  results <- updt(results, par="an")
  
  return(results)
}
summary.AN <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  cat("\tMin: ",obj$prep$par$min,"\n")
  cat("\tMax: ",obj$prep$par$max,"\n")
  cat("\tMeans: ",obj$postp$par$max,"\n")
}

#Subclass DIF
DIF <- function(lag=NULL, differences=NULL, type="simple",postp_par=list(addinit=FALSE)){
  processing(prep_func = diff, prep_par = list(lag=lag,differences=differences,type=type),
             postp_func = diff.rev, postp_par = c(list(lag=lag,differences=differences,type=type,xi=NULL),postp_par),
             method = "Differencing", subclass ="DIF")
}
preprocess.DIF <- function(obj,data,...,map=TRUE){
  if(attr(data,"subset") == "test")
    data[[1]] <- c( tail(attr(data,"train_data"),obj$prep$par$lag*obj$prep$par$differences), data[[1]] )
  
  results <- NextMethod(obj,data,...,map=map)
  
  if(is.null(obj$prep$par$lag)) results <- updt(results, par="lag")
  if(is.null(obj$prep$par$differences)) results <- updt(results, par="differences")
  if(is.null(obj$prep$par$type)) results <- updt(results, par="type")
  
  if(attr(data,"prep_test")) results <- updt(results, par="xi")
  else results <- updt(results, par="xi", refpar="xf")
  
  return(results)
}
summary.DIF <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  cat("\tType: ",obj$postp$par$type,"\n")
  cat("\tLag: ",obj$prep$par$lag,"\n")
  cat("\tDifferences: ",obj$prep$par$differences,"\n")
}


#Subclass MAS
MAS <- function(order=NULL,prep_par=NULL,postp_par=list(addinit=FALSE)){
  processing(prep_func = mas, prep_par = c(list(order=order),prep_par),
             postp_func = mas.rev, postp_par = c(list(order=order,xi=NULL),postp_par),
             method = "Moving average smoothing", subclass ="MAS")
}
preprocess.MAS <- function(obj,data,...,map=TRUE){
  if(attr(data,"subset") == "test")
    data[[1]] <- c( tail(attr(data,"train_data"),obj$prep$par$order-1), data[[1]] )
  
  results <- NextMethod(obj,data,...,map=map)
  
  if(is.null(obj$prep$par$order)) results <- updt(results, par="order")
  
  if(attr(data,"prep_test")) results <- updt(results, par="xi")
  else results <- updt(results, par="xi", refpar="xf")
  
  return(results)
}
summary.MAS <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  cat("\tOrder: ",obj$postp$par$order,"\n")
}


#Subclass PCT
PCT <- function(postp_par=NULL){
  processing(prep_func = pct, prep_par = NULL,
             postp_func = pct.rev, postp_par = c(list(xi=NULL),postp_par),
             method = "Percentage change transform", subclass ="PCT")
}
preprocess.PCT <- function(obj,data,...,map=TRUE){
  if(attr(data,"subset") == "test")
    data[[1]] <- c( tail(attr(data,"train_data"),1), data[[1]] )
  
  results <- NextMethod(obj,data,...,map=map)
  
  if(attr(data,"prep_test")) results <- updt(results, par="xi")
  else results <- updt(results, par="xi", refpar="xf")
  
  return(results)
}

#Subclass EMD  #::TSPred
EMD <- function(num_imfs=0,meaningfulImfs=NULL,prep_par=NULL){
  processing(prep_func = emd, prep_par = c(list(num_imfs=num_imfs,meaningfulImfs=meaningfulImfs),prep_par),
             postp_func = emd.rev, postp_par = NULL,
             method = "Empirical mode decomposition", subclass ="EMD")
}
preprocess.EMD <- function(obj,...){
  results <- NextMethod()
  
  #if preprocessing with undefined parameters, update computed values of parameters in the WT object(s)
  if(is.null(obj$prep$par$num_imfs)||obj$prep$par$num_imfs==0) results <- updt(results, par="num_imfs")
  if(is.null(obj$prep$par$meaningfulImfs)||obj$prep$par$meaningfulImfs==0) results <- updt(results, par="meaningfulImfs")
  
  return(results)
}
postprocess.EMD <- function(obj,...){
  NextMethod(obj,...,map=FALSE)
}
summary.EMD <- function(obj,...){
  NextMethod()
  if(!is.null(obj$prep$par) || !is.null(obj$postp$par))  cat("Parameters:\n")
  cat("\tNumber of IMF's: ",obj$prep$par$num_imfs,"\n")
  cat("\tMeaningful IMF's: ",obj$prep$par$meaningfulImfs,"\n")
  if(length(obj$prep$par)>2){
    cat("\nOther parameters:\n")
    print(obj$prep$par[-(1:2)])
  }
}

#============== DO ==============
#Subclass detrend  #DO
#Subclass THieF  #DO
#Subclass zscore  #DO