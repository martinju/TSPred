


load("/disk/home/jullum/Prosjekter_lokalt/demp/testing.RData")



mod_KF=fittestArimaKF(timeseries = timeseries,h=1,initQ=0,
                              max.p=3,max.q=2,d=1,ic="aic",allowdrift = F,allowmean=T,start.q=0,
                      stepwise=T,stationary=T,
                      na.action = na.pass)

initQ = log(var(timeseries,na.rm=T))/2 + 0.1
initH = log(var(timeseries,na.rm=T))/2 + 0.1

mod_KF2=fittestArimaKF(timeseries = timeseries,h=1,initQ=initQ,update.H=T,initH=initH,
                      max.p=3,max.q=2,d=1,ic="aic",allowdrift = F,allowmean=T,start.q=0,
                      stepwise=T,stationary=T,
                      na.action = na.pass)

mod_KF2$model$H
mod_KF2$model$Q


mod_KF$model$H
mod_KF$model$Q

mod_KF2$model$H
mod_KF2$model$Q

#all.equal(mod_KF,mod_KF2)

aa = NULL
# ARMA(2,1)
aa[4] = KFS(mod_KF$model)$alphahat[1,1] # Intercept
aa[1] = mod_KF$model$T[2,2,1] # AR 1
aa[2:3]=mod_KF$model$R[3:4] # MA-terms
aa[5] = mod_KF$model$Q[1] # sigma^2

bb = NULL
# ARMA(2,1)
bb[4] = KFS(mod_KF2$model)$alphahat[1,1] # Intercept
bb[1] = mod_KF2$model$T[2,2,1] # AR 1
bb[2:3]=mod_KF2$model$R[3:4] # MA-terms
bb[5] = mod_KF2$model$Q[1] # sigma^2
bb[6] = mod_KF2$model$H[1] # observation noise
bb

mod_KF$initPar$fitARIMA
Arima(timeseries,c(1,0,2))
aa
likfn(pars=inits,model=model, p=initPar$p, q=initPar$q, d=initPar$d, estimate=FALSE)

eps = 0.1
mod = mod_KF2$model
#mod$H[1,1,1] = eps

library(KFAS)
filter = KFAS:::predict.SSModel(mod,interval="prediction",filtered = T,level = 0.95)
smooth = KFAS:::predict.SSModel(mod,interval="prediction",filtered = F,level = 0.95) # NaN means exact fit

filter_conf = KFAS:::predict.SSModel(mod,interval="confidence",filtered = T,level = 0.95)
smooth_conf = KFAS:::predict.SSModel(mod,interval="confidence",filtered = F,level = 0.95) # NaN means exact fit

tail(filter_conf)
tail(filter)

last100 = 101:200
plot(seq_along(last100),timeseries[last100],type="l")
lines(filter[last100,1],col=2)
lines(filter[last100,2],col=2,lty=2)
lines(filter[last100,3],col=2,lty=2)
lines(smooth[last100,1],col=3)
lines(smooth[last100,2],col=3,lty=2)
lines(smooth[last100,3],col=3,lty=2)

lines(filter_conf[last100,1],col=2)
lines(filter_conf[last100,2],col=2,lty=2)
lines(filter_conf[last100,3],col=2,lty=2)
#lines(smooth_conf[last100,1],col=3)
#lines(smooth_conf[last100,2],col=3,lty=2)
#lines(smooth_conf[last100,3],col=3,lty=2)


bb = seq_along(timeseries)
bb[is.na(timeseries)] = NA
rug(bb)

first100 = 1:100
Arimamod = Arima(timeseries,c(1,0,2))

pred_dt = matrix(NA,ncol=3,nrow=length(last100))
new_ts = timeseries[first100]
for(i in seq_along(last100)){
  new_ts <- c(new_ts,c(timeseries[last100])[i])
  mod_new <- Arima(new_ts,model = Arimamod)
  forcast0 <- forecast(mod_new,h = 1,level = 0.95)
  pred_dt[i,] <- c(forcast0$mean,forcast0$lower,forcast0$upper)
}
pred_dt = rbind(c(0,-Inf,Inf),pred_dt[seq_along(last100)[-length(last100)],])


lines(pred_dt[,1],col=2,lwd=2)
lines(pred_dt[,2],col=2,lwd=2,lty=2)
lines(pred_dt[,3],col=2,lwd=2,lty=2)

###

pred_dt2 = matrix(NA,ncol=3,nrow=length(last100))
new_ts = timeseries[first100]
for(i in seq_along(last100)){
  new_ts <- c(new_ts,c(timeseries[last100])[i])
  mod_new = mod
  mod_new$y = as.ts(as.matrix(new_ts))
  attr(mod_new,"n") <- length(new_ts)
  forcast0 <- KFAS:::predict.SSModel(mod_new,n.ahead = 1,interval="confidence",filtered = T,level = 0.95)

  pred_dt2[i,] <- as.numeric(forcast0)
}



pred_dt2 = rbind(c(0,-Inf,Inf),pred_dt2[seq_along(last100)[-length(last100)],])

lines(pred_dt2[,1],col=2,lwd=2)
lines(pred_dt2[,2],col=2,lwd=2,lty=2)
lines(pred_dt2[,3],col=2,lwd=2,lty=2)


### Opplegg:

# Tilpass tidsrekke med TSPred
# Hent ut objektet du trenger til slutt
# Kjør eventuelt en oppdatering av H og Q til slutt
# Kjør så predict med confidence of filtered = T, som gir meg det jeg skal ha
# Kan også kjøre en variant med smoothed


# Prøver å oppdatere parametere til slutt

mod_KF3 = mod_KF2

mod_KF3$model$H[1,1,1]  = NA
mod_KF3$model$Q[1,1,1] = mod_KF3$model$Q[1,1,1]/2#NA

mod_KF4 = fitSSM(mod_KF3$model,inits=c(log(0.15)))
exp(mod_KF4$optim.out$par)

