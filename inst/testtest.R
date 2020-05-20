


load("/disk/home/jullum/Prosjekter_lokalt/demp/testing.RData")

initQ = 0.25

mod_KF=fittestArimaKF(timeseries = timeseries,h=1,initQ=log(0.25)+1,
                              max.p=1,max.q=0,d=0,ic="aic",allowdrift = F,allowmean=F,start.q=0,stepwise=T,stationary=F,
                      na.action = na.pass)

KFS(mod_KF$model)
arima(timeseries,c(1,0,0))

likfn(pars=inits,model=model, p=initPar$p, q=initPar$q, d=initPar$d, estimate=FALSE)