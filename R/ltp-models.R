## This program is fre esoftware: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.

## project name: ltp
## project website: http://code.google.com/p/ltp/
## created: 2011

## tutti i risultati dei modelli contengono un campo della lista contenente la serie strorica del product
## oltre ai data product con la forma originaes.

############## best.lm()
mod.lm <- function(product, n.ahead, period.start, period.freq, xreg.lm = NA, logtransform, stepwise, formula.right.lm = NULL,negTo0=negTo0,toInteger=toInteger) {
  
  ##__##logger(DEBUG, "  function mod.lm")
  if(is.null(formula.right.lm)) formula.right.lm = match.arg(formula.right.lm, " S * trend + S * trend2")
  
  y = as.vector(product)
  n = max(length(y), nrow(y))
  y = ts(y, start = period.start, frequency = period.freq)
  attr(y, "product") = names(product)
  
  k = n + n.ahead
  
  
                                        # var trend
  trend = 1:k
                                        # dummy stag
  S = as.factor(((period.start[2] - 1 + (0:(k - 1)))%%period.freq) + 1)
  
  d = data.frame(tempo = rep(NA, k), qta = rep(NA, k), trend = trend, 
    trend2 = trend^2, S = S, xreg.lm = xreg.lm)
  
  form = as.formula(paste(ifelse(logtransform, "log(stima$qta)", "stima$qta"), 
    " ~ ", formula.right.lm, sep = ""))
  
  d$tempo[1:n] = rownames(product)
  d$qta[1:n] = unlist(product)
  stima = d[(1:n), ]
  new.data = d[-(1:n), ]
  
  if (stepwise) steps=100 
  else steps=0
  modlm = step(lm(form,data=stima),steps=steps, direction = "both", trace = 0)
 
  pred = predict(modlm, newdata = new.data, interval = "prediction", level = 0.95)
  
  if (logtransform) {
    pred.modlm = exp(pred[, "fit",drop=FALSE])
    IC.pred.modlm = list(upr = exp(pred[, "upr"]), lwr = exp(pred[, "lwr"]))
  }
  else {
    pred.modlm = (pred[, "fit",drop=FALSE])
    IC.pred.modlm = list(upr = pred[, "upr"], lwr = pred[, "lwr"])
  }
  if(negTo0) {
	pred.modlm[pred.modlm<0]=0
	IC.pred.modlm$upr[IC.pred.modlm$upr<0]=0
	IC.pred.modlm$lwr[IC.pred.modlm$lwr<0]=0
	}
  if(toInteger) {
	pred.modlm=round(pred.modlm,0)
	}
  
  pred.modlm=ts(pred.modlm, start=.incSampleTime(now=end(y), period.freq = period.freq) , frequency=period.freq)
  
  lm.AIC = AIC(modlm, k = 2)
  lm.R2 = summary(modlm)$r.squared
  ic.delta = mean(IC.pred.modlm$upr - IC.pred.modlm$lwr)
  maxJump = getMaxJump (product, period.freq, pred.modlm)
  VarCoeff= getVarCoeff(product,pred.modlm) 
  # m=matrix(NA,period.freq, ceiling((dim(pred.modlm)[1])/period.freq)+1)
  # m[1:(length(pred.modlm)+period.freq)]=c(y[nrow(y):(nrow(y)-period.freq+1),],pred.modlm)
  # m=apply(m,2,mean,na.rm=TRUE)
  # sdJumps = sd(m[-1]/m[-length(m)])
  res = ts(residuals(modlm), start = period.start, frequency = period.freq)
                                        #media_errori = mean(errori_lm)
  lista.lm = list(ts.product = y, model = modlm, prediction = pred.modlm, 
    IC = IC.pred.modlm, AIC = lm.AIC, R2 = lm.R2, IC.width = ic.delta, maxJump=maxJump,  VarCoeff=VarCoeff, Residuals = res)
  lista.lm
}
############## best.arima()
mod.arima <- function(product,logtransform,diff.sea,diff.trend,idDiff,max.p,max.q,max.P,
                       max.Q,n.ahead,period.freq,xreg.arima,period.start,stepwise,stationary.arima,negTo0=negTo0,toInteger=toInteger) {
  ##__##logger(DEBUG, "  function mod.arima")
  y = as.vector(product)
  n = max(length(y), nrow(y))
                                        # vettore errori
                                        #errori_arima = rep(NA,fuori)
  y = ts(y, start = period.start, frequency = period.freq)
  if (idDiff) { d = IDdiff(y,period.freq=period.freq)
                diff.trend = as.integer(d[1])
                diff.sea = as.integer(d[2])
              }
  if (logtransform) {
    if (stepwise) 
      if (is.null(xreg.arima)) {
        mod = try(arimaId(log(y),c(max.p,diff.trend,max.q),list(order=c(max.P,diff.sea,max.Q)),idDiff=FALSE,method="ML",verbose=FALSE),TRUE)
        if (is(mod,"try-error")) { mod.arima=auto.arima(log(y),d=diff.trend,D=diff.sea,max.p=max.p,max.q=max.q,stationary=stationary.arima,max.Q=max.Q,max.P=max.P,ic="aic",xreg=xreg.arima,stepwise=stepwise)}
        else {
          index=print.allArima(mod, nshow=1)
          mod.arima = try(arima(log(y),c(as.integer(index["p"]),as.integer(index["d"]),as.integer(index["q"])),c(as.integer(index["P"]),as.integer(index["D"]),as.integer(index["Q"])),method="ML"),TRUE)
          if (is(mod.arima,"try-error")) { mod.arima=auto.arima(log(y),d=diff.trend,D=diff.sea,max.p=max.p,max.q=max.q,stationary=stationary.arima,max.Q=max.Q,max.P=max.P,ic="aic",xreg=xreg.arima,stepwise=stepwise)}
        }
      }
      else mod.arima=auto.arima(log(y),d=diff.trend,D=diff.sea,max.p=max.p,max.q=max.q,stationary=stationary.arima,max.Q=max.Q,max.P=max.P,ic="aic",xreg=xreg.arima,stepwise=stepwise)  

    else {mod.arima = arima(log(y), order = c(max.p,diff.trend,max.q), seasonal = list(order=c(max.P,diff.sea,max.Q)), xreg = xreg.arima )}
    arima.AIC = mod.arima$aic
    pred = predict(mod.arima, n.ahead)
    pred.arima = pred$pred
    ic_arima = pred.arima + qnorm(0.975) * cbind(-pred$se, pred$se)


    colnames(ic_arima) = c("lwr", "upr")
    IC.pred.arima = list(lwr = exp(ic_arima[, "lwr"]), upr = exp(ic_arima[, "upr"]))


    pred.arima = exp(pred.arima)
    tss = var(log(y)) * (n - 1)
    res = residuals(mod.arima)
  }
  else {
    if (stepwise)
      if (is.null(xreg.arima)) {
        mod = try(arimaId(y,c(max.p,diff.trend,max.q),list(order=c(max.P,diff.sea,max.Q)),idDiff=FALSE,method="ML",verbose=FALSE),TRUE)
        if (is(mod,"try-error")) { mod.arima=auto.arima(y,d=diff.trend,D=diff.sea,max.p=max.p,max.q=max.q,stationary=stationary.arima,max.Q=max.Q,max.P=max.P,ic="aic",xreg=xreg.arima,stepwise=stepwise)} 
        else {
          index=print.allArima(mod, nshow=1)
          mod.arima = try(arima(y,c(as.integer(index["p"]),as.integer(index["d"]),as.integer(index["q"])),c(as.integer(index["P"]),as.integer(index["D"]),as.integer(index["Q"])),method="ML"),TRUE)
          if (is(mod.arima,"try-error")) { mod.arima=auto.arima(y,d=diff.trend,D=diff.sea,max.p=max.p,max.q=max.q,stationary=stationary.arima,max.Q=max.Q,max.P=max.P,ic="aic",xreg=xreg.arima,stepwise=stepwise)} 
        }
      }
      else mod.arima=auto.arima(y,d=diff.trend,D=diff.sea,max.p=max.p,max.q=max.q,stationary=stationary.arima,max.Q=max.Q,max.P=max.P,ic="aic",xreg=xreg.arima,stepwise=stepwise)  
    else {mod.arima = arima(y, order = c(max.p,diff.trend,max.q), seasonal = list(order=c(max.P,diff.sea,max.Q)),xreg = xreg.arima)}
    arima.AIC = mod.arima$aic
    pred = predict(mod.arima, n.ahead)
    pred.arima = pred$pred
    ic_arima = pred.arima + qnorm(0.975) * cbind(-pred$se,pred$se)


    colnames(ic_arima) = c("lwr", "upr")
    IC.pred.arima = list(lwr = ic_arima[, "lwr"], upr = ic_arima[, "upr"])

    tss = var(y) * (n - 1)
    res = residuals(mod.arima)
  }
   if(negTo0) {
	pred.arima[pred.arima<0]=0
	IC.pred.arima$upr[IC.pred.arima$upr<0]=0
	IC.pred.arima$lwr[IC.pred.arima$lwr<0]=0
	} 
   if(toInteger) {
	pred.arima=round(pred.arima,0)
	}
	#errori_arima = abs(as.vector(log(validazione_arima) - log(pred.arima[1:fuori])))
                                        #media_errori = mean(errori_arima)
                                        #res=mod.arima[[1]]$residuals
  rss = sum(res^2)
  r2 = 1 - (rss/tss)
                                        #k = length(as.vector(mod.arima$coef))
                                        #N = n - 3
  arima.R2 = r2
  attr(y, "product") = names(product)
  ic.delta = mean(IC.pred.arima$upr - IC.pred.arima$lwr)
  maxJump = getMaxJump (product, period.freq, pred.arima)
  VarCoeff= getVarCoeff(product,pred.arima) 
  # m=matrix(NA,period.freq, ceiling((length(pred.arima))/period.freq)+1)
  # m[1:(length(pred.arima)+period.freq)]=c(y[nrow(y):(nrow(y)-period.freq+1),],pred.arima)
  # m=apply(m,2,mean,na.rm=TRUE)
  # sdJumps = sd(m[-1]/m[-length(m)])
  lista.arima = list(ts.product = y, model = mod.arima, prediction = pred.arima, 
    IC = IC.pred.arima, AIC = arima.AIC, R2 = arima.R2, IC.width = ic.delta, maxJump=maxJump, VarCoeff=VarCoeff,  Residuals = res)
  lista.arima
}

############## best.es()

mod.es <- function(product, n.ahead, period.start, period.freq, n, logtransform.es, stepwise,negTo0=negTo0,toInteger=toInteger) {
	
  h <- n.ahead / period.freq
  h.round <- round(h)
  if ( h.round > h)
    h <- h + 1

  ##__##logger(DEBUG, "  function mod.es")
	#occhio qui:
	product[product==0]=1
	
  y = as.vector(product)
  n = max(length(y), nrow(y))
  y = ts(y, start = period.start, frequency = period.freq)
                                        # data mancanti mercato
                                        #stima_le = window(y,end=end_stima)
                                        #validazione_le = window(y,start=start_val)
  if (logtransform.es) {
    modle = ets(log(y),model="ZZZ",opt.crit="lik",ic="aic")

    pred = try(forecast(modle, h=h, level=c(95)))
	
    if(is(pred,"try-error")) { 
      return(list(ts.product = y, model = modle, prediction = NA, IC = NA, AIC = NA, R2 = NA, IC.width = NA, VarCoeff =NA, maxJump = NA, Residuals = NA))
    } else {
      n.par = length(modle$par)
      es.AIC = modle$aic
      pred.modle = exp(as.vector(pred$mean))
	  pred.modle[abs(pred.modle) == Inf] = NA
	  pLower = as.vector(pred$lower)
	  pLower[pLower==Inf] = NA
	  pUpper = as.vector(pred$upper)
	  pUpper[pUpper==Inf] = NA
      IC.pred.modle = list(lwr = exp(pLower), upr = exp(pUpper))
      tss = var(y) * (n - 1)
                                        #errori_le = abs(as.vector(log(validazione_le) - log(pred.modle)))
                                        #media_errori = mean(errori_le)
    }
  }
  else {
    modle = ets(y,model="ZZZ",opt.crit="lik",ic="aic")
    pred = try(forecast(modle, h=h, level=c(95)))
	if( is(pred,"try-error") ) { 
      return(list(ts.product = y, model = modle, prediction = NA, IC = NA, AIC = NA, R2 = NA, IC.width = NA, VarCoeff =NA, maxJump = NA, Residuals = NA))
    } else {
	  n.par = length(modle$par)
      es.AIC = modle$aic
      pred.modle = as.vector(pred$mean)[1:n.ahead]
      pred.modle[abs(pred.modle) == Inf] = NA
	  pLower = as.vector(pred$lower)
	  pLower[pLower==Inf] = NA
	  pUpper = as.vector(pred$upper)
	  pUpper[pUpper==Inf] = NA
      IC.pred.modle = list(lwr = pLower , upr = pUpper )
      tss = var(y) * (n - 1)
                                        #errori_le = abs(as.vector(log(validazione_le) - log(pred.modle)))
                                        #media_errori = mean(errori_le)
    }
  }
  if(negTo0) {
	pred.modle[pred.modle<0]=0
	IC.pred.modle$upr[IC.pred.modle$upr<0]=0
	IC.pred.modle$lwr[IC.pred.modle$lwr<0]=0
	}
  if(toInteger) {
	pred.modle=round(pred.modle,0)
	}
	
  res = as.vector(residuals(modle))
  rss = sum(res^2)
  r2 = 1 - (rss/tss)
                                        #k = n.par
  es.R2 = r2
  attr(y, "product") = names(product)
  ic.delta = mean(IC.pred.modle$upr - IC.pred.modle$lwr)
  maxJump = getMaxJump (product, period.freq, pred.modle)
  VarCoeff= getVarCoeff(product,pred.modle) 
  # # m=matrix(NA,period.freq, ceiling((length(pred.modle))/period.freq)+1)
  # # m[1:(length(pred.modle)+period.freq)]=c(y[nrow(y):(nrow(y)-period.freq+1),],pred.modle)
  # # m=apply(m,2,mean,na.rm=TRUE)
  # # sdJumps = sd(m[-1]/m[-length(m)])
  lista.es = list(ts.product = y, model = modle, prediction = pred.modle, 
    IC = IC.pred.modle, AIC = es.AIC, R2 = es.R2, IC.width = ic.delta, maxJump=maxJump,  VarCoeff=VarCoeff, Residuals = res)
  lista.es
}



###############################################
mod.naive <- function(product, n.ahead, period.start, period.freq, period.end, logtransform, negTo0=negTo0,toInteger=toInteger,naive.values="last") {
  
  ##__##logger(DEBUG, "  function mod.naive")
  n = dim(product)[1]
  y = as.vector(product)
  
  if(is.null(naive.values)) naive.values="last"
  
  #print( dim(product))
  if(n==0){
	pred.modnaive=data.frame(pred = rep(ifelse(is.character(naive.values),0,naive.values),length=n.ahead))
	IC.pred.modnaive = list(upr = pred.modnaive, lwr = pred.modnaive)	
  } else {
	y = ts(y, start = period.start, frequency = period.freq)
	attr(y, "product") = names(product)
	if(is.character(naive.values)){
		if(naive.values=="last" || length(y)<period.freq){
			pred = y[length(y)]
		} else if(naive.values=="lastPeriod"){
			pred = y[(length(y)-period.freq)+(1:period.freq)] ## se length(y)<period.freq il risultato perde senso
		}   
	} else pred = naive.values 
  
	pred=data.frame(pred = rep(pred,length=n.ahead))
	if (logtransform) {
		pred.modnaive = exp(pred)
	} else {
		pred.modnaive = pred
	}
	IC.pred.modnaive = list(upr = pred.modnaive, lwr = pred.modnaive)
	  
	if(negTo0) {
		pred.modnaive[pred.modnaive<0]=0
		IC.pred.modnaive$upr[IC.pred.modnaive$upr<0]=0
		IC.pred.modnaive$lwr[IC.pred.modnaive$lwr<0]=0
	}
	if(toInteger) {
		pred.modnaive=round(pred.modnaive,0)
	}
  }
  
  pred.modnaive=ts(pred.modnaive, start=.incSampleTime(now=period.end, period.freq = period.freq) , frequency=period.freq)
  naive.AIC = Inf
  naive.R2 = NA
  ic.delta = Inf
  maxJump = Inf
  VarCoeff= Inf
  # m=matrix(NA,period.freq, ceiling((dim(pred.modnaive)[1])/period.freq)+1)
  # m[1:(length(pred.modnaive)+period.freq)]=c(y[nrow(y):(nrow(y)-period.freq+1),],pred.modnaive)
  # m=apply(m,2,mean,na.rm=TRUE)
  # sdJumps = sd(m[-1]/m[-length(m)])
  res = y-pred.modnaive
  lista.naive = list(ts.product = y, model = naive.values, prediction = pred.modnaive, 
    IC = IC.pred.modnaive, AIC = naive.AIC, R2 = naive.R2, IC.width = ic.delta, maxJump=maxJump,  VarCoeff=VarCoeff, Residuals = res)
 lista.naive
}

########################################



## funzione che calcola in modo automatico il numero delle differenze per l'arima
IDdiff = function(y,period.freq){
  diff.trend = 0
  diff.sea = 0
  v = rep(NA,6)
  v[1] = var(y)
  v[2] = var(diff(y,period.freq))
  v[3] = var(diff(y))
  v[4] = var(diff(diff(y,period.freq)))
  v[5] = var(diff(diff(y,period.freq),period.freq))
  v[6] = var(diff(diff(y)))
  d_sea = c(0,1,0,1,2,0)
  d_trend = c(0,0,1,1,0,2)
  d = cbind(v,d_sea,d_trend)
  if (sum(is.na(v))==0){  
    index = which.min(v)
    diff.trend = d[index,3]
    diff.sea = d[index,2]}
  
  c(diff.trend,diff.sea)
}

## funzione che valuta l'uso del log
## ATTENZIONE possibili valori negativi per le previsioni ed IC
IDlog = function(product,period.start){
  y = as.vector(product)
  y = ts(y, start = period.start, frequency = period.freq)
  mod = esId(y, keep = 1)
  mod = mod[which(mod$rankAIC == 1), ]
  if (mod$sea=="m" | mod$sea=="c/m") logtransform = TRUE
  else logtransform = FALSE
  logtransform
} 
