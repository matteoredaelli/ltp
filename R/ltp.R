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
  
############## ltp()

ltp <- function(product, try.models=c('mean','trend','lm','es','arima','naive'), rule = "BestAIC", ruleSetting=list(rule.noMaxCVOver = Inf,rule.noMaxJumpOver = Inf), n.ahead = 4, logtransform = TRUE,logtransform.es=FALSE, 
                period.freq=NULL,increment=1, xreg.lm = NA,diff.sea=1,diff.trend=1,max.p=2,max.q=1,max.P=1,max.Q=0, 
                xreg.arima = NULL,idDiff=FALSE,idLog=FALSE, stationary.arima = FALSE, period.start = NULL,
                period.end=NULL, 
				NA2value = 3, range = c(3, Inf), n.min = 15, 
				stepwise = TRUE, formula.right.lm = "S*trend+S*trend2", negTo0 = TRUE, toInteger = TRUE,
				naive.values="last", naive.ifConstantLastValues=5, naive.ifConstant0LastValues=5) {

  ## filling period.freq if missing
   if (is.null(period.freq)) 
     period.freq <- max(sapply(strsplit(rownames(product), "-"), function(x) as.integer(x[2])))

  ## !!!!!!! 
  ## some limitations when period.fre == 1
  if (period.freq == 1) {
    try.models <- setdiff(try.models, "arima")
    formula.right.lm="trend+trend2"
  }
   
  ## filling period.min if missing
   if (is.null(period.start)) 
     period.start <- Period.FromString(min(rownames(product)))

  ## filling period.mx if missing
   if (is.null(period.end)) 
     period.end <- Period.FromString(max(rownames(product)))

#####################################
  ## ATTENZIONE normalizedata: ho sistemato la mia versione a funziona
  ## l'importante secondo me e' che venga aggiornata anche il nuovo period.start
#####################################
  ##__##logger(DEBUG, "Original data:")
  ##__##logger(DEBUG, product)
  ##__##logger(DEBUG, rownames(product))
  ## result.normalize <- ltp.normalizeData(product, range, NA2value,period.end)
  result.normalize <- ltp.normalizeData(product=product,range=range,NA2value=NA2value,period.start=period.start,increment=increment,period.end=period.end,period.freq=period.freq)
  ##__##logger(DEBUG, "Normalized data:")
  ##__##logger(DEBUG, result.normalize)
  period.start = result.normalize$start
  product = result.normalize$product
  n = nrow(product)
  
  
  if(idLog) logtransform = IDlog(product,period.start)
  # CHecking if the latest values are constant...
  if(nrow(product)>0){
    tot.points <- nrow(product)
    last.value <- product[tot.points,]
###############OCCHIO QUI!!!
 # print(str(last.value))
    if(all(product[max(1,tot.points-naive.ifConstant0LastValues+1):max(1,tot.points),] == 0)){
      warning(paste("The latest", naive.ifConstant0LastValues, "historical data are constant (see param naive.ifConstant0LastValues): predictions will be constant (value=", 0, ") and naive model will be forced!"))
      try.models="naive"
      naive.values=0
    } else
    if(all(product[max(1,tot.points-naive.ifConstantLastValues+1):max(1,tot.points),] == last.value)){
      warning(paste("The latest", naive.ifConstantLastValues, "historical data are constant (see param naive.ifConstantLastValues): predictions will be constant (value=", last.value, ") and naive model will be forced!"))
      try.models="naive"
      naive.values=last.value
    }
  } else {try.models="naive";  naive.values=0}

  if (is.null(try.models)) 
    try.models = ltp.GetModels("id")


  models=list()
  if (("naive" %in% try.models)) {
    ##__##logger(DEBUG, "Evaluating model naive...")
    models$Naive = mod.naive(product = product, n.ahead = n.ahead, 
      period.start = period.start, period.freq = period.freq, period.end= period.end,
      logtransform = FALSE, negTo0=negTo0,toInteger=toInteger,naive.values=naive.values)
  }
  if (("mean" %in% try.models)&(n >= period.freq )) {
    ##__##logger(DEBUG, "Evaluating model mean...")
    models$Mean = mod.lm(product = product, n.ahead = n.ahead, 
      period.start = period.start, period.freq = period.freq, 
      xreg.lm = NA, logtransform = FALSE, 
      stepwise = FALSE, formula.right.lm = if(period.freq==1)'1' else 'S', negTo0=negTo0,toInteger=toInteger)
  }
  if (("trend" %in% try.models)&(n > 2*period.freq )) {
    ##__##logger(DEBUG, "Evaluating model trend...")
    models$Trend = mod.lm(product = product, n.ahead = n.ahead, 
      period.start = period.start, period.freq = period.freq, 
      xreg.lm = NA, logtransform = FALSE, 
      stepwise = FALSE, formula.right.lm = if(period.freq==1) 'trend' else 'S+trend', negTo0=negTo0,toInteger=toInteger)
  }
  if (("lm" %in% try.models)&((period.freq>1)&(n >= max(n.min,period.freq*3) ))) {
    ##__##logger(DEBUG, "Evaluating model lm...")
    models$Linear = mod.lm(product = product, n.ahead = n.ahead, 
      period.start = period.start, period.freq = period.freq, 
      xreg.lm = xreg.lm, logtransform = logtransform, 
      stepwise = stepwise, formula.right.lm = formula.right.lm, negTo0=negTo0,toInteger=toInteger)
  }
  if (("es" %in% try.models)&(n >= max(n.min,period.freq*3) )) {
    ##__##logger(DEBUG, "Evaluating model es...")
    models$ExpSmooth = mod.es(product = product, n.ahead = n.ahead, 
      period.freq = period.freq, period.start = period.start, 
      logtransform.es = logtransform.es, stepwise = stepwise, negTo0=negTo0,toInteger=toInteger)
  }
  if (("arima" %in% try.models)&(n >= max(n.min,period.freq*3) )) {
    ##__##logger(DEBUG, "Evaluating model arima...")
    models$Arima = mod.arima(product=product,logtransform=logtransform,
      diff.sea=diff.sea,diff.trend=diff.trend,idDiff=idDiff,max.p=max.p,max.q=max.q,
      max.P=max.P,max.Q=max.Q,stationary.arima=stationary.arima,n.ahead=n.ahead,
      period.freq=period.freq,xreg.arima=xreg.arima,period.start=period.start,stepwise=stepwise, negTo0=negTo0,toInteger=toInteger)
  }
 
  ID.model=getBestModel(models,rule,ruleSetting)
   
  ## calculating second best model
  if (ID.model == "Naive") {
    ID.model2 <- NULL
  } else {
    models2 <- models
    models2[[ID.model]] <- NULL
    ID.model2=getBestModel(models2,rule,ruleSetting)
  }
   
  out <- new("ltp.object")  
  out @ values = product
  out @ models=models
  out @ BestModel = ID.model
  out @ BestModel2 = ID.model2
  out @ rule=rule
  out @ ruleSetting=ruleSetting  
  ##__##logger(DEBUG, "Predicted data (BestModel):")
  ##__##logger(DEBUG, ID.model)
  ##__##logger(DEBUG, (results["Naive"]))
  out
}


