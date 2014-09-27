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

###################################################
#Crea tabella report

ltp.GetModelsComparisonTable <-  function(obj) {
  
  col.names <- c("formula", "R2","AIC","IC.width","maxJump","VarCoeff")

  if (is.null(obj@BestModel)) {
    ReporTable <- cbind( rep(NA, length(col.names)))
    colnames(ReporTable) <- col.names
    rownames(ReporTable) <- "None"
    return (ReporTable)
  }
  
  ReporTable = matrix("--",6,6)

  model.names <- ltp.GetModels("name")
  #model.names <- model.names[! model.names == "Naive"]
  
  colnames(ReporTable) <- col.names
  rownames(ReporTable) <- model.names #c("Linear Model","Arima", "Exponential Smooth","Trend","Mean")

  indicator.list <- c("R2","AIC", "IC.width","maxJump","VarCoeff")
  
  if(!is.null(obj@models$ExpSmooth)) {
    terms=sapply(c("drift","seasonality"),
      function(compon){ if(obj@models$ExpSmooth$model[compon]=="none") return() 
                        compon})
    terms=terms[!sapply(terms,is.null)] 
    
    es.string=paste( "level",sep="+", paste(terms,collapse=ifelse(length(grep("multiplicative",obj@models$ExpSmooth$model["seasonality"])>0),"*","+")))
  }
  ## TODO: pay attention: the list of models is important... maybe it is better to use explicit coordinates ReporTable["Arima"][1] ..
  ReporTable[,1] <- 
    c(ifelse(is.null(obj@models$Linear),"--", gsub("~","=",gsub("stima$qta","y",as.character(obj@models$Linear$model$call[2]),fixed=TRUE))),
      ##paste("Y=",paste(attributes(obj@models$Linear$model$call[[2]])$term.labels,collapse="+"),sep="")), 
      ifelse(is.null(obj@models$Arima),"--",
             ifelse(length(obj@models$Arima$model$coef)==0,
                    "-constant-",
                    paste(obj@models$Arima$model$series,"=", paste(names(obj@models$Arima$model$coef), collapse = "+"),sep=""))), 
      ifelse(is.null(obj@models$ExpSmooth),"--", es.string ),
      ifelse(is.null(obj@models$Trend),"--",paste("y=",paste(attributes(obj@models$Trend$model$call[[2]])$term.labels,collapse="+"),sep="")),
      ifelse(is.null(obj@models$Mean),"--",paste("y=",paste(attributes(obj@models$Mean$model$call[[2]])$term.labels,collapse="+"),sep="")),
      ifelse(is.null(obj@models$Naive),"--","Fixed Values")   )
  
  temp <- rbind(unlist(obj@models$Linear[indicator.list]), unlist(obj@models$Arima[indicator.list]), 
    unlist(obj@models$ExpSmooth[indicator.list]),unlist(obj@models$Trend[indicator.list]),unlist(obj@models$Mean[indicator.list]),unlist(obj@models$Naive[indicator.list]))
  colnames(temp)= indicator.list
  temp[,"R2"]=round(temp[,"R2"],4)	
  temp[,"AIC"]=round(temp[,"AIC"],2)
  temp[,"IC.width"]=round(temp[,"IC.width"],0)
  temp[,"maxJump"]=round(temp[,"maxJump"],3)
  temp[,"VarCoeff"]=round(temp[,"VarCoeff"],3)

  ReporTable[which(!(ReporTable[,1]=="--")),indicator.list] = as.matrix(temp)
  ReporTable=as.data.frame(ReporTable)
  ReporTable = ReporTable[which(!(ReporTable[,1]=="--")),]
  ReporTable
}

###########################
## crea report sintetico


ltp.BuildOneRowSummary <- function(id=1, model, param) {
  models.names <- ltp.GetModels("name")
  no.values <- nrow(model@values)
  
  return.code <- 0

  if (is.null(model@BestModel))
    return.code <- 1

  if (no.values == 0)
    return.code <- 2
  
  stats=as.list(rep(NA,12))
  names(stats)=c(
         "BestModel", "SuggestedModel2",
         "Points","NotZeroPoints",
         "LastNotEqualValues","MeanPredicted",
         "MeanValues","MeanPredictedRatioMeanValues",
         "SdPredictedRatioSdValues","BestAICNoOutRangeExclude",
         "BestICNoOutRangeExclude","Timestamp"
         )
  ##stats["id"] <- id
  ##mean values (ie observed data)
  stats["MeanValues"]=mean(model@values$V,na.rm=TRUE)
  ##nunb of points (observations)
 

  stats["Points"] <- no.values
  ##non zero values
  stats["NotZeroPoints"]=ifelse(no.values==0,0, sum(model@values$V!=0))
  stats["BestModel"] = ifelse(is.null(model@BestModel), NA, model@BestModel)
  stats["SuggestedModel"] =  stats["BestModel"]
  stats["SuggestedModel2"] = ifelse(is.null(model@BestModel2), NA, model@BestModel2)
  stats["Timestamp"] = Sys.time()
  stats["TotModels"] = length(param$try.models)
  stats["Parameters"] = Param.ToString(param)
  stats["ReturnCode"] = return.code
  stats["Run"] = 0

  if (!is.null(model@BestModel) & (stats["MeanValues"] != 0) ) {
    ##stats[c("R2","AIC","maxJump","VarCoeff")]=round(unlist(model[[model@BestModel]][c("R2","AIC","maxJump","VarCoeff")]),4)
    ##stats["ICwidth"] = round(model[[model@BestModel]][["IC.width"]],0)

    ##find (che cum sum of) not equal (ie constant) consecutive values
    temp=cumsum((model@values[-1,]-model@values[-no.values,])==0)
    ##length of last not-constant consecutives serie of values
    stats["LastNotEqualValues"]=sum(temp==max(temp))-1
		
    ##mean predicted
    stats["MeanPredicted"]=mean(model@models[[model@BestModel]]$prediction,na.rm=T)
    ##mean predicted over mean values (ie observed data)
    stats["MeanPredictedRatioMeanValues"]=stats[["MeanPredicted"]]/stats[["MeanValues"]]
    ##and rounding
    stats[c("MeanPredicted","MeanValues","MeanPredictedRatioMeanValues")]=lapply(stats[c("MeanPredicted","MeanValues","MeanPredictedRatioMeanValues")],round,3)
    ##sd predicted over sd values (ie observed data)
    stats["SdPredictedRatioSdValues"]=round(sd((model@models[[model@BestModel]]$prediction),na.rm=T)/sd((model@values$V)),3)
		
    ##Best Model if not exclusion rule were performed
    st=names(which.min(unlist(lapply(model@models[models.names],function(x) x$AIC))))
    stats["BestAICNoOutRangeExclude"]=ifelse(is.null(st),"None",st)
    st=names(which.min(unlist(lapply(model@models[models.names],function(x) x$IC.width))))
    stats["BestICNoOutRangeExclude"]=ifelse(is.null(st),"None",st)
    ##note: stat is changed from numeric to string
  }

  
  ##clean out the (possible) Inf values
  stats= lapply(stats,function(x) ifelse(is.numeric(x) & (!is.finite(x)), NA,x))	
  summ=data.frame(stats)
  rownames(summ) <- c(id)
  summ
}


#####################

## # crea report html
ltp.HTMLreport <- function(obj, id, value, value.description, directory=".", width=1000 * 0.6, height=600 * 0.6) {
  
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  
  html.filename = file.path(directory, "report.html")
  
  title = paste("Strategico: Long Term Prediction for ID=", id, " - ", value.description, sep = " ")

  ReporTable <- ltp.GetModelsComparisonTable(obj)
 
    
  text = paste("<html>\n<head>\n<title>", title, "</title>\n</html>\n<body>\n<h1>", 
    title, "</h1><a href=\"http://code.google.com/p/strategico/wiki/LTP\"/>Quick Help</a>",

    "<h2>Best Model</h2>Criterion:",obj@rule,", and Max CV smaller than  ",obj@ruleSetting["rule.noMaxCVOver"], ", Max Jump smaller than  ",obj@ruleSetting["rule.noMaxJumpOver"],",<br><img src=\"best_model.png\" />\n<h2>All Models </h2>\n<img src=\"all_models.png\" />\n",

  
 hwrite(ReporTable), sep = "")
  cat(text, append = FALSE, file = html.filename)

  

  #html.form.eval = GetStrHTMLformItem.Eval(project.path, .Item.GetPath(keys), value, param)
  #cat(html.form.eval, append = TRUE, file = html.filename)  

  ## TODO Using ltp.GetModels("name")
  notNA <- sapply(ltp.GetModels("name"), 
                  function(i) if(!is.null(obj@models[[i]])) ( !is.null(obj@models[[i]]$Residuals))&(!any(is.na(obj@models[[i]]$Residuals))) else FALSE )

    for (modType in setdiff(ltp.GetModels("name")[notNA],"Naive")) {
	residPlot = paste("resid_", modType,".png", sep = "")
	.plot.resid(obj,modType,filename=file.path(directory, residPlot), width = width, height = height)
	
    text = paste("\n<h3> Model: ", modType, "</h3>\n<img src=\"", residPlot, "\" />", sep = "")
    cat(text, append = TRUE, file = html.filename)
    HTML(file = html.filename, report(obj@models[[modType]]$model,obj@models[[modType]]))
  }

  text = "<h2>Recorded and Predicted Data</h2>"
  cat(text, append = TRUE, file = html.filename)
  
  y = obj@values
  names(y)="Historical values"
  #HTML(file = html.filename, y,digits=12)
  cat(hwrite(y), append = TRUE, file = html.filename) 
  
  if(!is.null(obj@BestModel)){ 	 
    pred = as.data.frame(obj@models[[obj@BestModel]]$prediction,ncol=1)
    period.freq = frequency(obj@models[[obj@BestModel]]$ts.product)
    end_serie = end(obj@models[[obj@BestModel]]$ts.product)
    ## TODO end_serie is wrong! do not correspond with period.start
    ##cat(end_serie)
    pred.names <- Period.BuildRange(end_serie, period.freq, nrow(pred), shift=0) 
    ##pred.names = sapply(1:nrow(pred),function(x) paste(.incSampleTime(period.freq = period.freq, now = end_serie,increment =x),collapse="-"))
    rownames(pred)=pred.names
    colnames(pred)="Predicted values"

    ##HTML(file = html.filename, pred,digits=12)
    cat(hwrite(pred), append = TRUE, file = html.filename) 
  }

  cat("</body> </html>", append = TRUE, file = html.filename)  

}

"report" <- function(model,list, ...) {
  UseMethod("report")
}

"report.lm" <- function(model,list, ...) { 
  summary(model)
}


"report.Arima" <- function(model,list, ...) {
  AIC = list$AIC
  R2 = list$R2
  Coefficients = cbind(Estimate = model$coef, Std.Error=matrix(sqrt(diag(model$var.coef))))
  colnames(Coefficients) = c("Estimate", "Std. Error")
  if (nrow(Coefficients)==0) Coefficients=rbind(Coefficients,NA)
  rr=t(as.matrix(summary(model$residuals)))
  list(Call = model$call, residuals = list("Residuals",summary(model$residuals)), 
       Coefficients = list("Coefficients",Coefficients), AIC = paste("AIC:",round(AIC, 4), sep = " "), 
       R2 = paste("R2:",R2, sep = " "), 
       Residuals = paste("Residuals standard error:", round(sqrt(model$sigma2), 4), sep = " "))
}


"report.expSmoothingFit" <- function(model,list, ...) {
  ## ritornare una list eventualmente con table come elementi simile a quella per Arima
  equations = .eq.es(model) 
  list(Drift= paste("type Trend:",model$drift, sep = " "),
       Seasonality=paste("type Seasonality:",model$seasonality, sep = " "),
       ## Innovation=paste("Innovation:",model$innovation, sep = " "),
       alpha=paste("Parameter Smoothing Level:",round(model$par[1],4), sep = " "),
       beta=paste("Parameter Smoothing Seasonality:",round(model$par[4],4), sep = " "),
       gamma=paste("Parameter Smoothing Trend:",round(model$par[3],4), sep = " "),
       R2=paste("R2:",round(list$R2,4), sep = " "),AIC=paste("AIC:",round(list$AIC,3), sep = " "),
       paste("Residuals standard error:",round(sqrt(model$sigma2), 2), sep = " "),
       list(paste("Recursive equations:",sep = " "),
			paste(equations[1]),paste(equations[2]),paste(equations[3]))
		)
  
  
  
  ## se la stagionalita' moltiplicativa ed è presente trend:
  ## y(t+1) = level(t) + trend(t)*stagionalità(t)
  
  ## dove 
  ## level(t) = alpha * y(t) + (1-alpha)*(level(t-1) + trend(t-1))
  ## trend(t) = beta * (level(t) - level(t-1)) + (1-beta) * trend(t-1)
  ## stag.(t) = gamma * ()
  
  ## se la stagionalità è adittiva ed è presente trend:
  
  ## y(t+1) = level + trend + stagionalità
  
  ## alpha(parametro del livello) è il parametro che modella la memoria del mio modello
  ## infatti alpha determina il peso dato alle osservazioni
  ## un alpha prossimo a 1 mi dara peso solo alle ultime osservazioni
  ## un alpha molto basso dara peso significante anche a osservazioni passate lontane dall'ultima osservata
  ## per avere un idea:
  ## k = 1:10
  ## par(mfrow=c(2,1))
  ## plot((1-0.3330807)^k,type="l")
  ## plot((1-0.9)^k,type="l")
  
}




.eq.es = function(model) {
 model$par[c(1,3,4)] =round(model$par[c(1,3,4)],4)
  trend = model$drift
  sea = model$seasonality
  if ((trend=="additive") | (trend=="c/additive") | (trend=="d/additive"))  {trend="additive"}
    else { if ((trend=="multiplicative") | (trend=="c/multiplicative")) trend="multiplicative"} 

  if ((sea=="additive") | (sea=="c/additive"))  {sea="additive"}
    else { if ((sea=="multiplicative") | (sea=="c/multiplicative")) sea="multiplicative"} 
  
  if ((trend=="none")&(sea=="none")) {
     eq1 = paste("level(t)=","(1-",model$par[1],") * level(t-1) + ",model$par[1]," * y(t)",sep="")
     eq2 = "Trend none"
     eq3 = "Seasonality none" }
    else if ((trend=="additive")&(sea=="none")) {
              eq1 = paste("level(t)=","(1-",model$par[1],") * (level(t-1) + drift(t-1)) + ",model$par[1]," * y(t)",sep="")
              eq2 = paste("drift(t)=","(1-",model$par[3],") * drift(t-1) + ",model$par[3]," * (level(t) - level(t-1))",sep="")
              eq3 = "Seasonality none"  }
            else if ((trend=="multiplicative")&(sea=="none")) {
                      eq1 = paste("level(t)=","(1-",model$par[1],") * level(t-1) * drift(t-1) + ",model$par[1]," * y(t)",sep="")
                      eq2 = paste("drift(t)=","(1-",model$par[3],") * drift(t-1) + ",model$par[3]," * (level(t) / level(t-1))",sep="")
                      eq3 = "Seasonality none"  }
                    else if ((trend=="none")&(sea=="additive")) {
                              eq1 = paste("level(t)=","(1-",model$par[1],") * level(t-1) + ",model$par[1]," * (y(t) - sea(t-f))",sep="")
                              eq2 = "Trend none"
                              eq3 = paste("sea(t)=","(1-",model$par[4],") * sea(t-f) + ",model$par[4]," * (y(t) - level(t-1))",sep="")  }
                            else if ((trend=="none")&(sea=="multiplicative")) {
                                      eq1 = paste("level(t)=","(1-",model$par[1],") * level(t-1) + ",model$par[1]," * (y(t) / sea(t-f))",sep="")
                                      eq2 = "Trend none"
                                      eq3 = paste("sea(t)=","(1-",model$par[4],") * sea(t-f) + ",model$par[4]," * (y(t) / level(t-1))",sep="")  }                                   
            
            
  if ((trend=="additive")&(sea=="additive")) {
     eq1 = paste("level(t)=","(1-",model$par[1],") * ((level(t-1) + drift(t-1)) + ",model$par[1]," * (y(t) - sea(t-f))",sep="")
     eq2 = paste("drift(t)=","(1-",model$par[3],") * drift(t-1) + ",model$par[3]," * (level(t) - level(t-1))",sep="")
     eq3 = paste("sea(t)=","(1-",model$par[4],") * sea(t-f) + ",model$par[4]," * (y(t) - level(t-1) - drift(t-1))",sep="") }
  
  if ((trend=="additive")&(sea=="multiplicative")) {
     eq1 = paste("level(t)=","(1-",model$par[1],") * ((level(t-1) + drift(t-1)) + ",model$par[1]," * (y(t)/sea(t-f))",sep="")
     eq2 = paste("drift(t)=","(1-",model$par[3],") * drift(t-1) + ",model$par[3]," * (level(t) - level(t-1))",sep="")
     eq3 = paste("sea(t)=","(1-",model$par[4],") * sea(t-f) + ",model$par[4]," * (y(t) / (level(t-1) - drift(t-1)) )",sep="") }  
     
  if ((sea=="additive")&(trend=="multiplicative")) {
     eq1 = paste("level(t)=","(1-",model$par[1],") * level(t-1) * drift(t-1) + ",model$par[1]," * (y(t) - sea(t-f))",sep="")
     eq2 = paste("drift(t)=","(1-",model$par[3],") * drift(t-1) + ",model$par[3]," * (level(t) / level(t-1))",sep="")
     eq3 = paste("sea(t)=","(1-",model$par[4],") * sea(t-f) + ",model$par[4]," * (y(t) - level(t-1) - drift(t-1))",sep="") }

  if ((trend=="multiplicative")&(sea=="multiplicative")) {
     eq1 = paste("level(t)=","(1-",model$par[1],") * level(t-1) * drift(t-1) + ",model$par[1]," * (y(t) / sea(t-f))",sep="")
     eq2 = paste("drift(t)=","(1-",model$par[3],") * drift(t-1) + ",model$par[3]," * (level(t) / level(t-1))",sep="")
     eq3 = paste("sea(t)=","(1-",model$par[4],") * sea(t-f) + ",model$par[4]," * (y(t) / (level(t-1) - drift(t-1)) )",sep="") }
 c(eq1,eq2,eq3)
 }

 
