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

.plot.best = function(best, plot.trend =TRUE, color.ic, 
  color.forecast, title,filename="modelBest.png", width , height ) {
  
    png(units="px",filename, width = width, height = height)#, type="cairo")
	ltp.plot.best(best, plot.trend =TRUE, color.ic, color.forecast, title)
    dev.off()
}

ltp.plot.best = function(best, plot.trend =TRUE, color.ic, color.forecast, title) {
  
  y = best$ts.product
  period.freq = frequency(y)
  end_serie = end(y)
  period.start = start(y)
  start_pred = .incSampleTime(period.freq = period.freq, now = end_serie)
  
  pred = best$prediction
  ic.lwr = best$IC$lwr
  ic.upr = best$IC$upr
  
  if (!is.ts(pred)) pred = ts(pred,start=start_pred,frequency=period.freq)
  if (!is.ts(ic.lwr)) ic.lwr = ts(ic.lwr,start=start_pred,frequency=period.freq)
  if (!is.ts(ic.upr)) ic.upr = ts(ic.upr,start=start_pred,frequency=period.freq)
  
  p.best = append(as.vector(window(y,end=end_serie)),pred[1])
  p.best = ts(p.best,start=period.start,frequency=period.freq)
  
  y.best = append(as.vector(window(y,end=end_serie)),pred)
  y.best = ts(y.best,start=period.start,frequency=period.freq)
  
  inf = min(ic.lwr,y,na.rm = TRUE)
  sup = max(ic.upr,y,na.rm = TRUE)

  plot(window(p.best, end = start_pred), ylim = c((inf - (inf/4)), 
                                           (sup + (sup/2))), xlim = c(period.start[1], end(pred)[1]), 
       main = title,ylab="y")
  lines(y.best, col = color.forecast[1], pch = "*", lwd = 2)
  lines(ic.lwr, col = color.ic, lwd = 1)
  lines(ic.upr, col = color.ic, lwd = 1)
  points(ic.upr, col = color.ic, cex = 1, pch = "*")
  points(ic.lwr, col = color.ic, cex = 1, pch = "*")
  lines(y, pch = "*", lwd = 2)
  legend(x = period.start[1], y = (sup + (sup/2)), legend = c("Prediction", 
                                                     "Confidence band 95%"), col = c(color.forecast[1], color.ic), 
         lty = 1, lwd = 2, horiz = FALSE, x.intersp = 1)
  
  if (plot.trend) {
    trend.best = try(smooth.spline(y.best),TRUE)
    if(!is(trend.best,"try-error"))  lines(trend.best, col = color.forecast[1], lwd = 1)
  }
}


######################################
#.plot.all(res,height=400,width=600,title="prova",color.forecast=ltp.GetModels("color"))


.plot.all <- function(model, color.forecast, plot.trend = TRUE, title = "",filename="modelAll.png",width = width, height = height,choosenModels=NULL) { 
  png(units="px",filename, width, height )#, type="cairo")
  ltp.plot.all(model, color.forecast, plot.trend = TRUE, title = "", choosenModels)
 dev.off()
 }
 
ltp.plot.all <- function(model, color.forecast, plot.trend = TRUE, title = "",choosenModels=NULL) {
  if(!is.null(choosenModels)) model@models = model@models[choosenModels]
  
  y = model@models[[1]]$ts.product
  period.freq = frequency(y)
  end_serie = end(y)
  period.start = start(y)
  start_pred = .incSampleTime(period.freq = period.freq, now = end_serie)
    
  pred=lapply(as.vector(ltp.GetModels("name")),function(name) model@models[[name]]$prediction)
  pred = lapply(pred, function(pr){ if (!is.null(pr))  if (!is.ts(pr)) pr = ts(pr, start = start_pred, frequency = period.freq); pr})
  #concateno la prima prediction
  p = lapply( pred, function(pr) {pr=append(as.vector(window(y, end = end_serie)), pr); 
                                  pr= ts(pr, start = period.start, frequency = period.freq)})
  names(p)=ltp.GetModels("name")


  yy=list()
  for(i in which(sapply(p,function(yyy)!is.null(yyy) ))){
    if(!is.null(p[i])){ 
      yy[[i]]=append(as.vector(window(y, end = end_serie)), p[[i]])
      yy[[i]] = ts(yy[[i]], start = period.start, frequency = period.freq)
    }
  }

  inf = min(unlist(pred)[is.finite(unlist(pred))], y,na.rm = TRUE)
  sup = max(unlist(pred)[is.finite(unlist(pred))], y,na.rm = TRUE)
  plot(y, ylim = c((inf - (inf/4)), (sup + (sup/2))), xlim = c(period.start[1], end(p[[model@BestModel]])[1]), 
       lwd = 2, main = title,ylab="y")
 for(i in names(p)[which(sapply(pred,function(pp)!is.null(pp) ))]){
    lines(window(p[[i]], start = end_serie) , col = color.forecast[i], pch = "*", cex = 2, lwd = 2)
  }
  
  legend(x = period.start[1], y = (sup + (sup/2)), legend = ltp.GetModels("legend")[which(sapply(pred,function(pp)!is.null(pp) ))], 
         col = color.forecast[names(color.forecast)[which(sapply(pred,function(pp)!is.null(pp) ))]], lty = 1, lwd = 2, horiz = FALSE, x.intersp = 1)
  if (plot.trend) {
    for(i in which(sapply(pred,function(yyy)!is.null(yyy) ))){
      trend = try(smooth.spline(yy[[i]]),TRUE)
      if(!is(trend,"try-error")) lines(trend, pch = "*", col = color.forecast[i], lwd = 1)
    }
  } 
}
  

## best e' la il risultato fornito da ltp una lista che contiene il model migliore
plot.ltp = function(model, plot.try.models = c("best", 
                             "all"), color.forecast = NULL, color.ic = "red", 
  plot.trend = FALSE, title = "Time Series", filename,width , height ,choosenModels=NULL) {
  
  if(is.null(color.forecast)) {
	color.forecast=as.vector(ltp.GetModels("color"))
	names(color.forecast)=as.vector(ltp.GetModels("name"))
	}
  
  for (i in plot.try.models) {
    if (i == "all") 
      .plot.all(model = model, color.forecast = color.forecast, 
                plot.trend = plot.trend, title = title,filename=filename, width = width, height = height,choosenModels=choosenModels)
    if (i == "best") 
      .plot.best(best = model@models[[model@BestModel]], color.ic = color.ic, 
                 plot.trend = plot.trend, color.forecast = color.forecast[model@BestModel], 
                 title = title, filename = filename,width = width, height = height)
  }
}

PlotLtpResults <- function(obj, directory=NULL, width=600, height=400,choosenModels=NULL) {
  plot.ltp(obj, plot.try.models = c("best"), color.forecast = NULL, color.ic = "orange", plot.trend = FALSE, title = obj@BestModel ,filename=file.path(directory, "best_model.png"),width = width, height = height)
        
  ## plot ALL models
  plot.ltp(obj, plot.try.models = c("all"), color.forecast = NULL, color.ic = "orange", plot.trend = FALSE, title = "All Models" ,filename=file.path(directory, "all_models.png"),width = width, height = height,choosenModels=choosenModels)
}



.plot.resid <- function(obj,modType,filename,width=600, height=400){
	residPlot = paste("resid_", modType,".png", sep = "")
    png(units="px",filename, width = width, height = height)#, type="cairo")
	ltp.plot.resid (obj,modType)
	dev.off()
}

ltp.plot.resid <- function(obj,modType){
		plot(obj@models[[modType]]$Residuals, type = "p", col="blue", main = paste("Residuals of ", modType, sep = ""),ylab="Residuals")
		abline(0, 0, col="red")
}
