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

#################UTILITIES

##################################
 ltp.GetModels <- function(what=NULL) {
	model=data.frame(id=c("lm", "arima","es", "trend", "mean","naive"),
			name=c("Linear", "Arima", "ExpSmooth", "Trend", "Mean","Naive"),
			color = c("green", "red", "blue", "gray", "black","yellow"),
			legend= c("Linear","Arima" , "Exp.Smooth" , "Trend" ,"Mean", "Naive"))
	rownames(model)=model$id
	
	if(!is.null(what)) { 
		names=model$id
		model=as.character(model[,what])
		names(model)=names
		}
  
  model
}
#es: ltp.GetModels("es")

#########################

getMaxJump <- function(product, period.freq, pred.mod)  {
  res=exp(abs(log(
    mean(product[max(1,(nrow(product)-period.freq+1)):nrow(product),1],na.rm=TRUE)/
	mean(pred.mod[1:min(period.freq,nrow(product))],na.rm=TRUE))
	))
  if(is.null(res)) {
	res = NA 
  } else {
	if(length(res)!=1) res=1
	if(any(is.na(res))) res=1
  }
  res
}

getVarCoeff <- function(product, pred.mod){
   sd(c(as.vector(pred.mod),unlist(product)),na.rm=TRUE)/mean(c(as.vector(pred.mod),unlist(product)),na.rm=TRUE)
}

getModelsStatistics <- function(object,whichStats=c("AIC","IC.width","R2","VarCoeff","maxJump")){
	res=c()
	for (i in names(object))
		res=rbind(res,object[[i]][whichStats])
	rownames(res)=names(object)
	return(res)
}

getRetainedModelsByRule <- function(models,ruleSetting){
	retain=(getModelsStatistics(models,"VarCoeff")<ruleSetting$rule.noMaxCVOver)&
	       (getModelsStatistics(models,"maxJump") <ruleSetting$rule.noMaxJumpOver)
	return(retain)	
}


getBestModel <- function(models,rule,ruleSetting){
	stats=getModelsStatistics(models,switch(rule, BestIC="IC.width",BestAIC="AIC"))
	stats=unlist(stats)
	names(stats)=names(models)
	retain=unlist(getRetainedModelsByRule (models,ruleSetting))
	ID.model <- names(which.min(stats[retain]))

  if((is.null(ID.model)||(length(ID.model)==0))||is.na(ID.model))  ID.model <-  "Naive"
  ID.model
  }

Param.ToString <- function(param, collapse="; ") {
  param <- lapply(param,function(p){if((length(p)==1)&(is.character(p))) p=paste("'",p,"'",sep="") else p })
  param <- param[names(param)!=""]
  gsub(" ","",gsub("\"","'",paste(names(param),param,sep="=",collapse=collapse)))
}

Param.FromString <- function(param.string) {
  if (is.character(param.string)) {
    param.string <- gsub(";",",", param.string)
    param <- eval(parse(text=paste("list(",param.string,")")))
  } else {
    param <- list()
  }
  param
}
