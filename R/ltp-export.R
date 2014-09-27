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
## created: 2012

library(XML)
library(googleVis)
library(reshape)

.dfToXML <- function(df,name) {
  xml <- xmlTree("ltp")
  xml$addNode(name, close=FALSE)
  for (i in 1:nrow(df)) {
    xml$addNode("row", close=FALSE)
    for (j in names(df)) {
         xml$addNode(j, df[i, j])
       }
    xml$closeTag()
  }
  xml$closeTag()
  return(xml)
}

.dfToXMLfile <- function(df,name, file=NULL) {
  if (is.null(file)) {
    file=sprintf("%s.xml", name)
  }
  tr <- dfToXML(df,name)
  cat(saveXML(tr$value(), file=file))
}


modelToListOfDF <- function(model, period.freq, param, id=1) {
  model.df <- list()
  model.df$BestModel <- model@BestModel
  model.df$models <- sort(names(model@models))

  if ( nrow(model@values) > 0) {
    normalized.periods <- rownames(model@values)
    normalized.data <- data.frame(PERIOD=normalized.periods, V=model@values$V)
    model.df$normalized.data <- normalized.data
  }

  summary <- ltp.BuildOneRowSummary(id=id, model=model, param=param)
  model.df$summary <- summary

  if (!is.null(model@BestModel)) {
    summary.models <- data.frame(ltp.GetModelsComparisonTable(model))
    summary.models = cbind(model=rownames(summary.models), summary.models)
    model.df$summary.models <- summary.models
  }

  if (!is.null(model@BestModel)) {
    all.results <- NULL
    all.residuals <- NULL

    tot.hist.points <- length(normalized.periods)
    n.ahead <- param$n.ahead
    period.end <- Period.FromString(tail(normalized.periods, 1))
    predicted.periods <-Period.BuildRange(period.start=period.end,
                                          period.freq=period.freq,
                                          n.ahead, shift=1)

    for (m in names(model@models)) {
      predictions <- as.vector(model@models[[m]]$prediction)
      residuals <- as.vector(model@models[[m]]$Residuals)

      if (length(predictions) == n.ahead) {
        model.results <- data.frame(
           model=rep(m, n.ahead),
           PERIOD=predicted.periods,
           V=predictions)
        all.results <- rbind(all.results, model.results)
      }

      if (length(residuals) == tot.hist.points) {
        model.residuals <- data.frame(
           model=rep(m, tot.hist.points),
           PERIOD=normalized.periods,
           V=trunc(residuals, 1))
        all.residuals <- rbind(all.residuals, model.residuals)
      }
    } ## end for model


    if (!is.null(all.results)) {
      colnames(all.results) <- c("model", "PERIOD", "V")
      model.df$results <- all.results
    }

    if (!is.null(all.residuals)) {
      colnames(all.residuals) <- c("model", "PERIOD", "V")
      model.df$residuals <- all.residuals
    }


  } ## end if
  return(model.df)
}

modelDFtoHtml5 <- function(model.df, img.options=list(width=850, height=500, legend="bottom", gvis.editor="Editor")) {
  body <- ""
  
  ## ##########################################################################
  ## RESULTS
  ## ##########################################################################
  results.pivot <- cast(model.df$results, PERIOD ~ model, df=TRUE, value="V")
  results.pivot <- data.frame(results.pivot)

  AM <- gvisLineChart(data=results.pivot, options=img.options)
  b_AM <- paste(capture.output(cat(AM$html$chart)), collapse="\n")

  ## merging hiostorical + predicted data
  nrow <- nrow(model.df$normalized.data)
  repeated.models <- unlist(lapply(model.df$models, function(x) rep(x,nrow)))
  i.hist <- data.frame(model=repeated.models, model.df$normalized.data)
  full.results <- rbind(i.hist, model.df$results)
  full.results.best <- subset(full.results, model==model.df$BestModel, select=-model)
  
  full.results.pivot <- cast(full.results, PERIOD ~ model, df=TRUE, value="V")
  full.results.pivot <- data.frame(full.results.pivot)

  ## best model data grouped by freq
  df <- full.results.best
  p = sapply(as.character(df$PERIOD), function(x) y=unlist(strsplit(x, "-")))
  df$PERIOD1 <- p[1,]
  df$PERIOD2 <- p[2,]
  full.results.best2.pivot <- as.data.frame(cast(df, PERIOD2 ~ PERIOD1, df=T, value="V"))
  full.results.best2.pivot <- as.data.frame(cbind(month=rownames(full.results.best2.pivot), full.results.best2.pivot))
  
  BM.full <- gvisLineChart(data=full.results.best, options=img.options)
  BM2.full <- gvisColumnChart(data=full.results.best2.pivot, options=img.options)
  AM.full <- gvisLineChart(data=full.results.pivot, options=img.options)
  
  b_BM.full<- paste(capture.output(cat(BM.full$html$chart)), collapse="\n")
  b_BM2.full<- paste(capture.output(cat(BM2.full$html$chart)), collapse="\n")

  b_AM.full<- paste(capture.output(cat(AM.full$html$chart)), collapse="\n")
  b_TR.full <- paste(capture.output(print(xtable(full.results.pivot), type="html")), collapse="\n")
  ##TR <- gvisTable(item.results.pivot, options=list(width=700, height=500))
  ##b_TR <- paste(capture.output(cat(TR$html$chart)), collapse="\n")

  b_SM <- paste(capture.output(print(xtable(model.df$summary.models), type="html")), collapse="\n")

  body <- paste(body, sprintf("<h1>Best Model %s</h1>", model.df$BestModel), b_BM.full, b_BM2.full, sep="\n")
  body <- paste(body, "<h1>All Models</h1>", b_AM.full, b_SM, b_AM, b_TR.full, sep="<br />\n")
  
  ## ##########################################################################
  ## SUMMARY
  ## ##########################################################################
  model.df$summary$Parameters <- NULL
  b_S <- paste(capture.output(print(xtable(t(model.df$summary)), type="html")), collapse="\n")
  body <- paste(body, "<h1>Summary</h1>", b_S, sep="\n")

  ## ##########################################################################
  ## RESIDUALS
  ## ##########################################################################
  residuals.pivot <- cast(model.df$residuals, PERIOD ~ model, df=TRUE, value="V")
  residuals.pivot <- data.frame(residuals.pivot)
  RES <- gvisLineChart(data=residuals.pivot, options=img.options)
  b_RES <- paste(capture.output(cat(RES$html$chart)), collapse="\n")
  body <- paste(body, "<h1>Residuals</h1>", b_RES, sep="\n")
  
  body
}

modelToHtml5 <- function(model, period.freq, param, id=1, img.options=list(width=850, height=500, legend="bottom", gvis.editor="Editor")) {
  model.df <- modelToListOfDF(model=model, period.freq=period.freq, param=param, id=1)
  modelDFtoHtml5(model.df, img.options=img.options)
}
