\name{ltp.GetModelsComparisonTable}
\alias{ltp.GetModelsComparisonTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ltp.GetModelsComparisonTable
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ltp.GetModelsComparisonTable(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (obj) 
{
    col.names <- c("formula", "R2", "AIC", "IC.width", "maxJump", 
        "VarCoeff")
    if (is.null(obj@BestModel)) {
        ReporTable <- cbind(rep(NA, length(col.names)))
        colnames(ReporTable) <- col.names
        rownames(ReporTable) <- "None"
        return(ReporTable)
    }
    ReporTable = matrix("--", 6, 6)
    model.names <- ltp.GetModels("name")
    colnames(ReporTable) <- col.names
    rownames(ReporTable) <- model.names
    indicator.list <- c("R2", "AIC", "IC.width", "maxJump", "VarCoeff")
    if (!is.null(obj@models$ExpSmooth)) {
        terms = sapply(c("drift", "seasonality"), function(compon) {
            if (obj@models$ExpSmooth$model[compon] == "none") 
                return()
            compon
        })
        terms = terms[!sapply(terms, is.null)]
        es.string = paste("level", sep = "+", paste(terms, collapse = ifelse(length(grep("multiplicative", 
            obj@models$ExpSmooth$model["seasonality"]) > 0), 
            "*", "+")))
    }
    ReporTable[, 1] <- c(ifelse(is.null(obj@models$Linear), "--", 
        gsub("~", "=", gsub("stima$qta", "y", as.character(obj@models$Linear$model$call[2]), 
            fixed = TRUE))), ifelse(is.null(obj@models$Arima), 
        "--", ifelse(length(obj@models$Arima$model$coef) == 0, 
            "-constant-", paste(obj@models$Arima$model$series, 
                "=", paste(names(obj@models$Arima$model$coef), 
                  collapse = "+"), sep = ""))), ifelse(is.null(obj@models$ExpSmooth), 
        "--", es.string), ifelse(is.null(obj@models$Trend), "--", 
        paste("y=", paste(attributes(obj@models$Trend$model$call[[2]])$term.labels, 
            collapse = "+"), sep = "")), ifelse(is.null(obj@models$Mean), 
        "--", paste("y=", paste(attributes(obj@models$Mean$model$call[[2]])$term.labels, 
            collapse = "+"), sep = "")), ifelse(is.null(obj@models$Naive), 
        "--", "Fixed Values"))
    temp <- rbind(unlist(obj@models$Linear[indicator.list]), 
        unlist(obj@models$Arima[indicator.list]), unlist(obj@models$ExpSmooth[indicator.list]), 
        unlist(obj@models$Trend[indicator.list]), unlist(obj@models$Mean[indicator.list]), 
        unlist(obj@models$Naive[indicator.list]))
    colnames(temp) = indicator.list
    temp[, "R2"] = round(temp[, "R2"], 4)
    temp[, "AIC"] = round(temp[, "AIC"], 2)
    temp[, "IC.width"] = round(temp[, "IC.width"], 0)
    temp[, "maxJump"] = round(temp[, "maxJump"], 3)
    temp[, "VarCoeff"] = round(temp[, "VarCoeff"], 3)
    ReporTable[which(!(ReporTable[, 1] == "--")), indicator.list] = as.matrix(temp)
    ReporTable = as.data.frame(ReporTable)
    ReporTable = ReporTable[which(!(ReporTable[, 1] == "--")), 
        ]
    ReporTable
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
