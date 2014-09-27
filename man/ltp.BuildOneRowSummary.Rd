\name{ltp.BuildOneRowSummary}
\alias{ltp.BuildOneRowSummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ltp.BuildOneRowSummary
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ltp.BuildOneRowSummary(id, model, param)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{
%%     ~~Describe \code{id} here~~
}
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{param}{
%%     ~~Describe \code{param} here~~
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
function (id, model, param) 
{
    models.names <- ltp.GetModels("name")
    no.values <- nrow(model@values)
    return.code <- 0
    if (is.null(model@BestModel)) 
        return.code <- 1
    if (no.values == 0) 
        return.code <- 2
    stats = as.list(rep(NA, 11))
    names(stats) = c("BestModel", "Points", "NotZeroPoints", 
        "LastNotEqualValues", "MeanPredicted", "MeanValues", 
        "MeanPredictedRatioMeanValues", "SdPredictedRatioSdValues", 
        "BestAICNoOutRangeExclude", "BestICNoOutRangeExclude", 
        "Timestamp")
    stats["MeanValues"] = mean(model@values$V, na.rm = TRUE)
    stats["Points"] <- no.values
    stats["NotZeroPoints"] = ifelse(no.values == 0, 0, sum(model@values$V != 
        0))
    stats["BestModel"] = ifelse(is.null(model@BestModel), NA, 
        model@BestModel)
    stats["SuggestedModel"] = stats["BestModel"]
    stats["Timestamp"] = Sys.time()
    stats["TotModels"] = length(param$try.models)
    stats["Parameters"] = Param.ToString(param)
    stats["ReturnCode"] = return.code
    stats["Run"] = 0
    if (!is.null(model@BestModel) & (stats["MeanValues"] != 0)) {
        temp = cumsum((model@values[-1, ] - model@values[-no.values, 
            ]) == 0)
        stats["LastNotEqualValues"] = sum(temp == max(temp)) - 
            1
        stats["MeanPredicted"] = mean(model@models[[model@BestModel]]$prediction, 
            na.rm = T)
        stats["MeanPredictedRatioMeanValues"] = stats[["MeanPredicted"]]/stats[["MeanValues"]]
        stats[c("MeanPredicted", "MeanValues", "MeanPredictedRatioMeanValues")] = lapply(stats[c("MeanPredicted", 
            "MeanValues", "MeanPredictedRatioMeanValues")], round, 
            3)
        stats["SdPredictedRatioSdValues"] = round(sd(model@models[[model@BestModel]]$prediction, 
            na.rm = T)/sd(model@values$V), 3)
        st = names(which.min(unlist(lapply(model@models[models.names], 
            function(x) x$AIC))))
        stats["BestAICNoOutRangeExclude"] = ifelse(is.null(st), 
            "None", st)
        st = names(which.min(unlist(lapply(model@models[models.names], 
            function(x) x$IC.width))))
        stats["BestICNoOutRangeExclude"] = ifelse(is.null(st), 
            "None", st)
    }
    stats = lapply(stats, function(x) ifelse(is.numeric(x) & 
        (!is.finite(x)), NA, x))
    summ = data.frame(stats)
    rownames(summ) <- c(id)
    summ
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
