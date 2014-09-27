\name{mod.es}
\alias{mod.es}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mod.es
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mod.es(product, n.ahead, period.start, period.freq, n, logtransform.es, stepwise, negTo0 = negTo0, toInteger = toInteger)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{product}{
%%     ~~Describe \code{product} here~~
}
  \item{n.ahead}{
%%     ~~Describe \code{n.ahead} here~~
}
  \item{period.start}{
%%     ~~Describe \code{period.start} here~~
}
  \item{period.freq}{
%%     ~~Describe \code{period.freq} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{logtransform.es}{
%%     ~~Describe \code{logtransform.es} here~~
}
  \item{stepwise}{
%%     ~~Describe \code{stepwise} here~~
}
  \item{negTo0}{
%%     ~~Describe \code{negTo0} here~~
}
  \item{toInteger}{
%%     ~~Describe \code{toInteger} here~~
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
function (product, n.ahead, period.start, period.freq, n, logtransform.es, 
    stepwise, negTo0 = negTo0, toInteger = toInteger) 
{
    product[product == 0] = 1
    y = as.vector(product)
    n = max(length(y), nrow(y))
    y = ts(y, start = period.start, frequency = period.freq)
    if (logtransform.es) {
        mod = esId(log(y), keep = 1)
        mod = mod[which(mod$rankAIC == 1), ]
        modle = esFit(log(y), mod$drift, mod$sea, mod$inn)
        pred = try(predict(modle, n.ahead, method = "resample"), 
            TRUE)
        if (is(pred, "try-error")) {
            return(list(ts.product = y, model = modle, prediction = NA, 
                IC = NA, AIC = NA, R2 = NA, IC.width = NA, VarCoeff = NA, 
                maxJump = NA, Residuals = NA))
        }
        else {
            n.par = mod$np
            es.AIC = modle$loglik + 2 * n.par
            pred.modle = exp(unlist(pred[, c("mean"), drop = FALSE][, 
                1]))
            pred.modle[abs(pred.modle) == Inf] = NA
            pred[abs(pred[, "97.5\%"]) == Inf, "97.5\%"] = NA
            pred[abs(pred[, "2.5\%"]) == Inf, "2.5\%"] = NA
            IC.pred.modle = list(lwr = exp(pred[, "2.5\%"]), upr = exp(pred[, 
                "97.5\%"]))
            tss = var(y) * (n - 1)
        }
    }
    else {
        mod = esId(y, keep = 1)
        mod = mod[which(mod$rankAIC == 1), ]
        modle = esFit(y, mod$drift, mod$sea, mod$inn)
        pred = try(predict(modle, n.ahead, method = "resample"), 
            TRUE)
        if (is(pred, "try-error")) {
            return(list(ts.product = y, model = modle, prediction = NA, 
                IC = NA, AIC = NA, R2 = NA, IC.width = NA, VarCoeff = NA, 
                maxJump = NA, Residuals = NA))
        }
        else {
            n.par = mod$np
            es.AIC = modle$loglik + 2 * n.par
            pred.modle = unlist(pred[, c("mean"), drop = FALSE][, 
                1])
            pred.modle[abs(pred.modle) == Inf] = NA
            pred[abs(pred[, "97.5\%"]) == Inf, "97.5\%"] = NA
            pred[abs(pred[, "2.5\%"]) == Inf, "2.5\%"] = NA
            IC.pred.modle = list(lwr = pred[, "2.5\%"], upr = pred[, 
                "97.5\%"])
            tss = var(y) * (n - 1)
        }
    }
    if (negTo0) {
        pred.modle[pred.modle < 0] = 0
        IC.pred.modle$upr[IC.pred.modle$upr < 0] = 0
        IC.pred.modle$lwr[IC.pred.modle$lwr < 0] = 0
    }
    if (toInteger) {
        pred.modle = round(pred.modle, 0)
    }
    res = residuals(modle)
    rss = sum(res^2)
    r2 = 1 - (rss/tss)
    es.R2 = r2
    attr(y, "product") = names(product)
    ic.delta = mean(IC.pred.modle$upr - IC.pred.modle$lwr)
    maxJump = getMaxJump(product, period.freq, pred.modle)
    VarCoeff = getVarCoeff(product, pred.modle)
    lista.es = list(ts.product = y, model = modle, prediction = pred.modle, 
        IC = IC.pred.modle, AIC = es.AIC, R2 = es.R2, IC.width = ic.delta, 
        maxJump = maxJump, VarCoeff = VarCoeff, Residuals = res)
    lista.es
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
