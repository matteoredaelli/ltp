\name{mod.arima}
\alias{mod.arima}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mod.arima
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mod.arima(product, logtransform, diff.sea, diff.trend, idDiff, max.p, max.q, max.P, max.Q, n.ahead, period.freq, xreg.arima, period.start, stepwise, stationary.arima, negTo0 = negTo0, toInteger = toInteger)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{product}{
%%     ~~Describe \code{product} here~~
}
  \item{logtransform}{
%%     ~~Describe \code{logtransform} here~~
}
  \item{diff.sea}{
%%     ~~Describe \code{diff.sea} here~~
}
  \item{diff.trend}{
%%     ~~Describe \code{diff.trend} here~~
}
  \item{idDiff}{
%%     ~~Describe \code{idDiff} here~~
}
  \item{max.p}{
%%     ~~Describe \code{max.p} here~~
}
  \item{max.q}{
%%     ~~Describe \code{max.q} here~~
}
  \item{max.P}{
%%     ~~Describe \code{max.P} here~~
}
  \item{max.Q}{
%%     ~~Describe \code{max.Q} here~~
}
  \item{n.ahead}{
%%     ~~Describe \code{n.ahead} here~~
}
  \item{period.freq}{
%%     ~~Describe \code{period.freq} here~~
}
  \item{xreg.arima}{
%%     ~~Describe \code{xreg.arima} here~~
}
  \item{period.start}{
%%     ~~Describe \code{period.start} here~~
}
  \item{stepwise}{
%%     ~~Describe \code{stepwise} here~~
}
  \item{stationary.arima}{
%%     ~~Describe \code{stationary.arima} here~~
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
function (product, logtransform, diff.sea, diff.trend, idDiff, 
    max.p, max.q, max.P, max.Q, n.ahead, period.freq, xreg.arima, 
    period.start, stepwise, stationary.arima, negTo0 = negTo0, 
    toInteger = toInteger) 
{
    y = as.vector(product)
    n = max(length(y), nrow(y))
    y = ts(y, start = period.start, frequency = period.freq)
    if (idDiff) {
        d = IDdiff(y, period.freq = period.freq)
        diff.trend = as.integer(d[1])
        diff.sea = as.integer(d[2])
    }
    if (logtransform) {
        if (stepwise) 
            if (is.null(xreg.arima)) {
                mod = try(arimaId(log(y), c(max.p, diff.trend, 
                  max.q), list(order = c(max.P, diff.sea, max.Q)), 
                  idDiff = FALSE, method = "ML", verbose = FALSE), 
                  TRUE)
                if (is(mod, "try-error")) {
                  mod.arima = auto.arima(log(y), d = diff.trend, 
                    D = diff.sea, max.p = max.p, max.q = max.q, 
                    stationary = stationary.arima, max.Q = max.Q, 
                    max.P = max.P, ic = "aic", xreg = xreg.arima, 
                    stepwise = stepwise)
                }
                else {
                  index = print.allArima(mod, nshow = 1)
                  mod.arima = try(arima(log(y), c(as.integer(index["p"]), 
                    as.integer(index["d"]), as.integer(index["q"])), 
                    c(as.integer(index["P"]), as.integer(index["D"]), 
                      as.integer(index["Q"])), method = "ML"), 
                    TRUE)
                  if (is(mod.arima, "try-error")) {
                    mod.arima = auto.arima(log(y), d = diff.trend, 
                      D = diff.sea, max.p = max.p, max.q = max.q, 
                      stationary = stationary.arima, max.Q = max.Q, 
                      max.P = max.P, ic = "aic", xreg = xreg.arima, 
                      stepwise = stepwise)
                  }
                }
            }
            else mod.arima = auto.arima(log(y), d = diff.trend, 
                D = diff.sea, max.p = max.p, max.q = max.q, stationary = stationary.arima, 
                max.Q = max.Q, max.P = max.P, ic = "aic", xreg = xreg.arima, 
                stepwise = stepwise)
        else {
            mod.arima = arima(log(y), order = c(max.p, diff.trend, 
                max.q), seasonal = list(order = c(max.P, diff.sea, 
                max.Q)), xreg = xreg.arima)
        }
        arima.AIC = mod.arima$aic
        pred = predict(mod.arima, n.ahead)
        pred.arima = pred$pred
        ic_arima = pred.arima + qnorm(0.975) * cbind(-pred$se, 
            pred$se)
        colnames(ic_arima) = c("lwr", "upr")
        IC.pred.arima = list(lwr = exp(ic_arima[, "lwr"]), upr = exp(ic_arima[, 
            "upr"]))
        pred.arima = exp(pred.arima)
        tss = var(log(y)) * (n - 1)
        res = residuals(mod.arima)
    }
    else {
        if (stepwise) 
            if (is.null(xreg.arima)) {
                mod = try(arimaId(y, c(max.p, diff.trend, max.q), 
                  list(order = c(max.P, diff.sea, max.Q)), idDiff = FALSE, 
                  method = "ML", verbose = FALSE), TRUE)
                if (is(mod, "try-error")) {
                  mod.arima = auto.arima(y, d = diff.trend, D = diff.sea, 
                    max.p = max.p, max.q = max.q, stationary = stationary.arima, 
                    max.Q = max.Q, max.P = max.P, ic = "aic", 
                    xreg = xreg.arima, stepwise = stepwise)
                }
                else {
                  index = print.allArima(mod, nshow = 1)
                  mod.arima = try(arima(y, c(as.integer(index["p"]), 
                    as.integer(index["d"]), as.integer(index["q"])), 
                    c(as.integer(index["P"]), as.integer(index["D"]), 
                      as.integer(index["Q"])), method = "ML"), 
                    TRUE)
                  if (is(mod.arima, "try-error")) {
                    mod.arima = auto.arima(y, d = diff.trend, 
                      D = diff.sea, max.p = max.p, max.q = max.q, 
                      stationary = stationary.arima, max.Q = max.Q, 
                      max.P = max.P, ic = "aic", xreg = xreg.arima, 
                      stepwise = stepwise)
                  }
                }
            }
            else mod.arima = auto.arima(y, d = diff.trend, D = diff.sea, 
                max.p = max.p, max.q = max.q, stationary = stationary.arima, 
                max.Q = max.Q, max.P = max.P, ic = "aic", xreg = xreg.arima, 
                stepwise = stepwise)
        else {
            mod.arima = arima(y, order = c(max.p, diff.trend, 
                max.q), seasonal = list(order = c(max.P, diff.sea, 
                max.Q)), xreg = xreg.arima)
        }
        arima.AIC = mod.arima$aic
        pred = predict(mod.arima, n.ahead)
        pred.arima = pred$pred
        ic_arima = pred.arima + qnorm(0.975) * cbind(-pred$se, 
            pred$se)
        colnames(ic_arima) = c("lwr", "upr")
        IC.pred.arima = list(lwr = ic_arima[, "lwr"], upr = ic_arima[, 
            "upr"])
        tss = var(y) * (n - 1)
        res = residuals(mod.arima)
    }
    if (negTo0) {
        pred.arima[pred.arima < 0] = 0
        IC.pred.arima$upr[IC.pred.arima$upr < 0] = 0
        IC.pred.arima$lwr[IC.pred.arima$lwr < 0] = 0
    }
    if (toInteger) {
        pred.arima = round(pred.arima, 0)
    }
    rss = sum(res^2)
    r2 = 1 - (rss/tss)
    arima.R2 = r2
    attr(y, "product") = names(product)
    ic.delta = mean(IC.pred.arima$upr - IC.pred.arima$lwr)
    maxJump = getMaxJump(product, period.freq, pred.arima)
    VarCoeff = getVarCoeff(product, pred.arima)
    lista.arima = list(ts.product = y, model = mod.arima, prediction = pred.arima, 
        IC = IC.pred.arima, AIC = arima.AIC, R2 = arima.R2, IC.width = ic.delta, 
        maxJump = maxJump, VarCoeff = VarCoeff, Residuals = res)
    lista.arima
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
