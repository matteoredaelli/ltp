\name{mod.lm}
\alias{mod.lm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mod.lm
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mod.lm(product, n.ahead, period.start, period.freq, xreg.lm = NA, logtransform, stepwise, formula.right.lm = NULL, negTo0 = negTo0, toInteger = toInteger)
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
  \item{xreg.lm}{
%%     ~~Describe \code{xreg.lm} here~~
}
  \item{logtransform}{
%%     ~~Describe \code{logtransform} here~~
}
  \item{stepwise}{
%%     ~~Describe \code{stepwise} here~~
}
  \item{formula.right.lm}{
%%     ~~Describe \code{formula.right.lm} here~~
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
function (product, n.ahead, period.start, period.freq, xreg.lm = NA, 
    logtransform, stepwise, formula.right.lm = NULL, negTo0 = negTo0, 
    toInteger = toInteger) 
{
    if (is.null(formula.right.lm)) 
        formula.right.lm = match.arg(formula.right.lm, " S * trend + S * trend2")
    y = as.vector(product)
    n = max(length(y), nrow(y))
    y = ts(y, start = period.start, frequency = period.freq)
    attr(y, "product") = names(product)
    k = n + n.ahead
    trend = 1:k
    S = as.factor(((period.start[2] - 1 + (0:(k - 1)))\%\%period.freq) + 
        1)
    d = data.frame(tempo = rep(NA, k), qta = rep(NA, k), trend = trend, 
        trend2 = trend^2, S = S, xreg.lm = xreg.lm)
    form = as.formula(paste(ifelse(logtransform, "log(stima$qta)", 
        "stima$qta"), " ~ ", formula.right.lm, sep = ""))
    d$tempo[1:n] = rownames(product)
    d$qta[1:n] = unlist(product)
    stima = d[(1:n), ]
    new.data = d[-(1:n), ]
    if (stepwise) 
        steps = 100
    else steps = 0
    modlm = step(lm(form, data = stima), steps = steps, direction = "both", 
        trace = 0)
    pred = predict(modlm, newdata = new.data, interval = "prediction", 
        level = 0.95)
    if (logtransform) {
        pred.modlm = exp(pred[, "fit", drop = FALSE])
        IC.pred.modlm = list(upr = exp(pred[, "upr"]), lwr = exp(pred[, 
            "lwr"]))
    }
    else {
        pred.modlm = (pred[, "fit", drop = FALSE])
        IC.pred.modlm = list(upr = pred[, "upr"], lwr = pred[, 
            "lwr"])
    }
    if (negTo0) {
        pred.modlm[pred.modlm < 0] = 0
        IC.pred.modlm$upr[IC.pred.modlm$upr < 0] = 0
        IC.pred.modlm$lwr[IC.pred.modlm$lwr < 0] = 0
    }
    if (toInteger) {
        pred.modlm = round(pred.modlm, 0)
    }
    pred.modlm = ts(pred.modlm, start = .incSampleTime(now = end(y), 
        period.freq = period.freq), frequency = period.freq)
    lm.AIC = AIC(modlm, k = 2)
    lm.R2 = summary(modlm)$r.squared
    ic.delta = mean(IC.pred.modlm$upr - IC.pred.modlm$lwr)
    maxJump = getMaxJump(product, period.freq, pred.modlm)
    VarCoeff = getVarCoeff(product, pred.modlm)
    res = ts(residuals(modlm), start = period.start, frequency = period.freq)
    lista.lm = list(ts.product = y, model = modlm, prediction = pred.modlm, 
        IC = IC.pred.modlm, AIC = lm.AIC, R2 = lm.R2, IC.width = ic.delta, 
        maxJump = maxJump, VarCoeff = VarCoeff, Residuals = res)
    lista.lm
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
