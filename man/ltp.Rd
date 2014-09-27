\name{ltp}
\alias{ltp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ltp
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ltp(product, try.models, rule = "BestAIC", ruleSetting = list(rule.noMaxCVOver = Inf, rule.noMaxJumpOver = Inf), n.ahead = 4, logtransform = TRUE, logtransform.es = FALSE, period.freq = 2, increment = 1, xreg.lm = NA, diff.sea = 1, diff.trend = 1, max.p = 2, max.q = 1, max.P = 1, max.Q = 0, xreg.arima = NULL, idDiff = FALSE, idLog = FALSE, stationary.arima = FALSE, period.start = c(1997, 1), period.end = c(2010, 1), NA2value = 3, range = c(3, Inf), n.min = 15, stepwise = TRUE, formula.right.lm = NULL, negTo0 = TRUE, toInteger = TRUE, naive.values = "last")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{product}{
%%     ~~Describe \code{product} here~~
}
  \item{try.models}{
%%     ~~Describe \code{try.models} here~~
}
  \item{rule}{
%%     ~~Describe \code{rule} here~~
}
  \item{ruleSetting}{
%%     ~~Describe \code{ruleSetting} here~~
}
  \item{n.ahead}{
%%     ~~Describe \code{n.ahead} here~~
}
  \item{logtransform}{
%%     ~~Describe \code{logtransform} here~~
}
  \item{logtransform.es}{
%%     ~~Describe \code{logtransform.es} here~~
}
  \item{period.freq}{
%%     ~~Describe \code{period.freq} here~~
}
  \item{increment}{
%%     ~~Describe \code{increment} here~~
}
  \item{xreg.lm}{
%%     ~~Describe \code{xreg.lm} here~~
}
  \item{diff.sea}{
%%     ~~Describe \code{diff.sea} here~~
}
  \item{diff.trend}{
%%     ~~Describe \code{diff.trend} here~~
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
  \item{xreg.arima}{
%%     ~~Describe \code{xreg.arima} here~~
}
  \item{idDiff}{
%%     ~~Describe \code{idDiff} here~~
}
  \item{idLog}{
%%     ~~Describe \code{idLog} here~~
}
  \item{stationary.arima}{
%%     ~~Describe \code{stationary.arima} here~~
}
  \item{period.start}{
%%     ~~Describe \code{period.start} here~~
}
  \item{period.end}{
%%     ~~Describe \code{period.end} here~~
}
  \item{NA2value}{
%%     ~~Describe \code{NA2value} here~~
}
  \item{range}{
%%     ~~Describe \code{range} here~~
}
  \item{n.min}{
%%     ~~Describe \code{n.min} here~~
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
  \item{naive.values}{
%%     ~~Describe \code{naive.values} here~~
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
function (product, try.models, rule = "BestAIC", ruleSetting = list(rule.noMaxCVOver = Inf, 
    rule.noMaxJumpOver = Inf), n.ahead = 4, logtransform = TRUE, 
    logtransform.es = FALSE, period.freq = 2, increment = 1, 
    xreg.lm = NA, diff.sea = 1, diff.trend = 1, max.p = 2, max.q = 1, 
    max.P = 1, max.Q = 0, xreg.arima = NULL, idDiff = FALSE, 
    idLog = FALSE, stationary.arima = FALSE, period.start = c(1997, 
        1), period.end = c(2010, 1), NA2value = 3, range = c(3, 
        Inf), n.min = 15, stepwise = TRUE, formula.right.lm = NULL, 
    negTo0 = TRUE, toInteger = TRUE, naive.values = "last") 
{
    result.normalize <- ltp.normalizeData(product = product, 
        range = range, NA2value = NA2value, period.start = period.start, 
        increment = increment, period.end = period.end, period.freq = period.freq)
    period.start = result.normalize$start
    product = result.normalize$product
    n = nrow(product)
    if (idLog) 
        logtransform = IDlog(product, period.start)
    if (is.null(try.models)) 
        try.models = ltp.GetModels("id")
    models = list()
    if (("naive" \%in\% try.models)) {
        models$Naive = mod.naive(product = product, n.ahead = n.ahead, 
            period.start = period.start, period.freq = period.freq, 
            period.end = period.end, logtransform = FALSE, negTo0 = negTo0, 
            toInteger = toInteger, naive.values = naive.values)
    }
    if (("mean" \%in\% try.models) & (n >= period.freq)) {
        models$Mean = mod.lm(product = product, n.ahead = n.ahead, 
            period.start = period.start, period.freq = period.freq, 
            xreg.lm = NA, logtransform = FALSE, stepwise = FALSE, 
            formula.right.lm = "S", negTo0 = negTo0, toInteger = toInteger)
    }
    if (("trend" \%in\% try.models) & (n > 2 * period.freq)) {
        models$Trend = mod.lm(product = product, n.ahead = n.ahead, 
            period.start = period.start, period.freq = period.freq, 
            xreg.lm = NA, logtransform = FALSE, stepwise = FALSE, 
            formula.right.lm = "S+trend", negTo0 = negTo0, toInteger = toInteger)
    }
    if (("lm" \%in\% try.models) & (n >= max(n.min, period.freq * 
        3))) {
        models$Linear = mod.lm(product = product, n.ahead = n.ahead, 
            period.start = period.start, period.freq = period.freq, 
            xreg.lm = xreg.lm, logtransform = logtransform, stepwise = stepwise, 
            formula.right.lm = formula.right.lm, negTo0 = negTo0, 
            toInteger = toInteger)
    }
    if (("es" \%in\% try.models) & (n >= max(n.min, period.freq * 
        3))) {
        models$ExpSmooth = mod.es(product = product, n.ahead = n.ahead, 
            period.freq = period.freq, period.start = period.start, 
            logtransform.es = logtransform.es, stepwise = stepwise, 
            negTo0 = negTo0, toInteger = toInteger)
    }
    if (("arima" \%in\% try.models) & (n >= max(n.min, period.freq * 
        3))) {
        models$Arima = mod.arima(product = product, logtransform = logtransform, 
            diff.sea = diff.sea, diff.trend = diff.trend, idDiff = idDiff, 
            max.p = max.p, max.q = max.q, max.P = max.P, max.Q = max.Q, 
            stationary.arima = stationary.arima, n.ahead = n.ahead, 
            period.freq = period.freq, xreg.arima = xreg.arima, 
            period.start = period.start, stepwise = stepwise, 
            negTo0 = negTo0, toInteger = toInteger)
    }
    ID.model = getBestModel(models, rule, ruleSetting)
    out <- new("ltp.object")
    out@values = product
    out@models = models
    out@BestModel = ID.model
    out@rule = rule
    out@ruleSetting = ruleSetting
    out
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
