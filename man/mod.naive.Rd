\name{mod.naive}
\alias{mod.naive}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mod.naive
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mod.naive(product, n.ahead, period.start, period.freq, period.end, logtransform, negTo0 = negTo0, toInteger = toInteger, naive.values = "last")
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
  \item{period.end}{
%%     ~~Describe \code{period.end} here~~
}
  \item{logtransform}{
%%     ~~Describe \code{logtransform} here~~
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
function (product, n.ahead, period.start, period.freq, period.end, 
    logtransform, negTo0 = negTo0, toInteger = toInteger, naive.values = "last") 
{
    n = dim(product)[1]
    y = as.vector(product)
    if (is.null(naive.values)) 
        naive.values = "last"
    if (n == 0) {
        pred.modnaive = data.frame(pred = rep(ifelse(is.character(naive.values), 
            0, naive.values), length = n.ahead))
        IC.pred.modnaive = list(upr = pred.modnaive, lwr = pred.modnaive)
    }
    else {
        y = ts(y, start = period.start, frequency = period.freq)
        attr(y, "product") = names(product)
        if (is.character(naive.values)) {
            if (naive.values == "last") {
                pred = y[length(y)]
            }
            else if (naive.values == "lastPeriod") {
                pred = y[(length(y) - period.freq) + (1:period.freq)]
            }
        }
        else pred = naive.values
        pred = data.frame(pred = rep(pred, length = n.ahead))
        if (logtransform) {
            pred.modnaive = exp(pred)
        }
        else {
            pred.modnaive = pred
        }
        IC.pred.modnaive = list(upr = pred.modnaive, lwr = pred.modnaive)
        if (negTo0) {
            pred.modnaive[pred.modnaive < 0] = 0
            IC.pred.modnaive$upr[IC.pred.modnaive$upr < 0] = 0
            IC.pred.modnaive$lwr[IC.pred.modnaive$lwr < 0] = 0
        }
        if (toInteger) {
            pred.modnaive = round(pred.modnaive, 0)
        }
    }
    pred.modnaive = ts(pred.modnaive, start = .incSampleTime(now = period.end, 
        period.freq = period.freq), frequency = period.freq)
    naive.AIC = Inf
    naive.R2 = NA
    ic.delta = Inf
    maxJump = Inf
    VarCoeff = Inf
    res = y
    lista.naive = list(ts.product = y, model = naive.values, 
        prediction = pred.modnaive, IC = IC.pred.modnaive, AIC = naive.AIC, 
        R2 = naive.R2, IC.width = ic.delta, maxJump = maxJump, 
        VarCoeff = VarCoeff, Residuals = res)
    lista.naive
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
