\name{ltp.plot.best}
\alias{ltp.plot.best}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ltp.plot.best}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ltp.plot.best(best, plot.trend = TRUE, color.ic, color.forecast, title)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{best}{
%%     ~~Describe \code{best} here~~
}
  \item{plot.trend}{
%%     ~~Describe \code{plot.trend} here~~
}
  \item{color.ic}{
%%     ~~Describe \code{color.ic} here~~
}
  \item{color.forecast}{
%%     ~~Describe \code{color.forecast} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
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
function (best, plot.trend = TRUE, color.ic, color.forecast, 
    title) 
{
    y = best$ts.product
    period.freq = frequency(y)
    end_serie = end(y)
    period.start = start(y)
    start_pred = .incSampleTime(period.freq = period.freq, now = end_serie)
    pred = best$prediction
    ic.lwr = best$IC$lwr
    ic.upr = best$IC$upr
    if (!is.ts(pred)) 
        pred = ts(pred, start = start_pred, frequency = period.freq)
    if (!is.ts(ic.lwr)) 
        ic.lwr = ts(ic.lwr, start = start_pred, frequency = period.freq)
    if (!is.ts(ic.upr)) 
        ic.upr = ts(ic.upr, start = start_pred, frequency = period.freq)
    p.best = append(as.vector(window(y, end = end_serie)), pred[1])
    p.best = ts(p.best, start = period.start, frequency = period.freq)
    y.best = append(as.vector(window(y, end = end_serie)), pred)
    y.best = ts(y.best, start = period.start, frequency = period.freq)
    inf = min(ic.lwr, y, na.rm = TRUE)
    sup = max(ic.upr, y, na.rm = TRUE)
    plot(window(p.best, end = start_pred), ylim = c((inf - (inf/4)), 
        (sup + (sup/2))), xlim = c(period.start[1], end(pred)[1]), 
        main = title, ylab = "y")
    lines(y.best, col = color.forecast[1], pch = "*", lwd = 2)
    lines(ic.lwr, col = color.ic, lwd = 1)
    lines(ic.upr, col = color.ic, lwd = 1)
    points(ic.upr, col = color.ic, cex = 1, pch = "*")
    points(ic.lwr, col = color.ic, cex = 1, pch = "*")
    lines(y, pch = "*", lwd = 2)
    legend(x = period.start[1], y = (sup + (sup/2)), legend = c("Prediction", 
        "Confidence band 95\%"), col = c(color.forecast[1], color.ic), 
        lty = 1, lwd = 2, horiz = FALSE, x.intersp = 1)
    if (plot.trend) {
        trend.best = try(smooth.spline(y.best), TRUE)
        if (!is(trend.best, "try-error")) 
            lines(trend.best, col = color.forecast[1], lwd = 1)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
