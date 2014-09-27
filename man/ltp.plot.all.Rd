\name{ltp.plot.all}
\alias{ltp.plot.all}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ltp.plot.all}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ltp.plot.all(model, color.forecast, plot.trend = TRUE, title = "", choosenModels = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{color.forecast}{
%%     ~~Describe \code{color.forecast} here~~
}
  \item{plot.trend}{
%%     ~~Describe \code{plot.trend} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{choosenModels}{
%%     ~~Describe \code{choosenModels} here~~
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
function (model, color.forecast, plot.trend = TRUE, title = "", 
    choosenModels = NULL) 
{
    if (!is.null(choosenModels)) 
        model@models = model@models[choosenModels]
    y = model@models[[1]]$ts.product
    period.freq = frequency(y)
    end_serie = end(y)
    period.start = start(y)
    start_pred = .incSampleTime(period.freq = period.freq, now = end_serie)
    pred = lapply(as.vector(ltp.GetModels("name")), function(name) model@models[[name]]$prediction)
    pred = lapply(pred, function(pr) {
        if (!is.null(pr)) 
            if (!is.ts(pr)) 
                pr = ts(pr, start = start_pred, frequency = period.freq)
        pr
    })
    p = lapply(pred, function(pr) {
        pr = append(as.vector(window(y, end = end_serie)), pr)
        pr = ts(pr, start = period.start, frequency = period.freq)
    })
    names(p) = ltp.GetModels("name")
    yy = list()
    for (i in which(sapply(p, function(yyy) !is.null(yyy)))) {
        if (!is.null(p[i])) {
            yy[[i]] = append(as.vector(window(y, end = end_serie)), 
                p[[i]])
            yy[[i]] = ts(yy[[i]], start = period.start, frequency = period.freq)
        }
    }
    inf = min(unlist(pred)[is.finite(unlist(pred))], y, na.rm = TRUE)
    sup = max(unlist(pred)[is.finite(unlist(pred))], y, na.rm = TRUE)
    plot(y, ylim = c((inf - (inf/4)), (sup + (sup/2))), xlim = c(period.start[1], 
        end(p[[model@BestModel]])[1]), lwd = 2, main = title, 
        ylab = "y")
    for (i in names(p)[which(sapply(pred, function(pp) !is.null(pp)))]) {
        lines(window(p[[i]], start = end_serie), col = color.forecast[i], 
            pch = "*", cex = 2, lwd = 2)
    }
    legend(x = period.start[1], y = (sup + (sup/2)), legend = ltp.GetModels("legend")[which(sapply(pred, 
        function(pp) !is.null(pp)))], col = color.forecast[names(color.forecast)[which(sapply(pred, 
        function(pp) !is.null(pp)))]], lty = 1, lwd = 2, horiz = FALSE, 
        x.intersp = 1)
    if (plot.trend) {
        for (i in which(sapply(pred, function(yyy) !is.null(yyy)))) {
            trend = try(smooth.spline(yy[[i]]), TRUE)
            if (!is(trend, "try-error")) 
                lines(trend, pch = "*", col = color.forecast[i], 
                  lwd = 1)
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
