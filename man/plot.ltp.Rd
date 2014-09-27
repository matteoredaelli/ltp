\name{plot.ltp}
\alias{plot.ltp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot.ltp
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.ltp(model, plot.try.models = c("best", "all"), color.forecast = NULL, color.ic = "red", plot.trend = FALSE, title = "Time Series", filename, width = width, height = height, choosenModels = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{plot.try.models}{
%%     ~~Describe \code{plot.try.models} here~~
}
  \item{color.forecast}{
%%     ~~Describe \code{color.forecast} here~~
}
  \item{color.ic}{
%%     ~~Describe \code{color.ic} here~~
}
  \item{plot.trend}{
%%     ~~Describe \code{plot.trend} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
  \item{width}{
%%     ~~Describe \code{width} here~~
}
  \item{height}{
%%     ~~Describe \code{height} here~~
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
function (model, plot.try.models = c("best", "all"), color.forecast = NULL, 
    color.ic = "red", plot.trend = FALSE, title = "Time Series", 
    filename, width = width, height = height, choosenModels = NULL) 
{
    if (is.null(color.forecast)) {
        color.forecast = as.vector(ltp.GetModels("color"))
        names(color.forecast) = as.vector(ltp.GetModels("name"))
    }
    for (i in plot.try.models) {
        if (i == "all") 
            .plot.all(model = model, color.forecast = color.forecast, 
                plot.trend = plot.trend, title = title, filename = filename, 
                width = width, height = height, choosenModels = choosenModels)
        if (i == "best") 
            .plot.best(best = model@models[[model@BestModel]], 
                color.ic = color.ic, plot.trend = plot.trend, 
                color.forecast = color.forecast[model@BestModel], 
                title = title, filename = filename, width = width, 
                height = height)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
