\name{PlotLtpResults}
\alias{PlotLtpResults}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{PlotLtpResults}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PlotLtpResults(obj, directory = NULL, width = 600, height = 400, choosenModels = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
  \item{directory}{
%%     ~~Describe \code{directory} here~~
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
function (obj, directory = NULL, width = 600, height = 400, choosenModels = NULL) 
{
    plot.ltp(obj, plot.try.models = c("best"), color.forecast = NULL, 
        color.ic = "orange", plot.trend = FALSE, title = obj@BestModel, 
        filename = file.path(directory, "best_model.png"), width = width, 
        height = height)
    plot.ltp(obj, plot.try.models = c("all"), color.forecast = NULL, 
        color.ic = "orange", plot.trend = FALSE, title = "All Models", 
        filename = file.path(directory, "all_models.png"), width = width, 
        height = height, choosenModels = choosenModels)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
