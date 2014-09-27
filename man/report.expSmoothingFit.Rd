\name{report.expSmoothingFit}
\alias{report.expSmoothingFit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
report.es}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
report.expSmoothingFit(model, list, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{list}{
%%     ~~Describe \code{list} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (model, list, ...) 
{
    equations = .eq.es(model)
    list(Drift = paste("type Trend:", model$drift, sep = " "), 
        Seasonality = paste("type Seasonality:", model$seasonality, 
            sep = " "), alpha = paste("Parameter Smoothing Level:", 
            round(model$par[1], 4), sep = " "), beta = paste("Parameter Smoothing Seasonality:", 
            round(model$par[4], 4), sep = " "), gamma = paste("Parameter Smoothing Trend:", 
            round(model$par[3], 4), sep = " "), R2 = paste("R2:", 
            round(list$R2, 4), sep = " "), AIC = paste("AIC:", 
            round(list$AIC, 3), sep = " "), paste("Residuals standard error:", 
            round(sqrt(model$sigma2), 2), sep = " "), list(paste("Recursive equations:", 
            sep = " "), paste(equations[1]), paste(equations[2]), 
            paste(equations[3])))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
