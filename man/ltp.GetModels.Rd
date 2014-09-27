\name{ltp.GetModels}
\alias{ltp.GetModels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ltp.GetModels
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ltp.GetModels(what = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{what}{
%%     ~~Describe \code{what} here~~
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
function (what = NULL) 
{
    model = data.frame(id = c("lm", "arima", "es", "trend", "mean", 
        "naive"), name = c("Linear", "Arima", "ExpSmooth", "Trend", 
        "Mean", "Naive"), color = c("green", "red", "blue", "gray", 
        "black", "yellow"), legend = c("Linear", "Arima", "Exp.Smooth", 
        "Trend", "Mean", "Naive"))
    rownames(model) = model$id
    if (!is.null(what)) {
        names = model$id
        model = as.character(model[, what])
        names(model) = names
    }
    model
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
