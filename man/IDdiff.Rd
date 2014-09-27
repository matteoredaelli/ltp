\name{IDdiff}
\alias{IDdiff}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
IDdiff
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
IDdiff(y, period.freq)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{period.freq}{
%%     ~~Describe \code{period.freq} here~~
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
function (y, period.freq) 
{
    diff.trend = 0
    diff.sea = 0
    v = rep(NA, 6)
    v[1] = var(y)
    v[2] = var(diff(y, period.freq))
    v[3] = var(diff(y))
    v[4] = var(diff(diff(y, period.freq)))
    v[5] = var(diff(diff(y, period.freq), period.freq))
    v[6] = var(diff(diff(y)))
    d_sea = c(0, 1, 0, 1, 2, 0)
    d_trend = c(0, 0, 1, 1, 0, 2)
    d = cbind(v, d_sea, d_trend)
    if (sum(is.na(v)) == 0) {
        index = which.min(v)
        diff.trend = d[index, 3]
        diff.sea = d[index, 2]
    }
    c(diff.trend, diff.sea)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
