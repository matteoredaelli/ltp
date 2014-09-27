\name{getMaxJump}
\alias{getMaxJump}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{getMaxJump
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getMaxJump(product, period.freq, pred.mod)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{product}{
%%     ~~Describe \code{product} here~~
}
  \item{period.freq}{
%%     ~~Describe \code{period.freq} here~~
}
  \item{pred.mod}{
%%     ~~Describe \code{pred.mod} here~~
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
function (product, period.freq, pred.mod) 
{
    res = exp(abs(log(mean(product[max(1, (nrow(product) - period.freq + 
        1)):nrow(product), 1], na.rm = TRUE)/mean(pred.mod[1:min(period.freq, 
        nrow(product))], na.rm = TRUE))))
    if (is.null(res)) {
        res = NA
    }
    else {
        if (length(res) != 1) 
            res = 1
        if (any(is.na(res))) 
            res = 1
    }
    res
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
