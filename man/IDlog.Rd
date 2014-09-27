\name{IDlog}
\alias{IDlog}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
IDlog
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
IDlog(product, period.start)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{product}{
%%     ~~Describe \code{product} here~~
}
  \item{period.start}{
%%     ~~Describe \code{period.start} here~~
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
function (product, period.start) 
{
    y = as.vector(product)
    y = ts(y, start = period.start, frequency = period.freq)
    mod = esId(y, keep = 1)
    mod = mod[which(mod$rankAIC == 1), ]
    if (mod$sea == "m" | mod$sea == "c/m") 
        logtransform = TRUE
    else logtransform = FALSE
    logtransform
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
