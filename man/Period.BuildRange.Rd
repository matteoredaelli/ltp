\name{Period.BuildRange}
\alias{Period.BuildRange}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Period.BuildRange}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Period.BuildRange(period.start, period.freq, n, shift = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{period.start}{
%%     ~~Describe \code{period.start} here~~
}
  \item{period.freq}{
%%     ~~Describe \code{period.freq} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{shift}{
%%     ~~Describe \code{shift} here~~
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
function (period.start, period.freq, n, shift = 0) 
{
    n.char <- nchar(period.freq)
    sapply((0 + shift):(n + shift - 1), function(i) Period.ToString(.incSampleTime(now = period.start, 
        period.freq = period.freq, increment = i), n.char = n.char))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
