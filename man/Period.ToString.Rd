\name{Period.ToString}
\alias{Period.ToString}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Period.ToString}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Period.ToString(period, n.char = NULL, period.freq = 2, sep = "-")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{period}{
%%     ~~Describe \code{period} here~~
}
  \item{n.char}{
%%     ~~Describe \code{n.char} here~~
}
  \item{period.freq}{
%%     ~~Describe \code{period.freq} here~~
}
  \item{sep}{
%%     ~~Describe \code{sep} here~~
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
function (period, n.char = NULL, period.freq = 2, sep = "-") 
{
    p1 <- period[1]
    p2 <- as.character(period[2])
    nchar.p2 <- nchar(p2)
    if (is.null(n.char)) 
        n.char <- nchar(period.freq)
    if (nchar.p2 < n.char) {
        zeros <- paste(rep("0", n.char - nchar.p2), collapse = "")
        p2 <- paste(zeros, p2, collapse = "", sep = "")
    }
    paste(p1, p2, sep = sep)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
