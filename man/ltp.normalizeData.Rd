\name{ltp.normalizeData}
\alias{ltp.normalizeData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ltp.normalizeData}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ltp.normalizeData(product, range, NA2value = 0, period.start, period.freq, increment, period.end)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{product}{
%%     ~~Describe \code{product} here~~
}
  \item{range}{
%%     ~~Describe \code{range} here~~
}
  \item{NA2value}{
%%     ~~Describe \code{NA2value} here~~
}
  \item{period.start}{
%%     ~~Describe \code{period.start} here~~
}
  \item{period.freq}{
%%     ~~Describe \code{period.freq} here~~
}
  \item{increment}{
%%     ~~Describe \code{increment} here~~
}
  \item{period.end}{
%%     ~~Describe \code{period.end} here~~
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
function (product, range, NA2value = 0, period.start, period.freq, 
    increment, period.end) 
{
    product[is.na(product), ] = NA2value
    period.start = Period.FromNumber(min(Period.ToNumber(period.end, 
        period.freq), max(Period.ToNumber(period.start, period.freq), 
        min(Period.ToNumber(rownames(product), period.freq)))), 
        period.freq)
    n <- Period.ToNumber(period.end, period.freq) - Period.ToNumber(period.start, 
        period.freq) + 1
    times <- Period.BuildRange(period.start, period.freq, n, 
        shift = 0)
    productnew = data.frame(rep(NA2value, len = length(times)))
    rownames(productnew) = times
    colnames(productnew) = colnames(product)
    if (n == 0) {
        productnew[1, ] = NA2value
        rownames(productnew) = Period.ToString(period.end)
    }
    ValidValues = intersect(rownames(product), rownames(productnew))
    if (length(ValidValues) > 0) 
        productnew[ValidValues, ] = product[ValidValues, ]
    if (any(is.na(productnew))) {
        for (i in 1:period.freq) {
            if (length(grep(paste("-", i, sep = ""), rownames(productnew))) > 
                0) 
                productnew[grep(paste("-", i, sep = ""), rownames(productnew)), 
                  ][is.na(productnew[grep(paste("-", i, sep = ""), 
                  rownames(productnew)), ])] = mean(productnew[grep(paste("-", 
                  i, sep = ""), rownames(productnew)), ], na.rm = TRUE)
        }
    }
    productnew[is.na(productnew)] = 0
    productnew[productnew < range[1], ] = range[1]
    productnew[productnew > range[2], ] = range[2]
    return(list(product = productnew, start = period.start))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
