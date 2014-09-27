\name{getBestModel}
\alias{getBestModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
getBestModel
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getBestModel(models, rule, ruleSetting)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{models}{
%%     ~~Describe \code{models} here~~
}
  \item{rule}{
%%     ~~Describe \code{rule} here~~
}
  \item{ruleSetting}{
%%     ~~Describe \code{ruleSetting} here~~
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
function (models, rule, ruleSetting) 
{
    stats = getModelsStatistics(models, switch(rule, BestIC = "IC.width", 
        BestAIC = "AIC"))
    retain = getRetainedModelsByRule(models, ruleSetting)
    ID.model <- which.min(stats[retain])
    ID.model <- ltp.GetModels("name")[ID.model]
    if ((is.null(ID.model) || (length(ID.model) == 0)) || is.na(ID.model)) 
        ID.model <- "Naive"
    ID.model
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
