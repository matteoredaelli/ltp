\name{report.Arima}
\alias{report.Arima}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
report.arima
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
report.Arima(model, list, ...)
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
    AIC = list$AIC
    R2 = list$R2
    Coefficients = cbind(Estimate = model$coef, Std.Error = matrix(sqrt(diag(model$var.coef))))
    colnames(Coefficients) = c("Estimate", "Std. Error")
    if (nrow(Coefficients) == 0) 
        Coefficients = rbind(Coefficients, NA)
    rr = t(as.matrix(summary(model$residuals)))
    list(Call = model$call, residuals = list("Residuals", summary(model$residuals)), 
        Coefficients = list("Coefficients", Coefficients), AIC = paste("AIC:", 
            round(AIC, 4), sep = " "), R2 = paste("R2:", R2, 
            sep = " "), Residuals = paste("Residuals standard error:", 
            round(sqrt(model$sigma2), 4), sep = " "))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
