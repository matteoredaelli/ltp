\name{ltp.object-class}
\Rdversion{1.1}
\docType{class}
\alias{ltp.object-class}
\alias{result,ltp.object-method}
\alias{show,ltp.object-method}
\alias{summary,ltp.object-method}

\title{Class "ltp.object"}
\description{ltp.object-class}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ltp.object", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{values}:}{Object of class \code{"data.frameOrNULL"} ~~ }
    \item{\code{models}:}{Object of class \code{"listOrNULL"} ~~ }
    \item{\code{BestModel}:}{Object of class \code{"characterOrNULL"} ~~ }
    \item{\code{rule}:}{Object of class \code{"characterOrNULL"} ~~ }
    \item{\code{ruleSetting}:}{Object of class \code{"listOrNULL"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{result}{\code{signature(object = "ltp.object")}: ... }
    \item{show}{\code{signature(object = "ltp.object")}: ... }
    \item{summary}{\code{signature(object = "ltp.object")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("ltp.object")
}
\keyword{classes}
