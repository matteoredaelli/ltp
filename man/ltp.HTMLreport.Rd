\name{ltp.HTMLreport}
\alias{ltp.HTMLreport}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ltp.HTMLreport}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ltp.HTMLreport(obj, id, value, value.description, directory = ".", width = 1000, height = 600)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
  \item{id}{
%%     ~~Describe \code{id} here~~
}
  \item{value}{
%%     ~~Describe \code{value} here~~
}
  \item{value.description}{
%%     ~~Describe \code{value.description} here~~
}
  \item{directory}{
%%     ~~Describe \code{directory} here~~
}
  \item{width}{
%%     ~~Describe \code{width} here~~
}
  \item{height}{
%%     ~~Describe \code{height} here~~
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
function (obj, id, value, value.description, directory = ".", 
    width = 1000, height = 600) 
{
    dir.create(directory, showWarnings = FALSE, recursive = TRUE)
    html.filename = file.path(directory, "report.html")
    title = paste("Strategico: Long Term Prediction for ID=", 
        id, " - ", value.description, sep = " ")
    ReporTable <- ltp.GetModelsComparisonTable(obj)
    text = paste("<html>\n<head>\n<title>", title, "</title>\n</html>\n<body>\n<h1>", 
        title, "</h1><a href=\"http://code.google.com/p/strategico/wiki/LTP\"/>Quick Help</a>", 
        "<h2>Best Model</h2>Criterion:", obj@rule, ", and Max CV smaller than  ", 
        obj@ruleSetting["rule.noMaxCVOver"], ", Max Jump smaller than  ", 
        obj@ruleSetting["rule.noMaxJumpOver"], ",<br><img src=\"best_model.png\" />\n<h2>All Models </h2>\n<img src=\"all_models.png\" />\n", 
        hwrite(ReporTable), sep = "")
    cat(text, append = FALSE, file = html.filename)
    notNA <- sapply(ltp.GetModels("name"), function(i) if (!is.null(obj@models[[i]])) 
        (!is.null(obj@models[[i]]$Residuals)) & (!any(is.na(obj@models[[i]]$Residuals)))
    else FALSE)
    for (modType in setdiff(ltp.GetModels("name")[notNA], "Naive")) {
        residPlot = paste("resid_", modType, ".png", sep = "")
        .plot.resid(obj, modType, filename = file.path(directory, 
            residPlot), width = width * 0.6, height = height * 
            0.6)
        text = paste("\n<h3> Model: ", modType, "</h3>\n<img src=\"", 
            residPlot, "\" />", sep = "")
        cat(text, append = TRUE, file = html.filename)
        HTML(file = html.filename, report(obj@models[[modType]]$model, 
            obj@models[[modType]]))
    }
    text = "<h2>Recorded and Predicted Data</h2>"
    cat(text, append = TRUE, file = html.filename)
    y = obj@values
    names(y) = "Historical values"
    cat(hwrite(y), append = TRUE, file = html.filename)
    if (!is.null(obj@BestModel)) {
        pred = as.data.frame(obj@models[[obj@BestModel]]$prediction, 
            ncol = 1)
        period.freq = frequency(obj@models[[obj@BestModel]]$ts.product)
        end_serie = end(obj@models[[obj@BestModel]]$ts.product)
        pred.names <- Period.BuildRange(end_serie, period.freq, 
            nrow(pred), shift = 0)
        rownames(pred) = pred.names
        colnames(pred) = "Predicted values"
        cat(hwrite(pred), append = TRUE, file = html.filename)
    }
    cat("</body> </html>", append = TRUE, file = html.filename)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
