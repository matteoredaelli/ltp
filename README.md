= ltp

== Time series forecasting - log term predictions over timeseries

The Long Term Prediction is the first statistical analysis we have implemented inside Strategico: it automatically finds the best model that fits each time series.

The implemented models are

* Mean,
* Trend,
* Linear,
* Exponential smoothing,
* Arima
* Naive 

See the LongTermPrediction Wiki Page for more details.

The LTP package is used by:

* predictoR (https://github.com/matteoredaelli/predictoR) gives a web frontend (using the R internal web server) for a single time series analysis. There is also an online demo...
* Strategico (https://code.google.com/p/strategico/)

== Installation:

library(devtools)
install_github("matteoredaelli/ltp")

== Requirements:

install.packages(c("hwriter", "reshape", "tseries", "forecast", "xtable", "R2HTML","fracdiff", "googleVis", "XML"), dependencies = TRUE) 
