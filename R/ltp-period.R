## This program is fre esoftware: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.

## Authors: L. Finos, M. Redaelli

## project name: strategico
## project website: http://code.google.com/p/strategico/
## created: 2011

.incSampleTime <- function(now, period.freq = 2, increment = 1) {
  if (now[2] + increment - 1 <= period.freq - 1) 
    now[2] = now[2] + increment
  else now = c(now[1] + (now[2] - 1 + increment)%/%period.freq, 
         ((now[2] + increment - 1)%%period.freq) + 1)
  now
}

Period.BuildRange <- function(period.start, period.freq, n, shift=0) {
  n.char <- nchar(period.freq)
  sapply ((0+shift):(n+shift-1),
          function(i) Period.ToString(.incSampleTime(now=period.start, period.freq = period.freq, increment = i),
                                      n.char=n.char
                                      )
          )
}

Period.ToNumber <- function(period,period.freq=2){
  if(is.numeric(period)) #un unico valore espresso in un vector di due elementi
	return(sum((period-c(0,1))*c(period.freq,1))) else #un vector di stringhe ognuna contenente un period
	return(sapply(period,function(per) sum((Period.FromString(per)-c(0,1))*c(period.freq,1))))
  }

Period.FromNumber <- function(period,period.freq=2){  return(c(period%/%period.freq,period%%period.freq+1))  }

Period.FromString <- function (period.string) {
  unlist(lapply(strsplit(period.string, "-"), as.numeric))
}

Period.ToString <- function (period, n.char=NULL, period.freq=2, sep="-") {
  p1 <- period[1]
  p2 <- as.character(period[2])
  nchar.p2 <- nchar(p2)

  if (is.null(n.char)) n.char <- nchar(period.freq)
  
  if (nchar.p2 < n.char) {
    zeros <- paste(rep("0",n.char - nchar.p2), collapse="")
    p2 <- paste(zeros, p2, collapse="", sep="")
  }
  paste(p1, p2, sep=sep)
}
