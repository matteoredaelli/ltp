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

## project name: ltp
## project website: http://code.google.com/p/ltp/
## created: 2011

##==========================================================
## CLASS DEFINITION *** CLASS DEFINITION *** CLASS DEFINITION
##==========================================================

#setClassUnion("matrixOrNULL", c("matrix", "NULL"))
setClassUnion("data.frameOrNULL", c("data.frame", "NULL"))
setClassUnion("characterOrNULL", c("character", "NULL"))
setClassUnion("listOrNULL", c("list", "NULL"))

setClass("ltp.object", 
  representation(
    values = "data.frameOrNULL",
    models = "listOrNULL", 
	BestModel="characterOrNULL",
        BestModel2="characterOrNULL",       
	rule="characterOrNULL",
	ruleSetting="listOrNULL"
  ),
  prototype = list(
	values = NULL,
    models = NULL, 
	BestModel=NULL,
        BestModel2=NULL,
	rule="BestAIC",
	ruleSetting=NULL
  )
)

#==========================================================
# Function "show" prints a "ltp.object" object
#==========================================================
setMethod("show", "ltp.object", function(object)
{
  cat("ltp-object\n values:\n")
  print(rbind(head(object@values),etc="..."))
  cat(" Models: ") 
  cat(paste(names(object@models),collapse=", "))
  cat(paste("\n Rule for Suggested Model: ",object@rule,sep=""))
  cat("\n Rule's setting: ")
  cat(paste(names(object @ruleSetting),object @ruleSetting,sep="=",collapse=";  "))
  cat("\n")
})

setGeneric("summary")
setMethod("summary", "ltp.object", function(object, ...)
{
print(getModelsSummary(object@models)) 
})


#==========================================================
# Functions to extract relevant information from 
# a ltp.object object
#==========================================================
setGeneric("result", function(object, ...) standardGeneric("result"))
setMethod("result", "ltp.object",
  function(object) { 
    show(object)
})

