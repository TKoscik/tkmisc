faster.cor <-
function(inputMx, method="pearson") {
#=======================================================================================
# File:     		faster.cor.R
#	Type:					R function
#	Description:	Take brain timeseries and generate RDM/RSM.
# Usage:        inputMx [required]  An m x n numeric matrix
#               method  [optional]  Options: "pearson", "rank"
#               outputMx            Output is an n x n matrix
#	Project:			MLM-RSA General Functions
#	Author:				T.R. Koscik
#	Created:			2015-09-30 -- 12:10
#	Revision:
#=======================================================================================
if (!is.matrix(inputMx)) {
  inputMx <- as.matrix(inputMx)
}

if (method != "pearson") {
  inputMx <- apply(inputMx, 2L, rank, na.last = "keep")
}

inputMx <- t(inputMx)
inputMx <- inputMx - rowMeans(inputMx);
inputMx <- inputMx / sqrt(rowSums(inputMx^2))
outputMx <- tcrossprod(inputMx)
return(outputMx)

}
