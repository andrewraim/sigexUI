#' @export
setValidity("SigexParamVARMA", function(object) {
	if (length(dim(object@ar)) != 3) {
		return("@ar must be a 3-dimensional array")
	}
	if (length(dim(object@ma)) != 3) {
		return("@ma must be a 3-dimensional array")
	}
	if (dim(object@ar)[1] != dim(object@ar)[2]) {
		return("@ar must have first two dimensions equal")
	}
	if (dim(object@ma)[1] != dim(object@ma)[2]) {
		return("@ma must have first two dimensions equal")
	}
	if (any(dim(object@ar)[1:2] != dim(object@ma)[1:2])) {
		return("@ar and @ma arrays must have same 1st and 2nd dimensions")
	}
	return(TRUE)
})

#' @export
setMethod("modelClass", "SigexParamVARMA", function(object) {
	p = dim(object@ar)[3]
	q = dim(object@ma)[3]
	sprintf("VARMA(%d,%d)", p, q)
})

#' @export
setMethod("show", "SigexParamVARMA", function(object) {
	N = dim(object@ar)[1]
	p = dim(object@ar)[3]
	q = dim(object@ma)[3]
	printf("--- Param for N=%d dimensional VARMA(%d,%d) series ---\n", N, p, q)
	printf("ar:\n")
	print(object@ar)
	printf("\n")
	printf("ma:\n")
	print(object@ma)
})

#' @export
SigexParamVARMA = function(ar, ma) {
	varma_par = new("SigexParamVARMA", ar = ar, ma = ma)
}
