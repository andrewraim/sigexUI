# ----- SigexParamARMA -----
#' @export
setClass("SigexParamARMA",
	contains = "SigexParamTS",
	slots = c(ar = "matrix", ma = "matrix"),
	prototype = list(model_class = "ARMA")
)

#' @export
setValidity("SigexParamARMA", function(object) {
	if (nrow(object@ar) != ncol(object@ma)) {
		return("@ar and @ma must have same row dimension")
	}
	return(TRUE)
})

#' @export
setMethod("modelclass", "SigexParamARMA", function(object) {
	p = ncol(object@ar)
	q = ncol(object@ma)
	sprintf("ARMA(%d,%d)", p, q)
})

#' @export
setMethod("show", "SigexParamARMA", function(object) {
	p = ncol(object@ar)
	q = ncol(object@ma)
	printf("--- Param for ARMA(%d,%d) ---\n", p, q)
	printf("ar:\n")
	print(object@ar)
	printf("\n")
	printf("ma:\n")
	print(object@ma)
})
