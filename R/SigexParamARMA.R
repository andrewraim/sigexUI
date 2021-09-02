#' @export
setValidity("SigexParamARMA", function(object) {
	return(TRUE)
})

#' @export
SigexParamARMA = function(ar, ma) {
	new("SigexParamARMA", ar = ar, ma = ma)
}

#' @export
setMethod("modelClass", "SigexParamARMA", function(object) {
	p = length(object@ar)
	q = length(object@ma)
	sprintf("ARMA(%d,%d)", p, q)
})

#' @export
setMethod("show", "SigexParamARMA", function(object) {
	p = length(object@ar)
	q = length(object@ma)
	printf("--- Param for ARMA(%d,%d) ---\n", p, q)
	printf("ar:\n")
	print(object@ar)
	printf("\n")
	printf("ma:\n")
	print(object@ma)
})

#' @export
setMethod("asSigexParamARMA", c(object = "ar"), function(object) {
	ar_hat = object$ar
	stopifnot("numeric" %in% class(ar_hat))
	p = object$order
	SigexParamARMA(ar = ar_hat, ma = numeric(0))
})
