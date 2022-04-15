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
	p = ncol(object@ar)
	q = ncol(object@ma)
	sprintf("ARMA(%d,%d)", p, q)
})

#' @export
setMethod("show", "SigexParamARMA", function(object) {
	p = dim(object@ar)[2]
	q = dim(object@ma)[2]
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


#' @export
setMethod("to_sigex",
	c(object = "SigexParamARMA"),
	function(object) {

		# dimensionallity of sigexParamARMA object
		#  object@ar matrix of dim (N x p)
		#  object@ma matrix of dim (N x q)
		N <- dim(object@ar)[1]
		p <- dim(object@ar)[2]
		q <- dim(object@ma)[2]

		# init matrix of correct dim.
		# sigex cbinds coef matricies to dim N x (p+q)
		out <- matrix(nrow = N, ncol = p + q)

		# set index of output array for ar/ma slots
		idx_ar = seq_len(p)
		idx_ma = p + seq_len(q)

		# fill output array
		out[, idx_ar] <- object@ar
		out[, idx_ma] <- object@ma

		return(out)
	}
)
