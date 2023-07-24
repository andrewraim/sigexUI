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
SigexParamVARMA = function(ar, ma) {
	new("SigexParamVARMA", ar = ar, ma = ma)
}

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
	if(p == 0 & q > 0){
		printf("--- %d-dimensional VMA(%d) ---\n", N, q)
		print(object@ma)
	}else if(p > 0 & q == 0){
		printf("--- %d-dimensional VAR(%d) ---\n", N, p)
		print(object@ar)
	}else{
		printf("--- %d-dimensional VARMA(%d,%d) ---\n", N, p, q)
		printf("ar:\n")
		print(object@ar)
		printf("\n")
		printf("ma:\n")
		print(object@ma)
	}
})

#' @export
setMethod("asSigexParamVARMA", c(object = "ar"), function(object) {
	ar_hat = aperm(object$ar, c(2, 3, 1))
	N = dim(ar_hat)[1]
	p = object$order
	SigexParamVARMA(ar = ar_hat, ma = array(0, dim = c(N,N,0)))
})


#' @export
setMethod("to_sigex",
	c(object = "SigexParamVARMA"),
	function(object) {

		# dimensionallity of sigexParamVARMA object
		N <- dim(object@ar)[1]
		p <- dim(object@ar)[3]
		q <- dim(object@ma)[3]

		# init array of correct dim
		out <- array(dim = c(N, N, p+q))

		# set index of output array for ar/ma slots
		idx_ar = seq_len(p)
		idx_ma = p + seq_len(q)

		# fill output array
		out[,,idx_ar] <- object@ar
		out[,,idx_ma] <- object@ma

		return(out)
	}
)
