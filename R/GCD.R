# This might be missing the point a little bit. Maybe the fact that it's a GCD
# can be an internal implementation detail most of the time.
#' @export
setClass("GCD",
	slots = c(
		L = "matrix",
		D_vec = "numeric"
	),
	prototype = list(
		L = matrix(NA_real_, 0, 0),
		D_vec = NA_real_
	)
)

#' @export
setValidity("GCD", function(object) {
	if (nrow(object@L) != ncol(object@L)) {
		return("@L must be a square matrix")
	}
	if (nrow(object@L) != length(object@D_vec)) {
		return("@L and @D_vec must have compatible dimensions")
	}
	return(TRUE)
})

#' @export
GCD = function(L, D_vec) {
	new("GCD", L = L, D_vec = D_vec)
}

#' @export
setMethod("show", "GCD", function(object) {
	printf("L D L' with\n")
	printf("L:\n")
	print(object@L)
	printf("\n")
	printf("Diagonal of D:\n")
	print(object@D_vec)
})

#' @export
setMethod("dim", "GCD", function(x) {
	dim(x@L)
})
