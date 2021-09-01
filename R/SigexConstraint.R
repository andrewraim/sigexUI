
# ' Constraint
# '
# ' Create a constraint of the form A psi = b.

#' @export
setClass("SigexConstraint",
	slots = c(
		A = "matrix",
		b = "numeric"
	),
	prototype = list(
		A = matrix(NA_real_, 0, 0),
		b = NA_real_
	)
)

#' @export
setValidity("SigexConstraint", function(object) {
	if (ncol(object@A) != length(object@b)) {
		return("ncol(@A) must be same as length of @b")
	}
	return(TRUE)
})

#' @export
SigexConstraint = function(A, b) {
	new("SigexConstraint", A = A, b = b)
}

#' @export
setMethod("show", "SigexConstraint", function(object) {
	printf("Constraint is A psi = b\n")
	printf("A:\n")
	print(object@A)
	printf("\n")
	printf("b:\n")
	print(object@b)
})
