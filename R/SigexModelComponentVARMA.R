#' @export
setValidity("SigexModelComponentVARMA", function(object) {
	if (length(object@p) != 1) {
		return("@p must be a scalar")
	}
	if (length(object@q) != 1) {
		return("@q must be a scalar")
	}
	return(TRUE)
})

#' @export
SigexModelComponentVARMA = function(p, q, delta = 1, epithet = "NULL") {
	new("SigexModelComponentVARMA", p = p, q = q, delta = delta, epithet = epithet)
}

#' @export
setMethod("modelClass", "SigexModelComponentVARMA", function(object) {
	sprintf("VARMA(%d,%d)", object@p, object@q)
})

#' @export
setMethod("orderVector", "SigexModelComponent", function(object) {
	c(object@p, object@q)
})
