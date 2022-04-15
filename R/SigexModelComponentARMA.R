#' @export
setValidity("SigexModelComponentARMA", function(object) {
	if (length(object@p) != 1) {
		return("@p must be a scalar")
	}
	if (length(object@q) != 1) {
		return("@q must be a scalar")
	}
	return(TRUE)
})

#' @export
SigexModelComponentARMA = function(p, q, delta = 1, epithet = "NULL") {
	new("SigexModelComponentARMA", p = p, q = q, delta = delta, epithet = epithet)
}

#' @export
setMethod("modelClass", "SigexModelComponentARMA", function(object) {
	sprintf("ARMA(%d,%d)", object@p, object@q)
})

#' @export
setMethod("orderVector", "SigexModelComponentARMA", function(object) {
	c(object@p, object@q)
})
