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
SigexModelComponentVARMA = function(p, q, epithet = "NULL") {
	new("SigexModelComponentVARMA", p = p, q = q, epithet = "NULL")
}

#' @export
setMethod("modelclass", "SigexModelComponentVARMA", function(object) {
	sprintf("VARMA(%d,%d)", object@p, object@q)
})

# ' @export
setMethod("show", "SigexModelComponentVARMA", function(object) {
	printf("SigexModelComponent for %s\n", modelname(object))
})
