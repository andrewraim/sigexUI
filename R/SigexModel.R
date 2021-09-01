#' @export
SigexModel = function() {
	new("SigexModel")
}

#' @export
setMethod("show", "SigexModel", function(object) {
	L = length(object@components)
	printf("SigexModel with %d component(s)\n", L)
	for (l in seq_len(L)) {
		comp_class = modelclass(object@components[[l]])
		comp_ep = epithet(object@components[[l]])
		if (comp_ep == "NULL") {
			printf("%d) %s\n", l, comp_class)
		} else {
			printf("%d) %s with epithet %s\n", l, comp_class, comp_ep)
		}
	}
	return(invisible(NULL))
})

#' @export
setMethod("add",
	c("SigexModel", "SigexModelComponentVARMA", "numeric", "integer", "numeric"),
	function(object, component, delta, vrank, bounds = NULL) {
		# Add component to internal mdl structure
		order = c(component@p, component@q)
		object@mdl = sigex.add(object@mdl, vrank, "varma", order, bounds, component@epithet, delta)

		# Add description of component to list
		L = length(object@components)
		object@components[[L + 1]] = component

		invisible(object)
	}
)

# ' @export
#setValidity("GCD", function(object) {
#	if (nrow(object@L) != ncol(object@L)) {
#		return("@L must be a square matrix")
#	}
#	if (nrow(object@L) != length(object@D_vec)) {
#		return("@L and @D_vec must have compatible dimensions")
#	}
#	return(TRUE)
#})

# ' @export
#setMethod("dim", "GCD", function(x) {
#	dim(x@L)
#})
