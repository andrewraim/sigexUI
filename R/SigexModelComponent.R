#' @export
setMethod("modelClass", "SigexModelComponent", function(object) { object@model_class })

#' @export
setMethod("epithet", "SigexModelComponent", function(object) { object@epithet })

#' @export
setMethod("delta", "SigexModelComponent", function(object) { object@delta })

# ' @export
setMethod("show", "SigexModelComponent", function(object) {
	if (object@epithet == "NULL") {
		printf("SigexModelComponent: model_class = %s, delta = (%s)\n",
			object@model_class, paste(object@delta, collapse = ","))
	} else {
		printf("SigexModelComponent: model_class = %s, epithet = \"%s\", delta = (%s)\n",
			object@model_class, object@epithet, paste(object@delta, collapse = ","))
	}
})
