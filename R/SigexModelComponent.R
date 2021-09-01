#' @export
setMethod("modelclass", "SigexModelComponent", function(object) { object@model_class })

#' @export
setMethod("epithet", "SigexModelComponent", function(object) { object@epithet })
