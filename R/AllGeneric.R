#' @export
setGeneric("epithet", function(object) standardGeneric("epithet"))

#' @export
setGeneric("modelClass", function(object) standardGeneric("modelClass"))

#' @export
setGeneric("delta", function(object) standardGeneric("delta"))

#' @export
setGeneric("orderVector", function(object) standardGeneric("orderVector"))

#' @export
setGeneric("asSigexParamARMA", function(object) standardGeneric("asSigexParamARMA"))

#' @export
setGeneric("asSigexParamVARMA", function(object) standardGeneric("asSigexParamVARMA"))

# The add method is a method only of SigexModel. I like the idea of having it
# be a method instead of a function, since types are checked without having to
# do it ourselves. It looks like a way to accomplish this is to make it generic,
# but do not export the generic outside of the package.
# <https://stat.ethz.ch/R-manual/R-devel/library/methods/html/Methods_for_Nongenerics.html>
# ' @export
setGeneric("add", function(object, component, vrank, bounds) standardGeneric("add"))

