#' @rdname NormalizeData
#' @export
setGeneric(name="NormalizeData",
           def=function(object, IsImputation) standardGeneric("NormalizeData")
)


#' @rdname RunLTMG
#' @export
setGeneric(name="RunLTMG",
           def=function(object,Gene_use,k=5) standardGeneric("RunLTMG")
)




#' @export
#' @rdname GetLTMGmatrix
setGeneric(name = "GetLTMGmatrix",
           def = function(object) standardGeneric("GetLTMGmatrix"))

#' @export
#' @rdname CalBinarySingleSignal
setGeneric(name = "CalBinarySingleSignal",
           def = function(object) standardGeneric("CalBinarySingleSignal"))


#' @export
#' @rdname CalBinarySingleSignal
setGeneric(name = "GetBinarySingleSignal",
           def = function(object) standardGeneric("GetBinarySingleSignal"))


#' @export
#' @rdname CalBinarySingleSignal
setGeneric(name = "CalBinaryMultiSignal",
           def = function(object) standardGeneric("CalBinaryMultiSignal"))


#' @export
#' @rdname GetBinaryMultiSignal
setGeneric(name = "GetBinaryMultiSignal",
           def = function(object) standardGeneric("GetBinaryMultiSignal"))










