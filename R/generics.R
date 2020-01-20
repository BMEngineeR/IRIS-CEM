#' @rdname NormalizeData
#' @export
setGeneric(name="NormalizeData",
           def=function(object, ...) standardGeneric("NormalizeData")
)


#' @rdname RunLTMG
#' @export
setGeneric(name="RunLTMG",
           def=function(object, ...) standardGeneric("RunLTMG")
)



#' @export
#' @rdname GetAddNormalNoise
setGeneric(name="GetNormalNoiseMatirx",
           def=function(object, ...) standardGeneric("GetNormalNoiseMatirx")
)


#' @export
#' @rdname GetLTMGmatrix
setGeneric(name = "GetLTMGmatrix",
           def = function(object, ...) standardGeneric("GetLTMGmatrix"))



#' @export
#' @rdname CalBinarySingleSignal
setGeneric(name = "CalBinarySingleSignal",
           def = function(object, ...) standardGeneric("CalBinarySingleSignal"))


#' @export
#' @rdname GetBinarySingleSignal
setGeneric(name = "GetBinarySingleSignal",
           def = function(object, ...) standardGeneric("GetBinarySingleSignal"))

#' @export
#' @rdname CalBinaryMultiSignal
setGeneric(name = "CalBinaryMultiSignal",
           def = function(object, ...) standardGeneric("CalBinaryMultiSignal"))


#' @export
#' @rdname GetBinaryMultiSignal
setGeneric(name = "GetBinaryMultiSignal",
           def = function(object, ...) standardGeneric("GetBinaryMultiSignal"))


#' @export
#' @rdname RunDiscretization
setGeneric(name = "RunDiscretization",
           def = function(object, ...) standardGeneric("RunDiscretization"))


#' @export
#' @rdname RunBicluster
setGeneric(name = "RunBicluster",
           def = function(object, ...) standardGeneric("RunBicluster"))





