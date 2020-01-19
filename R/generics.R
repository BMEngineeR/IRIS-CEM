#' @rdname NormalizeData
#' @export
setGeneric(name="NormalizeData",
           def=function(object, IsImputation) standardGeneric("NormalizeData")
)


#' @rdname RunLTMG
#' @export
setGeneric(name="RunLTMG",
           def=function(object, NFeatures) standardGeneric("RunLTMG")
)


#' @export
#' @rdname AddNormalNoise
setGeneric(name="AddNormalNoise",
           def=function(object) standardGeneric("AddNormalNoise")
)


#' @export
#' @rdname GetAddNormalNoise
setGeneric(name="GetNormalNoiseMatirx",
           def=function(object) standardGeneric("GetNormalNoiseMatirx")
)


#' @export
#' @rdname GetLTMGmatrix
setGeneric(name = "GetLTMGmatrix",
           def = function(object) standardGeneric("GetLTMGmatrix"))



#' @export
#' @rdname CalBinarySingleSignal
setGeneric(name = "CalBinaryMultiSignal",
           def = function(object) standardGeneric("CalBinaryMultiSignal"))


#' @export
#' @rdname GetBinaryMultiSignal
setGeneric(name = "GetBinaryMultiSignal",
           def = function(object) standardGeneric("GetBinaryMultiSignal"))


#' @export
#' @rdname RunDiscretization
setGeneric(name = "RunDiscretization",
           def = function(object) standardGeneric("RunDiscretization"))







