#' @rdname NormalizeData
#' @export
setGeneric(name="NormalizeData",
           def=function(object, ...) standardGeneric("NormalizeData")
)


#' @export
#' @rdname AddMeta
setGeneric(name = "AddMeta",
           def =function(object, ...) standardGeneric("AddMeta"))

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

# LTMG series

#' @export
#' @rdname RunDimensionReduction
setGeneric(name = "RunDimensionReduction",
           def =function(object, ...) standardGeneric("RunDimensionReduction"))


#' @export
#' @rdname RunClassification
setGeneric(name = "RunClassification",
           def =function(object, ...) standardGeneric("RunClassification"))


#' @export
#' @rdname FindMarkers
setGeneric(name = "FindMarkers",
           def =function(object, ...) standardGeneric("FindMarkers"))

#' @export
#' @rdname RunPathway
setGeneric(name = "RunPathway",
           def =function(object, ...) standardGeneric("RunPathway"))

#' @export
#' @rdname PlotHeatmap
setGeneric(name = "PlotHeatmap",
           def =function(object, ...) standardGeneric("PlotHeatmap"))

