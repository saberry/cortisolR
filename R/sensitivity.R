#' Determine Lower Detection Threshold
#'
#' @param dat
#' The data object returned from initialInspectionCortisol
#' @param logStdVar
#' The logStd variable from the data
#' @param logValVar
#' The logVal variable from the data
#' @return
#' @export
#'
#' @examples
sensitivity = function(dat, calibVar, calibVal, logStdVar, logValVar) {

  testDat = dat[dat[, calibVar] %in% calibVal, ]

  bindingsSD = sd(testDat[["bindings"]], na.rm = TRUE)

  lowerBinding = 100 - (3 * bindingsSD)

  lowerBindingLog = log((100 - lowerBinding) / lowerBinding)

  backEstMod = lm(dat[[logStdVar]] ~ dat[[logValVar]], data = dat)

  logStdConstant = backEstMod$coefficients[[1]]

  logStdCoef = backEstMod$coefficients[[2]]

  lowerThresh = 2.71828 ^ (logStdConstant + logStdCoef * lowerBindingLog)

  lowerThresh
}


