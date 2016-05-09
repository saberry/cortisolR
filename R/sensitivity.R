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

  dat0 = dat[dat[[calibVar]] == 0, ]

  bindingsSD = sd(dat0[["bindings"]], na.rm = TRUE)

  lowerBinding = 100 - (3 * bindingsSD)

  lowerBindingLog = log((100 - lowerBinding) / lowerBinding)

  dat0 = dat[dat[["logStd"]] >= 0, ]

  backEstMod = lm(dat0[[logStdVar]] ~ dat0[[logValVar]], data = dat0)

  logStdConstant = backEstMod$coefficients[[1]]

  logStdCoef = backEstMod$coefficients[[2]]

  lowerThresh = 2.71828 ^ (logStdConstant + logStdCoef * lowerBindingLog)

  lowerThresh
}


