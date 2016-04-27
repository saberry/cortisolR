#' Back-estimating Cortisol Concentration
#'
#' @param dat
#' Should be the data saved from the 'initialInspectionCortisol' function
#' @param logStdVar
#' The logStdVar variable from the data.
#' @param logValVar
#' The logStdVar variable from the data.
#' @param calibVar
#' The calibVar variable from the data.
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 %+%
#' @importFrom ggplot2 aes

backEstimateCortisol = function(dat, logStdVar, logValVar,
                                calibVar) {

  backEstMod = lm(dat[[logStdVar]] ~ dat[[logValVar]], data = dat)

  logStdConstant = backEstMod$coefficients[[1]]

  logStdCoef = backEstMod$coefficients[[2]]

  estConc = 2.71828 ^ (logStdConstant + logStdCoef * dat[[logValVar]])

  plotDat = cbind(dat, estConc)

  maxCalVal = as.numeric(max(plotDat[[calibVar]]) + (max(plotDat[[calibVar]]) * .25))

  suppressWarnings(ggplot(plotDat, aes(plotDat[["estConc"]], plotDat[[calibVar]])) +
    geom_smooth(span = .5) +
    geom_point() +
    scale_y_continuous(name = "Calibration Variable",
                       limits = c(0, as.numeric(maxCalVal))) +
    scale_x_continuous(name = "Estimated Concentration",
                       limits = c(0, as.numeric(maxCalVal))) +
    ggtheme()
  )

  return(plotDat)
}
