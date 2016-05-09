#' Back-estimating Cortisol Concentration
#'
#' @param dat
#' Should be the data saved from the 'initialInspectionCortisol' function
#' @param calibVar
#' The calibVar variable from the data.
#' @export
#' @examples
#' dat = data.frame(N = 1:10,
#'                  CPM = c(44396.5, 47774.5, 23676.0, 24290.3, 23541.0,
#'                          20108.7, 20101.3, 19383.7, 17013.7, 17678.7),
#'                  STAND = c(NA, NA, 0, 0, 1,
#'                            1, 3, 10, 30, 100),
#'                  bindings = c(445.7136, 479.6267, 237.6925, 243.8597,
#'                               236.3372, 201.8790, 201.8047, 194.6005,
#'                               170.8071, 177.4833),
#'                  logVals = c(-1.13, -1.34, -1.18, 0.1, .08,
#'                              .32, -1.47, -1.65, -1.61, -.74),
#'                  logStd = c(0, 0, 0, 1.09, 1.09,
#'                             1.1, 2.30, 2.30, 2.30, 3.40))
#' backEstimateCortisol(dat, "STAND")
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 %+%
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 scale_x_continuous

backEstimateCortisol = function(dat, calibVar) {

  dat0 = dat[dat[["logStd"]] >= 0, ]

  backEstMod = lm(dat0[["logStd"]] ~ dat0[["logVals"]], data = dat0)

  logStdConstant = backEstMod$coefficients[[1]]

  logStdCoef = backEstMod$coefficients[[2]]

  estConc = 2.71828 ^ (logStdConstant + logStdCoef * dat[["logVals"]])

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
