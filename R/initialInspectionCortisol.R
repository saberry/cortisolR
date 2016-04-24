#' Initial Data Inspection
#'
#' @description This function does an intial inspection on the data.
#'
#' @usage initialInspectCortisol(dat, calibName, calibValue, nVar, drop)
#' @param dat
#' A data frame
#' @param calibVar
#' The name of the calibration variable (usually STAND or A)
#' @param calibValue
#' The value for calibration (usually 0)
#' @param cpmVar
#' The name for the CPM variable (usually CPM)
#' @param idVar
#' The ID variable; is often "N".
#' @param drop
#' Which participant to drop.
#' @export
#'
#' @examples
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 %+%
#' @importFrom ggplot2 aes

initialInspectCortisol = function(dat, calibVar, calibValue, cpmVar,
                          idVar = NULL, drop = NULL) {

  if(calibVar %in% names(dat) == FALSE) stop(paste(calibVar, "is not in your data.", sep = " "))

  if (is.null(drop)) {
    initialDat = dat[dat[, calibVar] %in% calibValue, ]
  } else {
    initialDat = dat[dat[, calibVar] %in% calibValue & corDat[, idVar] != drop, ]
    if(idVar %in% names(dat) == FALSE) stop(paste(idVar, "is not in your data.", sep = " "))
  }

  cpmMean = mean(dat[[cpmVar]], na.rm = TRUE)

  bindings = (dat[[cpmVar]]/cpmMean) * 100

  logVals = log((100 - bindings) / bindings)

  logStd = log(dat[[calibVar]])

  plotDat = cbind(dat, bindings, logVals, logStd)

  logStdMod = lm(logVals ~ logStd, data = plotDat[plotDat$logStd >= 0, ])

  logStdConstant = logStdMod$coefficients[[1]]

  logStdCoef = logStdMod$coefficients[[2]]

  logModSum = summary(logStdMod)

  logModRSqr = logModSum$adj.r.squared

  resultReturn = paste("Your R-square is", round(logModRSqr, 5))

  p1 = ggplot(plotDat, aes(logStd, plotDat[[calibVar]])) +
    geom_smooth(span = .5) +
    geom_point() +
    scale_y_continuous(name = "STAND") +
    ggtitle(resultReturn) +
    ggtheme()

  p2 = ggplot(plotDat, aes(logVals, plotDat[[calibVar]])) +
    geom_point() +
    scale_y_continuous(name = "STAND") +
    ggtheme()

  p3 = ggplot(plotDat, aes(logVals, plotDat[[calibVar]])) +
    geom_smooth(method = "lm") +
    geom_point() +
    scale_y_continuous(name = "STAND") +
    ggtheme()

  p4 = ggplot(plotDat, aes(logVals, logStd)) +
    geom_smooth() +
    geom_point() +
    ggtheme()

  p5 = ggplot(plotDat, aes(logVals, logStd)) +
    geom_point() +
    ggtheme()

  multiplot(p1, p2, p3, p4, p5)

  return(plotDat)
}
