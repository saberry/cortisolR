#' Initial Data Inspection
#'
#' @description This function does an intial inspection on the data.
#'
#' @usage initialInspect(dat, calibName, calibValue, nVar, drop)
#' @param dat
#' A data frame
#' @param calibName
#' The name of the calibration variable (usually STAND or A)
#' @param calibValue
#' The value for calibration (usually 0)
#' @param nVar
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
initialInspect = function(dat, calibName, calibValue,
                          nVar = NULL, drop = NULL) {

  if (is.null(drop)) {
    initialDat = dat[dat[, calibName] %in% calibValue, ]
  } else {
    initialDat = dat[dat[, calibName] %in% calibValue & corDat[, nVar] != drop, ]
  }

  cpmMean = mean(initialDat$CPM)

  bindings = (dat$CPM/cpmMean) * 100

  logVals = log((100 - bindings) / bindings)

  logStd = log(dat$STAND)

  plotDat = cbind(dat, bindings, logVals, logStd)

  plotDat = plotDat


  p1 = ggplot(plotDat, aes(logStd, STAND)) +
    geom_smooth(span = .5) +
    geom_point() +
    ggtheme

  p2 = ggplot(plotDat, aes(logVals, STAND)) +
    geom_point() +
    ggtheme

  p3 = ggplot(plotDat, aes(logVals, logStd)) +
    geom_smooth(method = "lm") +
    geom_point() +
    ggtheme

  p4 = ggplot(plotDat, aes(logVals, logStd)) +
    geom_smooth() +
    geom_point() +
    ggtheme

  p5 = ggplot(plotDat, aes(logVals, logStd)) +
    geom_point() +
    ggtheme

  multiplot(p1, p2, p3, p4, p5)

  logStdMod = lm(logVals ~ logStd, data = plotDat[plotDat$logStd >= 0, ])

  logStdConstant = logStdMod$coefficients[[1]]

  logStdCoef = logStdMod$coefficients[[2]]

  logModSum = summary(logStdMod)

  logModRSqr = logModSum$adj.r.squared

  paste("Your R-square is", round(logModRSqr, initialInspect = function(dat, calibName, calibValue,
                                                                        nVar = NULL, drop = NULL) {

    if (is.null(drop)) {
      initialDat = dat[dat[, calibName] %in% calibValue, ]
    } else {
      initialDat = dat[dat[, calibName] %in% calibValue & corDat[, nVar] != drop, ]
    }

    cpmMean = mean(initialDat$CPM)

    bindings = (dat$CPM/cpmMean) * 100

    logVals = log((100 - bindings) / bindings)

    logStd = log(dat$STAND)

    plotDat = cbind(dat, bindings, logVals, logStd)

    plotDat = plotDat


    p1 = ggplot(plotDat, aes(logStd, STAND)) +
      geom_smooth(span = .5) +
      geom_point() +
      ggtheme

    p2 = ggplot(plotDat, aes(logVals, STAND)) +
      geom_point() +
      ggtheme

    p3 = ggplot(plotDat, aes(logVals, logStd)) +
      geom_smooth(method = "lm") +
      geom_point() +
      ggtheme

    p4 = ggplot(plotDat, aes(logVals, logStd)) +
      geom_smooth() +
      geom_point() +
      ggtheme

    p5 = ggplot(plotDat, aes(logVals, logStd)) +
      geom_point() +
      ggtheme

    multiplot(p1, p2, p3, p4, p5)

    logStdMod = lm(logVals ~ logStd, data = plotDat[plotDat$logStd >= 0, ])

    logStdConstant = logStdMod$coefficients[[1]]

    logStdCoef = logStdMod$coefficients[[2]]

    logModSum = summary(logStdMod)

    logModRSqr = logModSum$adj.r.squared

    paste("Your R-square is", round(logModRSqr, 4))
  }))
}
