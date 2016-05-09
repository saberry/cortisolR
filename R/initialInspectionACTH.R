
acth <- read.csv('R/ACTH.csv', stringsAsFactors = F)

initialInspectACTH = function(dat, calibName, cpmName,
                                  nVar = NULL, drop = NULL) {

  if(calibName %in% names(dat) == FALSE) stop(paste(calibName, "is not in your data.", sep = " "))

#  if (is.null(drop)) {
#    initialDat = dat[dat[, calibName] %in% calibValue, ]
#  } else {
#    initialDat = dat[dat[, calibName] %in% calibValue & corDat[, nVar] != drop, ]
#    if(nVar %in% names(dat) == FALSE) stop(paste(nVar, "is not in your data.", sep = " "))
#  }



  p1 = ggplot(dat, aes(dat[[cpmName]], dat[[calibName]])) +
    geom_smooth(method = "lm") + xlab('CPM') + ylab('STAND') +
    geom_point() + ggtheme()

  p2 = ggplot(dat, aes(dat[[cpmName]], dat[[calibName]])) +
    geom_smooth() + xlab('CPM') + ylab('STAND') +
    geom_point() + ggtheme()

  p3 = ggplot(dat, aes(dat[[calibName]], dat[[cpmName]])) +
    geom_smooth() + ylab('CPM') + xlab('STAND') +
    geom_point() + ggtheme()




  linear <- lm(dat[[cpmName]] ~ dat[[calibName]])
  quadratic <- lm(dat[[cpmName]] ~ I(dat[[calibName]] * dat[[calibName]]))
  quadlin <- lm(dat[[cpmName]] ~ dat[[calibName]] + I(dat[[calibName]] * dat[[calibName]]))


  assay_reg <- data.frame(regression = c('Linear-Only Adjusted', 'Quadratic Adjusted', 'Linear + Quadratic Adjusted'),
  rSquared = c(summary(linear)[[9]], summary(quadratic)[[9]], summary(quadlin)[[9]]))

  p4 <- tableGrob(assay_reg, rows = NULL)

  grid.arrange(p1, p2, p3, p4, ncol=1)


  estconc <- (-117.393 + (sqrt(13781.116 - (-0.184 *(788.602-dat[[cpmName]])))))/ (-0.092)

  q1 <- ggplot(dat, aes(estconc, dat[[calibName]])) +
    geom_smooth(method = "lm") + xlab('ESTCONC') + ylab('STAND') + ylim(0, 1500) +
    geom_point() +
    ggtheme()

  plot(q1)
}

