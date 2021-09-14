# Geomaker anims

library(ggplot2)

geomaker <- function(savename, angle_offset, nPoints, xrparams, yrparams) {
  angle = seq(0.5 * angle_offset * 2 * pi, (1 + 0.5 * angle_offset) * 2 * pi, length.out = nPoints + 1);
  r = xrparams[1] + xrparams[2] * cos(xrparams[3] * (angle + xrparams[4] * 2 * pi))
  r2 = yrparams[1] + yrparams[2] * cos(yrparams[3] * (angle + yrparams[4] * 2 * pi))
  x = r * cos(angle);
  y = r2 * sin(angle);
  D <- data.frame(x, y);
  p0 <- ggplot(D, aes(x = x, y = y)) + geom_polygon(fill="white", colour="black", size=5)
  p0 <- p0 + theme_void() + theme(legend.position="none") + theme(aspect.ratio=1)
  if (savename != '') {
    ggsave(savename, plot = p0, dpi=72)
  }
  print(p0)
}

zstr0 <- function(val, len0) {
  str0 <- ''
  for (pow0 in 1:len0) {
    valmod <- val %% 10;
    val <- floor(val / 10);
    str0 <- paste0(valmod, str0)
  }
  return(str0)
}

minNPoints <- 199
maxNPoints <- 200
nPoses <- 2 + 1
nSteps <- 200
maxF1 <- 8
maxF2 <- 8

offset_00 <- 0
nPoints_00 <- 4
xrparams_00 <- c(1, 0, 0, 0.5)
yrparams_00 <- c(1, 0, 0, 0.25)

offset_1 <- offset_00
nPoints_1 <- nPoints_00
xrparams_1 <- xrparams_00
yrparams_1 <- yrparams_00
iFrame <- 1
for (iPose in 1:nPoses) {
  if (iPose == nPoses){
    offset_2 <- offset_00
    nPoints_2 <- nPoints_00
    xrparams_2 <- xrparams_00
    yrparams_2 <- yrparams_00
  } else {
    offset_2 <- 2 * pi * runif(1)
    nPoints_2 <- minNPoints + sample.int(maxNPoints - minNPoints, size=1)
    if (nPoints_2 == 8) {
      nPoints_2 = 200
    }
    xrparams_2 <- runif(4) * c(1, 1, maxF1, 1)
    yrparams_2 <- runif(4) * c(1, 1, maxF2, 1)
  }
  for (iStep in 1:nSteps) {
    prop0 <- (iStep - 1) / nSteps
    offset_0 <- offset_1 + (offset_2 - offset_1) * prop0;
    nPoints_0 <- floor(nPoints_1 + (nPoints_2 - nPoints_1) * prop0);
    xrparams_0 <- xrparams_1 + (xrparams_2 - xrparams_1) * prop0;
    yrparams_0 <- yrparams_1 + (yrparams_2 - yrparams_1) * prop0;
    geomaker(paste0('anim3_', zstr0(iFrame, 8), '.png'), offset_0, nPoints_0, xrparams_0, yrparams_0)
    iFrame <- iFrame + 1
  }
  offset_1 <- offset_2
  nPoints_1 <- nPoints_2
  xrparams_1 <- xrparams_2
  yrparams_1 <- yrparams_2
}
