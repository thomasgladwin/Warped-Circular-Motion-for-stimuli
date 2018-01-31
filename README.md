# geomaker

This is a simple but very generalizable function to parametrically generate abstract geometric stimuli. Useful for conditioning experiments for instance. The implementation is in R and also plots the figure using the ggplot2 library.

The function requires: library(ggplot2).

The parameters are: save-name, base-angle [in proportion of PI], and number of vertices, and then two subsets of parameters: base-radius, and amplitude, frequency, and phase [in proportion of 2 PI] of radius oscillations.
  
geomaker <- function(savename, angle_offset, nPoints, xrparams, yrparams) {

  angle = seq(0.5 * angle_offset * 2 * pi, (1 + 0.5 * angle_offset) * 2 * pi, length.out = nPoints + 1);

  r = xrparams[1] + xrparams[2] * cos(xrparams[3] * (angle + xrparams[4] * 2 * pi))
  
  r2 = yrparams[1] + yrparams[2] * cos(yrparams[3] * (angle + yrparams[4] * 2 * pi))
  
  x = r * cos(angle);
  
  y = r2 * sin(angle);
  
  D <- data.frame(x, y);
  
  p0 <- ggplot(D, aes(x = x, y = y)) + geom_polygon(fill="white", colour="black", size=5)
  
  p0 <- p0 + theme_void() + theme(legend.position="none") + theme(aspect.ratio=1)
  
  ggsave(savename, plot = p0)
  
  print(p0)
  
}

It's all based on warped circles, but that encompasses triangles and squares through very squiggly shapes.

Some examples are:

Basic shapes:

Triangle upright: geomaker('test.png', 1/2, 3, c(1, 0, 0, 0), c(1, 0, 0, 0))

Triangle pointing left: geomaker('test.png', 1, 3, c(1, 0, 0, 0.5), c(1, 0, 0, 0.25))

Square: geomaker('test.png', 1/4, 4, c(1, 0, 0, 0.5), c(1, 0, 0, 0.25))

Diamond: geomaker('test.png', 0, 4, c(1, 0, 0, 0.5), c(1, 0, 0, 0.25))

Circle: geomaker('test.png', 0, 200, c(1, 0, 0, 0), c(1, 0, 0, 0))

Complex shapes:

Wobbly circle: geomaker('test.png', 0, 200, c(0.9, 0.1, 16, 0.5), c(0.9, 0.1, 16, 0.5))

Soft cross: geomaker('test.png', 0, 200, c(0.7, 0.3, 4, 0), c(0.7, 0.3, 4, 0))

Goat head: geomaker('test.png', 0, 200, c(1.5, 0.5, 5, -0.25), c(0.5, 1.5, 3, 0.25))

Frog: geomaker('test.png', 0, 200, c(0.5, 0.5, 5, 0), c(1.5, 0.5, 3, 0))

Shark attack: geomaker('test.png', 0, 200, c(1, 0, 2, 0), c(0.5, 0.35, 20, 0))

Seemingly random squiggles: geomaker('test.png', 0, 400, c(5, 30, 4, 0.15), c(10.5, 10.35, 20, 0))

And so on!

[![DOI](https://zenodo.org/badge/108257987.svg)](https://zenodo.org/badge/latestdoi/108257987)

Please cite as: Thomas Edward Gladwin. (2017, October 25). thomasgladwin/geomaker: v1. Zenodo. http://doi.org/10.5281/zenodo.1036613
