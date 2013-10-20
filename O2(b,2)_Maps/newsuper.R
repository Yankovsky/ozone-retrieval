data <- read.csv('science/Maps.csv', sep=';', check.names=F)
data.rev <- apply(data, 2, rev)

library(akima)
library(fields)
library(rgl)
library(MBA)

heights <- as.numeric(rownames(data.rev))
lats <- as.numeric(colnames(data.rev))
colnames(data.rev) <- NULL
#filled.contour(heights, lats, data.rev, col=c(4, 2))
#contour(heights, lats, data.rev)

#persp(heights, lats, data.rev)

#  open renderer
#open3d()
#  plot surface
#rgl.surface( heights, lats , unlist(data.rev))

heights <- rep(heights, ncol(data))
lats <- as.vector(sapply(lats, function(x) rep(x, nrow(data))))
data.rev <- as.vector(data.rev * 500000000)
splined <- interp(heights, lats, data.rev, 
                  xo=seq(min(heights), max(heights), length = 50),
                  yo=seq(min(lats), max(lats), length = 50),
                  linear=T, extrap=F)
rgl.surface(splined$x,splined$y,splined$z,color="green",alpha=c(0.5))