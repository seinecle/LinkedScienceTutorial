# by Benedikt Gräler (ben.graeler@uni-muenster.de)
# 
install.packages("spcopula", repos="http://R-Forge.R-project.org")
library(spcopula)

## fix perspCopula, from package copula
perspCopula <- function (x, fun, n = 51, theta = -30, phi = 30, 
                         expand = 0.618, ...) {
  eps <- (.Machine$double.eps)^(1/6)
  xis <- yis <- seq(0 + eps, 1 - eps, len = n)
  grids <- as.matrix(expand.grid(xis, yis, KEEP.OUT.ATTRS = FALSE))
  zmat <- matrix(fun(grids, x), n, n)
  persp(xis, yis, zmat, theta = theta, phi = phi, expand = expand, 
        ...)
  invisible(list(x = xis, y = yis, z = zmat))
}
##

colnames(subAmazon@data)

# droping the margins through the rank-order-transformation, 
# for records with deforestation rate > 0
rtAmazon <- rankTransform(as.matrix(subAmazon@data[subAmazon@data[,35]>0,c(32,35)]))
str(rtAmazon)

## population vs rel. defor.
# copula estimation
BiCopSelect(rtAmazon[,1],rtAmazon[,2]) # Frank copula

# plot of sample
plot(rtAmazon,asp=1)
dependencePlot(smpl=rtAmazon)

# density of dofferent copulas
# Frank:
perspCopula(frankCopula(0.30), dCopula, 
            ticktype="detail", zlim=c(0.6,1.4))
# Gaussian
perspCopula(normalCopula(0.04),dCopula,
            ticktype="detail",zlim=c(0.6,1.4))

# calculate log-likelihood
sum(dCopula(rtAmazon, frankCopula(0.275), log=T)) # 2.31
sum(dCopula(rtAmazon,normalCopula(0.033), log=T)) # 1.19

# fit additional copulas
asCopFit <- fitCopula(asCopula(param=c(1,-1)),rtAmazon,method="ml")@copula
sum(log(dCopula(rtAmazon, asCopFit))) # 3.47
cqsCopFit <- fitCopula(cqsCopula(param=c(1,-1)),rtAmazon,method="ml")@copula
sum(log(dCopula(rtAmazon, cqsCopFit))) # 3.16

# density plots
perspCopula(asCopFit, dCopula,
            ticktype="detail", zlim=c(0.6,1.4))

perspCopula(cqsCopFit, dCopula,
            ticktype="detail", zlim=c(0.6,1.4))