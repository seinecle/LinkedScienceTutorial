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
rtAmazon <- rankTransform(as.matrix(subAmazon@data[subAmazon@data[,34]>0,c(32,33,34)]))
str(rtAmazon)
## population vs rel. defor.
BiCopSelect(rtAmazon[,1],rtAmazon[,3],familyset=1) # Frank copula

dependencePlot(smpl=rtAmazon[,c(1,3)])
perspCopula(frankCopula(0.275), dCopula, 
            ticktype="detail", zlim=c(0.6,1.4))
perspCopula(normalCopula(0.033),dCopula,
            ticktype="detail",zlim=c(0.6,1.4))

sum(dCopula(rtAmazon[,c(1,3)], frankCopula(0.275), log=T)) # 1.90
sum(dCopula(rtAmazon[,c(1,3)],normalCopula(0.033), log=T)) # 0.93

asCopFit <- fitCopula(asCopula(param=c(1,-1)),rtAmazon[,c(1,3)],method="ml")@copula
sum(log(dCopula(rtAmazon[,c(1,3)], asCopFit))) # 2.74
cqsCopFit <- fitCopula(cqsCopula(param=c(1,-1)),rtAmazon[,c(1,3)],method="ml")@copula
sum(log(dCopula(rtAmazon[,c(1,3)], cqsCopFit))) # 2.26

perspCopula(asCopFit, dCopula,
            ticktype="detail", zlim=c(0.6,1.4))

perspCopula(cqsCopFit, dCopula,
            ticktype="detail", zlim=c(0.6,1.4))

## inf. dis. vs rel. defor.
BiCopSelect(rtAmazon[,2],rtAmazon[,3]) # Frank copula

perspCopula(frankCopula(0.534), dCopula, 
            ticktype="detail", zlim=c(0.5,2))
perspCopula(normalCopula(0.085),dCopula,
            ticktype="detail",zlim=c(0.5,2))

sum(dCopula(rtAmazon[,c(2,3)], frankCopula(0.534), log=T)) # 6.24
sum(dCopula(rtAmazon[,c(2,3)],normalCopula(0.085), log=T)) # 4.76

asCopFit <- fitCopula(asCopula(param=c(1,-1)),rtAmazon[,c(2,3)],method="ml")@copula
sum(log(dCopula(rtAmazon[,c(2,3)], asCopFit))) # 10.18
cqsCopFit <- fitCopula(cqsCopula(param=c(1,-1)),rtAmazon[,c(2,3)],method="ml")@copula
sum(log(dCopula(rtAmazon[,c(2,3)], cqsCopFit))) # 6.24

perspCopula(asCopFit, dCopula,
            ticktype="detail", zlim=c(0,2))
perspCopula(cqsCopFit, dCopula,
            ticktype="detail", zlim=c(0,2))