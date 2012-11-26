# linked Amazon in R, analysis by municipality
# by Benedikt Gräler, ben.graeler@uni-muenster.de
# adapted by Willem Robert van Hage, W.R.van.Hage@vu.nl
library(SPARQL) # version 1.9
library(spacetime)
library(lattice)

# setting the endpoint
#endpoint <- "http://spatial.linkedscience.org/sparql"
endpoint <- "http://localhost:3030/amazon_big/sparql"

# defining a first querry as spatial setup
q <- "prefix amazon: <http://spatial.linkedscience.org/context/amazon/>

SELECT * WHERE { 
?cell a <http://linkedscience.org/lsv/ns#Item> ;
      amazon:hasItsCentroidInsideOf ?muni ;
      amazon:DEFOR_2002 ?def02;
      amazon:DEFOR_2003 ?def03;
      amazon:DEFOR_2004 ?def04;
      amazon:DEFOR_2005 ?def05;
      amazon:DEFOR_2006 ?def06;
      amazon:DEFOR_2007 ?def07;
      amazon:DEFOR_2008 ?def08;
}"

pixelRes <- SPARQL(endpoint, q, extra=list(output="csv"), format="csv")$result

# sum up defor per municipality
pixelRes$id <- as.integer(pixelRes$muni)
muniDefor <- NULL
for(id in (1:length(levels(pixelRes$muni)))) { # id <- 1
  muniDefor <- rbind(muniDefor,apply(pixelRes[which(pixelRes$id==id),3:9],2,mean))
}

# define new collapsed data.frame
muniDefor <- as.data.frame(muniDefor)
muniDefor$muni <- levels(pixelRes$muni)

# querry the municipalties' geometries
muniDefor$geometry <- vector("character",length=nrow(muniDefor))
for(i in 1:nrow(muniDefor)) { # muni <- muniDefor$muni[1] i <- 790
  muni <- muniDefor$muni[i]
  q_muni <- paste("select ?c where { <",muni,"> <http://observedchange.com/tisc/ns#geometry> ?c }",sep="")
  muniDefor$geometry[i] <- SPARQL(endpoint, q_muni, extra=list(output="csv"), format="csv")$result
}

muniDefor$geometry <- lapply(muniDefor$geometry, as.character)

# construct polygons
allPolys <- NULL
for ( i in 1:nrow(muniDefor)) { # i <- 267
  if(i%%100 == 0) cat(i,"\n") # show progress
  
  tmpStr <- muniDefor$geometry[[i]]
  
  tmpPoly <- NULL
  for(j in 1:length(tmpStr)) { # j <- 1
    tmpPoly <- append(tmpPoly, 
                      Polygon(matrix(unlist(lapply(strsplit(strsplit(tmpStr[j],";")[[1]],","),as.numeric)),
                                     ncol=2,byrow=T)))
  }
  
  allPolys <- append(allPolys, Polygons(tmpPoly, ID=i))
}

# create SpatialPolygons object
spPoly <- SpatialPolygons(allPolys, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
plot(spPoly[267],axes=T) # check for split states/islands

# creating the STFDF
amazon <- STFDF(sp=spPoly,time=as.POSIXct(c("2002-01-01","2003-01-01","2004-01-01",
                                            "2005-01-01","2006-01-01","2007-01-01",
                                            "2008-01-01")),
                data=data.frame(defor=numeric(length(spPoly)*7)))

# match IDs - just in case, and reorder
polyMatch <- match(getSpPPolygonsIDSlots(spPoly),1:nrow(muniDefor))
muniDefor <- muniDefor[polyMatch,]

# adding municipality names to sp slot
amazon@sp <- SpatialPolygonsDataFrame(amazon@sp, muniDefor[8])

# adding deforestation values for 7 years
for(i in 1:7) { # i <- 1
  amazon@data$defor[((i-1)*length(amazon@sp)+1):(i*length(amazon@sp))] <- muniDefor[[i]]
}

## sample plots - take some time
# spatio-temopral
stplot(amazon[,2:7])

# spatial
spplot(amazon[,2],col.regions=bpy.colors())