# demo script by Benedikt Gräler (ben.graeler@uni-muenster.de)
# and Jim Jones (jim.jones@uni-muenster.de)

library(SPARQL) # version > 1.9
library(spacetime)
library(lattice)

# setting the endpoint
# endpoint <- "http://spatial.linkedscience.org/sparql"
endpoint <- "http://localhost:3030/amazon_big/sparql"

# defining a first query as spatial setup
q <- "PREFIX amazon: <http://spatial.linkedscience.org/context/amazon/>
PREFIX tisc:   <http://observedchange.com/tisc/ns#>
PREFIX owl:    <http://www.w3.org/2002/07/owl#>

SELECT DISTINCT ?muni ?aka
(SUM (?overlapArea) AS ?area)
(SUM (?defor2002 * ?overlapArea) AS ?def02)
(SUM (?defor2003 * ?overlapArea) AS ?def03) 
(SUM (?defor2004 * ?overlapArea) AS ?def04) 
(SUM (?defor2005 * ?overlapArea) AS ?def05)
(SUM (?defor2006 * ?overlapArea) AS ?def06) 
(SUM (?defor2007 * ?overlapArea) AS ?def07)
(SUM (?defor2008 * ?overlapArea) AS ?def08)
WHERE
{
 ?cell amazon:DEFOR_2002 ?defor2002 .
 ?cell amazon:DEFOR_2003 ?defor2003 .
 ?cell amazon:DEFOR_2004 ?defor2004 .
 ?cell amazon:DEFOR_2005 ?defor2005 .
 ?cell amazon:DEFOR_2006 ?defor2006 .
 ?cell amazon:DEFOR_2007 ?defor2007 .
 ?cell amazon:DEFOR_2008 ?defor2008 .
 ?overlapObject tisc:partialOverlapFrom ?cell .
 ?overlapObject tisc:partialOverlapTo ?muni .
 ?overlapObject tisc:partialOverlapArea ?overlapArea .
 ?aka owl:sameAs ?muni .
} GROUP BY ?muni ?aka"
  
ns <- c("amazon","http://spatial.linkedscience.org/context/amazon/")
muniDefor <- SPARQL(endpoint, q, ns=ns, extra=list(output="csv"), format="csv")$result

str(muniDefor)

# query the municipalties' geometries
muniDefor$geometry <- as.character(1:nrow(muniDefor))
for(i in 1:nrow(muniDefor)) { # i <- 790
  q_muni <- paste("PREFIX amazon: <http://spatial.linkedscience.org/context/amazon/>
                  PREFIX tisc: <http://observedchange.com/tisc/ns#>  
                  SELECT ?geom where { ",muniDefor$muni[i]," tisc:geometry ?geom }",sep="")
  muniDefor$geometry[i] <- SPARQL(endpoint, q_muni, extra=list(output="csv"), format="csv")$result
}

muniDefor$geometry <- lapply(muniDefor$geometry, as.character)

# construct polygons
allPolys <- NULL
for ( i in 1:nrow(muniDefor)) { # i <- 1
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

# creating the STFDF
amazon <- STFDF(sp=spPoly,time=as.POSIXct(c("2002-01-01","2003-01-01","2004-01-01",
                                            "2005-01-01","2006-01-01","2007-01-01",
                                            "2008-01-01")),
                data=data.frame(defor=numeric(length(spPoly)*7)))

# adding municipality names, aka, and area to sp slot
amazon@sp <- SpatialPolygonsDataFrame(amazon@sp, muniDefor[1:3])
str(amazon@sp@data)

# adding deforestation values for 7 years
for(i in 1:7) { # i <- 1
  amazon@data$defor[((i-1)*length(amazon@sp)+1):(i*length(amazon@sp))] <- muniDefor[[i+3]]
}
str(amazon@data)

## spatio-temopral sample plot - takes some time
stplot(amazon[,2:3])

###############################
## death and population data ##
###############################

# tmpD <- paste("PREFIX amazon:  <http://spatial.linkedscience.org/context/amazon/>
#                 PREFIX dbpedia: <http://www.dbpedia.org/resource/>
#               PREFIX owl:     <http://www.w3.org/2002/07/owl#>
#               
#               SELECT ?deaths ?reason ?year where { \n",
#                 aka, " amazon:hasObservation ?obs .
#                 ?obs amazon:observationType dbpedia:Death .
#               ?obs amazon:amountProduced ?deaths .
#               ?obs amazon:isAbout ?reason .
#               ?obs amazon:year ?year .}", sep="") 
#   tmpDRes <- SPARQL(endpoint, tmpD, ns=c("dbpedia", "http://www.dbpedia.org/resource/"),
#                     extra=list(output="csv"), format="csv")$result
# tmpDRes <- cbind(rep(aka,nrow(tmpDRes)),tmpDRes)
# deathData <- rbind(deathData, tmpDRes)
# 
# tmpP <- paste("PREFIX amazon:  <http://spatial.linkedscience.org/context/amazon/>
#                 PREFIX dbpedia: <http://www.dbpedia.org/resource/>
#               PREFIX owl:     <http://www.w3.org/2002/07/owl#>
#               
#               SELECT ?popul ?year where { \n",
#                 aka, " amazon:hasObservation ?obs .
#                 ?obs amazon:observationType dbpedia:Population .
#               ?obs amazon:year ?year.
#               ?obs amazon:population ?popul .}", sep="")
#   tmpPRes <- SPARQL(endpoint, tmpP, extra=list(output="csv"), format="csv")$result
# tmpPRes <- cbind(rep(aka,nrow(tmpPRes)),tmpPRes)
# 
# populData <- rbind(populData, tmpPRes)

deathData <- NULL
populData <- NULL
for (aka in amazon@sp$aka) { # aka <- amazon@sp$aka[1]
  tmp <- paste("PREFIX amazon:  <http://spatial.linkedscience.org/context/amazon/>
                PREFIX dbpedia: <http://www.dbpedia.org/resource/>
                PREFIX owl:     <http://www.w3.org/2002/07/owl#>

                SELECT ?deaths ?reason ?year ?popul where { \n",
                aka, " amazon:hasObservation ?obs .
                ?obs amazon:observationType dbpedia:Death .
                ?obs amazon:amountProduced ?deaths .
                ?obs amazon:isAbout ?reason .
                ?obs amazon:year ?year .",
                aka, " amazon:hasObservation ?obsPop .
                ?obsPop amazon:observationType dbpedia:Population .
                ?obsPop amazon:year ?year.
                ?obsPop amazon:population ?popul .
                }", sep="") 
  tmpRes <- SPARQL(endpoint, tmp, ns=c("dbpedia", "http://www.dbpedia.org/resource/"),
                    extra=list(output="csv"), format="csv")$result

  tmpDRes <- cbind(rep(aka,nrow(tmpRes)),tmpRes[,1:3])
  deathData <- rbind(deathData, tmpDRes)
  
  tmpPRes <- cbind(rep(aka,5),tmpRes[(1:5)*30,c(4,3)])
  populData <- rbind(populData, tmpPRes)
}
colnames(deathData) <- c("aka","deaths","reason","year")
colnames(populData) <- c("aka","popul","year")

str(deathData)
str(populData)

## merging all the data
# deaths
for (reason in levels(deathData$reason)) { # reason <- levels(deathData$reason)[1]
  res <- rep(NA,nrow(amazon@data))
  uniqueYears <- unique(deathData$year)
  uniqueYears <- uniqueYears[which(uniqueYears %in% c("2002","2003","2004","2005","2006","2007","2008") )]
  for(year in uniqueYears) { # year <- uniqueYears[1]
    sel <- deathData$year==year & deathData$reason==reason # sum(sel) # should be 887
    ids <- match(amazon@sp$aka, deathData[sel,]$aka)
    tId <- which(c("2002","2003","2004","2005","2006","2007","2008") %in% year)
    res[((tId-1)*length(amazon@sp)+1):(tId*length(amazon@sp))] <- deathData[sel,][ids,]$deaths
  }
  amazon@data[[reason]] <- res
  rm(res)
}

# droping the prefix
colnames(amazon@data)[-1] <-  lapply(colnames(amazon@data)[-1], function(x) substr(x,9,nchar(x)))

# population
populLongTab <- NULL
for(year in 2002:2008) { # year <- 2002
  sel <- populData$year == year # sum(sel) # should be 887
  if(sum(sel)==0) 
    res <- rep(NA,length(amazon@sp))
  else
    res <- populData[sel,2]
  populLongTab <- c(populLongTab,res)
}
amazon@data[["Population"]] <- populLongTab
rm(populLongTab)

summary(amazon@data)
str(amazon@data)
subAmazon <- amazon[,4:6]
str(subAmazon@data)
colnames(subAmazon@data)

# correlation of deforestation rate and deaths/population
corLine <- cor(subAmazon@data[,1]/subAmazon@sp$area, cbind(subAmazon@data[,2:31]/subAmazon@data[,32],subAmazon@data[,32,drop=F]),
               use="pairwise",method="kendall")

barchart(corLine[,], origin=0,col="darkgrey",
         main="Kendall's tau for defor. rates and rel. deaths in Amazonian Municipalities for 2005, 2006 and 2007",
         xlab="Kendall's tau coorelation coefficient")

colnames(corLine)[which.max(corLine[-31])] # 31: Population, 16: Infectious_disease

# population
cor.test(subAmazon@data[,1]/subAmazon@sp$area, subAmazon@data[,32],
         method="kendall",use="pairwise.complete") # p< 0.01

# Infectious_disease
cor.test(subAmazon@data[,1]/subAmazon@sp$area, subAmazon@data[,17]/subAmazon@data[,32],
         method="kendall",use="pairwise.complete") # p< 0.01

subAmazon@data[["rel_Infect_dis"]] <- subAmazon@data[["Infectious_disease"]]/subAmazon@data[,32]
subAmazon@data[["Infect_dis_per_25"]] <- subAmazon@data[["Infectious_disease"]]/subAmazon@data[,32]*25
subAmazon@data[["rel_defor"]] <- subAmazon@data[["defor"]]/subAmazon@sp$area

plot(cbind(subAmazon@data[,"rel_defor",drop=F],subAmazon@data[,"rel_Infect_dis"]))

##
stplot(subAmazon[,1:2,"rel_Infect_dis"],col.regions=bpy.colors(),
       main="relative number of deaths by \"infectious disease\"")

spplot(subAmazon[,2],c("Infect_dis_per_25","rel_defor"),col.regions=bpy.colors(),
       main="deaths/25 caused by \"infectious disease\" vs deforestation rate in 2006")


