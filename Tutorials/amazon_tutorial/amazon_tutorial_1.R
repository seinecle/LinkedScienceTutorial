# linked Amazon in R, part I
# by Benedikt Gräler, ben.graeler@uni-muenster.de
# and Willem Robert van Hage, W.R.van.Hage@vu.nl
library(SPARQL) # version 1.9
library(sp)

# Linked Science endpoint
# endpoint <- "http://spatial.linkedscience.org/sparql"
# options <- NULL

# Local Jena Fuseki setup
endpoint <- "http://localhost:3030/amazon/sparql"
options <- "output=xml"

# defining a first querry as spatial setup
q <- "SELECT ?cell ?row ?col ?polygon
WHERE { 
   ?cell a <http://spatial.linkedscience.org/lsv/ns#Item> ;
         <http://spatial.linkedscience.org/context/amazon/Lin> ?row ;
         <http://spatial.linkedscience.org/context/amazon/Col> ?col ;
         <http://observedchange.com/tisc/ns#geometry> ?polygon .
}"

# getting the data piece-wise to reduce the XML's size

res <- SPARQL(url=endpoint, query=q, extra=options)$results

for(var in c("DEFOR_2002", "DEFOR_2003", "DEFOR_2004", "DEFOR_2005", 
             "DEFOR_2006", "DEFOR_2007", "DEFOR_2008")) {
  tmp_q <- paste("SELECT ?cell ?", var, " ",
                 "WHERE { ?cell a <http://spatial.linkedscience.org/lsv/ns#Item> ; ",
                                "<http://spatial.linkedscience.org/context/amazon/", var, "> ?", var, " . }\n",
                 sep="")
  cat(tmp_q)
  res <- merge(res, SPARQL(endpoint, tmp_q)$results, by="cell")
}

# creating the SpatialPixelsDataFrame
amazon <- res
amazon$row <- -res$row # swapping the y-axis

coordinates(amazon) <- ~ col+row
gridded(amazon) <- TRUE

# single map
spplot(amazon, "DEFOR_2002", col.regions=rev(heat.colors(17))[-1], at=(0:16)/100,
       main="relative deforestation per pixel during 2002")

# time series of maps
spplot(amazon, c("DEFOR_2002", "DEFOR_2003", "DEFOR_2004", "DEFOR_2005", 
                 "DEFOR_2006", "DEFOR_2007","DEFOR_2008"), 
       col.regions=rev(heat.colors(26))[-1], at=(0:20)/80, as.table=TRUE,
       main="relative deforestation per pixel")

# cumulative deforestation per year
# assuming grid cells of 25km x 25km
cumDefor <- apply(amazon@data[,-c(1,2)],2,function(x) sum(x)*25*25) 

plot(2002:2008,cumDefor,type="b", col="blue", ylab="Deforestation [km?]",
     xlab="year", main="Deforestation from 2002 to 2008", ylim=c(0,26000))

