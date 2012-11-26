## deaths

# defining a first querry 
q <- "PREFIX amazon: <http://spatial.linkedscience.org/context/amazon/>
PREFIX tisc:   <http://observedchange.com/tisc/ns#>
PREFIX owl:    <http://www.w3.org/2002/07/owl#>

SELECT DISTINCT ?muni
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
 ?overlapObject tisc:partialOverlapArea ?overlapArea
} GROUP BY ?muni">

ns <- c("amazon",  "http://spatial.linkedscience.org/context/amazon/", 
        "xsd",     "http://www.w3.org/2001/XMLSchema#",
        "dbpedia", "http://www.dbpedia.org/resource/",
        "tisc",    "http://observedchange.com/tisc/ns#",
        "owl",     "http://www.w3.org/2002/07/owl#")

  
municipalities <- SPARQL(endpoint, q, ns=ns, extra=list(output="csv"), format="csv")$result

# prefix workaround
# levels(municipalities[[1]]) <- str_replace_all(levels(municipalities[[1]]), "http://spatial.linkedscience.org/context/amazon/","")
# levels(municipalities[[2]]) <- str_replace_all(levels(municipalities[[2]]), "http://spatial.linkedscience.org/context/amazon/","")

# linking to deforestation data & borders
subMuni <- municipalities[match(muniDefor$muni,municipalities[[2]]),]

# get data on deaths from all relevant municipalities
deathData <- NULL
for (muni in subMuni$muni) { # muni <- subMuni$muni[1]
  tmpQ<- paste("prefix amazon: <http://spatial.linkedscience.org/context/amazon/>
prefix dbpedia: <http://www.dbpedia.org/resource/>

  select ?num ?reason ?year where { ", muni," amazon:hasObservation ?obs .
    { ?obs amazon:amountProduced ?num .
      ?obs amazon:isAbout ?reason .
      ?obs amazon:year ?year .
    }
  }", sep="") 
  tmpres <- SPARQL(endpoint, tmpQ, ns=ns, extra=list(output="csv"), format="csv")$result
  tmpres <- cbind(rep(muni,nrow(tmpres)),tmpres)
  deathData <- rbind(deathData, tmpres)
}

# levels(deathData$reason) <- str_replace_all(levels(deathData$reason), "http://www.dbpedia.org/resource/","dbpedia:")
# once: 61.99, each using ns: 64.43

q <- "PREFIX amazon:  <http://spatial.linkedscience.org/context/amazon/>
PREFIX tisc:     <http://observedchange.com/tisc/ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
prefix dbpedia: <http://dbpedia.org/resource/>
prefix w3dbpedia: <http://www.dbpedia.org/resource/>

SELECT distinct ?aka ?muni ?deaths ?year ?reason ?popul
WHERE
{
 ?cell a <http://linkedscience.org/lsv/ns#Item> .
 ?overlapObject tisc:partialOverlapFrom ?cell .
 ?overlapObject tisc:partialOverlapTo ?muni .
 ?overlapObject tisc:partialOverlapArea ?overlapArea .
 ?aka owl:sameAs ?muni .
 ?aka amazon:hasObservation ?obs .
 ?obs amazon:observationType w3dbpedia:Death .
 ?obs amazon:amountProduced ?deaths .
 ?obs amazon:isAbout ?reason .
 ?obs amazon:year ?year .
 ?aka amazon:hasObservation ?obsPop .
 ?obsPop amazon:observationType dbpedia:Population .
 ?obsPop amazon:year ?year .
 ?obsPop amazon:population ?popul .
}"

system.time(deathData <- SPARQL(endpoint,q,ns=ns, extra=list(output="csv"), format="csv")$result)
# 

write.table(deathData, file="deathData.csv")

  ?aka amazon:hasObservation ?obs .
?obs amazon:observationType w3dbpedia:Death .
?obs amazon:amountProduced ?deaths .
?obs amazon:isAbout ?reason .
?obs amazon:year ?year .

str(deathData)


str(deathData)
colnames(deathData) <- c("muni","deaths","reason","year")

deathData$aka <- NULL
deathData$aka <- subMuni$aka[match(deathData$muni,subMuni$muni)]

for (reason in levels(deathData$reason)) { # reason <- levels(deathData$reason)[1]
  res <- rep(NA,nrow(amazon@data))
  uniqueYears <- unique(deathData$year)
  uniqueYears <- uniqueYears[which(uniqueYears %in% c("2002","2003","2004","2005","2006","2007","2008") )]
  for(year in uniqueYears) { # year <- uniqueYears[1]
    sel <- deathData$year==year & deathData$reason==reason
    ids <- match(amazon@sp$muni, deathData[sel,]$aka)
    tId <- which(c("2002","2003","2004","2005","2006","2007","2008") %in% year)
    res[((tId-1)*length(amazon@sp)+1):(tId*length(amazon@sp))] <- deathData[sel,][ids,]$deaths
  }
  amazon@data[[reason]] <- res
}

summary(amazon@data)

## for population data

# get data on deaths from all relevant municipalities
populData <- NULL
for (muni in subMuni$muni) { # muni <- subMuni$muni[1]
  tmpQ<- paste("prefix amazon: <http://spatial.linkedscience.org/context/amazon/>
  select ?popul ?year where { 
    amazon:",muni," amazon:hasObservation ?obs .
    { ?obs amazon:population ?popul .
      ?obs amazon:year ?year .
    }
  }", sep="") 
  tmpres <- SPARQL(endpoint, tmpQ, ns=ns, extra=list(output="csv"), format="csv")$result
  tmpres <- cbind(rep(muni,nrow(tmpres)),tmpres)
  populData <- rbind(populData, tmpres)
}

colnames(populData)
<- c("muni","deaths","reason","year")

deathData$aka <- NULL
deathData$aka <- subMuni$aka[match(deathData$muni,subMuni$muni)]

for (reason in levels(deathData$reason)) { # reason <- levels(deathData$reason)[1]
  res <- rep(NA,nrow(amazon@data))
  uniqueYears <- unique(deathData$year)
  uniqueYears <- uniqueYears[which(uniqueYears %in% c("2002","2003","2004","2005","2006","2007","2008") )]
  for(year in uniqueYears) { # year <- uniqueYears[1]
    sel <- deathData$year==year & deathData$reason==reason
    ids <- match(amazon@sp$muni, deathData[sel,]$aka)
    tId <- which(c("2002","2003","2004","2005","2006","2007","2008") %in% year)
    res[((tId-1)*length(amazon@sp)+1):(tId*length(amazon@sp))] <- deathData[sel,][ids,]$deaths
  }
  amazon@data[[reason]] <- res
}


summary(amazon@data)

# slecting years 2005, 2006, 2007
subAmazon <- amazon[,4:6]

# calculating all Pearson correlation coefficients
corLine <- cor(subAmazon@data[,1],subAmazon@data[,-1],use="pairwise.complete")

# selecting min and max
which.min(corLine)
which.max(corLine)
cor.test(subAmazon@data[,1],subAmazon@data[,10],use="pairwise.complete") # p=0.25
cor.test(subAmazon@data[,1],subAmazon@data[,19],use="pairwise.complete") # p=0.35

# calculating all Kendall correlation coefficients
corLine <- cor(subAmazon@data[,1], subAmazon@data[,-1],
               method="kendall", use="pairwise.complete")

# selecting min and max
which.min(corLine) # 20
which.max(corLine[-c(16)]) # 16, 27,
cor.test(subAmazon@data[,1],subAmazon@data[,21],method="kendall",use="pairwise.complete") # p=0.27
cor.test(subAmazon@data[,1],subAmazon@data[,17],method="kendall",use="pairwise.complete") # p<0.001
cor.test(subAmazon@data[,1],subAmazon@data[,28],method="kendall",use="pairwise.complete") # p=0.123

# the only significant correlation
colnames(subAmazon@data)[17] # Infectious_disease

# all Kendall correlation coefficients
barchart(corLine[,], origin=0,col="darkgrey",
         main="Kendall's tau for deforestation rates in Amazonian Municipalities for 2005, 2006 and 2007",
         xlab="Kendall's tau coorelation coefficient")

# a map of deaths for two years
stplot(subAmazon[,1:2,"Infectious_disease"],col.regions=bpy.colors(),
       main="number of death by infectious disease per municipality",
       at=c(0,20,40,60,80,100,120,140,160,180,200,500,800))

# save the final STFDF object
save(subAmazon,file="amazon_deaths.RData")