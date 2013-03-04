library(SPARQL)
library(irlba)
library(Matrix)
library(lattice)
library(igraph)

endpoint <- "http://localhost:3030/lop/sparql"
options <- "output=xml"

prefix <- c("lop","http://semanticweb.cs.vu.nl/poseidon/ns/instances/",
            "eez","http://semanticweb.cs.vu.nl/poseidon/ns/eez/",
            "geo","http://www.geonames.org/ontology#",
            "skos","http://www.w3.org/2004/02/skos/core#",
            "wn30","http://purl.org/vocabularies/princeton/wn30/",
            "wn20schema","http://www.w3.org/2006/03/wn/wn20/schema/")

sparql_prefix <-
  "PREFIX sem: <http://semanticweb.cs.vu.nl/2009/11/sem/>
   PREFIX poseidon: <http://semanticweb.cs.vu.nl/poseidon/ns/instances/>
   PREFIX eez: <http://semanticweb.cs.vu.nl/poseidon/ns/eez/>
   PREFIX wgs84: <http://www.w3.org/2003/01/geo/wgs84_pos#>
   PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
   PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
   PREFIX geo: <http://www.geonames.org/ontology#>
   PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
   PREFIX wn30: <http://purl.org/vocabularies/princeton/wn30/>
   PREFIX wn20schema: <http://www.w3.org/2006/03/wn/wn20/schema/>
"

# get event description per region

q <- paste(sparql_prefix, 
  "SELECT *
   WHERE {
     ?event sem:eventType ?event_type .
     ?event sem:hasActor/sem:actorType ?actor_type .
     ?event sem:hasPlace/eez:inPiracyRegion ?region .
     ?event rdfs:comment ?text .
     FILTER regex(?text,'[a-z]')
     FILTER regex(?text,'^(?!For the full report)')
   }")

res <- SPARQL(endpoint,q,ns=prefix,extra=options)$results

tokenize <- function(text) {
  terms <- unlist(strsplit(text,'\\W'))
  lowercase <- tolower(terms)
  lowercase <- lowercase[which(lowercase!="")]
  freq_table <- table(lowercase)
  rm(terms)
  tokens <- sort(lowercase[!duplicated(lowercase)])
  return(list(dict=freq_table,tokens=tokens,all_tokens=lowercase))
}

all_text <- paste(res$text, collapse=" ")
all <- tokenize(all_text)

dict <- seq(1,length(all$dict))
names(dict) <- names(all$dict)

incidence <- Matrix(0,nrow=dim(all$dict),ncol=length(res$text))

# build inverted index, takes a while...
for (doc_i in seq(1,length(res$text))) {
  if (doc_i %% 100 == 0) cat("DOCUMENT",doc_i,"\n")
  d <- tokenize(res$text[doc_i])$dict
  for (term_j in names(d)) {
    incidence[dict[[term_j]],doc_i] <- d[term_j]
  }
}

rownames(incidence) <- names(dict)
colnames(incidence) <- paste(substring(res$text,1,10),"…")

# perform SVD and rank reduction
reduced_rank <- 5
lsa <- irlba(incidence,nu=reduced_rank,nv=reduced_rank)

smoothed <- lsa$u %*% (lsa$d * t(lsa$v))
rownames(smoothed) <- rownames(incidence)
colnames(smoothed) <- colnames(incidence)

tt <- smoothed %*% t(smoothed)
dd <- t(smoothed) %*% smoothed

dd2 <- ifelse(dd > max(dd)/3,dd,0)
g <- graph.adjacency(dd2,mode='undirected',diag=FALSE,weighted=TRUE)
V(g)$region <- res$region
V(g)$actor_type <- res$actor_type
V(g)$event_type <- res$event_type
V(g)$Label <- colnames(incidence)
write.graph(g,'piracy.graphml',format='graphml')
