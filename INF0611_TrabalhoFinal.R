library(ggplot2)
require(reshape2)

setwd("C:\\Users\\rafae\\Documents\\INF0611_Final")

#-----------------------------------------------------------
# Funções
# ----------------------------------------------------------
# normalizar dados
normalize <- function (values) {
  print(dim(values))
  (values - mean(values))/sd(values)
}

# Calculo de Distancia L2
distL2 <- function(x, y){
  d = 0
  for (i in c(1:length(x))) {
    d = d+(x[i]-y[i])^2
  }
  return(sqrt(d))
}

# Calculo de Distancia Cossenos
distCos <- function(x, y){

}

# Calculo das distancias
calcDistances <- function(db, query, dist_func) {
  for (q in 1:dim(query)[1]) {
    dists <- c()
    for (i in 1:(dim(query)[2]-1)) {
      dists <- c(dists, dist_func(c(descritor[i,2], descritor[i,3]), consulta))
    }
  }
  return(order(dists))
}

# calcular precision e recall
calcPrecisionRecall <- function(qb, qy, distances) {
  
  
}

getDescriptor <- function(db, descriptor) {
  
  if (descriptor == "SAX"){
    
  }
  else if (descriptor == "RP") {
    
  }
  else{
    db <- db
  }
  return(db)
}

plotClasses <- function (db) {
  # plotar as classes
  for (i in unique(db[,1])){
    
    data <- data.frame(cbind(t(db[db[,1]==i, 2:129][1,]), vars=c(1:128)))
    colnames(data) <- c("a","b","c", "time")
    df <- melt(data ,  id.vars = 'time', variable.name = 'series')
    ggplot(df, aes(time,value)) + geom_line(aes(colour = series))
    ggplot(df, aes(time,value)) + geom_line() + facet_grid(series ~ .)
  }
}

plotPrecRecall <- function(prec_recall) {
  
}
# ----------------------------------------------------------

# Main
db_original <- read.csv("SwedishLeaf_TRAIN.csv", header = F)
qy_original <- read.csv("SwedishLeaf_TEST.csv", header = F)

summary(db_original[,2:129])

plotClasses(db_original)

db_desc <- getDescriptor(db_original, "None")
qy_desc <- getDescriptor(qy_original, "None")


distances <- calcDistances(db_desc, qy_desc, distL2)

prec_recall <- calcPrecisionRecall(db_desc, qy_desc, distances)

plotPrecisionRecall(prec_recall)
