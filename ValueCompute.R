
#library
library(tm)
library(dplyr)
library(Matrix)

#read documents
corpus = Corpus(DirSource("C:/Users/User/Documents/documents")) # need to check final submit by right workspace
doc.list = list()
for(i in 1:2048){
  doc.list[[corpus[[i]]$meta$id]] = content(corpus[[i]])
}
N.docs = length(doc.list)
query = "drill wood sharp"

#enter query
args = commandArgs(trailingOnly = TRUE)
if(length(args) == 0){
  print("Need to enter searching . ")
}else{
  query = args
}
my.docs = VectorSource(c(doc.list,query))
my.docs$names = c(names(doc.list),"query")
my.corpus = Corpus(my.docs)

#text cleaning
toSpace = content_transformer(function(x,pattern)gsub(pattern," ",x))
my.corpus = tm_map(my.corpus , toSpace , "/")
my.corpus = tm_map(my.corpus , toSpace , "@")
my.corpus = tm_map(my.corpus , toSpace , "\\|")
my.corpus = tm_map(my.corpus,removePunctuation)
my.corpus = tm_map(my.corpus , content_transformer(tolower))
my.corpus = tm_map(my.corpus , removeNumbers)
my.corpus = tm_map(my.corpus , removeWords , stopwords("english"))
my.corpus = tm_map(my.corpus , removeWords , c("blabla1" , "blabla2"))
my.corpus = tm_map(my.corpus , stripWhitespace)

term.doc.matrix.stm = TermDocumentMatrix(my.corpus)
colnames(term.doc.matrix.stm) = c(names(doc.list),"query")
term.doc.matrix = as.matrix(term.doc.matrix.stm)

# tf-idf computation
tf <- apply(term.doc.matrix, 2, sum) # term frequency
idf <- function(word_doc){ log2( (length(word_doc)+1) / nnzero(word_doc) ) }
idf <- apply(term.doc.matrix, 1, idf)
###tf weighting matrix
doc.tf = as.matrix(term.doc.matrix)
for(i in 1:nrow(term.doc.matrix)){
  for(j in 1:ncol(term.doc.matrix)){
    doc.tf[i,j] = doc.tf[i,j] / tf[j]
  }
}
###tf-idf weighting matrix
doc.tfidf <- as.matrix(term.doc.matrix)
for(i in 1:nrow(term.doc.matrix)){
  for(j in 1:ncol(term.doc.matrix)){
    doc.tfidf[i,j] <- (doc.tfidf[i,j] / tf[j]) * idf[i]
  }
}

#Cosine and Euclidean Distance Computation
###Cosine
cos.sim <- function(x, y){ x%*%y / sqrt(x%*%x * y%*%y) }
query.vector = term.doc.matrix[,(N.docs+1)]
doc.cos <- apply(term.doc.matrix, 2, cos.sim, y = query.vector)

###Euclidean Distance
euclidean = function(x,y){
  dist(rbind(x,y))
}
doc.Euclidean = apply(term.doc.matrix, 2, euclidean, y= query.vector)

#Term Frequency (TF) Weighting + Cosine Similarity
print("Term Frequency (TF) Weighting + Cosine Similarity : ")
doc.scores <- (t(query.vector) %*% doc.tf[,1:N.docs])   #arguments imply differing number of rows
results.tf <- data.frame(doc = names(doc.list), score = t(doc.scores),
                         text = unlist(doc.list))
results<- results.tf[,c(1,2)]
for(i in 1:2048){
  results$realScore[i] = results$score[i] + doc.cos[i]
}
results <- results[order(results$realScore, decreasing = TRUE), ]
print(results[1:5,c(1,3)], row.names = FALSE, right = FALSE, digits = 6)

#Term Frequency (TF) Weighting + Euclidean Distance
print("Term Frequency (TF) Weighting + Euclidean Distance : ")
results<- results.tf[,c(1,2)]
for(i in 1:2048){
  results$realScore[i] = abs(results$score[i] - doc.Euclidean[i])
}
results <- results[order(results$realScore, decreasing  = FALSE), ]
print(results[1:5,c(1,3)], row.names = FALSE, right = FALSE, digits = 6)

#TF-IDF Weighting + Cosine Similarity
print("TF-IDF Weighting + Cosine Similarity : ")
doc.scores <- (t(query.vector) %*% doc.tfidf[,1:N.docs])   #arguments imply differing number of rows
results.tfidf <- data.frame(doc = names(doc.list), score = t(doc.scores),
                         text = unlist(doc.list))
results<- results.tfidf[,c(1,2)]
for(i in 1:2048){
  results$realScore[i] = results$score[i] + doc.cos[i]
}
results <- results[order(results$realScore, decreasing = TRUE), ]
print(results[1:5,c(1,3)], row.names = FALSE, right = FALSE, digits = 6)

#TF-IDF Weighting + Euclidean Distance
print("TF-IDF Weighting + Euclidean Distance : ")
results<- results.tf[,c(1,2)]
for(i in 1:2048){
  results$realScore[i] = abs(results$score[i] - doc.Euclidean[i])
}
results <- results[order(results$realScore, decreasing  = FALSE), ]
print(results[1:5,c(1,3)], row.names = FALSE, right = FALSE, digits = 6)
