library(tm)
library(dplyr)
corpus = Corpus(DirSource("documents"))
doc.list = list()
for(i in 1:2048){
  doc.list[[paste0("doc",i)]] = content(corpus[[i]])
}
N.docs = length(doc.list)
names(doc.list) = paste0("doc",c(1:N.docs))
query = "drill wood sharp"
my.docs = VectorSource(c(doc.list,query))
my.docs$names = c(names(doc.list),"query")
my.corpus = Corpus(my.docs)

#word cleaning
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
inspect(term.doc.matrix.stm[1:2049,])

term.doc.matrix = as.matrix(term.doc.matrix.stm)

#VectorSpaceModel
get.tf.idf.weights = function(tf.vec){
  n.docs = length(tf.vec)
  doc.frequncy = length(tf.vec[tf.vec>0])
  weights = rep(0,length(tf.vec))
  weights[tf.vec>0] = (1+log2(tf.vec[tf.vec>0])) * log2(n.docs/doc.frequncy)
  return(weights)
}
tfidf.matrix = t(apply(term.doc.matrix,1,
                       FUN = function(row){
                         get.tf.idf.weights(row)
                       }))
colnames(tfidf.matrix) = colnames(term.doc.matrix)
tfidf.matrix[1:3,70:90]

query.vector = tfidf.matrix[,(N.docs+1)]
tfidf.matrix = tfidf.matrix[,1:N.docs]
doc.scores = t(query.vector) %*% tfidf.matrix
results.df = data.frame(doc = names(doc.list), score = t(doc.scores),
                        text = unlist(doc.list))
results.df = results.df[order(results.df$score , decreasing = T) , ]
results = results.df[1:5,c(1,2)]
print(results , row.names = F , right = F , digits = 2)
