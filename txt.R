library(stringi)

jm1 = read.csv("jm1.txt")
jm2 = read.csv("jm2.txt")
okapi1 = read.csv("okapi1.txt")
okapi2 = read.csv("okapi2.txt")
tfidf1 = read.csv("tfidf1.txt")
tfidf2 = read.csv("tfidf2.txt")
model = list(jm1, jm2, okapi1, okapi2, tfidf1, tfidf1)
names(model) = c("jm1", "jm2", "okapi1", "okapi2", "tfidf1", "tfidf2")
save(model, file = "model.rda")

load("model.rda")
for(i in 1:6){
  docs = NULL
  precision = NULL
  for(j in 20:28){
    num = stri_extract_all_regex(model[[i]][j,1], "[0-9.]+")
    docs[j - 19] = num[[1]][1]
    precision[j - 19] = num[[1]][2]
  }
  assign(paste0(names(model)[[i]], "docs"), data.frame(docs, precision))
}

for(i in 1:6){
  level = NULL
  precision = NULL
  for(j in 6:16){
    num = stri_extract_all_regex(model[[i]][j,1], "[0-9.]+")
    level[j - 5] = num[[1]][1]
    precision[j - 5] = num[[1]][2]
  }
  assign(paste0(names(model)[[i]], "pr"), data.frame(level, precision))
}
