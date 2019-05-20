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

plot(jm1pr$precision, jm1pr$level)

### precision-recall
pr = list(okapi1pr, okapi2pr, tfidf1pr, tfidf2pr, jm1pr, jm2pr)
doc = list(okapi1docs, okapi2docs, tfidf1docs, tfidf2docs, jm1docs, jm2docs)
for(i in 1:6){
  for(j in 1:2){
    pr[[i]][[j]] = as.numeric(as.character(pr[[i]][[j]]))
    doc[[i]][[j]] = as.numeric(as.character(doc[[i]][[j]]))
  }
}
par(mfrow = c(2,1))
for(i in 1:6){
  plot(pr[[i]][[2]] ~ pr[[i]][[1]], main = "precision-recall plot", xlab = "Recall level", ylab = "Precision", xlim = c(0,1), type = "line")
  plot(doc[[i]][[2]] ~ doc[[i]][[1]], main = "precision at docs plot", xlab = "Docs number", ylab = "Precision", type = "line")
  
}

library(ggplot2)
### model with stemming the p-r plot
method = c("okapi", "Lapalce smoothing", "Jelinek-Mercer smoothing")
pr1 = rbind(pr[[1]], pr[[3]], pr[[5]])
pr1$group = c(rep("okapi", 11), rep("Laplace", 11), rep("Jelinek-Mercer", 11))

ggplot(data = pr1, aes(x = level, y = precision, color = group)) +
  geom_line() + xlab("Recall") + ggtitle("Method with stemming")


### the precision at docs plot


### model without stemming 