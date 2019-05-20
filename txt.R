library(stringi)

jm1 = read.csv("jm1.txt")
jm2 = read.csv("jm2.txt")
okapi1 = read.csv("okapi1.txt")
okapi2 = read.csv("okapi2.txt")
tfidf1 = read.csv("tfidf1.txt")
tfidf2 = read.csv("tfidf2.txt")
model = list(jm1, jm2, okapi1, okapi2, tfidf1, tfidf1)
for(i in 1:6){
  for(j in 6:16){
    num = stri_extract_all_regex(model[[i]][j,1], "[0-9.]+")
    level[] = num[[1]][1]
    precision[] = num[[1]][2]
  }
}
