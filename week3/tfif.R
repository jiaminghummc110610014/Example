#TFIF
library(tm)
library(tmcn)
library(Matrix)
library(wordcloud)

docs <- readLines("lunyu.txt", encoding = "UTF-8")
docs <- gsub("\\[[0-9]+\\]", "", docs)
docs.corpus <- Corpus(VectorSource(docs))
docs.seg <- tm_map(docs.corpus, segmentCN)
docs.tdm <- TermDocumentMatrix(docs.seg, control = list())
inspect(docs.tdm)

docs.tf <- apply(as.matrix(docs.tdm), 2, function(doc) {doc / sum(doc)})
idf.function <- function(word_doc) { log2( (length(word_doc)+1) / nnzero(word_doc) ) }
docs.idf <- apply(docs.tdm, 1, idf.function)
docs.tfidf <- docs.tf * docs.idf
head(docs.tfidf)

query.tfidf <- function(q){
  q.position <- which(rownames(docs.tfidf) %in% q)
  q.tfidf <- docs.tfidf[q.position, ]
  return (q.tfidf)
}
query.tfidf(c("結構", "人類學", "整體"))
#K-means
