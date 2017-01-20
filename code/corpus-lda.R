require(tm)
require(mallet)

corp <- Corpus(VectorSource(ideas$suggestion), readerControl=list(language="ru", encoding="UTF-8"))
corp <- tm_map(corp, removeWords, c(stopwords("ru"), "очень", "просто", "вообще", "хотя", "вроде", "это", readLines("~/atlas-moskovsky-district-data/data/stoplist-ideas")))

dtm.control <- list(weighting = weightTf, stemming = FALSE, bounds = list(global = c(1,Inf)))
dtm <- DocumentTermMatrix(corp, control = dtm.control)

# BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
# dtm.bi <- DocumentTermMatrix(corp, control = list(tokenize = BigramTokenizer, language = "ru"))

mallet.instances <- mallet.import(as.character(ideas$idea), as.character(ideas$suggestion), "~/atlas-moskovsky-district-data/data/stoplist-ideas", token.regexp = "[\\p{L}\\p{N}-]*\\p{L}+") #as.character so that there is no error (important!)
topic.model <- MalletLDA(num.topics = 30) # количество тем
topic.model$loadDocuments(mallet.instances) 
topic.model$setAlphaOptimization(20, 50)
topic.model$train(8000)
topic.model$maximize(20)

## таблица распределения тем по документам
doc.topics <- mallet.doc.topics(topic.model, smoothed=TRUE, normalized=TRUE)
## таблица распределения слов по темам
topic.words <- mallet.topic.words(topic.model, smoothed=TRUE, normalized=TRUE)
## метки для тем (по трем главным словам)
topic.labels <- mallet.topic.labels(topic.model, topic.words, 5)

word.freqs <- mallet.word.freqs(topic.model)

for (k in 1:nrow(topic.words)) {
  top <- paste(mallet.top.words(topic.model, topic.words[k,], 20)$words,collapse=" ")
  cat(paste(k, top, "\n"))
}

topic.model.saved <- topic.model #don't overwrite!
doc.topics.bin <- ifelse(doc.topics > 0.25, 1, 0)
