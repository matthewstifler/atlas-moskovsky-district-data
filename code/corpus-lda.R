require(tm)

corp <- Corpus(VectorSource(ideas$suggestion), readerControl=list(language="ru", encoding="UTF-8"))
corp <- tm_map(corp, removeWords, c(stopwords("ru"), "очень", "просто", "вообще", "хотя", "вроде", "это", readLines("~/atlas-moskovsky-district-data/data/stoplist-ideas")))

dtm.control <- list(weighting = weightTf, stemming = FALSE, bounds = list(global = c(1,Inf)))
dtm <- DocumentTermMatrix(corp, control = dtm.control)

