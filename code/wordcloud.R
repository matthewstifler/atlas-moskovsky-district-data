require(wordcloud)

#needs dtm to work
wordcloud(words = names(sort(slam::col_sums(dtm, na.rm = T), decreasing = T)),
          freq = sort(slam::col_sums(dtm, na.rm = T), decreasing = T),
          scale = c(4,0.15),
          min.freq = 10,
          max.words = 100,
          random.order = F,
          colors = RColorBrewer::brewer.pal(6, "Dark2"))