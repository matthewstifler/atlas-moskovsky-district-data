require(dplyr)
require(stringr)

#loading
#NOTE:
ideas.web.raw <- read.delim("data/web-ideas-18.01.tsv", stringsAsFactors = F)
ideas.field.raw <- read.csv("data/field-ideas-18.01.csv", 
                            stringsAsFactors = F,
                            header = T,
                            allowEscapes = T, # line breaks are not parsed as separate observations
                            sep = ";")

#combine both sources together
ideas <- rbind(setNames(ideas.web.raw[,c("idea", "field1")], names(ideas.field.raw[,c("idea", "suggestion")])), #setNames lets ignore different names
      ideas.field.raw[,c("idea", "suggestion")])

#cleanup
ideas$suggestion <- gsub("(https://)(\\S*)", " ", ideas$suggestion) %>% 
  gsub("ё", "е", .) %>%
  gsub("Ё", "Е", .) %>%
  gsub("[^А-Яа-я]", " ", .) %>%
  str_to_lower() %>% 
  trimws() %>%
  gsub("\\s{2,}", " ", .)

#stemming
writeLines(ideas$suggestion, "data/ideas-vec")
ideas$suggestion <- system("mystem -cl -d data/ideas-vec | cat", intern = T) %>% 
  #TODO: Get rid of mediate file (ideas-vec)
  gsub("[^А-Яа-я]", " ", .) %>% 
  gsub("\\s{2,}", " ", .) %>% 
  trimws()

#monitoring
ideas$suggestion %>% strsplit(" ") %>% sapply(length) %>% sum
ideas$suggestion %>% strsplit(" ") %>% sapply(length) %>% mean
