require(dplyr)
require(stringr)

start <- Sys.time()

#loading
#NOTE:
ideas.web.raw <- read.delim(paste0("data/input/", list.files("data/input")[2]), stringsAsFactors = F)
ideas.field.raw <- read.csv(paste0("data/input/", list.files("data/input")[1]), 
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
cat("Number of words in corpus:\n", ideas$suggestion %>% strsplit(" ") %>% sapply(length) %>% sum, "\n")
cat("Average number of words in a document:\n", ideas$suggestion %>% strsplit(" ") %>% sapply(length) %>% mean, "\n")
cat("Script finished in ", Sys.time() - start %>% as.numeric(), " seconds")
rm(start)