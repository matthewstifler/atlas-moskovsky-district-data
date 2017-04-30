require(xlsx)
ideas.fixed.04.18 <- read.xlsx("data/data.xlsx", sheetIndex = 1)
ideas.fixed.04.18 <- ideas.fixed.04.18[,-71]
ideas.fixed.04.18 <- ideas.fixed.04.18[-c(995:998),]

ideas.fixed.04.18[,6:12] <- lapply(ideas.fixed.04.18[,6:12], function(x) as.factor(ifelse(x == "NA", NA, x)))

#----------Data Imputation---------
#97 incomplete cases, we have to impute them
require(mice)
mice.try <- mice(as.data.frame(lapply(ideas.fixed.04.18[,6:70], as.factor)))
mice.try$method #hmmm
mice.out <- complete(mice.try)
ideas.fixed.04.18[,6:70] <- mice.out

#------------Chi squares-----------
chisq.full <- lapply(c("sex", "age", "education", "money", "source"), function(dependent) {
  lapply(ideas.fixed.04.18[,15:ncol(ideas.fixed.04.18)], function(x, dep) chisq.test(x, dep), dep = ideas.fixed.04.18[,dependent])
})

sapply(chisq.full[[1]], function(x) x$p.value) %>% `[`(. < 0.05) %>% sort
sapply(chisq.full[[2]], function(x) x$p.value) %>% `[`(. < 0.05) %>% sort
sapply(chisq.full[[3]], function(x) x$p.value) %>% `[`(. < 0.05) %>% sort
sapply(chisq.full[[4]], function(x) x$p.value) %>% `[`(. < 0.05) %>% sort
sapply(chisq.full[[5]], function(x) x$p.value) %>% `[`(. < 0.05) %>% sort

#-------------EFA-------------
cor.matrix.all <- cor(as.data.frame(lapply(ideas.fixed.04.18[,c(6:70)], as.numeric)))
cor.matrix.ideas <- cor(as.data.frame(lapply(ideas.fixed.04.18[,c(13:70)], as.numeric)))
scree(cor.matrix.all) #5 factors!
#another approach, this says 20 factors
fa.parallel(as.data.frame(lapply(ideas.fixed.04.18[,6:70], as.numeric)), fa = "both", fm = "ml")

#5 factors solution
ideas.factanal.5.bentler <- factanal(x = ~., data = as.data.frame(lapply(ideas.fixed.04.18[,6:70], as.numeric)), factors = 5, rotation = "bentlerQ", scores = "reg", control = list(rotate = list(maxit = 10000, normalize = T)))

#factors just for the respondents (probably not sufficient)
ideas.people.factanal.3.promax <- factanal(x = ~., data = as.data.frame(lapply(ideas.fixed.04.18[,6:12], as.numeric)), factors = 3, rotation = "promax", scores = "reg")
#factors just for the ideas
fa.parallel(as.data.frame(lapply(ideas.fixed.04.18[,13:70], as.numeric)), fa = "both", fm = "ml")

#here we can probably use correlation matrix between factors to make conclusions regarding demographic info for the factors where it was not included
#also this is not that bad, because all but one of these 20 have >1 SS loadings
ideas.factanal.20.promax <- factanal(x = ~., data = as.data.frame(lapply(ideas.fixed.04.18[,6:70], as.numeric)), factors = 20, rotation = "promax", scores = "reg")

require(GPArotation)
#the best one appeared to be bentlerQ, the most explained cumulative variance
#also, note the controls in factanal() call
#we also have to check if the factors turned out to be interpretable
ideas.factanal.20.bentler <- factanal(x = ~., data = as.data.frame(lapply(ideas.fixed.04.18[,6:70], as.numeric)), factors = 20, rotation = "bentlerQ", scores = "reg", control = list(rotate = list(maxit = 10000)))

#normalize gives +3% cum variance!
ideas.factanal.20.bentler <- factanal(x = ~., data = as.data.frame(lapply(ideas.fixed.04.18[,6:70], as.numeric)), factors = 20, rotation = "bentlerQ", scores = "reg", control = list(rotate = list(maxit = 10000, normalize = T)))

#alternative way
#it's more or less the same, but it makes it possible to extract factor correlations
ideas.fa.20.bentler <- fa(as.data.frame(lapply(ideas.fixed.04.18[,6:70], as.numeric)), nfactors = 20, rotate = "bentlerQ", scores = "tenBerge", fm = "ml", maxit = 10000, normalize = T)

#viewing method, for both ways
ideas.fa.20.bentler$loadings[,1:20] %>%
  as.data.frame() %>%
  lapply(function(x) ifelse(abs(x) < 0.15, "", x)) %>% #loadings cutoff 0.15
  as.data.frame(row.names = colnames(ideas.fixed.04.18[,6:70])) %>%
  View

#should vars probably be standartized?
#no they should not, it made absolutely no difference to loadings

#I'm not really sure factors should be further clustered

#writing data
ideas.factanal.20.bentler$loadings[,1:20] %>%
  as.data.frame() %>%
  lapply(function(x) ifelse(abs(x) < 0.15, 0, x)) %>% #loadings cutoff 0.15
  as.data.frame(row.names = colnames(ideas.fixed.04.18[,6:70])) %>%
  write.csv("data/output/factor-loadings-20-bentler.csv")

#--------------Example of working with factors and plotting-----------
#First we take two close factors and extract loadings for all of the variables for the two of them
loadings <- as.data.frame(ideas.factanal.5.bentler$loadings[,1:2])
#plot it
ggplot(loadings, aes(x = Factor1, y = Factor2, label = colnames(ideas.fixed.04.18[,6:70]))) + geom_point() + geom_text()
