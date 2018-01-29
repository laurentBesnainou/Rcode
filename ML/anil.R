
library(rvest)

library(tm)  # ce package propose un ensemble de fonctions facilitant le traitement de donnees textuelles
library(wordcloud)  # ce package permet la creation de wordcloud
library(wordcloud2)  # ce package permet la creation de wordcloud
library(RColorBrewer)
library(readr)
mystring <- read_file("anil.txt")
# 
# 
# text_Anil <- html("https://www.anil.org/copropriete-financement-travaux/")
# 

txt <- text_Anil %>% 
#   html_node(".col-sm-9") %>%
#   html_text()

 
# text_Anil <- read

text_corpus <- Corpus(VectorSource(mystring))
text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus <- tm_map(text_corpus, removePunctuation)
text_corpus <- tm_map(text_corpus, function(x)removeWords(x,stopwords(kind = "fr")))
text_corpus <- tm_map(text_corpus, removeWords, c("chaque", "notamment", "savoir", "chaque", "lorsqu'", "celle", "non",
                                                      "dune", "ainsi", "faire", "lors",
                                                      "peut","peuvent","doivent","doit")) 


dtm <- TermDocumentMatrix(text_corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)

wordcloud2(d, size = 2.3, minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1)

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.2, 
          colors= c("indianred1","indianred2","indianred3","indianred"))

# save the image in png format
png("Anil.png", width=12, height=8, units="in", res=800)


wordcloud(words = d$word, max.words=200, min.freq = 1,random.color = TRUE,
          random.order=FALSE, rot.per=0.35, scale=c(4, 0.5), colors= c("indianred1","indianred2","indianred3","indianred"))

dev.off()


liste <- c("parcours_résidentiel", "projet", "intérêts_intercalaires", "PTZ", "mensualité_modulable", "trajets",
"performances énergétique", "anticipation", "acquisition_amélioration", "construction", "famille", "prêteur", 
"taxes_d'urbanisme", "épargne", "charges", "copropriété", "rénovation", 
           "compromis", "réservation", "tableau_d'amortissement", "pédagogie", "personnalisation", "sécurisation")
prob <- 1:12 * 
set.seed(1234)
sample(1:100,23)

s <- sample(liste,replace = TRUE,200,runif(21, 0, 1))
quantile(s, probs=seq(0,1,1/23))

text_corpus <- Corpus(VectorSource(s))
text_corpus <- data.frame(Data = sample(liste,replace = TRUE,200))
dtm <- TermDocumentMatrix(text_corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
