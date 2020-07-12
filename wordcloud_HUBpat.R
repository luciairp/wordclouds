library(tm)
library(wordcloud)
library(tidyverse)

# como gráficos desde base necesito decidir márgenes razonables
par(mar = c(2,2,2,1)+0.1, oma = c(0,0,0,0)+0.1)


# ejemplo dataframe cat y PlainText ---------------------------------------


guess_encoding("ejemploarboles.csv")

# carga de datos
HUBpat <- read_csv("ejemploarboles.csv",locale = readr::locale(encoding = "windows-1252"))

# la lógica es calcular la frecuencia de aparición de palabras 
# y usar eso para graficar en una nube que relaciona tamaño con frecuencia

# aproximación desde base
tbl <- table(HUBpat$Árbol)
wordcloud(names(tbl), tbl, min.freq=1)

#aprox desde dplyr
freqsparawc <- count(HUBpat, Árbol)
pal <- brewer.pal(9,"BuGn")
pal <- pal[-(1:4)]
wordcloud(words = freqsparawc$Árbol, 
          freq = freqsparawc$n, 
          min.freq = 1, 
          colors = pal) 

# en caso que la variable en una columna sea una cadena de caracteres necesita otros pasos...
# Fabricar corpus de palabras

# convertir a solo texto:
corpus <- Corpus(tm::VectorSource(HUBpat$Árbol))

#todo a minúsculas
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, tolower)

#sin puntuación
corpus = tm_map(corpus, removePunctuation)

#sin considerar algunas palabras, a saber:
corpus = tm_map(corpus, removeWords, c("palo", stopwords("spanish")))

# familias de palabras asociadas a una sola palabra
corpus = tm_map(corpus, stemDocument)

# eliminar espacios vacíos
corpus = tm_map(corpus, stripWhitespace)

# ahora sí fabrico un objeto con las palabras
# le dicen "DocumentTermMatrix" DTM
TDM <- TermDocumentMatrix(corpus)
TDM <- as.matrix(TDM)

f <- sort(rowSums(TDM),decreasing=TRUE)
dat <- data.frame(word = names(f),freq=f)

wordcloud(words = names(f), 
          freq = f, 
          random.order = F,
          random.color = F,
          min.freq = 1,
          colors = brewer.pal(8, "Dark2")) 


# prueba datos parciales --------------------------------------------------


