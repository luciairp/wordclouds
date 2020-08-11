library(tm)
library(SnowballC)
library(wordcloud)
library(tidyverse)

# como gráficos desde base necesito decidir márgenes razonables
par(mar = c(2,2,2,1)+0.1, oma = c(0.2,0,0.2,0)+0.1)


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


HUBpat <- read_csv("rtas_jue1130.csv")
HUBpat <- read_csv("rtas_vie1130.csv")
display.brewer.pal(8,"Dark2")

# Fabricar corpus de palabras

# convertir a solo texto:
corpus <- Corpus(tm::VectorSource(HUBpat$EXPLORER))

#todo a minúsculas
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, tolower)

#sin puntuación
corpus = tm_map(corpus, removePunctuation)

#sin considerar algunas palabras, a saber:
corpus = tm_map(corpus, removeWords, stopwords("spanish"))

# familias de palabras asociadas a una sola palabra
corpus = tm_map(corpus, stemDocument, language = "spanish" )

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

EXPLORER <- wordcloud(words = names(f), 
                      freq = f, 
                      random.order = F,
                      random.color = F,
                      min.freq = 1,
                      colors = brewer.pal(8, "Dark2")) 

### HUBhoy
corpus <- Corpus(tm::VectorSource(HUBpat$HUBHOY))
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("cono",stopwords("spanish")))
#corpus = tm_map(corpus, stemDocument, language = "spanish" )
corpus = tm_map(corpus, stripWhitespace)

TDM <- TermDocumentMatrix(corpus)
TDM <- as.matrix(TDM)

f <- sort(rowSums(TDM),decreasing=TRUE)
dat <- data.frame(word = names(f),freq=f)

HUBhoy <- wordcloud(words = names(f), 
                    scale = c(3,.3),
          freq = f, 
          random.order = F,
          random.color = F,
          min.freq = 1,
          colors = brewer.pal(8, "Dark2")) 

### HUBdeseo

corpus <- Corpus(tm::VectorSource(HUBpat$HUBdeseo))
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("spanish"))
corpus = tm_map(corpus, stemDocument, language = "spanish" )
corpus = tm_map(corpus, stripWhitespace)

TDM <- TermDocumentMatrix(corpus)
TDM <- as.matrix(TDM)

f <- sort(rowSums(TDM),decreasing=TRUE)
dat <- data.frame(word = names(f),freq=f)

HUBdeseo <- wordcloud(words = names(f),
                      scale = c(4,.4),
                    freq = f, 
                    random.order = F,
                    random.color = F,
                    min.freq = 1,
                    colors = brewer.pal(8, "Dark2")) 

### FORMAtrabajo
corpus <- Corpus(tm::VectorSource(HUBpat$FORMAtrabajo))
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("hacia","hub",
                                       "trabajar",stopwords("spanish")))
#corpus = tm_map(corpus, stemDocument, language = "spanish")
corpus = tm_map(corpus, stripWhitespace)

TDM <- TermDocumentMatrix(corpus)
TDM <- as.matrix(TDM)

f <- sort(rowSums(TDM),decreasing=TRUE)
dat <- data.frame(word = names(f),freq=f)

FORMAtrabajo <- wordcloud(words = names(f),
                          scale = c(3,.2),
                      freq = f, 
                      random.order = F,
                      random.color = F,
                      min.freq = 1,
                      colors = brewer.pal(8, "Dark2")) 

### COMUNIDAD
corpus <- Corpus(tm::VectorSource(HUBpat$COMUNIDAD))
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("allá","hace","acá","buen",
                                       "etc","creo","van","gran","fondo",
                                       "parecer","afán","aquellas","manera",
                                       "existe","indagar","ejemplo","tópicos",
                                       "directa","mientras","ganas","posible",
                                       "cambia","olvidar","comenzando","nutren",
                                       "ambos","inserta","desproporción",
                                       "topicos","enfásis","siempre","favor",
                                       "conocer","desafío","lado","comunidad",
                                       "compuesta","indirectamente",
                                       "transmitirlo",stopwords("spanish")))
#corpus = tm_map(corpus, stemDocument, language = "spanish" )
corpus = tm_map(corpus, stripWhitespace)

TDM <- TermDocumentMatrix(corpus)
TDM <- as.matrix(TDM)

f <- sort(rowSums(TDM),decreasing=TRUE)
dat <- data.frame(word = names(f),freq=f)

COMUNIDAD <- wordcloud(words = names(f), 
                       scale = c(4,.4),
                      freq = f, 
                      random.order = F,
                      random.color = F,
                      min.freq = 1,
                      colors = brewer.pal(8, "Dark2")) 

### VERBO
corpus <- Corpus(tm::VectorSource(HUBpat$VERBO))
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("swamps","mundo",
                                       "adentro","hacia","afuera",
                                       "historia","natural","cultural",
                                       "culturales",stopwords("spanish")))
#corpus = tm_map(corpus, stemDocument, language = "spanish" )
corpus = tm_map(corpus, stripWhitespace)

TDM <- TermDocumentMatrix(corpus)
TDM <- as.matrix(TDM)

f <- sort(rowSums(TDM),decreasing=TRUE)
dat <- data.frame(word = names(f),freq=f)

VERBO <- wordcloud(words = names(f),
                   scale = c(3.7,.5),
                    freq = f, 
                    random.order = F,
                    random.color = F,
                    min.freq = 1,
                    colors = brewer.pal(8, "Dark2")) 

### POBLobj
corpus <- Corpus(tm::VectorSource(HUBpat$POBLACIÓNobj))
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords,c("llegar","acotaría","gustaria",
                                      "quizas","solo","aquellos",
                                      "interesadas","formar","facilidades",
                                      "interesados","resto","cambios",
                                      stopwords("spanish")))
#corpus = tm_map(corpus, stemDocument, language = "spanish" )
corpus = tm_map(corpus, stripWhitespace)

TDM <- TermDocumentMatrix(corpus)
TDM <- as.matrix(TDM)

f <- sort(rowSums(TDM),decreasing=TRUE)
dat <- data.frame(word = names(f),freq=f)

POBLACIÓNobj <- wordcloud(words = names(f),
                   scale = c(5,.5),
                   freq = f, 
                   random.order = F,
                   random.color = F,
                   min.freq = 1,
                   colors = brewer.pal(8, "Dark2")) 

### ese algo medible
corpus <- Corpus(tm::VectorSource(HUBpat$ALGOMEDIBLE))
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords,c("plazo","genera","sienta",
                                      stopwords("spanish")))
#corpus = tm_map(corpus, stemDocument, language = "spanish")
corpus = tm_map(corpus, stripWhitespace)

TDM <- TermDocumentMatrix(corpus)
TDM <- as.matrix(TDM)

f <- sort(rowSums(TDM),decreasing=TRUE)
dat <- data.frame(word = names(f),freq=f)

ALGOMEDIBLE <- wordcloud(words = names(f),
                          scale = c(2.8,.3),
                          freq = f, 
                          random.order = F,
                          random.color = F,
                          min.freq = 1,
                          colors = brewer.pal(8, "Dark2")) 
