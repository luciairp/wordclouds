library(tm)
library(wordcloud)
library(tidyverse)

# como gráficos desde base necesito decidir márgenes razonables
par(mar = c(2,2,2,1)+0.1, oma = c(0,0,0,0)+0.1)

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
