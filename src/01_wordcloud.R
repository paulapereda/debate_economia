library(quanteda)
library(tidyverse)
library(topicmodels)

red_pink <- "#e64173"
purple <- "#6A5ACD"
turquoise <- "#20B2AA"
orange <- "#FFA500"
rojo <- "#d11013"
celeste <- "#d8f7fe"
violeta <- "#682497"
azul <- "#050099"

# Nubes de palabras con quanteda
## Nubes de palabras sin desagregación
textplot_wordcloud(mydfm,
                   min.count = 10,
                   max_words = 400,
                   random.order = FALSE,
                   rot.per = .25, 
                   colors = c(red_pink, purple),
                   font = "Georgia")


# genero un nuevo data.frame por grupos según alguna variable: partido
dfm_partido <- dfm(mydfm, groups = "partido")

# tiro la función para generar nube de palabras 
textplot_wordcloud(dfm_partido, 
                   min.count = 10,
                   max_words = 400,
                   random.order = FALSE,
                   rot.per = .25, 
                   colors = c(azul, rojo, violeta, celeste),
                   comparison = T,
                   font = "Georgia")
mydfm

## Topic models

dtm <- convert(mydfm, to = "topicmodels")
lda <- LDA(dtm, k = 10)
get_terms(lda, 5)
