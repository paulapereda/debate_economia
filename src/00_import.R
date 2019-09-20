library(syuzhet)
library(quanteda)
library(tidyverse)

df <- readxl::read_xlsx('data/debate.xlsx') %>% 
  filter(!is.na(referente)) %>% 
  rename(text = linea) %>% 
  mutate(partido = case_when(
    referente == "Ana Inés Zerbino" ~ "Partido Colorado",
    referente == "Azucena Arbeleche" ~ "Partido Nacional",
    referente == "Fernando Isabella" ~ "Frente Amplio",
    referente == "Luis Freda" ~ "Partido Independiente",
    referente == "Marcel Vaillant" ~ "Partido Independiente"
  ))

# Limpio el texto con el paquete quanteda y construyo un DFM

# Genero el corpus

debate_df <- quanteda::corpus(df, text_field = "text")

# Abro un archivo con stopwords propias y modismos

vector <- file.path('data/stopes.csv') %>% 
  read.csv(sep = ";", 
           encoding = "latin1",
           stringsAsFactors = FALSE) %>% 
  transmute(stop = as.character(X........HEAD))

# Aplico la función dfm con los argumentos para limpiar el texto

mydfm <- dfm(debate_df,
             stem = FALSE,
             tolower = TRUE,                             # paso a minúscula todas las palabras
             remove = c(stopwords("spanish"), vector),   # saco las palabras definidas en stop
             remove_punct = TRUE,                        # elimino puntuaciones
             remove_numbers = TRUE,                      # elimino números
             verbose = TRUE)

# Me quedo solo con las palabras

palabras <- featnames(mydfm)

# Saco palabras con 1 y 2 caracteres
palabras_tam_1 <- palabras[sapply(palabras, stringr::str_length) == 1]
palabras_tam_2 <- palabras[sapply(palabras, stringr::str_length) == 2]

otraspalabras <- c(palabras_tam_1, 
                   palabras_tam_2,
                   "bueno",
                   "tema",
                   "hace",
                   "tener",
                   "hecho",
                   "gracias",
                   "creo",
                   "ser",
                   "van")

mydfm <- dfm(mydfm, remove = otraspalabras)

readr::write_rds(mydfm, 'data/mydfm.rds')
