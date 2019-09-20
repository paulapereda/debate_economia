library(syuzhet)
library(tidyverse)

tor::load_rds('data/')

## creo un score que es la difrencia entre términos positivos y negativos
df$clean_text <- stringr::str_replace_all(df$text, "@\\w+ *", "")
Sentiment <- get_nrc_sentiment(df$clean_text, language = "spanish")
base_senti <- cbind(df, Sentiment)
base_senti$puntaje <- base_senti$positive - base_senti$negative
base_senti$sentimiento <- ifelse(base_senti$puntaje < 0, "Negativo", "Positivo")
base_senti$sentimiento <- ifelse(base_senti$puntaje == 0, "Neutral",
                                 base_senti$sentimiento)

# Gráfico score sentimientos
score_sentiment <- base_senti %>% 
  group_by(referente, sentimiento) %>% 
  summarise(count = n()) %>% 
  mutate(per = round(prop.table(count)*100,1))

ggplot(score_sentiment, aes(x = referente, 
                            y = per, 
                            fill = sentimiento)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("#F7977A", "#FFF79A","#82CA9D")) +
  geom_text(data = tweets_sent, 
            aes(x = referente, y = per, label = per),
            position = position_dodge(0.9),
            vjust = -0.5, 
            size = 4,
            color = "gray25") +
  theme_bw() +
  theme(text = element_text(family = "Georgia", color = "grey20"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(fill = "Sentimiento") + 
  xlab('') 
+
  ggsave('imgs/sentiment.png', dpi = 550, width = 10, height = 5)

## Grafico puntajes
tweets_sent_puntaje <- base_senti %>% 
  group_by(puntaje, referente) %>% 
  summarise(count=n()) %>% 
  mutate(per = round(prop.table(count)*100,1))

ggplot(tweets_sent_puntaje,
       aes(x = puntaje,
           y = count, 
           fill = referente)) +
  geom_bar(position = "dodge",
           stat = "identity") +
  geom_text(data = tweets_sent_puntaje, 
            aes(x = puntaje, 
                y = count, 
                label = count),
            position = position_dodge(width = 1.5), 
            vjust = -0.25)

