# descarga de tweets de gustavo petro
library(rtweet)
library(tidyverse)
library(echarts4r)
library(magrittr)
library(lubridate)
library(ggbump)
library(tidytext)
library(glue)

# descarga
df <- get_timeline(user = "petrogustavo", n = 3200)

# zona horaria: Colombia
df$created_at <-  df$created_at - dhours(5)

# a qué hora publica petro
(df %>% 
  mutate(
    hora = hour(created_at)
  ) %>% 
  count(hora) %>%
  ggplot(aes(hora, n)) +
  geom_bump(
    smooth = 7, color = "#4361ee",
    size = 1.5
  ) +
  scale_x_continuous(breaks = seq(1, 24, 2)) +
  hrbrthemes::theme_ipsum_rc(
    base_family = "IBM Plex Sans", 
    grid = "XY"
  ) +
  labs(
    title = "Gustavo Petro tuitea principalmente a las 7 am",
    subtitle = "En base a los últimos 3200 tweets",
    y = "número de tweets"
  ) +
  theme(plot.subtitle = element_text(family = "IBM Plex Sans")) -> p1
)
# petro retuitea?
tidytext::get_stopwords(language = "es") %>% 
  pull(word) %>% 
  unique() -> remove

(df %>% 
  unnest_tokens(palabras, text) %>% 
  count(palabras) %>% 
  mutate(
    numeros = grepl("[0-9]", palabras),
    nchar = nchar(palabras),
    prop = prop.table(n)*100
  ) %>%
  filter(numeros == F) %>% 
  filter(!palabras %in% remove) %>% 
  filter(!palabras %in% c("https", "t.co")) %>% 
  arrange(desc(n)) %>% 
  slice(1:20) %>% 
  mutate(num = 1:nrow(.)) %>% 
  ggplot(aes(fct_reorder(palabras, num, .desc = T), n)) +
  geom_col(fill = "#e76f51", color = NA) +
  coord_flip() +
  hrbrthemes::theme_ipsum_rc(
    base_family = "IBM Plex Sans", 
    grid = "XY"
  ) +
  labs(
    title = "Las 20 palabras más tuiteadas por Petro",
    subtitle =  glue("Más de una presente en todos los 3200 últimos tweets"),
    y = "número de veces que la palabra fue mencionadas", 
    x = ""
  ) +
  theme(plot.subtitle = element_text(family = "IBM Plex Sans")) -> p2)
  
  
  
  

  
  


  
  
  

  
  
  
  
  
  


  
  

  
  
  
  
  
  




