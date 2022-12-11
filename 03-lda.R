library(tidyverse)
library(tidyquant)

sp500<-tq_index("SP500") %>% 
  select(1,2,5,6)


df <- list.files("intermediate_data/2020/", full.names=T) %>%
  keep(~str_ends(., "rds")) %>% 
  map_dfr(readRDS)%>% 
  unnest(raw_text)%>% 
  unnest_tokens(word,raw_text) %>% 
  anti_join(get_stopwords()) %>% 
  count(word, sort = TRUE)
