library(janeaustenr)

covariates<-pin_read(board, "covariates")

cleaned_text_data <- pin_read(board, "cleaned_text_data") %>% 
  select(time,symbol,word) %>% 
  mutate(join=str_c(time,"-",symbol)) %>% 
  count(word,join) %>% 
  bind_tf_idf(word,join,n)


model_input<-cleaned_text_data %>% 
  semi_join(top_10,by=c("word"="term")) %>% 
  separate(join,c("time","symbol"))




top_10<-pin_read(board,"stm-time_10") |> 
  pluck(1) |> 
  broom::tidy() |> 
  group_by(topic) |> 
  slice_max(beta, n = 10)

join<-cleaned_text_data %>% 
  rename(term=word) %>% 
  select(-c(lead_change_firm,lead_change_sector,lead_esg_score_mean))

report_words <- join %>%
  group_by(time,symbol) %>%
  count(term, sort = TRUE) 

total_words <- report_words %>% group_by(time,symbol) %>% summarize(total = sum(n))

report_words <- report_words %>% 
  left_join(total_words,by=c("time","symbol")) %>% 
  left_join(top_10,by="term")


report_words <- report_words %>%
  group_by(time,symbol) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total,
         date_time=str_c(time,symbol))

# TF-IDF

report_words %>%
  bind_tf_idf(term, c("time","symbol"),n)

