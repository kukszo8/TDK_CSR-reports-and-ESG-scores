library(tidyverse)
library(tidytext)
library(tidyquant)
library(stm)
library(ggplot2)


sp500<-tq_index("SP500") %>% 
  select(1,2,5,6)

esg<-list.files("final_data/",full.names=T) %>% 
  keep(str_ends,"csv") %>% 
  read_csv(show_col_types = FALSE)%>% 
  slice(3:98) %>% 
  rename(date=1)

seq_esg=seq(from=2,to=,1801,by=4)
seq_e=seq(from=3,to=,1801,by=4)
seq_s=seq(from=4,to=,1801,by=4)
seq_g=seq(from=5,to=,1801,by=4)

esg_cleaned<-esg%>% 
  rename_with(~paste0(sub("...[0-9]+", "_", .),"ESG"), seq_esg) %>%
  rename_with(~paste0(sub("...[0-9]+", "_", .),"E"), seq_e) %>% 
  rename_with(~paste0(sub("...[0-9]+", "_", .),"S"), seq_s) %>% 
  rename_with(~paste0(sub("...[0-9]+", "_", .),"G"), seq_g) %>% 
  mutate(date=lubridate::ymd(date),
         year=str_sub(date,start=1,end=4)
  ) %>% 
  select(year, everything()) %>%
  mutate_if(is.character, as.numeric) %>% 
  fill(., 3:1802, .direction = 'down') %>% 
  group_by(year) %>%
  filter(date==max(date))%>% 
  select(-date) %>% 
  pivot_longer(!year, names_to = "ESG_type", values_to = "score") %>% 
  mutate(symbol=gsub("\\_.*", "", ESG_type))
  
esg_score<-esg_cleaned %>% 
  filter(str_detect(ESG_type,"_ESG")) %>%
  mutate(year=as.character(year)) %>% 
  arrange(symbol, year) %>% 
  group_by(symbol) %>% 
  mutate(esg_change=ifelse(score>lag(score),"Javított","Rontott"))

boxplot<-esg_score %>% 
  left_join(sp500) %>% 
  filter(year<2019) %>% 
  drop_na() %>% 
ggplot(aes(x=year, y=score, fill=sector)) + 
  geom_boxplot() +
  facet_wrap(~sector)

boxplot


filter_year <- function(x){
  list.files("intermediate_data/", full.names=T)%>%
  keep(~str_ends(., "rds")) %>% 
  map_dfr(readRDS) %>% 
  tibble()%>% 
  filter(year %in% x) %>% 
  rename(symbol=company) %>% 
  left_join(esg_score) %>% 
  drop_na() %>% 
  left_join(sp500) %>% 
  unnest(raw_text) %>% 
  filter(row_number() %% 2 != 0) %>% 
  unnest(raw_text) %>% 
  mutate(line = row_number()) %>%
  unnest_tokens(word,raw_text) %>% 
  filter(!grepl('[0-9]', word)) %>%  # remove numbers
  filter(nchar(word)>1) %>% 
  filter(tolower(symbol)!=word) %>% ##remove symbol names from words
  filter(tolower(company)!=word) %>% ##remove company names from words
  mutate(company_short=gsub(' [A-z ]*', '' , company)) %>%
  filter(tolower(company_short)!=word) %>% ##remove company names from words
  anti_join(get_stopwords())
}


dataframe<-filter_year(2014:2019)

saveRDS(dataframe, file = "dataframe_14_19.rds")

esg_sparse <-dataframe %>%
  count(line, word) %>%
  cast_sparse(line, word, n)

covariates <- dataframe %>%
  sample_frac() %>%
  distinct(line,esg_change,sector) %>% 
  drop_na

dim(esg_sparse)
set.seed(123)


topic_model <- stm(esg_sparse, 
                   K = 22, 
                   prevalence = ~ esg_change+sector,
                   data = covariates,
                   verbose = FALSE, 
                   max.em.its = 5,
                   init.type = "Spectral")


summary(topic_model)

word_topics <- tidy(topic_model, matrix = "beta")
word_topics


word_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  mutate(topic = paste("Topic", topic)) %>%
  ggplot(aes(beta, reorder_within(term, beta, topic), fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(topic), scales = "free_y") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_reordered() +
  labs(x = expression(beta), y = NULL)


plot(topic_model, type = "summary", xlim = c(0, .3))

# Graphical display of topic correlations

topic_correlation<-topicCorr(topic_model)
plot(topic_correlation)


##Predict effects


predict_topics<-estimateEffect(formula = 1:22 ~ esg_change,
                               topic_model,
                               uncertainty = "Global",
                               prior = 1e-5)

effects <-
  estimateEffect(
    1:22 ~ esg_change,
    topic_model,
    df_2020 %>% distinct(symbol, esg_change) %>% arrange(symbol)
  )

tidy(effects)
