library(tidyverse)
library(tidytext)
library(tidyquant)
library(stm)
library(ggplot2)


sp500<-tq_index("SP500") %>% 
  select(1,2,5,6)

esg<-list.files("final_data/",full.names=T) %>% 
  keep(str_ends,"csv") %>% 
  read_csv(show_col_types = FALSE) %>% 
  slice(3:98) %>% 
  rename(date=1)


seq_esg=seq(from=2,to=,1801,by=4)
seq_e=seq(from=3,to=,1801,by=4)
seq_s=seq(from=4,to=,1801,by=4)
seq_g=seq(from=5,to=,1801,by=4)


esg<-esg %>% 
    rename_at(vars(seq_esg),funs(sub(seq_esg, 'ESG', .)))





df <- function(x){
  list.files("intermediate_data/", full.names=T)%>%
    keep(str_ends, "rds") %>% 
    map_dfr(readRDS) %>% 
    tibble()%>% 
    filter(year %in% x) %>% 
    rename(symbol=company) %>% 
    left_join(sp500) %>% 
    unnest(raw_text) %>% 
    filter(row_number() %% 2 != 0) %>% 
    unnest(raw_text)
}

df_2020<-df(2020)

dat.proc <-  textProcessor(documents=df_2020$raw_text,
                           metadata = select(df_2020, year,symbol,company,weight,sector), 
                           lowercase = TRUE, #*
                           removestopwords = TRUE, #*
                           removenumbers = TRUE, #*
                           removepunctuation = TRUE, #*
                           stem = TRUE, #*
                           wordLengths = c(4,Inf), #*
                           sparselevel = 1, #*
                           language = "en", #*
                           verbose = TRUE, #*
                           onlycharacter = TRUE, # not def
                           striphtml = FALSE, #*
                           customstopwords = NULL, #*
                           v1 = FALSE) #*


save(dat.proc, file = "final_data/dat_proc.RData")

prep <- prepDocuments(dat.proc$documents, dat.proc$vocab, dat.proc$meta)

save(prep, file = "final_data/prep.RData")


load("final_data/dat_proc.RData")


mod <- stm(documents = dat.proc$documents, 
           vocab = dat.proc$vocab,
           K = 14,
           prevalence = ~ sector,
           data = dat.proc$meta,
           init.type = "Spectral")


plot(First_STM)




