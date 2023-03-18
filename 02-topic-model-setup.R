#############################ESG data version 1: Sustainalytics

sp500 <- tq_index("SP500") %>% 
  select(1,2,5,6)

esg_scores_raw <- rio::import("esg_scores.csv") |> 
  t() |> 
  data.frame() |> 
  tibble() 

esg_score_names <- esg_scores_raw |> 
  # manage empty cells for colnames
  slice(1) |> 
  unlist(use.names = FALSE) |> 
  assign_in(1, "company") |>
  assign_in(2, "name")



esg_score_df_first<-esg_scores_raw|> 
  slice(-1) |> 
  set_names(esg_score_names) |> 
  pivot_longer(-c(1,2,3),names_to="time") %>% 
  select(-Date) %>% 
  mutate(
    value = as.numeric(value),
  ) |> 
  arrange(company, name, time) |> 
  group_by(company, name) |> 
  fill(value) |> 
  filter(str_ends(time,"12-01")) %>% 
  mutate(time=str_sub(time,1,4)) %>%
  mutate(time=as.numeric(time)) %>%
  filter(time %in% 2014:2019) %>% 
  pivot_wider() |> 
  janitor::clean_names()


####Cheking if ESG scoring methodology changed in 2020: Boxplot differs significantly in 2019.

esg_score_df_first %>%
  rename(symbol = company) %>% 
  mutate(time=as.character(time)) %>% 
  left_join(sp500)%>% 
  ggplot(aes(x=time, y=total_score, fill=sector)) + 
  geom_boxplot() +
  facet_wrap(~sector)

###Outlies checks from 2019: Filtering those whos ESG score methodology already changed in 2019
outliers_from_2019<-esg_score_df_first %>% 
  rename(symbol = company) %>% 
  mutate(time=as.character(time)) %>% 
  left_join(sp500)%>% 
  group_by(company) %>% 
  mutate(change=(total_score/lag(total_score)-1)*100,
         change_abs=abs(change)) %>% 
  filter(time%in%2018:2019)%>% 
  filter(change_abs>30) %>% 
  pull(1)


###ESG dataframe for the first model: 2014:2019 and higher ESG means better performance
  
esg_score_df_2014_2019<-esg_scores_raw|> 
  slice(-1) |> 
  set_names(esg_score_names) |> 
  pivot_longer(-c(1,2,3),names_to="time") %>% 
  select(-Date) %>% 
  mutate(
    value = as.numeric(value),
  ) |> 
  arrange(company, name, time) |> 
  group_by(company, name) |> 
  fill(value) |> 
  filter(str_ends(time,"12-01")) %>% 
  mutate(time=str_sub(time,1,4)) %>%
  mutate(time=as.numeric(time)) %>%
  filter(time %in% 2014:2019) %>% 
  mutate(outlier_drop=ifelse(time==2019&company %in% outliers_from_2019,"outlier","no outlier")) %>% 
  filter(outlier_drop=="no outlier") %>% ##dropping those whole ESG scoring method in 2019
  pivot_wider() |> 
  janitor::clean_names() |> 
    mutate(
      across(
        e_score:total_score,
        ~ . - lag(.),
        .names = "change_{.col}"
        ),
      across(
        e_score:total_score,
        ~ ifelse(. > lag(.), "improved", "not_improved"),
        .names = "improve_{.col}"
        ))%>%  
  group_by(time) %>% 
  mutate(
    across(
      e_score:total_score,
      ~ ifelse(. > mean(.,na.rm=TRUE),"better_than_avg", "worse_than_avg"),
      .names = "mean_{.col}")
    )


###ESG dataframe for the second model: 2019:2021 and lower ESG means better performance

esg_score_df_2019_2022<-esg_scores_raw|> 
  slice(-1) |> 
  set_names(esg_score_names) |> 
  pivot_longer(-c(1,2,3),names_to="time") %>% 
  select(-Date) %>% 
  mutate(
    value = as.numeric(value),
  ) |> 
  arrange(company, name, time) |> 
  group_by(company, name) |> 
  fill(value) |> 
  rename(yymmdd=time) %>% 
  mutate(time=str_sub(yymmdd,1,4)) %>%
  mutate(time=as.numeric(time)) %>% 
  filter(time %in% 2019:2022) %>%
  mutate(time_drop=ifelse(str_ends(yymmdd,"12-01")|(time==2022&str_ends(yymmdd,"08-01")),"keep","drop"),
    outlier_drop=ifelse(time==2019&!company %in% outliers_from_2019,"outlier","no outlier"))%>% 
  filter(outlier_drop=="no outlier"&time_drop=="keep") %>% ##dropping those whole ESG scoring method in 2019
  pivot_wider() |> 
  janitor::clean_names() |> 
  mutate(
    across(
      e_score:total_score,
      ~ . - lag(.),
      .names = "change_{.col}"
    ),
    across(
      e_score:total_score,
      ~ ifelse(. < lag(.), "improved", "not_improved"),
      .names = "improve_{.col}"
    )) %>% 
group_by(time) %>% 
  mutate(
    across(
      e_score:total_score,
      ~ ifelse(. < mean(.,na.rm=TRUE),"better_than_avg", "worse_than_avg"),
      .names = "mean_{.col}")
  )

##Binding two datasets together and creating lead variables


esg_score_df<-bind_rows(esg_score_df_2014_2019,esg_score_df_2019_2022) %>% 
  select(company,time,improve_e_score:mean_total_score) %>% 
  group_by(company) %>% 
  mutate(
    across(
      improve_e_score:mean_total_score,
      ~ lead(.),
      .names = "lead_{.col}")
    )%>% 
  rename(symbol=company) %>% 
  left_join(sp500) %>% 
  drop_na() %>% 
  select(time,symbol,lead_improve_s_score:sector)


##Saving out the ESG dataframes to board

board |> 
  pin_write(
    esg_score_df,
    "esg_score_df"
  )

#############################ESG data version 2: Eikon Datastream


sp500 <- tq_index("SP500") %>% 
  select(1,2,5,6)

esg_score_df_eikon <- rio::import("eikon_esg_scores.xlsx") |> 
  data.frame() |> 
  tibble() %>% 
  slice(-1) %>% 
  rename(time=1)%>% 
  pivot_longer(-1) %>% 
  filter(!str_detect(value, 'ERROR|INVALID')) %>% 
  arrange(name, time) |> 
  group_by(name) %>% 
  mutate(value=as.numeric(value)) %>% 
  rename(esg_score=value) %>% 
  rename(symbol=name) %>% 
  left_join(sp500,by="symbol") %>% 
  mutate(change_firm=esg_score-lag(esg_score),
         improve_esg_score=ifelse(esg_score>lag(esg_score), "improved", "not_improved")) %>% #1.covariate
  group_by(time) %>% 
  mutate(qvartilis=ntile(esg_score,4)) %>%  #2.covariate
  group_by(time,sector) %>% 
  mutate(change_sector=mean(change_firm,na.rm=TRUE)) %>% 
  mutate(compared_to_sector=ifelse(change_firm>change_sector,"better_than_sector","worse_than_sector")) %>% #3.covariate
  group_by(time) %>% 
  mutate(esg_score_mean=mean(esg_score,na.rm=TRUE),
         compared_to_avg=ifelse(esg_score>esg_score_mean,"better_than_avg","worse_than_avg")) %>%  #4.covariate
  group_by(symbol) %>% 
  mutate(
    across(
      change_firm:compared_to_avg,
      ~ lead(.),
      .names = "lead_{.col}")
  )  %>% 
  drop_na()


board |> 
  pin_write(
    esg_score_df_eikon,
    "esg_score_df_eikon"
  )

##Creating text dataframe

esg_score_df_eikon<-pin_read(board,"esg_score_df_eikon") %>% 
select(time,company,symbol,lead_change_firm:lead_compared_to_avg) 

  
cleaned_text_data <- pin_read(board, "raw_text") |> 
    mutate(raw_text = map(raw_text, 1)) |> 
    rename(time = year) |> 
    mutate(time = as.numeric(time)) |> 
    filter(time %in% 2014:2021) %>% 
    rename(symbol=company) %>% 
    left_join(esg_score_df_eikon,
                 by = c("time", "symbol")) %>%
    left_join(sp500, by = "symbol")|> 
    mutate(line = row_number()) %>%
    unnest(raw_text) |> # these are only sep pages, but the same document
    unnest_tokens(word,raw_text) %>% 
    filter(!grepl('[0-9]', word)) %>%  # remove numbers
    filter(nchar(word)>1) %>% 
    mutate(word=str_replace_all(word,"ǻ","a"),
         word=str_replace_all(word,"č","c"),
         word=str_replace_all(word,"đ","d"),
         word=str_replace_all(word,"ě","e"),
         word=str_replace_all(word,"ģ","g"),
         word=str_replace_all(word,"ħ","h"),
         word=str_replace_all(word,"į","i"),
         word=str_replace_all(word,"ķ","k"),
         word=str_replace_all(word,"ŀ","l"),
         word=str_replace_all(word,"ǿ","o"),
         word=str_replace_all(word,"ŀ","l"),
         word=str_replace_all(word,"ș","s"),
         
         word=str_replace_all(word,"ț","t"),
         word=str_replace_all(word,"ň","n"),
         word=str_replace_all(word,"ř","r"),
         word=str_replace_all(word,"ų","u"),
         word=str_replace_all(word,"ẅ","w"),
         word=str_replace_all(word,"ỳ","y"),
         word=str_replace_all(word,"ż","u"),
  ) %>% 
  filter(str_detect(word,"[a-zA-Z]")) %>% ##Filter Russian and Chinese words
  filter(tolower(symbol)!=word) %>%  ##remove symbol names from words %>% 
  rename(company=company.x) %>% 
  select(-company.y) %>% 
  filter(tolower(company)!=word) %>% ##remove company names from words
  filter(tolower(gsub(' [A-z ]*', '' , company))!=word) %>% ##remove company names from words
  anti_join(get_stopwords(), by = "word") |> 
  drop_na()

###Checking terms that does not appear in at least 3 companies' report

unique_words<-cleaned_text_data %>% 
  count(symbol, word, sort = TRUE)%>% 
  group_by(word) %>% 
  mutate(count=n()) %>% 
  filter(count<=3) %>% 
  pull(2)
  
##Filtering out unique words

cleaned_text_data<-cleaned_text_data %>% 
  filter(!word %in% unique_words)
  
##Saving out databases for the STM model

cleaned_text_data %>% 
  pin_write(board = board, name = "cleaned_text_data")

cleaned_text_data %>%
  count(line, word) %>%
  cast_sparse(line, word, n) |> 
  pin_write(board = board, name = "esg_sparse")

cleaned_text_data |> 
  distinct(line, .keep_all = TRUE) |> 
  select(- word) |> 
  pin_write(board = board, name = "covariates")