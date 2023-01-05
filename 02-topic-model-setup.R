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
  
esg_score_df <- esg_scores_raw |> 
  slice(-1) |> 
  set_names(esg_score_names) |> 
  select(company, name, ends_with("12-01")) |> 
  pivot_longer(- c("company", "name"), names_to = "time") |> 
  mutate(
    time = ymd(time),
    time = year(time),
    value = as.numeric(value),
  ) |> 
  arrange(company, name, time) |> 
  group_by(company, name) |> 
  fill(value) |> 
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
        )
    )
  
cleaned_text_data <- pin_read(board, "raw_text") |> 
  mutate(raw_text = map(raw_text, 1)) |> 
  rename(time = year) |> 
  mutate(time = as.numeric(time)) |> 
  filter(time %in% 2014:2019) |> 
  left_join(esg_score_df, by = c("time", "company")) |> 
  rename(symbol = company) |> 
  left_join(sp500, by = "symbol") |> 
  unnest(raw_text) |> 
  mutate(line = row_number()) %>%
  unnest_tokens(word,raw_text) %>% 
  filter(!grepl('[0-9]', word)) %>%  # remove numbers
  filter(nchar(word)>1) %>% 
  filter(tolower(symbol)!=word) %>% ##remove symbol names from words
  filter(tolower(company)!=word) %>% ##remove company names from words
  filter(tolower(gsub(' [A-z ]*', '' , company))!=word) %>% ##remove company names from words
  anti_join(get_stopwords(), by = "word") |> 
  select(line, word, improve_total_score, sector) |> 
  drop_na()

cleaned_text_data %>%
  count(line, word) %>%
  cast_sparse(line, word, n) |> 
  pin_write(board = board, name = "esg_sparse")

cleaned_text_data |> 
  distinct(line, .keep_all = TRUE) |> 
  select(- word) |> 
  pin_write(board = board, name = "covariates")
