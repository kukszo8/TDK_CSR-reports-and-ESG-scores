cleaned_text_data <- pin_read(board, "cleaned_text_data")

estimate_effect_score <- function(pin_name, f = "improve_total_score") {
  
  .fit <- pin_read(board, pin_name)[[1]]
  
  estimateEffect(
    as.formula(str_c("1:", .fit$settings$dim$K, " ~ ", f)),
    .fit,
    cleaned_text_data %>% 
      distinct(line, improve_total_score) %>%
      arrange(line)
  ) |> 
    broom::tidy() |> 
    mutate(k = .fit$settings$dim$K, .before = 1)
}

str_c("stm-improve_total_score_", 2:60) |> 
  cp_map_dfr(estimate_effect_score) |> 
  arrange(k, topic) |> 
  pin_write(
    board = board,
    "stm-effects-improve_total_score"
  )


poliblogPrevFit<-pin_read(board,"stm-effects-improve_total_score") %>% 
  group_by(k) %>% 
  arrange(statistic) %>% 
  filter(k==87)

poliblogPrevFit<-pin_read(board,"stm-improve_total_score_87")[[1]]

prep <- estimateEffect(1:87 ~ improve_total_score,
                       poliblogPrevFit,
                       cleaned_text_data %>% 
                         distinct(line, improve_total_score) %>%
                         arrange(line),uncertainty = "Global")


plot(prep, covariate = "improve_total_score", topics = c(23, 48,5),
     model = poliblogPrevFit, method = "pointestimate",
     xlab = "Topic prevalance",
     xlim = c(-0.05, .05))
