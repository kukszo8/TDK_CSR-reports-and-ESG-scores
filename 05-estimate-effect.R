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



###Etimating 30
 
model_30<-pin_read(board,"stm-improve_total_score_30")[[1]]

model_30_est <- estimateEffect(1:30 ~ lead_improve,
                        model_30,
                        cleaned_text_data_v2 %>% 
                         distinct(line, lead_improve) %>%
                         arrange(line),uncertainty = "Global")


plot(prep, covariate = "improve_total_score", topics = c(23, 48,5),
     model = poliblogPrevFit, method = "pointestimate",
     xlab = "Topic prevalance",
     xlim = c(-0.05, .05))
