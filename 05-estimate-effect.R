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

pin_list(board) |> 
  keep(str_starts, "stm-improve_total_score_") |> 
  cache_map(estimate_effect_score) |> 
  bind_rows() |> 
  arrange(k, topic) |> 
  pin_write(
    board = board,
    "stm-effects-improve_total_score"
  )
