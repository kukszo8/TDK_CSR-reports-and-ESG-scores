cleaned_text_data <- pin_read(board, "cleaned_text_data")

estimate_effect_score <- function(pin_name, f = "lead_improve_esg_score+s(time)") {
  
  .fit <- pin_read(board, pin_name)[[1]]
  
  estimateEffect(
    as.formula(str_c("1:", .fit$settings$dim$K, " ~ ", f)),
    .fit,
    cleaned_text_data %>% 
      distinct(line, lead_improve_esg_score,time) %>%
      arrange(line,uncertainty="Global")
  ) |> 
    broom::tidy() |> 
    mutate(k = .fit$settings$dim$K, .before = 1)
}

str_c("stm-time_", c(2,4:60)) |> 
  cp_map_dfr(estimate_effect_score) |> 
  arrange(k, topic) |> 
  pin_write(
    board = board,
    "stm_ef_change_comp_firm"
  )

stm_ef_change_comp_firm <- pin_read(board, "stm_ef_change_comp_firm") %>% 
  filter(term!="(Intercept)") %>% 
  arrange(p.value) %>% 
  filter(k<13)


plot(prep, covariate = "improve_total_score", topics = c(23, 48,5),
     model = poliblogPrevFit, method = "pointestimate",
     xlab = "Topic prevalance",
     xlim = c(-0.05, .05))
