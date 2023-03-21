cleaned_text_data <- pin_read(board, "cleaned_text_data")

estimate_effect_score <- function(pin_name, f = "s(lead_qvartilis)+s(time)") {
  
  .fit <- pin_read(board, pin_name)[[1]]
  
  estimateEffect(
    as.formula(str_c("1:", .fit$settings$dim$K, " ~ ", f)),
    .fit,
    cleaned_text_data %>% 
      distinct(line, lead_qvartilis,time) %>%
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
    "stm_ef_lead_qvart"
  )

stm_ef_lead_qvart <- pin_read(board, "stm_ef_lead_qvart") %>% 
  filter(term!="(Intercept)") %>% 
  arrange(p.value) %>% 
  filter(k<13)


###One by one test
 
model_10<-pin_read(board,"stm-time_10")[[1]]

model_10_est <- estimateEffect(1:10 ~ lead_compared_to_sector,
                               model_10,
                        cleaned_text_data %>% 
                         distinct(line, lead_compared_to_sector) %>%
                         arrange(line),uncertainty = "Global")

summary(model_10_est)


plot(prep, covariate = "improve_total_score", topics = c(23, 48,5),
     model = poliblogPrevFit, method = "pointestimate",
     xlab = "Topic prevalance",
     xlim = c(-0.05, .05))
