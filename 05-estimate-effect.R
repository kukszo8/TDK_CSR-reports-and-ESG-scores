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

models_to_read<- pin_list(board) %>% 
  keep(str_detect, "stm_ef.*")

est_effect <- tibble(models_to_read) %>% 
mutate(data=map(models_to_read,
    ~pin_read(board = board,.)),
    data = map(data, ~ filter(., term!="(Intercept)")),
    data=map(data,~ filter(., k<=13))) %>% 
  unnest(data) %>% 
  filter(models_to_read %in% c("stm_ef_change_comp_sector","stm_ef_change_comp_firm",
                               "stm_ef_comp_avg")) %>% 
  filter(term %in% c("lead_compared_to_sectorworse_than_sector","lead_improve_esg_scorenot_improved",
                     "lead_compared_to_avgworse_than_avg"))%>% 
  mutate(p.value=round(p.value,digits=2))


ggplot(est_effect %>% 
         filter(k>=3), 
       aes(x = k, y = topic, fill = p.value)) +
  facet_wrap(~models_to_read,nrow=1 ) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  geom_text(aes(label = p.value), color = "white", size = 3)+
  coord_fixed()+
  labs(
       title = "Estimate topic prevalance with different ESG covariates")