
esg_sparse <- pin_read(board, "esg_sparse")

heldout <- make.heldout(esg_sparse)

model_2<- pin_read(board, "stm-improve_total_score-sector_2")


k_result <-pin_list(board) %>% 
  keep(~str_starts(.,"stm-improve_total_score-sector_")) %>%
  enframe(value="file_name",name=NULL) %>% 
  slice(1:2) %>% 
  mutate(topic_model=map(file_name, pin_read, board = board),
         topic_model=map(topic_model,1),
         K=parse_number(file_name),
         exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, esg_sparse),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, esg_sparse),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))


k_result  %>%
  transmute(K=1:2,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics",
       subtitle = "These diagnostics indicate that a good number of topic")
