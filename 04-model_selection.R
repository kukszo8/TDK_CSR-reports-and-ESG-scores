esg_sparse <- pin_read(board, "esg_sparse")
heldout <- make.heldout(esg_sparse)

k_result <- pin_list(board) %>% 
  keep(~str_starts(., "stm-improve_total_score_")) %>%
  enframe(value = "file_name", name=NULL) %>% 
  splitted_transmute(
    topic_model = map(file_name, pin_read, board = board),
    topic_model = map(topic_model, 1),
    k = str_remove_all(file_name, "\\D") |> 
      as.numeric(),
    exclusivity = map(topic_model, exclusivity),
    semantic_coherence = map(topic_model, semanticCoherence, esg_sparse),
    eval_heldout = map(topic_model, eval.heldout, heldout$missing),
    residual = map(topic_model, checkResiduals, esg_sparse),
    bound =  map_dbl(topic_model, \(x) max(x$convergence$bound)),
    lfact = map_dbl(topic_model, \(x) lfactorial(x$settings$dim$K)),
    lbound = bound + lfact,
    iterations = map_dbl(topic_model, \(x) length(x$convergence$bound)),
    `Lower bound` = lbound,
    Residuals = map_dbl(residual, "dispersion"),
    `Semantic coherence` = map_dbl(semantic_coherence, mean),
    `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout"),
    .keep = c("k", "exclusivity", "semantic_coherence", "eval_heldout", "residual", "bound", "lfact",
      "lbound", "iterations", "Lower bound", "Residuals",
      "Semantic coherence", "Held-out likelihood")
  )

k_result |> 
  pin_write(
    board = board, 
    "k_result_stm-improve_total_score"
  )


##Exploring topic_30
  
topic_model_30 <- pin_list(board) %>% 
keep(~str_starts(., "stm-improve_total_score_30")) %>%
enframe(value = "file_name", name=NULL) %>% 
mutate(topic_model = map(file_name, pin_read, board = board),
topic_model = map(topic_model, 1),
word_topics=map(topic_model,broom::tidy)) %>% 
  select(word_topics) %>% 
  unnest()


topic_model_30 %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  mutate(topic = paste("Topic", topic)) %>%
  ggplot(aes(beta, reorder_within(term, beta, topic), fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(topic), scales = "free_y") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_reordered() +
  labs(x = expression(beta), y = NULL)

topic_model_30_v2<-pin_read(board=board,"stm-improve_total_score_30") %>% 
  .[[1]]

plot(topic_model_30_v2, type = "summary", xlim = c(0, .3))
labelTopics(topic_model_30_v2, c(2, 19, 10))


###Estimateffect

cleaned_text_data <- pin_read(board, "cleaned_text_data")

effects <-
  estimateEffect(
    1:30 ~ improve_total_score,
    topic_model_30_v2,
    cleaned_text_data %>% distinct(line, improve_total_score) %>% arrange(line)
  )

results<- tidy(effects)
