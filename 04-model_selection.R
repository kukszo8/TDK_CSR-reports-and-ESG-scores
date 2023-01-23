esg_sparse <- pin_read(board, "esg_sparse")
heldout <- make.heldout(esg_sparse)

k_result <-pin_list(board) %>% 
  keep(~str_starts(.,"stm-improve_total_score-sector_")) %>%
  enframe(value="file_name",name=NULL) %>% 
  mutate(
    topic_model = map(file_name, pin_read, board = board),
    topic_model = map(topic_model,1),
    K = str_remove_all(file_name, "\\D") |> 
      as.numeric(),
    exclusivity = map(topic_model, exclusivity),
    semantic_coherence = map(topic_model, semanticCoherence, esg_sparse),
    eval_heldout = map(topic_model, eval.heldout, heldout$missing),
    residual = map(topic_model, checkResiduals, esg_sparse),
    bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
    lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
    lbound = bound + lfact,
    iterations = map_dbl(topic_model, function(x) length(x$convergence$bound))
  )

k_result |> 
  pin_write(
    board = board, 
    "k_result_stm-improve_total_score-sector"
  )

