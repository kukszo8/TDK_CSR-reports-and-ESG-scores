esg_sparse <- pin_read(board, "esg_sparse")
heldout <- make.heldout(esg_sparse)

options(currr.folder = ".currr", currr.workers = 1)

model_names <- str_c("stm-time_", c(2, 4:60))

x = model_names[1]

stm_glance <- function(topic_model) {
  
  K <- topic_model$settings$dim$K
  exclusivity <- exclusivity(topic_model)
  semantic_coherence <- semanticCoherence(topic_model, documents = esg_sparse)
  eval_heldout <- eval.heldout(topic_model, heldout$missing)
  residual <- checkResiduals(topic_model, esg_sparse)
  bound <- max(topic_model$convergence$bound)
  lfact <- lfactorial(topic_model$settings$dim$K)
  lbound <- bound + lfact
  iterations <- length(topic_model$convergence$bound)
  
  tibble(
    K,
    `Lower bound` = lbound,
    Residuals = residual$dispersion,
    `Semantic coherence` = mean(semantic_coherence),
    `Held-out likelihood` = eval_heldout$expected.heldout,
  )
}

options(currr.folder = ".currr.data", currr.workers = 8, currr.wait = 1)

k_result <- model_names |> 
  cp_map_dfr(~ {
    pin_read(., board = board) %>%
    pluck(., 1) %>%
      stm_glance
  }, name = "stm_glance")

k_result |>
  pin_write(
    board = board,
    "k_result_stm-time"
  )


