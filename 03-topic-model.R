esg_sparse <- pin_read(board, "esg_sparse")
covariates <- pin_read(board, "covariates")

stm_prevalence <- ~ improve_total_score + sector

for (k in 2:20) {
  
message("Fitting topic model w ", crayon::blue(k), " topics started. ", crayon::magenta(str_c("(", Sys.time(), ")")))
  
tictoc::tic()

topic_model <- stm(esg_sparse, 
                   K = k, 
                   prevalence = stm_prevalence,
                   data = covariates,
                   verbose = FALSE, 
                   max.em.its = 5,
                   init.type = "Spectral")

runtime <- capture.output(tictoc::toc())

board |> 
  pin_write(
    x = list(topic_model, runtime), 
    type = "rds",
    name = stm_prevalence |> 
      as.character() |> 
      pluck(2) |> 
      str_replace(" \\+ ", "-") %>%
      str_c("stm-", .,  "_", k)
  )

}
