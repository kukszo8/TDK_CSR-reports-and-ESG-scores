library("tidyverse")


input <- tibble(file_name = list.files("input_7", full.names = TRUE)) %>%
  slice(5:19) %>% 
  transmute(
    file_name,
    company = sub(".*/", "", file_name) %>% 
      str_replace(.,".pdf",""),
    raw_text = map(file_name, pdftools::pdf_text)
  )


df <- list.files("intermediate_data/", full.names=T) %>%
  keep(~str_ends(., "rds")) %>% 
  map_dfr(readRDS)
