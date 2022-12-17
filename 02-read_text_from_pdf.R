library(furrr)
plan(multisession(workers = 8))

read_pdf <- safely( # hande errors
  .f = function(x) {
    message("Reading: ", x)
    pdftools::pdf_text(x)
  }, otherwise = NA, quiet = FALSE
)

list.files("raw_pdf_files", full.names = TRUE) %>%
  map(list.files, full.names = TRUE) %>%
  reduce(c) %>%
  tibble(file_name = .) %>%
  transmute(
    year = str_extract(file_name, "\\d{4}"),
    company = sub(".*/", "", file_name) %>%
      str_replace(.,".pdf",""),
    raw_text = future_map(file_name, read_pdf, .progress = T)
    # read pdf in parallel
  ) %>%
  saveRDS("intermediate_data/raw_text.rds")
