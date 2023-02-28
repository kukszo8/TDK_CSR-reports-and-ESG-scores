library(furrr)

sp500_ticker <- tidyquant::tq_index("SP500")$symbol
years_to_download <- 2010:2021

dir.create("raw_pdf_files")
walk(str_c("raw_pdf_files/", years_to_download), dir.create)

url_nasdaq <- str_c("https://www.responsibilityreports.com/HostedData/ResponsibilityReportArchive/",
                    sp500_ticker %>%
                      str_sub(end = 1) %>%
                      str_to_lower(),
                    "/NASDAQ_",
                    sp500_ticker,
                    "_") %>%
  ifelse(str_detect(., "MMM"), str_replace(., "/m/", "/3/"), .) # manual cleaning

url_nasdaq <- map(years_to_download, ~ str_c(url_nasdaq, ., ".pdf")) %>%
  # combine with year
  reduce(c)

url_nyse <- str_replace(url_nasdaq, "NASDAQ", "NYSE")

download_pdf <- function(x) {

  ticker <- str_extract(x, "NASDAQ_.*_20|NYSE_.*_20") %>%
    str_remove("NASDAQ_|NYSE_") %>%
    str_remove("_20")

  file_name <- str_c("raw_pdf_files/", str_extract(x, "20\\d\\d"), "/", ticker, ".pdf")

  if (!file.exists(file_name)) {
    tryCatch({
      download.file(x, destfile = file_name, quiet = TRUE, mode = "wb")
      message(ticker, " ", str_extract(x, "20\\d\\d"), ": ", crayon::bgGreen("downloaded"))
    }, error = function(e) message(ticker, " ", str_extract(x, "20\\d\\d"), ": ", crayon::bgRed("failed"))
    )
  } else {
    message(ticker, " ", str_extract(x, "20\\d\\d"), ": ", "already exists, skip to next")
  }
}

plan(multisession(workers = parallel::detectCores() - 1))

future_walk(c(url_nasdaq, url_nyse), download_pdf, .progress = TRUE)

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
  pin_write(
    board = board,
    name = "raw_text",
    description = "Text extracted with pdftools from the downloaded CSR reports."
  )

# unlink("raw_pdf_files", recursive = TRUE) 
# remove the temp directory
