sp500_ticker <- tidyquant::tq_index("SP500")$symbol

# sp500_ticker <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") %>%
#   html_table %>%
#   .[1] %>%
#   reduce(c) %>%
#   pull(Symbol)

url_nasdaq <- str_c("https://www.responsibilityreports.com/HostedData/ResponsibilityReportArchive/",
                    sp500_ticker %>%
                      str_sub(end = 1) %>%
                      str_to_lower(),
                    "/NASDAQ_",
                    sp500_ticker,
                    "_2020.pdf") %>%
  ifelse(str_detect(., "MMM"), str_replace(., "/m/", "/3/"), .) # manual cleaning

url_nyse <- str_replace(url_nasdaq, "NASDAQ", "NYSE")

download_pdf <- function(x) {

  ticker <- str_extract(x, "NASDAQ_.*_2020|NYSE_.*_2020") %>%
    str_remove("NASDAQ_|NYSE_") %>%
    str_remove("_2020")

  file_name <- str_c("raw_pdf_files/", ticker, ".pdf")

  if (!file.exists(file_name)) {
    message("Download ", ticker)
    tryCatch({
      download.file(x, destfile = file_name, quiet = TRUE)
    }, error = function(e) message(crayon::bgRed("Failed to download."))
    )
  } else {
    message(ticker, ": This file already exists, skip to next")
  }
}

walk(c(url_nasdaq, url_nyse), download_pdf)
