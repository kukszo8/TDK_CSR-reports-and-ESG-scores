library(magrittr)
library(tidyverse)
library(tidytext)
library(tidyquant)
library(stm)
library(pins)
library(knitr)


suppressMessages({
  tryCatch({
    od <- Microsoft365R::get_business_onedrive(tenant = "common")
    
    if (od$properties$owner$user$displayName == "Granát Marcell Péter") {
      board <- board_ms365(
        drive = od, 
        path = "csr_reports"
      )
    } else {
      shared_items <- od$list_shared_files()
      folder_to_board <- shared_items$remoteItem[[which(shared_items$name == "csr_reports")]]
      if (!exists("folder_to_board")) message("You need access to the data")
      board <- board_ms365(od, folder_to_board)
    }

  })
})

