library(magrittr)
library(xml2)
library(dplyr)

source("scripts/utils.R")

urls = readLines("data/incident_urls_20240816.txt")

victims_by_incident = sapply(
  urls,
  function(url) {
    x = as_list(read_html(url))
    x %>% 
      find_class("victims__victim") %>% 
      lapply(parse_victim) %>% 
      lapply(as.data.frame) %>%
      bind_rows
  }
)
