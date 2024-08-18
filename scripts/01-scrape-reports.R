library(magrittr)
library(stringr)
library(lubridate)
library(xml2)
library(dplyr)

source("scripts/utils.R")



# 0. Define Functions -------------------------------------------------

parse_victim = function(x) {
  
  out = list(
    name = x %>% find_class("victims__name") %>% unlist %>% paste0(collapse = ""),
    age = x %>% find_class("commas") %>% find_class("victim-age") %>% unlist,
    gender = x %>% find_class("commas") %>% find_class("victim-gender") %>% unlist,
    pregnant = x %>% find_class("commas") %>% find_class("victim-pregnant") %>% unlist,
    notes = x %>% find_class("commas") %>% find_class("victim-notes") %>% unlist,
    killed_or_injured = x %>% find_class("commas") %>% find_class("victim-killed-injured") %>% unlist,
    moh_id = x %>% find_class("commas") %>% find_class("victim-reconciliation-id") %>% unlist
  )
  
  out[sapply(out, function(x) !is.null(x))]
  
}

parse_incident = function(x) {
  
  find_class(x, "victims__victim") %>% 
    lapply(parse_victim) %>% 
    lapply(as.data.frame, row.names = NULL) %>%
    bind_rows %>%
    mutate(victim_id = row_number()) %>%
    select(victim_id, everything())
  
}



# 1. Scrape Reports ---------------------------------------------------

urls = readLines("data/incident_urls_20240816.txt")

victims_by_incident = list()

for (i in seq(length(urls))) {

  url = urls[i]
  cat(url)
  cat('\n')
  
  victims_by_incident[[i]] = 
    read_html(url) %>%
    as_list %>%
    parse_incident %>% 
    mutate(
      incident_id = str_extract(url, "ispt[0-9a-z]+"),
      incident_date = mdy(str_extract(url, "[a-z]+-[0-9]{1,2}-[0-9]{4}"))
    ) %>%
    select(incident_id, incident_date, everything())
  
}

df_victims = 
  bind_rows(victims_by_incident) %>% 
  arrange(incident_date, incident_id, victim_id) %>%
  mutate_if(is.character, function(x) str_replace_all(x, ',', '..')) %>%
  mutate_if(is.character, function(x) str_replace_all(x, '[“”"]', '`'))



# 2. Output -----------------------------------------------------------

write.csv(df_victims, "data/victims.csv", row.names = FALSE, na = "", quote = FALSE)


# Check

tmp = read.csv(
  "data/victims.csv", na = "",
  colClasses = { x = rep("character", 10); x[2] = "Date"; x[3] = "integer"; x }
)

all.equal(df_victims, tmp)
