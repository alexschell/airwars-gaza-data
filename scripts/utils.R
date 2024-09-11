library(httr)
library(rvest)
library(xml2)
library(dplyr)
library(stringr)
library(purrr)
library(lubridate)

# deprecated
search_nested_list = function(lst, predicate) {

  search_helper = function(item) {
    if (is.list(item)) {
      if (predicate(item)) {
        list(item)
      } else {
        unlist(lapply(item, search_helper), recursive = FALSE)
      }
    } else if (predicate(item)) {
      list(item)
    } else {
      list()
    }
  }
  
  search_helper(lst)

}

safe_read_html = function(url) {
  tryCatch(
    {
      response = GET(url)
      stop_for_status(response)
      list(result = read_html(response), error = NULL)
    },
    error = function(e) {
      list(result = NULL, error = e)
    }
  )
}

parse_victim = function(x) {
  name_en = x %>% html_node(".victims__name strong") %>% html_text()
  name_ar = x %>% html_node(".victims__name span") %>% html_text()
  details = x %>% html_node(".commas")
  
  age = details %>% html_node(".victim-age") %>% html_text()
  gender = details %>% html_node(".victim-gender") %>% html_text()
  pregnant = details %>% html_node(".victim-pregnant") %>% html_text()
  notes = details %>% html_node(".victim-notes") %>% html_text()
  killed_or_injured = details %>% html_node(".victim-killed-injured") %>% html_text()
  moh_id = details %>% html_node(".victim-reconciliation-id") %>% html_text()
  
  data.frame(
    name_en = name_en,
    name_ar = name_ar,
    age = age,
    gender = gender,
    pregnant = pregnant,
    notes = notes,
    killed_or_injured = killed_or_injured,
    moh_id = moh_id
  )
}

get_family_ids = function(x) {
  
  victims_by_family = x %>% html_nodes(".info-main-block .victims") %>% html_children()
  
  n_victims_ungrouped = x %>% html_nodes(".info-main-block > .victims__victim") %>% length()
  n_victims_by_family = x %>% html_nodes(".info-main-block .victims .victims__victim") %>% length()
  
  if (n_victims_by_family > 0) {
    
    idx_labels = which(html_attr(victims_by_family, "class") == "victims__label")
    idx_victims = which(html_attr(victims_by_family, "class") == "victims__victim")
    idx_labels = idx_labels[idx_labels < max(idx_victims)]  # edge case: ispt0580
    family_ids = sapply(idx_victims, function(x) which(x < c(idx_labels[-1], Inf))[1]) %>% unname
    
    # Checks
    n_labels = victims_by_family[idx_labels] %>% html_text() %>% sapply(str_extract, "[0-9]+") %>% as.numeric
    stopifnot(all(n_labels == table(family_ids)))
    stopifnot(all(idx_victims > idx_labels[1]))
    stopifnot(all(unique(family_ids) == seq_along(unique(family_ids))))
    
    c(family_ids, rep(NA_integer_, n_victims_ungrouped))
    
  } else {
    
    rep(NA_integer_, n_victims_ungrouped)
    
  }
  
}

parse_incident = function(html) {
    
  if (is.null(html)) {
    return(NULL)
  }
  
  incident_id = html %>% html_node(".meta-block.code.current span") %>% html_text(trim = TRUE) %>% tolower()
  incident_date = html %>% html_node("div[class='meta-block']") %>% html_text(trim = TRUE) %>% 
    str_extract("[A-Za-z]+ [0-9]{1,2}([^A-Za-z0-9 ][0-9]{1,2})?, [0-9]{4}") %>% str_remove("[^A-Za-z0-9 ][0-9]{1,2}") %>% mdy()
  victims = html %>% html_nodes(".victims__victim")
  victim_family_ids = html %>% get_family_ids()
  
  victims %>% 
    lapply(parse_victim) %>% 
    bind_rows() %>%
    mutate(
      incident_id = incident_id,
      incident_date = incident_date,
      family_id = victim_family_ids,
      victim_id = row_number()
    ) %>%
    select(incident_id, incident_date, family_id, victim_id, everything())
  
}
