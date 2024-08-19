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

class_attr = function(x, y) !is.null(attr(x, ".class")) && attr(x, ".class") == y

find_class = function(x, y) search_nested_list(x, function(u) class_attr(u, y))

parse_victim = function(x) {
  
  out = list(
    name_en = x %>% find_class("victims__name") %>% unlist %>% `[`(c("div.strong", "div.strong.a")) %>% na.omit %>% `[`(1),
    name_ar = x %>% find_class("victims__name") %>% unlist %>% `[`(c("div.span", "div.strong.a.span")) %>% na.omit %>% `[`(1),
    age = x %>% find_class("commas") %>% find_class("victim-age") %>% unlist,
    gender = x %>% find_class("commas") %>% find_class("victim-gender") %>% unlist,
    pregnant = x %>% find_class("commas") %>% find_class("victim-pregnant") %>% unlist,
    notes = x %>% find_class("commas") %>% find_class("victim-notes") %>% unlist,
    killed_or_injured = x %>% find_class("commas") %>% find_class("victim-killed-injured") %>% unlist,
    moh_id = x %>% find_class("commas") %>% find_class("victim-reconciliation-id") %>% unlist
  )
  
  out[sapply(out, function(x) !is.null(x))]
  
}

get_family_ids = function(x) {
  
  victims_by_family = 
    find_class(x, "info-main-block victims") %>% 
    find_class("victims")
  
  n_victims_ungrouped = 
    find_class(x, 'info-main-block victims')[[1]] %>% 
    sapply(class_attr, "victims__victim") %>% 
    sum
  
  if (length(victims_by_family) > 0) {
    
    idx_labels = victims_by_family[[1]] %>% sapply(class_attr, "victims__label") %>% which
    idx_victims = victims_by_family[[1]] %>% sapply(class_attr, "victims__victim") %>% which
    idx_labels = idx_labels[idx_labels < max(idx_victims)]  # edge case: ispt0580
    family_ids = sapply(idx_victims, function(x) which(x < c(idx_labels[-1], Inf))[1]) %>% unname
    
    # Checks
    n_labels = victims_by_family[[1]][idx_labels] %>% sapply(str_extract, "[0-9]+") %>% as.numeric
    stopifnot(all(n_labels == table(family_ids)))
    stopifnot(all(idx_victims > idx_labels[1]))
    stopifnot(all(unique(family_ids) == seq_along(unique(family_ids))))
    
    c(family_ids, rep(NA_integer_, n_victims_ungrouped))
    
  } else {
    
    rep(NA_integer_, n_victims_ungrouped)
    
  }
  
}

parse_incident = function(x) {
  
  find_class(x, "victims__victim") %>% 
    lapply(parse_victim) %>% 
    lapply(as.data.frame, row.names = NULL) %>%
    bind_rows %>%
    mutate(victim_id = row_number()) %>%
    mutate(family_id = get_family_ids(x)) %>%
    select(family_id, victim_id, everything())
  
}
