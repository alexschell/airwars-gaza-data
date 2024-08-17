library(magrittr)

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
  
  unlist(search_helper(lst), recursive = FALSE)

}

class_attr = function(x, y) !is.null(attr(x, ".class")) && attr(x, ".class") == y

find_class = function(x, y) search_nested_list(x, function(u) class_attr(u, y))

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
