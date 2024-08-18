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
