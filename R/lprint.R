# goal: print name of object and object contents
# (saves from having to use paste0 and entering the variable names)
lprint <- function(., delim=" : ") {
  return(list(name = deparse(substitute(.)), value= .))
}