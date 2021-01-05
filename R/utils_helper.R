#' @import dplyr
#' @importFrom purrr pluck
NULL

#alias
f <- stringr::str_glue 

suffix <- c(".x", ".y")

# workaround till https://github.com/rstudio/pool/issues/111 is fixed
sql_join_suffix.default <- function(con, ...) {
  c(".x", ".y")
}
#environment(sql_join_suffix.default) <- as.environment("package:dbplyr")


#until stringr > 1.4 is released
subset_safely <- function(x, index) {
  if (length(x) < index) {
    return(NA_character_)
  }
  x[[index]]
}

str_split_n <- function(string, pattern, n) {
  out <- stringr::str_split(string, pattern)
  vapply(out, subset_safely, character(1L), index = n)
}
