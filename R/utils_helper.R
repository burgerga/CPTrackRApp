#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @importFrom purrr pluck
NULL

#' @importFrom dbplyr dbplyr_edition
#' @export
dbplyr_edition.myConnectionClass <- function(con) 2L

#alias
f <- stringr::str_glue 

suffix <- c(".x", ".y")

# workaround till https://github.com/rstudio/pool/issues/111 is fixed
sql_join_suffix.default <- function(con, ...) {
  c(".x", ".y")
}
#environment(sql_join_suffix.default) <- as.environment("package:dbplyr")


#until stringr > 1.? is released
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
