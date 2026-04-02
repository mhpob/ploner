title_case <- function(x) {
  s <- tolower(x)
  paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
}
