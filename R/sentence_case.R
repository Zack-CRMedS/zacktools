#' Transform to sentence case
#'
#' @param string
#'
#' @return
#' @export
#'
#' @examples
sentence_case <- function(string) {
  string <- tolower(string)
  string <- paste(toupper(substr(string,start = 1, stop = 1)),
            substr(string,start = 2, stop = nchar(string)),sep = "")
  string <- gsub(x = string, pattern = "[.]|[-]|[\\/]|[_]", replacement = " ")
}

