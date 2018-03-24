#' \code{brcrimR} package
#'
#' Downloads Brazilian cities crime indicators
#'
#'
#' @docType package
#' @name brcrimR
#' @importFrom magrittr %>%
#' @importFrom utils read.delim
NULL

parse_number_br <- function(char_numbers){
  char_numbers %>%
    stringr::str_replace_all("\\.", "") %>%
    stringr::str_replace_all(",", "\\.") %>%
    as.numeric()
}

if(getRversion() >= "2.15.1")
  utils::globalVariables(c(".", ".x",".y","y","ty",
                            "c","d","m","f","r",
                           "Jan","Total","helper",
                           "url","params","body",
                           "states","pivot","encode",
                           "city","year","month",
                           "type","police_station"))
