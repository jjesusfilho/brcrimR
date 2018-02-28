#' Download tables publicized by the São Paulo's Security Office
#'
#'@param year Year to download.
#'@param city Intended city. Is a number between 1 and 645, representing the rank of the city in alphabetical order.
#'@param type Formulary to make the request, encodes the type of information to be downloaded.
#'    "ctl00$conteudo$$btnMes" downloads recorded crime count.
#'@param region Intended region. Is a number between 0 and 9, representing an administrative region.
#'@param police_station number of the police station
#'
#' @export
get_summarized_table_sp <-
  function (year=NULL,
            city=NULL,
            type = "ctl00$conteudo$btnMensal",
            region = "0",
            police_station = "0")
  {
    url <- "http://www.ssp.sp.gov.br/Estatistica/Pesquisa.aspx"
    pivot <- httr::GET(url)
    states <- get_viewstate(pivot)
    params <- list(
      `__EVENTTARGET` = type,
      `__EVENTARGUMENT` = "",
      `__LASTFOCUS` = "",
      `__VIEWSTATE` = states$vs,
      `__EVENTVALIDATION` = states$ev,
      `ctl00$conteudo$ddlAnos` = year,
      `ctl00$conteudo$ddlRegioes` = region,
      `ctl00$conteudo$ddlMunicipios` = city,
      `ctl00$conteudo$ddlDelegacias` = police_station
    )
    httr::POST(url, body = params, encode = "form") %>%
      xml2::read_html() %>%
      rvest::html_table() %>% dplyr::first()  %>%
      dplyr::mutate_at(.funs = parse_number_br, .vars = dplyr::vars(Jan:Total)) %>%
      dplyr::mutate(municipio = city,
                    regiao = region,
                    ano = year)
  }


#' Download detailed information publicized by São Paulo's Security Office
#'
#' @param dir Destination directory. It defaults to current directory.
#' @param year Year to download.
#' @param month Intended city. Is a number between 1 and 645, representing the rank of the city in alphabetical order.
#' @param police_station number of the police station
#' @param helper Logical. If TRUE (default), it will compatibilize the argument values with those required by the department.
#' @param ...  other arguments passed on to the httr::POST function
#'
#' @export
get_detailed_table_sp <- function(dir=".", year=NULL, month=NULL, police_station=NULL,  helper = T,...){


  url = 'http://www.ssp.sp.gov.br/transparenciassp/'

  if(helper){
    h <- helper_sp(f=dir, y=year, m=month, d=police_station)
  } else {
    h <- list(f = dir, y = year, m = month, d = police_station)
  }

  httr::GET(url) %>%
    browse(h$f, dest = 'dir') %>%
    browse(h$y, dest = 'year') %>%
    browse(h$m, dest = 'month') %>%
    get_table(h$d, dest='police_station') %>%
    open_table()
}

browse <- function(r, val, dest){

  params <- basic_params(val, dest, get_viewstate(r))

  httr::POST(r$url, body = params, encode = 'form', handle = r)
}

get_table <- function(r, police_station = '0', hdf = "1504014009092", export_header = "ExportarBOLink", ...){

  params <- basic_params(export_header, 'dir', get_viewstate(r)) %>%
    append(list(`ctl00$cphBody$filtroDepartamento` = police_station,
                `ctl00$cphBody$hdfExport` = hdf))

  httr::POST(r$url, body = params, encode = 'form', handle = r, ...)
}

#' @export
#' @describeIn get_detailed_table_sp Get historical detailed table
get_historical_detailed_table_sp <- function(){

  h <- helper_sp(f=dir, y=year, m=month, d=police_station)

  expand.grid(dir = h$f, year = h$y, month = h$m, police_station = h$d,
              stringsAsFactors = F) %>%
    as.list() %>%
    purrr::pmap(get_detailed_table_sp, helper = F)
}

#' @export
#' @describeIn get_summarized_table_sp Allows for getting summarized tables from
#'     multiple years and cities.
get_historical_summarized_table_sp <- function(){

  expand.grid(year=year, city=city, type=type,
              stringsAsFactors = F) %>%
    as.list() %>%
    purrr::pmap_df(get_summarized_table_sp)
}
