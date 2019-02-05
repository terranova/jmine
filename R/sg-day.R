#' Downloads and parses one day
#'
#' Uses \code{esaj} to download and parse all files from cjsg and cposg from
#' TJSP for one day
#'
#' @param date date to download. Default is today minus one.
#' @param path path to directory where to create the folder.
#' @param folder_prefix folder prefix to create. Default is \code{sg_}.
#' @param verbose print messages. Default is to print messages.
#'
#' @importFrom magrittr %>%
#'
#' @export
sg_day <- function(date = Sys.Date() - 1, path = ".",
                   verbose = TRUE, folder_prefix = "sg_") {
  date <- as.Date(date)
  if (verbose) cat(paste("Downloading files for", date, "\n"))
  if (verbose) cat("    Downloading cjsg...\n")
  # folders --------------------------------------------------------------------
  full_path <- sprintf("%s/%s%s", path, folder_prefix, date)
  full_path_cjsg <- paste0(full_path, "/html_cjsg")
  full_path_cposg <- paste0(full_path, "/html_cposg")
  full_path_cposg_rds <- paste0(full_path, "/rds_cposg")
  fs::dir_create(full_path_cjsg)
  fs::dir_create(full_path_cposg)
  fs::dir_create(full_path_cposg_rds)

  # downloading cjsg------------------------------------------------------------
  pages <- esaj::peek_cjsg(
    query = "",
    registration_start = date,
    registration_end = date,
    max_page = Inf)
  m_page <- pages[2] %/% 20 + 1
  esaj::download_cjsg(query = "", path = full_path_cjsg,
                      registration_start = date,
                      registration_end = date,
                      max_page = m_page, wait = 1.0)

  # parsing cjsg to get ids-----------------------------------------------------
  if (verbose) cat("    Parsing cjsg...\n")
  cjsg_files <- fs::dir_ls(full_path_cjsg, regexp = "page")
  d_cjsg <- abjutils::pvec(cjsg_files, esaj::parse_cjsg) %>%
    tidyr::unnest(output) %>%
    dplyr::select(-id, -return)
  readr::write_rds(d_cjsg, paste0(full_path, "/cjsg.rds"), compress = "xz")
  ids <- d_cjsg %>%
    dplyr::pull(id_lawsuit) %>%
    unique() %>%
    stringr::str_replace_all("[^0-9]", "")

  # downloading cposg-----------------------------------------------------------
  if (verbose) cat("    Downloading cposg...\n")
  res_download <- abjutils::pvec(ids, esaj::download_cposg,
                                 path = full_path_cposg)
  if (any(res_download$return == "error"))
    warning("There were errors downloading CPOSG.")
  # parsing cposg --------------------------------------------------------------
  if (verbose) cat("    Parsing cposg...\n")

  parse_movs2 <- function(parser) {
    stopifnot("parser" %in% class(parser))
    get_movs <- function(html) {
      xp0 <- "//*[@id='tabelaTodasMovimentacoes']"
      tab <- xml2::xml_find_all(html, paste0(xp0, "//parent::table"))
      # if (length(tab) == 0) tab <- xml2::xml_find_all(html, xp0)
      tab %>%
        rvest::html_table(fill = TRUE) %>%
        purrr::pluck(1) %>%
        janitor::clean_names() %>%
        dplyr::as_tibble() %>%
        dplyr::select(movement = data, X3 = movimento) %>%
        dplyr::filter(movement != "") %>%
        tidyr::separate(X3, c("title", "txt"), sep = "\n\t",
                        extra = "merge", fill = "right") %>%
        dplyr::mutate_all(stringr::str_squish) %>%
        dplyr::mutate(movement = lubridate::dmy(movement, quiet = TRUE))
    }
    purrr::list_merge(parser, name = "movs", getter = get_movs)
  }

  cposg_files <- full_path_cposg %>%
    fs::dir_ls() %>% 
    normalizePath() %>% 
    purrr::set_names()
  parser <- esaj::make_parser()
  parser <- parser %>%
    esaj::parse_data() %>%
    esaj::parse_parts() %>%
    parse_movs2() %>%
    esaj::parse_decisions()
  d_cposg <- abjutils::pvec(cposg_files,
                            esaj::run_parser,
                            parser = parser,
                            path = full_path_cposg_rds)
  readr::write_rds(d_cposg, paste0(full_path, "/cposg.rds"), compress = "xz")
  return(invisible(TRUE))
}
