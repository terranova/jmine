#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


re_pj <- function() {
  stringr::regex(stringr::str_c(
    "S[/.]A", "LTDA", "EIRELI", " ME$", "ITAU", "FINANCEIR",
    "FINANCIAM", "SEGUR[AO]", "BANCO", "TELE[CF]O", "CARTAO", "CARTOES",
    "PETROB", "FUNDACAO", "ASSOCIACAO", "EDUCACION", "UNIMED", " SA$", "SAUDE",
    "CREDITO", "LOJAS", "CASAS BAHIA", "SANTANDER", "BRADES",
    sep = "|"), ignore_case = TRUE)
}

# extrai o teor da decisão
stat_decision <- function(x) {
  dct <- stringr::str_detect
  re_negaram <- "negaram|neagram|desprovido|nego prov|improced|indef[ei]r|deneg" %>%
    stringr::str_c("nega-se|mantid|n[aã]o prov|mantiveram|negado prov", sep = "|") %>%
    stringr::str_c("nega prov", sep = "|") %>%
    stringr::regex(ignore_case = TRUE)
  re_parcial <- stringr::regex("parcial|em parte", ignore_case = TRUE)
  re_deram <- stringr::regex("der[ea]m|alteraram|retific|proced|reform|acolh|provido|dar prov", ignore_case = TRUE)
  re_conhec <- stringr::regex("co?nhec", ignore_case = TRUE)
  re_dilig <- stringr::regex("dilig", ignore_case = TRUE)
  re_prejud <- stringr::regex("prejud", ignore_case = TRUE)
  re_acordo <- stringr::regex("acordo|autocom", ignore_case = TRUE)
  re_desist <- stringr::regex("desist|ren[uú]n", ignore_case = TRUE)
  re_anul <- stringr::regex("anul", ignore_case = TRUE)
  re_extin <- stringr::regex("extin|prescri", ignore_case = TRUE)
  dplyr::case_when(
    dct(x, re_parcial) ~ "parcial",
    dct(x, re_negaram) & dct(x, re_deram) ~ "parcial",
    dct(x, re_negaram) ~ "negou",
    dct(x, re_deram) ~ "aceitou",
    dct(x, re_acordo) ~ "acordo",
    dct(x, re_conhec) ~ "não conhecido",
    dct(x, re_dilig) ~ "diligência",
    dct(x, re_prejud) ~ "prejudicado",
    dct(x, re_desist) ~ "desistência",
    dct(x, re_anul) ~ "anulado",
    dct(x, re_extin) ~ "extinto",
    TRUE ~ "outro"
  )
}

# verifica se a decisão é unânime
stat_unanime <- function(x) {
  dct <- stringr::str_detect
  re_unanime <- stringr::regex(
    "v *\\. *u *\\. *|un[aâ]nim|v\\.? ?u$|^vu[, ]|VU\\.?$",
    ignore_case = TRUE
  )
  re_maioria <- stringr::regex("maioria|vencido", ignore_case = TRUE)
  dplyr::case_when(
    dct(x, re_unanime) ~ "unanime",
    dct(x, re_maioria) ~ "maioria",
    TRUE ~ "outro"
  )
}

# arruma o texto dos titulos das infos basicas
clean_key <- function(key) {
  key %>%
    stringr::str_squish() %>%
    abjutils::rm_accent() %>%
    stringr::str_to_lower()
}

clean_comarca <- function(x) {
  x %>%
    stringr::str_squish() %>%
    abjutils::rm_accent() %>%
    stringr::str_to_upper()
}

# arruma info de dinheiro
parse_real <- function(x) {
  loc <- readr::locale(decimal_mark = ",", grouping_mark = ".")
  readr::parse_number(x, locale = loc)
}

idade <- function(data_nascimento) {
  tod <- Sys.Date()
  nasc <- as.Date(as.numeric(data_nascimento), origin = "1900-01-01")
  as.numeric(tod - nasc) / 365.242
}
idade_ano <- function(data_nascimento) {
  ano <- lubridate::year(Sys.Date())
  ano - as.numeric(stringr::str_extract(data_nascimento, "[0-9]{4}"))
}
clean_nm <- function(nome) {
  nm <- nome %>%
    stringr::str_to_upper() %>%
    abjutils::rm_accent() %>%
    stringr::str_squish()
  f <- stringr::str_extract(nm, "^[A-Z]+")
  l <- stringr::str_extract(nm, "[A-Z]+$")
  paste(f, l)
}

arruma_titulo <- function(x) {
  dplyr::case_when(
    x == 'nome' ~ 'Nome',
    x == 'info_assunto' ~ 'Assunto',
    x == 'assunto' ~ 'Assunto',
    x == 'camara' ~ 'Câmara',
    x == 'area' ~ 'Área',
    x == 'faculdade' ~ 'Faculdade',
    x == 'origem' ~ 'Origem',
    x == 'idade' ~ 'Idade',
    x == 'tem_pos' ~ 'Tem pós',
    x == 'tempo_form' ~ 'Tempo formado',
    x %in% c('tempo_', 'tempo_2inst') ~ 'Tempo tribunal',
    x == 'cidade' ~ 'Cidade origem'
  )
}




