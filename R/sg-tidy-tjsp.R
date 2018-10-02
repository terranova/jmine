tidy_tjsp_cposg_data <- function(cposg) {
  re_coma <- "(?<=Comarca de ).*"
  cposg_data <- cposg %>%
    dplyr::filter(return != "error") %>%
    tidyr::unnest(output) %>%
    dplyr::select(id1, file, data) %>%
    tidyr::unnest() %>%
    dplyr::mutate(data = clean_key(data)) %>%
    dplyr::group_by(data) %>%
    dplyr::filter(n() > 1) %>%
    dplyr::group_by(id1, file, data) %>%
    dplyr::summarise(value = paste(unique(value), collapse = "@")) %>%
    dplyr::ungroup() %>%
    tidyr::spread(data, value) %>%
    janitor::clean_names() %>%
    dplyr::rename(n_processo = id1) %>%
    # arrumar
    dplyr::mutate(
      camara = stringr::str_extract(distribuicao, "[0-9]+"),
      camara = stringr::str_pad(camara, 2, "left", "0"),
      tipo_camara = dplyr::case_when(
        stringr::str_detect(distribuicao, "Criminal") ~ "Criminal",
        stringr::str_detect(distribuicao, "Privado") ~ "Privado",
        stringr::str_detect(distribuicao, "Empresarial") ~ "Empresarial",
        stringr::str_detect(distribuicao, "P[uú]blico") ~ "Público",
        stringr::str_detect(distribuicao, "Ambien") ~ "Ambiental",
        stringr::str_detect(distribuicao, "Recup") ~ "Falência e Recuperação",
        TRUE ~ NA_character_
      ),
      regime = dplyr::case_when(
        stringr::str_detect(distribuicao, "Extraordin") ~ "Extraordinária",
        stringr::str_detect(distribuicao, "ª Câmara (Reservada )?de.*[^A-Z]$") ~ "Ordinária",
        TRUE ~ "Outro"
      )
    ) %>%
    dplyr::mutate(
      area = dplyr::if_else(area == "Criminal", "Criminal", "Privado")
    ) %>%
    # arrumar
    dplyr::mutate(area = dplyr::case_when(
      area == "Privado" & camara %in% sprintf("%02d", 1:10) ~ "Família",
      area == "Privado" & camara %in% sprintf("%02d", c(11:24, 37:38)) ~ "Contratos",
      area == "Privado" & camara %in% sprintf("%02d", 25:36) ~ "Imobiliário",
      area == "Criminal" ~ "Criminal"
    )) %>%
    tidyr::separate(origem, c("info_comarca", "info_foro", "info_vara"),
                    sep = " / ", extra = "merge", fill = "right") %>%
    tidyr::separate(assunto, c("assunto_pai", "assunto_filho"),
                    sep = " ?- ?", remove = FALSE, extra = "merge",
                    fill = "right") %>%
    dplyr::mutate(info_cruzeiro = stringr::str_detect(valor_da_acao, "[cC]"),
                  info_valor = parse_real(valor_da_acao),
                  info_comarca = stringr::str_extract(info_comarca, re_coma))

  cposg_data %>%
    dplyr::select(
      n_processo,
      file,
      info_area = area,
      info_classe = classe,
      info_assunto_full = assunto,
      info_assunto_pai = assunto_pai,
      info_assunto_filho = assunto_filho,
      info_camara_nm = distribuicao,
      info_camara_num = camara,
      info_camara_tipo = tipo_camara,
      info_camara_regime = regime,
      info_relator = relator,
      info_comarca,
      info_foro,
      info_status = situacao,
      info_cruzeiro,
      info_valor
    )
}

tidy_tjsp_cposg_parts <- function(cposg) {
  passivo <- c(
    "apelado", "agravado",
    "apelada", "apdoapte", "recorrido", "apdaapte",
    "ru", "r", "sucitado", "recorrida", "reclamado", "requerido"
  )
  ativo <- c(
    "apelante", "agravante", "apteapdo", "apteapda",
    "recorrente", "impetrante", "autor", "autora",
    "suscitante", "requerente", "reclamante"
  )
  adv <- c("advogado", "advogada")

  cposg_parts <- cposg %>%
    dplyr::filter(return != "error") %>%
    tidyr::unnest(output) %>%
    dplyr::select(id1, file, parts) %>%
    tidyr::unnest() %>%
    dplyr::mutate(
      role = tolower(role),
      part = tolower(part),
      tipo_parte = dplyr::case_when(
        role %in% ativo ~ "ativo",
        role %in% passivo ~ "passivo",
        role %in% adv & part %in% passivo ~ "passivo_adv",
        role %in% adv & part %in% ativo ~ "ativo_adv",
        TRUE ~ "outro"
      )
    ) %>%
    dplyr::group_by(id1, file, tipo_parte) %>%
    dplyr::summarise(name = paste(name, collapse = "\n")) %>%
    dplyr::ungroup() %>%
    tidyr::spread(tipo_parte, name) %>%
    dplyr::mutate(
      ativo_pj = is_pj(ativo),
      passivo_pj = is_pj(passivo),
      tipo_litigio = dplyr::case_when(
        ativo_pj & passivo_pj ~ "nPF-nPF",
        ativo_pj & !passivo_pj ~ "nPF-PF",
        !ativo_pj & passivo_pj ~ "PF-nPF",
        !ativo_pj & !passivo_pj ~ "PF-PF"
      ),
      ativo = clean_part(ativo),
      passivo = clean_part(passivo)
    )

  cposg_parts %>%
    dplyr::select(
      n_processo = id1, file,
      part_ativo = ativo,
      part_ativo_adv = ativo_adv,
      part_passivo = passivo,
      part_passivo_adv = passivo_adv,
      part_tipo_litigio = tipo_litigio
    )
}

tidy_tjsp_cposg_movs <- function(cposg, cut_time = 3) {
  cposg_movs <- cposg %>%
    dplyr::filter(return != "error") %>%
    tidyr::unnest(output) %>%
    dplyr::select(id1, file, movs) %>%
    tidyr::unnest() %>%
    dplyr::filter(movement > "2000-01-01") %>%
    dplyr::arrange(id1, file, movement) %>%
    dplyr::group_by(id1, file) %>%
    dplyr::mutate(before = dplyr::lag(movement, default = 0),
                  time = as.numeric(movement - before)) %>%
    dplyr::summarise(time_tot = as.numeric(diff(range(movement))),
                     # soma todos os tempos menores de cut_time anos
                     time_clean = sum(time[time < 365 * cut_time])) %>%
    dplyr::ungroup() %>%
    dplyr::select(n_processo = id1, file, time_tot, time_clean)
  cposg_movs
}

tidy_tjsp_cposg_dec <- function(cposg) {
  re_adeq <- stringr::regex("ADEQUA", ignore_case = TRUE)

  cposg_dec <- cposg %>%
    dplyr::filter(return != "error") %>%
    tidyr::unnest(output) %>%
    dplyr::select(id1, file, decisions) %>%
    tidyr::unnest()  %>%
    dplyr::filter(!is.na(decision), !stringr::str_detect(decision, re_adeq)) %>%
    dplyr::mutate(dec = stat_decision(decision),
                  unanime = stat_unanime(decision)) %>%
    dplyr::arrange(dplyr::desc(date)) %>%
    dplyr::distinct(id1, file, .keep_all = TRUE) %>%
    dplyr::select(n_processo = id1, file,
                  dec_date = date,
                  dec_val = dec,
                  decision,
                  dec_unanime = unanime)

}


#' Tidy TJSP
#'
#' @param path folder that contains cpopg.rds file
#'
#' @export
tidy_tjsp_cposg <- function(path) {

  cposg <- readr::read_rds(paste0(path, "/cposg.rds"))
  cposg_data <- tidy_tjsp_cposg_data(cposg)
  cposg_parts <- tidy_tjsp_cposg_parts(cposg)
  cposg_movs <- tidy_tjsp_cposg_movs(cposg)
  cposg_dec <- tidy_tjsp_cposg_dec(cposg)

  cposg_data %>%
    dplyr::inner_join(cposg_parts, c("n_processo", "file")) %>%
    dplyr::inner_join(cposg_movs, c("n_processo", "file")) %>%
    dplyr::left_join(cposg_dec, c("n_processo", "file"))

}






