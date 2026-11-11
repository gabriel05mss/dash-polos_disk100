
grafico_linha_grupo <- function(
    dados,
    x,
    y,
    grupo,
    titulo = NULL,
    titulo_x = NULL,
    titulo_y = NULL,
    cores = NULL,
    legenda = TRUE,
    altura = 400,
    debug = FALSE
) {
  
  # --------------------------------------------------
  # 1. Identifica as colunas
  # --------------------------------------------------
  
  x_col <- rlang::as_name(rlang::ensym(x))
  y_col <- rlang::as_name(rlang::ensym(y))
  grupo_col <- rlang::as_name(rlang::ensym(grupo))
  
  colunas_necessarias <- c(x_col, y_col, grupo_col)
  
  if (!all(colunas_necessarias %in% names(dados))) {
    stop("Uma ou mais colunas informadas não existem em 'dados'.")
  }
  
  # --------------------------------------------------
  # 2. Prepara os dados
  # --------------------------------------------------
  
  dados_plot <- dados %>%
    dplyr::transmute(
      periodo_original = as.character(.data[[x_col]]),
      valor = as.numeric(.data[[y_col]]),
      grupo = as.character(.data[[grupo_col]])
    ) %>%
    dplyr::filter(
      !is.na(periodo_original),
      !is.na(valor),
      !is.na(grupo)
    )
  
  # --------------------------------------------------
  # 3. Extrai semestre e ano
  # --------------------------------------------------
  
  periodo <- trimws(dados_plot$periodo_original)
  
  semestre <- rep(NA_integer_, length(periodo))
  ano <- rep(NA_integer_, length(periodo))
  
  # Caso 1: formato 01/01/2011 ou 01/02/2011
  # O segundo campo representa diretamente o semestre.
  
  formato_semestre_completo <- grepl(
    "^01/0?[12]/[0-9]{4}($|[[:space:]])",
    periodo
  )
  
  semestre[formato_semestre_completo] <- as.integer(
    sub(
      "^01/0?([12])/.*$",
      "\\1",
      periodo[formato_semestre_completo]
    )
  )
  
  ano[formato_semestre_completo] <- as.integer(
    sub(
      "^01/0?[12]/([0-9]{4}).*$",
      "\\1",
      periodo[formato_semestre_completo]
    )
  )
  
  # Caso 2: formato 01/2011 ou 02/2011
  
  formato_semestre_simples <- grepl(
    "^0?[12]/[0-9]{4}$",
    periodo
  ) & is.na(semestre)
  
  semestre[formato_semestre_simples] <- as.integer(
    sub(
      "^0?([12])/.*$",
      "\\1",
      periodo[formato_semestre_simples]
    )
  )
  
  ano[formato_semestre_simples] <- as.integer(
    sub(
      "^0?[12]/([0-9]{4})$",
      "\\1",
      periodo[formato_semestre_simples]
    )
  )
  
  # Caso 3: datas convencionais DD/MM/AAAA
  # O mês é convertido em semestre.
  
  formato_data_br <- grepl(
    "^[0-9]{2}/[0-9]{2}/[0-9]{4}",
    periodo
  ) & is.na(semestre)
  
  mes_br <- as.integer(
    substr(periodo[formato_data_br], 4, 5)
  )
  
  semestre[formato_data_br] <- ifelse(
    mes_br <= 6, 1L, 2L
  )
  
  ano[formato_data_br] <- as.integer(
    substr(periodo[formato_data_br], 7, 10)
  )
  
  # Caso 4: datas ISO AAAA-MM-DD
  
  formato_iso <- grepl(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}",
    periodo
  ) & is.na(semestre)
  
  mes_iso <- as.integer(
    substr(periodo[formato_iso], 6, 7)
  )
  
  semestre[formato_iso] <- ifelse(
    mes_iso <= 6, 1L, 2L
  )
  
  ano[formato_iso] <- as.integer(
    substr(periodo[formato_iso], 1, 4)
  )
  
  # --------------------------------------------------
  # 4. Padroniza os períodos
  # --------------------------------------------------
  
  dados_plot$semestre <- semestre
  dados_plot$ano <- ano
  
  dados_plot$periodo <- ifelse(
    !is.na(semestre) & !is.na(ano),
    sprintf("%02d/%04d", semestre, ano),
    NA_character_
  )
  
  # --------------------------------------------------
  # 5. Diagnósticos
  # --------------------------------------------------
  
  if (debug) {
    
    cat("\n========== DIAGNÓSTICO DO GRÁFICO ==========\n")
    
    cat("\nPeríodos originais encontrados:\n")
    print(unique(periodo))
    
    cat("\nConversão dos períodos:\n")
    
    print(
      dados_plot %>%
        dplyr::distinct(periodo_original, semestre, ano, periodo) %>%
        dplyr::arrange(ano, semestre)
    )
    
    cat("\nQuantidade de observações por semestre:\n")
    
    print(
      dados_plot %>%
        dplyr::count(ano, semestre, periodo) %>%
        dplyr::arrange(ano, semestre)
    )
    
    cat("\nPeríodos não reconhecidos:\n")
    print(unique(dados_plot$periodo_original[is.na(dados_plot$periodo)]))
    
    cat("\nGrupos encontrados:\n")
    print(unique(dados_plot$grupo))
    
    cat("\n============================================\n")
  }
  
  # Remove períodos inválidos
  
  dados_plot <- dados_plot %>%
    dplyr::filter(!is.na(periodo))
  
  if (nrow(dados_plot) == 0) {
    stop("Não foi possível identificar nenhum período válido.")
  }
  
  # --------------------------------------------------
  # 6. Agrega os valores por grupo e semestre
  # --------------------------------------------------
  
  dados_plot <- dados_plot %>%
    dplyr::group_by(grupo, periodo, ano, semestre) %>%
    dplyr::summarise(
      valor = sum(valor, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Ordena cronologicamente
  
  periodos <- dados_plot %>%
    dplyr::distinct(periodo, ano, semestre) %>%
    dplyr::arrange(ano, semestre) %>%
    dplyr::pull(periodo)
  
  grupos <- sort(unique(dados_plot$grupo))
  
  # --------------------------------------------------
  # 7. Monta o gráfico
  # --------------------------------------------------
  
  grafico <- highcharter::highchart() %>%
    highcharter::hc_chart(
      type = "line",
      height = altura
    ) %>%
    highcharter::hc_title(
      text = titulo
    ) %>%
    highcharter::hc_xAxis(
      categories = periodos,
      title = list(text = titulo_x)
    ) %>%
    highcharter::hc_yAxis(
      title = list(text = titulo_y),
      min = 0
    ) %>%
    highcharter::hc_tooltip(
      shared = TRUE,
      crosshairs = TRUE
    ) %>%
    highcharter::hc_legend(
      enabled = legenda
    )
  
  # --------------------------------------------------
  # 8. Adiciona uma série para cada grupo
  # --------------------------------------------------
  
  for (i in seq_along(grupos)) {
    
    grupo_atual <- grupos[i]
    
    dados_grupo <- dados_plot %>%
      dplyr::filter(grupo == grupo_atual)
    
    valores <- dados_grupo$valor[
      match(periodos, dados_grupo$periodo)
    ]
    
    argumentos <- list(
      name = grupo_atual,
      data = as.list(valores),
      type = "line"
    )
    
    if (!is.null(cores) && length(cores) > 0) {
      argumentos$color <- cores[
        ((i - 1) %% length(cores)) + 1
      ]
    }
    
    grafico <- do.call(
      highcharter::hc_add_series,
      c(list(grafico), argumentos)
    )
  }
  
  # --------------------------------------------------
  # 9. Retorna o gráfico
  # --------------------------------------------------
  
  return(grafico)
}
