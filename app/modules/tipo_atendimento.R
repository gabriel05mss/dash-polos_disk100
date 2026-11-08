Ptipo_atendimentoUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    
    fluidRow(
      box(
        title = h1('Tipo Atendimento x Denuncias - Dados Disque 100', align = 'center'), #trocar titulo
        width = 12,
        collapsible = TRUE,
        solidHeader = TRUE,
        fluidRow(
          width = 12,
          column(offset = 1, 2, uiOutput(ns("regiao_ui"))),
          column(2, uiOutput(ns("estado_ui"))),
          column(2, uiOutput(ns("intermediaria_ui"))),
          column(2, uiOutput(ns("imediata_ui"))),
          column(2, uiOutput(ns("municipio_ui")))
        )
      )
    ),
    
    fluidRow(
      
      box(
        title = h1('Distribuição Tipo de Atendimento', align = 'center'), #trocar titulo
        width = 6,
        collapsible = TRUE,
        solidHeader = TRUE,
        withSpinner(highchartOutput(ns("plot_1")), type = 1, color = "#ffae00", size = 2)
      ),
      
      box(
        title = h1('Diferença 2026/1 com 2025/1', align = 'center'), #trocar titulo
        width = 6,
        collapsible = TRUE,
        solidHeader = TRUE,
        withSpinner(highchartOutput(ns("plot_2")), type = 1, color = "#ffae00", size = 2)
      )
    ),
    
    fluidRow(
      
      box(
        title = h1('Análise temporal', align = 'center'), #trocar titulo
        width = 12,
        style = "overflow-x: auto;",
        collapsible = TRUE,
        solidHeader = TRUE,
        withSpinner(highchartOutput(ns("plot_3")), type = 1, color = "#ffae00", size = 2)
      )
    )
  )
}

Ptipo_atendimentoServer <- function(input, output, session) {
  
  ns <- session$ns
  
  # LEITURA DOS DADOS ----
  
  dados_tipo_atendimento <- readxl::read_xlsx(
    "dados/tipo_atendimento.xlsx"
  )
  
  
  # UI DINÂMICA ----
  
  ## Região ----
  
  output$regiao_ui <- renderUI({
    
    selectizeInput(
      ns("Regiao"),
      "Região",
      choices = c(
        "TODOS",
        sort(unique(na.omit(dados_tipo_atendimento$regiao_ibge)))
      ),
      selected = "TODOS",
      multiple = TRUE,
      options = list(maxOptions = 10000)
    )
    
  })
  
  
  ## Estado ----
  
  output$estado_ui <- renderUI({
    
    selectizeInput(
      ns("Estado"),
      "Estado",
      choices = c(
        "TODAS",
        sort(unique(na.omit(dados_tipo_atendimento$uf_final)))
      ),
      selected = "TODAS",
      multiple = TRUE,
      options = list(maxOptions = 10000)
    )
    
  })
  
  
  ## Região intermediária ----
  
  output$intermediaria_ui <- renderUI({
    
    selectizeInput(
      ns("Intermediaria"),
      "Região Intermediária",
      choices = c(
        "TODAS",
        sort(unique(na.omit(dados_tipo_atendimento$rgint)))
      ),
      selected = "TODAS",
      multiple = TRUE,
      options = list(maxOptions = 10000)
    )
    
  })
  
  
  ## Região imediata ----
  
  output$imediata_ui <- renderUI({
    
    selectizeInput(
      ns("Imediata"),
      "Região Imediata",
      choices = c(
        "TODAS",
        sort(unique(na.omit(dados_tipo_atendimento$rgi)))
      ),
      selected = "TODAS",
      multiple = TRUE,
      options = list(maxOptions = 10000)
    )
    
  })
  
  
  ## Município ----
  
  output$municipio_ui <- renderUI({
    
    selectizeInput(
      ns("Municipio"),
      "Município",
      choices = c(
        "TODOS",
        sort(unique(na.omit(dados_tipo_atendimento$municipio_final)))
      ),
      selected = "TODOS",
      multiple = TRUE,
      options = list(maxOptions = 10000)
    )
    
  })
  
  
  # VALIDAÇÕES DOS FILTROS ----
  
  ## Região ----
  
  observeEvent(input$Regiao, {
    
    if (
      !is.null(input$Regiao) &&
      "TODOS" %in% input$Regiao &&
      length(input$Regiao) > 1
    ) {
      
      sendSweetAlert(
        session,
        "Erro",
        "A opção 'TODOS' não pode ser combinada com outras regiões.",
        type = "warning"
      )
      
      updateSelectizeInput(
        session,
        "Regiao",
        selected = "TODOS"
      )
    }
    
  }, ignoreNULL = TRUE)
  
  
  ## Estado ----
  
  observeEvent(input$Estado, {
    
    if (
      !is.null(input$Estado) &&
      "TODAS" %in% input$Estado &&
      length(input$Estado) > 1
    ) {
      
      sendSweetAlert(
        session,
        "Erro",
        "A opção 'TODAS' não pode ser combinada com outros estados.",
        type = "warning"
      )
      
      updateSelectizeInput(
        session,
        "Estado",
        selected = "TODAS"
      )
    }
    
  }, ignoreNULL = TRUE)
  
  
  ## Região intermediária ----
  
  observeEvent(input$Intermediaria, {
    
    if (
      !is.null(input$Intermediaria) &&
      "TODAS" %in% input$Intermediaria &&
      length(input$Intermediaria) > 1
    ) {
      
      sendSweetAlert(
        session,
        "Erro",
        "A opção 'TODAS' não pode ser combinada com outras regiões intermediárias.",
        type = "warning"
      )
      
      updateSelectizeInput(
        session,
        "Intermediaria",
        selected = "TODAS"
      )
    }
    
  }, ignoreNULL = TRUE)
  
  
  ## Região imediata ----
  
  observeEvent(input$Imediata, {
    
    if (
      !is.null(input$Imediata) &&
      "TODAS" %in% input$Imediata &&
      length(input$Imediata) > 1
    ) {
      
      sendSweetAlert(
        session,
        "Erro",
        "A opção 'TODAS' não pode ser combinada com outras regiões imediatas.",
        type = "warning"
      )
      
      updateSelectizeInput(
        session,
        "Imediata",
        selected = "TODAS"
      )
    }
    
  }, ignoreNULL = TRUE)
  
  
  ## Município ----
  
  observeEvent(input$Municipio, {
    
    if (
      !is.null(input$Municipio) &&
      "TODOS" %in% input$Municipio &&
      length(input$Municipio) > 1
    ) {
      
      sendSweetAlert(
        session,
        "Erro",
        "A opção 'TODOS' não pode ser combinada com outros municípios.",
        type = "warning"
      )
      
      updateSelectizeInput(
        session,
        "Municipio",
        selected = "TODOS"
      )
    }
    
  }, ignoreNULL = TRUE)
  
  
  # FILTRAGEM DOS DADOS ----
  
  dados_filtrados <- reactive({
    
    # Aguarda a inicialização dos cinco filtros
    
    req(
      input$Regiao,
      input$Estado,
      input$Intermediaria,
      input$Imediata,
      input$Municipio
    )
    
    dados_tipo_atendimento %>%
      dplyr::filter(
        
        regiao_ibge %in% if ("TODOS" %in% input$Regiao) {
          unique(dados_tipo_atendimento$regiao_ibge)
        } else {
          input$Regiao
        },
        
        uf_final %in% if ("TODAS" %in% input$Estado) {
          unique(dados_tipo_atendimento$uf_final)
        } else {
          input$Estado
        },
        
        rgint %in% if ("TODAS" %in% input$Intermediaria) {
          unique(dados_tipo_atendimento$rgint)
        } else {
          input$Intermediaria
        },
        
        rgi %in% if ("TODAS" %in% input$Imediata) {
          unique(dados_tipo_atendimento$rgi)
        } else {
          input$Imediata
        },
        
        municipio_final %in% if ("TODOS" %in% input$Municipio) {
          unique(dados_tipo_atendimento$municipio_final)
        } else {
          input$Municipio
        }
        
      )
    
  })
  
  
  # GRÁFICO 1 - DISTRIBUIÇÃO DE Tipo de AtendimentoS ----
  
  output$plot_1 <- renderHighchart({
    
    dados <- dados_filtrados()
    
    validate(
      need(
        nrow(dados) > 0,
        "Nenhum registro encontrado para os filtros selecionados."
      )
    )
    
    dataplot <- dados %>%
      dplyr::filter(
        !is.na(data_atendimento),
        !is.na(canal_padrao),
        data_atendimento == "01/2026"
      ) %>%
      dplyr::group_by(canal_padrao) %>%
      dplyr::summarise(
        freq = sum(freq, na.rm = TRUE),
        .groups = "drop"
      )
    
    validate(
      need(
        nrow(dataplot) > 0,
        "Não existem dados para construir o gráfico."
      )
    )
    
    grafico_barras_horizontal(
      dados = dataplot,
      frequencia = freq,
      grupo = canal_padrao,
      titulo_x = "Tipo de Atendimento",
      titulo_y = "Número de denúncias"
    )
    
  })
  
  
  # GRÁFICO 2 - VARIAÇÃO 2025 X 2026 ----
  
  output$plot_2 <- renderHighchart({
    
    dados <- dados_filtrados()
    
    validate(
      need(
        nrow(dados) > 0,
        "Nenhum registro encontrado para os filtros selecionados."
      )
    )
    
    aux1 <- dados %>%
      dplyr::filter(
        !is.na(data_atendimento),
        !is.na(canal_padrao),
        data_atendimento == "01/2026"
      ) %>%
      dplyr::group_by(canal_padrao) %>%
      dplyr::summarise(
        freq = sum(freq, na.rm = TRUE),
        .groups = "drop"
      )
    
    aux2 <- dados %>%
      dplyr::filter(
        !is.na(data_atendimento),
        !is.na(canal_padrao),
        data_atendimento == "01/2025"
      ) %>%
      dplyr::group_by(canal_padrao) %>%
      dplyr::summarise(
        freq = sum(freq, na.rm = TRUE),
        .groups = "drop"
      )
    
    dataplot <- dplyr::full_join(
      aux1 %>%
        dplyr::rename(freq_2026 = freq),
      
      aux2 %>%
        dplyr::rename(freq_2025 = freq),
      
      by = "canal_padrao"
    ) %>%
      dplyr::mutate(
        dplyr::across(
          c(freq_2026, freq_2025),
          ~ tidyr::replace_na(.x, 0)
        ),
        diferenca = freq_2026 - freq_2025
      ) %>%
      dplyr::arrange(dplyr::desc(diferenca))
    
    validate(
      need(
        nrow(dataplot) > 0,
        "Não existem dados para construir o gráfico."
      )
    )
    
    grafico_barras_horizontal(
      dados = dataplot,
      frequencia = diferenca,
      grupo = canal_padrao,
      titulo_x = "Tipo de Atendimento",
      titulo_y = "Variação das denúncias"
    )
    
  })
  
  
  # GRÁFICO 3 - ANÁLISE TEMPORAL ----
  
  output$plot_3 <- renderHighchart({
    
    dados <- dados_filtrados()
    
    validate(
      need(
        nrow(dados) > 0,
        "Nenhum registro encontrado para os filtros selecionados."
      )
    )
    
    dataplot <- dados %>%
      dplyr::filter(
        !is.na(data_atendimento),
        !is.na(canal_padrao)
      ) %>%
      dplyr::group_by(
        canal_padrao,
        data_atendimento
      ) %>%
      dplyr::summarise(
        freq = sum(freq, na.rm = TRUE),
        .groups = "drop"
      )
    
    validate(
      need(
        nrow(dataplot) > 0,
        "Não existem dados para construir o gráfico."
      )
    )
    
    grafico_linha_grupo(
      dados = dataplot,
      x = data_atendimento,
      y = freq,
      grupo = canal_padrao,
      titulo_x = "Mês/Ano",
      titulo_y = "Número de denúncias"
    )
    
  })
  
}
