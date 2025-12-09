#Projeto IPADC

#Função
# tcl = total cholesterol, sbp = systolic blood pressure, treated = treatment for hbp
estimated_risk = function(sex,age,treated,tcl,hdl,sbp,smoker,diabetic) {
  if (sex == 0) { #Woman
    S0 = 0.95012
    if (treated == 0) {
      logtreated = 2.76157
    } else {
      logtreated = 2.82263
    }
    
    soma = 2.32888*log(age) + 1.20904*log(tcl) - 0.70833*log(hdl) +
      logtreated*log(sbp) + 0.52873*smoker + 0.69154*diabetic
    risco = (1 - S0^exp(soma-26.1931))*100
    
    return(round(risco,digits=1))
    
  } else { #Men
    S0 = 0.88936
    if (treated == 0) {
      logtreated = 1.93303 
    } else {
      logtreated = 1.99881
    }
    
    soma = 3.06117*log(age) + 1.12370*log(tcl) - 0.93263*log(hdl) +
      logtreated*log(sbp) + 0.65451*smoker + 0.57367*diabetic
    risco = (1 - S0^exp(soma-23.9802))*100
    
    return(round(risco,digits=1))
  }
}  

####################################################################
#                        Interface da app
####################################################################
library(shiny)
library(shinyTime)
library(shinydashboard)
library(shinyjs)
library(rmarkdown)
library(readxl)
library(openxlsx)
library(scales)
library(ggplot2)
library(DT)
library(plotly)
library(htmltools)
library(data.table)
library(dplyr)
library(RColorBrewer)
library(gridExtra)
library(grid)
library(gtable)

#para resolver problemas de limitação do upload dos ficheiros:
options(shiny.maxRequestSize = 50 * 1024^2)


ui <- fluidPage(
  useShinyjs(),
  
  tags$style(HTML("
  .plot-container, .table-container {
    max-width: 800px;
    margin: auto;
    padding-bottom: 20px;
  }
")),
  
  tags$head(
    tags$style(HTML("
  .plot-container {
    max-width: 800px;
    margin: auto;
    padding-bottom: 20px;
  }
")),
    
    tags$style(HTML("
      nav {
        background-color: #2B2D42;
        padding: 10px;
      }
      nav ul {
        list-style-type: none;
        margin: 0;
        padding: 0;
        overflow: hidden;
      }
      nav li {
        float: left;
        margin-right: 20px;
      }
      nav li a {
        color: white;
        text-decoration: none;
        font-weight: bold;
        padding: 8px 12px;
        display: inline-block;
      }
      nav li a:hover {
        background-color: #34495e;
        border-radius: 5px;
      }
      .page-container {
        padding: 20px;
        font-family: 'Segoe UI', sans-serif;
      }
      h2 {
        color: #2c3e50;
      }
      .contact-box {
        background-color: #ecf0f1;
        padding: 15px;
        border-radius: 8px;
        width: fit-content;
      }
    "))
  ),
  
  # Barra de navegação
  tags$nav(
    tags$ul(
      tags$li(actionLink("nav_home", "Página inicial", , shiny::icon('home'))),
      tags$li(actionLink("nav_calculator", "Calculadora", , shiny::icon('calculator'))),
      tags$li(actionLink("nav_about", "  Sobre", , shiny::icon('info-circle'))),
    )
  ),
  
  # Page content container
  div(class = "page-container",
      
      # --- Home Page ---
      div(id = "page_home",
          h2("Calculadora do Risco Cardiovascular"),
          p("As doenças cardiovasculares (DCV) são a princípal causa de morte em 
            Portugal e a nível global, representando aproximadamente 37% de 
            todas as mortes na União Europeia (UE).", ),
          p("A prevenção primária assume um papel fundamental na mitigação do 
            risco de desenvolvimento destas patologias, promovendo modificações 
            comportamentais orientadas para o controlo de fatores de risco, 
            incluindo o tabagismo, o consumo de álcool, os hábitos alimentares 
            inadequados, a inatividade física e o excesso de peso. "),
          p("Esta calculadora tem como objetivo contribuir de forma útil em 
            consultas de Medicina Geral e Familiar, promovendo uma avaliação do 
            risco da ocorrência de eventos cardiovasculares num período de 10 anos.")
      ),
      
      # Página com a calculadora
      hidden(div(id = "page_calculator",
                 h2("Calculadora do Risco Cardiovascular"),
                 fluidRow(
                   box(title = "Dados do Paciente", width = 6,
                       textInput("patient_name", "Nome do Utente:"), 
                       numericInput("age", "Idade:", 50, min = 30, max = 120),
                       textInput("n_utente", "Número de utente"),
                       numericInput("tcl", "Colesterol Total:", 200, min = 100, max = 500),
                       htmlOutput("tcl_saved"),
                       dateInput(
                         'date',
                         'Data da análise',
                         value = NULL,
                         min = NULL,
                         max = NULL,
                         format = "dd-mm-yyyy",
                         startview = "month",
                         weekstart = 0,
                         language = "en",
                         width = NULL,
                         autoclose = TRUE,
                         datesdisabled = NULL,
                         daysofweekdisabled = NULL
                       ),
                       timeInput("time_input", "Hora: ", seconds=FALSE),
                       mainPanel(textOutput("time_output")),
                       numericInput("hdl", "hdl:", 50, min = 30, max = 100),
                       htmlOutput("hdl_saved"),
                       numericInput("sbp", "Pressão Arterial Sistólica:", 120, min = 80, max = 300),
                       htmlOutput("sbp_saved"),
                       radioButtons("sex", "Sexo:", choices = c("Feminino" = 0, "Masculino" = 1)),
                       radioButtons("treated", "Em Tratamento:", choices = c("Não" = 0, "Sim" = 1)),
                       radioButtons("smoker", "Fumador:", choices = c("Não" = 0, "Sim" = 1)),
                       htmlOutput("smoker_saved"),
                       radioButtons("diabetic", "Diabético:", choices = c("Não" = 0, "Sim" = 1)),
                       actionButton("calcular", "Calcular Risco"),
                       actionButton("save", "Guardar Valores")
                   ),
                   
                   box(title = "Estimativa de Risco", width = 6,
                       htmlOutput("estimated_risk"), 
                       htmlOutput("saved_risk"),
                       htmlOutput("tcl_high"),
                       htmlOutput("sbp_high"),
                       htmlOutput("aviso_legal"),
                       downloadButton("download_relatorio", "Criar PDF"),
                       br(), hr(),
                       h4("Base de Dados (Tempo Real):"),
                       p("Aqui podes ver a lista 'd' a crescer com cada clique em Guardar:"),
                       verbatimTextOutput("debug_lista")
                   )
                 )
      )),
      
      # About page
      hidden(div(id = "page_about",
                 h2("Informação Adicional"),
                 p("A calculadora baseia-se na implementação do algoritmo de 
                   risco cardiovascular geral de Framingham (D’Agostino, 2008, 
                   apêndice pp. 751–752)."),
                 p("Algoritmo:"),
                 p("Com base em variáveis clínicas de fácil obtenção, como a idade,
                   sexo, níveis de colesterol total e colesterol HDL, pressão arterial sistólica,
                   tratamentos para a hipertensão, tabagismo e diabetes, a aplicação calcula uma estimativa percentual do risco
                   de ocorrência de eventos cardiovasculares num período de 10 anos."),
                 p("Subsequentemente, a aplicação gera automaticamente um relatório em formato 
                   PDF contendo...")
      )),
  )
)


####################################################################
#                        Servidor da app
####################################################################

server <- function(input, output, session) {
  
  # Navegação na página
  showPage <- function(pageId) {
    shinyjs::hide("page_home")
    shinyjs::hide("page_about")
    shinyjs::hide("page_calculator")
    shinyjs::show(pageId)
  }
  observeEvent(input$nav_home, {showPage("page_home")})
  observeEvent(input$nav_about, {showPage("page_about")})
  observeEvent(input$nav_calculator,{showPage("page_calculator")})
  
  
  # Função para calcular o risco cardiovascular
  calculo_risco <- reactive({
    req(input$calcular)
    
    sex <- as.numeric(input$sex)
    age <- as.numeric(input$age)
    tcl <- as.numeric(input$tcl)
    hdl <- as.numeric(input$hdl)
    sbp <- as.numeric(input$sbp)
    treated <- as.numeric(input$treated)
    smoker <- as.numeric(input$smoker)
    diabetic <- as.numeric(input$diabetic)
    
    if (age<30 | age>120) {stop("A idade tem que estar compreendida entre os 30 e os 120 anos.")}
    if (tcl<100 | tcl>400) {stop("Os valores totais de colesterol tem que estar entre 100 e 400 mg/dl.")}
    if (hdl<10 | hdl>150) {stop("O colesterol HDL tem que estar entre 10 e 150 mg/dl.")}
    if (sbp<70 | sbp>300) {stop("A pressão arterial sistólica tem que estar entre 70 e 300 mmHg.")}
    
    else {
      risco <- estimated_risk(sex,age,treated,tcl,hdl,sbp,smoker,diabetic)
      return(risco)
    }
  })
  
  #código para guardar os valores do utente numa lista
  rv <- reactiveValues(d=list())
  observeEvent(input$save, {
    req(input$n_utente) 
    if (input$calcular==0) {
      showNotification("Erro: Risco deve ser calculado antes de guardar.",type="error")
      return()
    }
    risco_atual<-calculo_risco()
    id <- as.character(input$n_utente) 
    values <- list(
      idade=input$age,
      sexo=input$sex,
      colesterol_total=input$tcl,
      hdl=input$hdl,
      pas=input$sbp,
      treatment=input$treated,
      diab=input$diabetic,
      smoke=input$smoker,
      cdv=risco_atual,
      data=input$date,
      hora=as.character(input$time_input)
    )
    temp_d <- rv$d
    if (id %in% names(temp_d)) {
      temp_d[[id]] <- c(temp_d[[id]], list(values))
    } else {
      temp_d[[id]] <- list(values)
    }
    rv$d <- temp_d 
    showNotification(paste("Guardado! Total de registos:", length(rv$d[[id]])), type = "message")
  })
  
  output$debug_lista <- renderPrint({
    if (length(rv$d) == 0) cat("Lista vazia.") else print(rv$d)
  })
  
  guardar_risco <- eventReactive(input$save,{
    sex <- as.numeric(input$sex); age <- as.numeric(input$age)
    tcl_saved <- as.numeric(input$tcl); hdl_saved <- as.numeric(input$hdl)
    sbp_saved <- as.numeric(input$sbp); treated <- as.numeric(input$treated)
    smoker_saved <- as.numeric(input$smoker); diabetic <- as.numeric(input$diabetic)
    
    saved_risk <- estimated_risk(sex,age,treated,tcl_saved,hdl_saved,sbp_saved,smoker_saved,diabetic)
    return(c(saved_risk, tcl_saved, hdl_saved, sbp_saved, smoker_saved))
  })
  
  # outputs UI
  output$estimated_risk <- renderUI({
    risco <- calculo_risco()
    HTML(paste("<h5>Estimativa atual de risco: <strong>", risco, "%</strong></h3>"))
  })
  output$tcl_high <- renderUI({ if (as.numeric(input$tcl) > 200) HTML(paste("<h6 style=\"color:red;\">Valor de Colesterol Alto!")) })
  output$sbp_high <- renderUI({ if (as.numeric(input$sbp) > 130) HTML(paste("<h6 style=\"color:red;\">Valor de sbp high!")) })
  output$saved_risk <- renderUI({ vals <- guardar_risco(); HTML(paste("<h5>Estimativa original: <strong>", vals[1], "%</strong></h3>")) })
  output$tcl_saved <- renderUI({ vals <- guardar_risco(); HTML(paste("<h5 style=\"color:blue;\"> Valor anterior: <strong>", vals[2])) })
  output$hdl_saved <- renderUI({ vals <- guardar_risco(); HTML(paste("<h5 style=\"color:blue;\">Valor anterior: <strong>", vals[3])) })
  output$sbp_saved <- renderUI({ vals <- guardar_risco(); HTML(paste("<h5 style=\"color:blue;\">Valor anterior: <strong>", vals[4])) })
  output$smoker_saved <- renderUI({ 
    vals <- guardar_risco()
    valor_fumador <- ifelse(vals[5] == 0, "Não Fumador", "Fumador")
    HTML(paste("<h5 style=\"color:blue;\">Valor anterior: <strong>", valor_fumador)) 
  })
  
  output$aviso_legal <- renderUI({
    HTML("<br><strong>Aviso Legal:</strong><br>Relatório apenas para fins académicos.")
  })  
  
  # Código para gerar o PDF
  output$download_relatorio <- downloadHandler(
    filename = function() {
      paste0("relatorio_", input$patient_name, "_", format(Sys.Date(), "%d/%m/%Y"), ".pdf")
    },
    content = function(file) {
      
      risco <- calculo_risco()

      # Histórico (últimos 20 registos)
      id <- as.character(input$n_utente)
      historico_formatado_df <- NULL 
      tem_historico <- FALSE
      
      if (id %in% names(rv$d)) {
        historico <- rv$d[[id]]
        df_hist <- do.call(rbind, lapply(historico, as.data.frame))
        
        # Ordenar cronologicamente
        df_hist$datetime <- as.POSIXct(paste(df_hist$data, df_hist$hora))
        df_hist <- df_hist[order(df_hist$datetime), ]
        
        # Para que apenas surjam os últimos registos na tabela
        if(nrow(df_hist) > 20) {
          df_hist <- tail(df_hist, 20)
        }
        
        # Transformar os dados para a forma como queremos que apareçam na tabela
        vals_idade <- df_hist$idade
        vals_sexo <- ifelse(df_hist$sexo == 0, "Feminino", "Masculino")
        vals_col <- df_hist$colesterol_total
        vals_hdl <- df_hist$hdl
        vals_pas <- df_hist$pas
        vals_trat <- ifelse(df_hist$treatment == 1, "Sim", "Não")
        vals_diab <- ifelse(df_hist$diab == 1, "Sim", "Não")
        vals_fum <- ifelse(df_hist$smoke == 1, "Sim", "Não")
        vals_risco <- sprintf("%.1f %%", df_hist$cdv)
        
        historico_formatado_df <- data.frame(
          col1 = vals_idade, col2 = vals_sexo, col3 = rep(id, length(vals_idade)), 
          col4 = vals_col, col5 = vals_hdl, col6 = vals_pas, col7 = vals_trat,
          col8 = vals_diab, col9 = vals_fum, col10 = vals_risco,
          stringsAsFactors = FALSE
        )
        datas_header <- format(as.Date(df_hist$data), "%d/%m/%Y")
        tem_historico <- TRUE
      }
      
      # PDF
      pdf(file, width = 8.5, height = 11)
      
      # Nome e ID do paciente
      grid.text("Relatório de Risco Cardiovascular", y = 0.96, gp = gpar(fontsize = 22, fontface = "bold"))
      grid.text(paste0("Paciente: ", input$patient_name, "   |   Nº Utente: ", input$n_utente, "   |   Data: ", format(Sys.Date(), "%d/%m/%Y")),
                y = 0.93, gp = gpar(fontsize = 14))
      grid.text("Histórico Últimos 20 Dias", y=0.88, gp=gpar(fontsize=18, fontface="bold"))
      
      # Páginas do Histórico (2 tabelas com 5 registos cada por página, no máximo)
      if (!tem_historico) {
        grid.newpage()
        grid.text("Histórico do Paciente", y=0.97, gp=gpar(fontsize=18, fontface="bold"))
        grid.text("Sem histórico disponível.", y=0.8, gp=gpar(fontsize=12))
        
      } else {
        total_registos <- nrow(historico_formatado_df)
        tamanho_chunk <- 5
        num_paginas_hist <- ceiling(total_registos / tamanho_chunk)
        
        variaveis_labels <- c("Idade", "Sexo", "Número de utente", "Colesterol Total (CT)", "HDL", "PAS",
                              "Tratamento Hipertensão (TH)", "Diabetes", "Fumador", "Risco Cardiovascular (%)")
        
        # Variável que permite controlar o número de tabelas por página
        tabelas_na_pagina <- 0
        
        for (i in 1:num_paginas_hist) {
          
          # Cria uma página nova se precisarmos de uma nova tabela e a página anterior já tiver duas
          if (tabelas_na_pagina == 2) {
            grid.newpage()
            grid.text("Histórico Últimos 20 Dias (cont.)", y=0.88, gp=gpar(fontsize=18, fontface="bold"))
            tabelas_na_pagina <- 0
          }
          
          # Definir a posição Y baseada em se é a 1ª ou 2ª tabela da página
          # y=0.67 é topo, y=0.35 é fundo
          pos_y <- ifelse(tabelas_na_pagina == 0, 0.67, 0.35)
          
          # Dados do Chunk
          idx_inicio <- (i - 1) * tamanho_chunk + 1
          idx_fim <- min(i * tamanho_chunk, total_registos)
          
          sub_df <- historico_formatado_df[idx_inicio:idx_fim, ]
          sub_datas <- datas_header[idx_inicio:idx_fim]
          
          t_hist <- t(sub_df)
          colnames(t_hist) <- sub_datas
          
          df_chunk_final <- as.data.frame(t_hist, stringsAsFactors = FALSE)
          df_chunk_final <- cbind(Variável = variaveis_labels, df_chunk_final)
          
          #Selecionar as datas de cada tabela
          idx_inicio <- (i - 1) * tamanho_chunk + 1
          idx_fim <- min(i * tamanho_chunk, total_registos)
          data_inicio <- datas_header[idx_inicio]
          data_fim <- datas_header[idx_fim]
          
          #Título da tabela
          titulo_tabela <- paste0("Período: ", data_inicio, " até ", data_fim)
          grid.text(titulo_tabela, x=0.1, y=pos_y + 0.15, just="left", gp=gpar(fontsize=11, fontface="bold", col="black"))
          
          # Desenhar Tabela
          tema <- ttheme_default(core = list(fg_params=list(cex=1)),
                                 colhead = list(fg_params=list(cex=1.1, fontface="bold", col="white"),
                                                bg_params=list(fill="#2B2D42")))
          hist_table <- tableGrob(df_chunk_final, rows=NULL, theme=tema)
          pushViewport(viewport(x=0.5, y=pos_y, width=0.95, height=0.40)) # Altura reduzida para caberem 2
          grid.draw(hist_table)
          popViewport()
          
          # Incrementar contador
          tabelas_na_pagina <- tabelas_na_pagina + 1
        }
      }
      
      # Criação dos gráficos de evolução temporal
      if (!"Nota" %in% names(df_hist)) {
          
          # 1. Definir o layout e as margens
          # oma = c(Baixo, Esq, Cima, Dir) -> Nota o '5' no topo para o título
          par(mfrow=c(3,1), 
              oma=c(2, 6, 10, 6), 
              mar=c(4, 4, 2, 1)) 
          
          # 2. Desenhar os gráficos
          plot(df_hist$datetime, df_hist$cdv, type="b",
               col="#E63946", pch=19, lwd=3, cex=0.8,
               xlab="", ylab="Risco (%)",
               main="Risco Cardiovascular",
               cex.axis=0.8, cex.lab=0.9)
          
          plot(df_hist$datetime, df_hist$pas, type="b",
               col="#1D3557", pch=19, lwd=3, cex=0.8,
               xlab="", ylab="PAS",
               main="Pressão Arterial Sistólica",
               cex.axis=0.8, cex.lab=0.9)
          
          plot(df_hist$datetime, df_hist$colesterol_total, type="b",
               col="#2A9D8F", pch=19, lwd=3, cex=0.8,
               xlab="Data", ylab="Colesterol",
               main="Colesterol Total",
               cex.axis=0.8, cex.lab=0.9)
          
          
          mtext("Evolução Temporal", side=3, line=2, outer=TRUE, 
                cex=1.5, font=2) # font=2 é negrito
    
      } else {
        grid.text("Sem histórico suficiente para gráficos.", y=0.5, gp=gpar(fontsize=14, col="grey40"))
      }
      
      dev.off()
    }
  )
  
}

shinyApp(ui, server)