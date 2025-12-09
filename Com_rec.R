
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
  withMathJax(),
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
          p(style = "font-size:20px;","As doenças cardiovasculares (DCV) são a princípal causa de morte em 
            Portugal e a nível global, representando aproximadamente 37% de 
            todas as mortes na União Europeia (UE).", ),
          p(style = "font-size:20px;","A prevenção primária assume um papel fundamental na mitigação do 
            risco de desenvolvimento destas patologias, promovendo modificações 
            comportamentais orientadas para o controlo de fatores de risco, 
            incluindo o tabagismo, o consumo de álcool, os hábitos alimentares 
            inadequados, a inatividade física e o excesso de peso. "),
          p(style = "font-size:20px;","Esta calculadora tem como objetivo contribuir de forma útil em 
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
                       numericInput("hdl", "Colestrol HDL:", 50, min = 30, max = 100),
                       htmlOutput("hdl_saved"),
                       numericInput("sbp", "Pressão Arterial Sistólica (PAS):", 120, min = 70, max = 300),
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
                       htmlOutput("tcl_low"),
                       htmlOutput("sbp_high"),
                       htmlOutput("sbp_low"),
                       htmlOutput("hdl_high"),
                       htmlOutput("hdl_low"),
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
                 p(style = "font-size:20px;","A calculadora baseia-se na implementação do algoritmo de 
                   risco cardiovascular geral de Framingham (D’Agostino, 2008, 
                   apêndice pp.751–752)."),
                 p(style = "font-size:20px; margin-bottom:40px;","Algoritmo:$$\\hat{p} = 1 - 
                   S_0(t)^{\\exp\\left(\\sum_{i=1}^{p} \\beta_i x_i - \\sum_{i=1}^{p} \\beta_i \\bar{x}_i\\right)}$$"),
                 p(style = "font-size:20px;","Com base em variáveis clínicas de fácil obtenção, como a idade,
                   sexo, níveis de colesterol total e colesterol HDL, pressão arterial sistólica,
                   tratamentos para a hipertensão, tabagismo e diabetes, a aplicação calcula uma estimativa percentual do risco
                   de ocorrência de eventos cardiovasculares num período de 10 anos."),
                 p(style = "font-size:20px; margin-bottom:40px;","Subsequentemente, a aplicação gera
                 automaticamente um relatório em formato PDF. Neste relatório é 
                 possível encontrar o histórico do paciente com os valores em 
                 cada consulta e uma evolução temporal das variáveis Risco 
                 Cardiovascular, Pressão Arterial Sistólica e Colesterol Total 
                 ilustrada em gráficos."),
                 p(style = "font-size:20px;","Este projeto foi desenvolvido por Simão Tavares, 
                   Tomás Matias, Marta Carvalho, João Cordeiro, Rodrigo Curto 
                   e Pedro Correia no âmbito da disciplina de Introdução Ao 
                   Processo de Apoio à Decisão Clínica (IPADC).")
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
  output$tcl_high <- renderUI({ if (as.numeric(input$tcl) > 200) HTML(paste("<h6 style=\"color:red;\">Valor de Colesterol Total elevado!")) })
  output$tcl_low <- renderUI({ if (as.numeric(input$tcl) < 120) HTML(paste("<h6 style=\"color:red;\">Valor de Colesterol Total baixo!")) })
  output$sbp_high <- renderUI({ if (as.numeric(input$sbp) > 130) HTML(paste("<h6 style=\"color:red;\">Valor de PAS elevado!")) })
  output$sbp_low <- renderUI({ if (as.numeric(input$sbp) < 80) HTML(paste("<h6 style=\"color:red;\">Valor de PAS baixo!")) })
  output$hdl_high <- renderUI({ if (as.numeric(input$hdl) > 90) HTML(paste("<h6 style=\"color:red;\">Valor de Colesterol HDL elevado!")) })
  output$hdl_low <- renderUI({ if (as.numeric(input$hdl) < 40) HTML(paste("<h6 style=\"color:red;\">Valor de Colesterol HDL baixo!")) })
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
      footerLegal <- function() {
        grid.text(
          "Aviso Legal: Relatório apenas para fins académicos. Não substitui avaliação clínica qualificada.",
          x = 0.5, y = 0.02,
          gp = gpar(fontsize = 8, col = "grey40", fontface = "italic")
        )
      }
      
      
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
      footerLegal()
      
      # Nome e ID do paciente
      grid.text("Relatório de Risco Cardiovascular", y = 0.96, gp = gpar(fontsize = 22, fontface = "bold"))
      grid.text(paste0("Paciente: ", input$patient_name, "   |   Nº Utente: ", input$n_utente, "   |   Data: ", format(Sys.Date(), "%d/%m/%Y")),
                y = 0.93, gp = gpar(fontsize = 14))
      
      # Tabela com o último registo na primeira página
      dados <- data.frame(
        Variável = c("Idade", "Sexo", "Colesterol Total (CT)", "HDL", "PAS",
                     "Tratamento Hipertensão (TH)", "Diabetes", "Fumador", "Risco Cardiovascular (%)"),
        Valor = c(input$age,
                  ifelse(input$sex == 0, "Feminino", "Masculino"),
                  input$tcl,
                  input$hdl,
                  input$sbp,
                  ifelse(input$treated == 1, "Sim", "Não"),
                  ifelse(input$diabetic == 1, "Sim", "Não"),
                  ifelse(input$smoker == 1, "Sim", "Não"),
                  sprintf("%.1f %%", risco)),
        stringsAsFactors = FALSE)
      tema <- ttheme_default(
        core = list(fg_params=list(cex=1.1)),
        colhead = list(fg_params=list(cex=1.3, fontface="bold", col="white"),
                       bg_params=list(fill="#2B2D42"))
      )
      
      grid.text("Último Registo", y=0.88, gp=gpar(fontsize=18, fontface="bold"))
      t <- tableGrob(dados, rows=NULL, theme=tema)
      t$heights <- t$heights * 1.1
      pushViewport(viewport(x=0.5, y=0.70, width=0.9, height=0.30))
      grid.draw(t)
      popViewport()
      
      # Função para escrever o texto dos avisos e sugestões
      escrever_texto <- function(texto, y_atual, cor="black") {
        # divide o texto em linhas de 90 caracteres
        texto_quebrado <- paste(strwrap(texto, width = 90), collapse = "\n")
        # calcula o espaço necessário para as linhas
        num_linhas <- length(strwrap(texto, width = 90))
        # escreve o texto na posição certa
        grid.text(texto_quebrado, x = 0.1, y = y_atual, 
                  just = c("left", "top"), 
                  gp = gpar(fontsize = 11, col = cor))
        # calcula a posição da linha seguinte
        espaco_ocupado <- (num_linhas * 0.02) + 0.02
        return(y_atual - espaco_ocupado)
      }
      
      #Adicionar avisos e recomendações depois da tabela com o último registo
      pos_y <- 0.51 #esta posição vai sendo atualizada para que as mensagens vão passando para baixo
      grid.text("Avisos e Sugestões", x=0.1, y=pos_y + 0.03, just="left", gp=gpar(fontsize=14, fontface="bold", col="#2B2D42"))
      # Colesterol Alto
      if (as.numeric(input$tcl) > 200) {
        recom <- paste0("Colesterol Total Elevado (", input$tcl, " mg/dL): Deve tentar baixar os seus valores de colesterol para valores entre 130 e 200 mg/dL.\n",
                        "Sugestão: Sugerir um estilo de vida mais saudável, diminuindo as gorduras saturadas e aumentando o consumo de frutas e de vegetais na dieta e tentando praticar exercício físico.")
        pos_y <- escrever_texto(recom, pos_y)
      }
      # Colesterol Baixo
      if (as.numeric(input$tcl) < 120) {
        recom <- paste0("Colesterol Total Baixo (", input$tcl, " mg/dL): Deve tentar aumentar os seus valores de colesterol total para valores entre 130 e 200 mg/dL.\n",
                        "Sugestão: Tentar compreender a causa e sugerir exames complementares.")
        pos_y <- escrever_texto(recom, pos_y)
      }
      # HDL Alto
      if (as.numeric(input$hdl) > 90) {
        recom <- paste0("Colesterol HDL Elevado (", input$tcl, " mg/dL): Deve tentar baixar os seus valores de colesterol para valores inferiores entre 60 e 90 mg/dL.\n",
                        "Sugestão: Sugerir um estilo de vida mais saudável, diminuindo as gorduras saturadas e aumentando o consumo de frutas e de vegetais na dieta e tentando praticar exercício físico.")
        pos_y <- escrever_texto(recom, pos_y)
      }
      # HDL Baixo
      if (as.numeric(input$hdl) < 40) {
        recom <- paste0("Colesterol HDL Baixo (", input$tcl, " mg/dL): Deve tentar aumentar os seus valores de colesterol total para valores entre 40 e 90 mg/dL (idealmente entre 60 e 90 mg/dL).\n",
                        "Sugestão: Aconselhar a aumentar o consumo de peixes ricos em ómega-3 (salmão, cavala, sardinha e atum), abacate e frutos secos.")
        pos_y <- escrever_texto(recom, pos_y)
      }
      # PAS elevada
      if (as.numeric(input$sbp) > 130) {
        recom <- paste0("Pressão Arterial Sistólica Alta (", input$sbp, " mmHg): Deve tentar baixar a sua pressão arterial sistólica para valores inferiores entre 80 e 130 mmHg.\n",
                        "Sugestão: Reduzir o consumo de sal na dieta, aumentar a prática de exercício físico e avalie a administração de medicação para o controlo da PAS.")
        pos_y <- escrever_texto(recom, pos_y)
      }
      # PAS baixa
      if (as.numeric(input$sbp) < 80) {
        recom <- paste0("Pressão Arterial Sistólica Baixa (", input$sbp, " mmHg): Deve tentar aumentar oa sua pressão arterial sistólica para valores entre 80 e 130 mmHg.\n",
                        "Sugestão: Reveja a medicação adminostrada para a hipertensão (caso seja aplicável) e aumentar o consumo de água.")
        pos_y <- escrever_texto(recom, pos_y)
      }
      # Fumar
      if (as.numeric(input$smoker) == 1) {
        recom <- paste0("Tabagismo: Fumar aumenta o risco de ter doenças cardiovasculares.\n",
                        "Sugestão: Aconselhar o paciente a deixar de fumar.")
        pos_y <- escrever_texto(recom, pos_y)
      }
      # Diabetes
      if (as.numeric(input$diabetic) == 1) {
        recom <- paste0("Diabetes: As diabetes aumentam o risco de ter doenças cardiovasculares.\n",
                        "Sugestão: Alertar o paciente para a importância de manter uma alimentação saudável e de praticar exercício físico.")
        pos_y <- escrever_texto(recom, pos_y)
      }
      
      # Caso esteja tudo bem
      if (as.numeric(input$tcl) <= 200 && as.numeric(input$tcl) >= 120 && as.numeric(input$sbp) <= 130 && as.numeric(input$sbp) >= 80 && as.numeric(input$smoker) == 0 && as.numeric(input$diabetic) == 0 && as.numeric(input$hdl) >= 40 && as.numeric(input$hdl) <= 90) {
        parabens <- "Mantém o teu estilo de vida saudável. Assim, estarás sempre mais perto de não sofrer com doenças cardiovasculares."
        pos_y <- escrever_texto(parabens, pos_y, cor="darkgreen")
      }
      
      # Páginas do Histórico (2 tabelas com 5 registos cada por página, no máximo)
      if (!tem_historico) {
        grid.newpage()
        footerLegal()
        grid.text("Histórico do Paciente", y=0.97, gp=gpar(fontsize=18, fontface="bold"))
        grid.text("Sem histórico disponível.", y=0.8, gp=gpar(fontsize=12))
        
      } else {
        grid.newpage()
        grid.text("Histórico Últimos 20 Dias", y=0.88, gp=gpar(fontsize=18, fontface="bold"))
        footerLegal()
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
            footerLegal()
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
        lines(df_hist$datetime,df_hist$hdl, type = "b", col = "#E76F51")
        legend("topright", legend = c("Colesterol Total", "HDL"),
               col = c("#2A9D8F", "#E76F51"), pch = 19)
        
        mtext("Evolução Temporal", side=3, line=2, outer=TRUE, 
              cex=1.5, font=2) # font=2 é negrito
        
      } else {
        grid.text("Sem histórico suficiente para gráficos.", y=0.5, gp=gpar(fontsize=14, col="grey40"))
      }
      footerLegal()
      dev.off()
    }
  )
  
}


shinyApp(ui, server)

