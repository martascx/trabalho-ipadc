#-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
#################################################################################################
##                       CALCULADORA DO RISCO DE DOENÇA CARDIOVASCULAR: 
##             UMA FERRAMENTA PRÁTICA PARA APOIAR A DECISÃO CLÍNICA NA PREVENÇÃO
#################################################################################################
## Trabalho elaborado por: Ana Cardoso 122994 || Beatriz Lavado 102495 || Fátima Figueira 124413 
##                                  José Mendes 104050 || Maria Monteiro 124366
###  Mestrado em Bioinformática Clínica, Universidade de Aveiro
#################################################################################################
#-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

#################################################################################################
## Bibliotecas necessárias
#################################################################################################

library(shiny)
library(shinydashboard)
library(ggplot2)
library(rmarkdown)
library(readxl)
library(openxlsx)
library(grid)
library(scales)

#################################################################################################
## Cálculo do Risco Cardiovascular
#################################################################################################

# Função de estimativa de risco cardiovascular
estimativa_risco <- function(sexo, idade, col, HDL, fumador, diabetico, PAS, tratamento) {
  if (sexo == 1) {
    if (tratamento == 1) {
      constante = 2.82263
    } else {
      constante = 2.76157
    }
  } else {
    if (tratamento == 1) {
      constante = 1.99881
    } else {
      constante = 1.93303
    }
  }
  
  if (sexo == 1) { 
    somatorio = (2.32888 * log(idade)) + (1.20904 * log(col)) - (0.70833 * log(HDL)) + 
      (constante * log(PAS)) + (0.52873 * fumador) + (0.69154 * diabetico)
    estimativa_risco = 1 - 0.95012^exp(somatorio - 26.1931)
  } else {
    somatorio = (3.06117 * log(idade)) + (1.12370 * log(col)) - (0.93263 * log(HDL)) + 
      (constante * log(PAS)) + (0.65451 * fumador) + (0.57367 * diabetico)
    estimativa_risco = 1 - 0.88936^exp(somatorio - 23.9802)
  }
  
  return(estimativa_risco)
}

#################################################################################################
## Interface Gráfica
#################################################################################################

# UI do Shiny
ui <- dashboardPage(
  dashboardHeader(title = "Estimativa de Risco Cardiovascular"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Paciente Individual", tabName = "tab_paciente", icon = icon("user")),
      menuItem("Dados de Pacientes", tabName = "tab_dados", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "tab_paciente",
              fluidRow(
                box(title = "Dados do Paciente", width = 6,
                    textInput("nome_utente", "Nome do Utente:"), # Novo campo para o nome do utente
                    numericInput("idade", "Idade:", 50, min = 30, max = 120),
                    numericInput("col", "Colesterol Total:", 200, min = 100, max = 500),
                    htmlOutput("col_guardado"),
                    numericInput("HDL", "HDL:", 50, min = 30, max = 100),
                    htmlOutput("HDL_guardado"),
                    numericInput("PAS", "Pressão Arterial Sistólica:", 120, min = 80, max = 300),
                    htmlOutput("PAS_guardado"),
                    radioButtons("sexo", "Sexo:", choices = c("Masculino" = 0, "Feminino" = 1)),
                    radioButtons("tratamento", "Em Tratamento:", choices = c("Não" = 0, "Sim" = 1)),
                    radioButtons("fumador", "Fumador:", choices = c("Não" = 0, "Sim" = 1)),
                    htmlOutput("fumador_guardado"),
                    radioButtons("diabetico", "Diabético:", choices = c("Não" = 0, "Sim" = 1)),
                    actionButton("calcular", "Calcular Risco"),
                    actionButton("guardar", "Guardar Valores")
                ),
                box(title = "Estimativa de Risco", width = 6,
                    htmlOutput("risco_estimado"), 
                    htmlOutput("risco_guardado"),
                    htmlOutput("col_alto"),
                    htmlOutput("PAS_alto"),
                    htmlOutput("aviso_legal"),
                    downloadButton("download_relatorio", "Gerar PDF")
                )
              )
      ),
      tabItem(tabName = "tab_dados",
              fluidRow(
                box(title = "Importar Dados de Pacientes", width = 12,
                    fileInput("arquivo", "Escolha o arquivo Excel", accept = c(".xlsx")),
                    actionButton("processar", "Processar Dados")
                ),
                box(title = "Resumo de Estimativas", width = 12,
                    tableOutput("resumo_estatisticas"),
                    downloadButton("download_excel", "Gerar Excel")
                )
              )
      )
    )
  )
)

# Servidor do Shiny
server <- function(input, output, session) {
  
  # Função para calcular o risco cardiovascular
  calcular_risco <- reactive({
    req(input$calcular)
    
    sexo <- as.numeric(input$sexo)
    idade <- as.numeric(input$idade)
    col <- as.numeric(input$col)
    HDL <- as.numeric(input$HDL)
    PAS <- as.numeric(input$PAS)
    tratamento <- as.numeric(input$tratamento)
    fumador <- as.numeric(input$fumador)
    diabetico <- as.numeric(input$diabetico)
    
    risco <- estimativa_risco(sexo, idade, col, HDL, fumador, diabetico, PAS, tratamento)
    return(risco)
  })
  
  guardar_risco <- eventReactive(input$guardar,{
    
    sexo <- as.numeric(input$sexo)
    idade <- as.numeric(input$idade)
    col_guardado <- as.numeric(input$col)
    HDL_guardado <- as.numeric(input$HDL)
    PAS_guardado <- as.numeric(input$PAS)
    tratamento <- as.numeric(input$tratamento)
    fumador_guardado <- as.numeric(input$fumador)
    diabetico <- as.numeric(input$diabetico)
    
    risco_guardado <- estimativa_risco(sexo, idade, col_guardado, HDL_guardado, fumador_guardado, diabetico, PAS_guardado, tratamento)
    return(c(risco_guardado, col_guardado, HDL_guardado, PAS_guardado, fumador_guardado))
  })
  
  # Exibir a estimativa de risco
  output$risco_estimado <- renderUI({
    risco <- calcular_risco()
    HTML(paste("<h5>Estimativa atual de risco: <strong>", round(risco * 100, 2), "%</strong></h3>"))
  })
  
  output$col_alto <- renderUI({
    if (as.numeric(input$col) > 200){
      HTML(paste("<h6 style=\"color:red;\">Valor de Colesterol Alto!"))
    }
  })
  
  output$PAS_alto <- renderUI({
    if (as.numeric(input$PAS) > 130){
      HTML(paste("<h6 style=\"color:red;\">Valor de PAS alto!"))
    }
  })
  
  output$risco_guardado <- renderUI({
    valores_guardado <- guardar_risco()
    HTML(paste("<h5>Estimativa original do risco: <strong>", round(valores_guardado[1] * 100, 2), "%</strong></h3>"))
  })
  
  output$col_guardado <- renderUI({
    valores_guardados <- guardar_risco()
    
    HTML(paste("<h5 style=\"color:blue;\"> Valor anterior: <strong>", valores_guardados[2]))
  })
  
  output$HDL_guardado <- renderUI({
    valores_guardados <- guardar_risco()
    
    HTML(paste("<h5 style=\"color:blue;\">Valor anterior: <strong>", valores_guardados[3]))
  })
  
  output$PAS_guardado <- renderUI({
    valores_guardados <- guardar_risco()
    
    HTML(paste("<h5 style=\"color:blue;\">Valor anterior: <strong>", valores_guardados[4]))
  })
  
  output$fumador_guardado <- renderUI({
    valores_guardados <- guardar_risco()
    
    if (valores_guardados[5] == 0){
      valor_fumador <- "Não Fumador"
    } else {
      valor_fumador <- "Fumador"
    }
    HTML(paste("<h5 style=\"color:blue;\">Valor anterior: <strong>", valor_fumador))
  })
  
  output$aviso_legal <- renderUI({
    HTML("<br><strong>Aviso Legal:</strong><br>
       Este relatório destina-se exclusivamente a fins académicos e não constitui um documento médico oficial.
       Não foi revisto pelas autoridades reguladoras e pode conter erros.
       A utilização deste relatório é da inteira responsabilidade do utilizador.
       Os criadores e a universidade afiliada não assumem qualquer responsabilidade por decisões tomadas com base no seu conteúdo.
       Para questões médicas, por favor consulte um profissional de saúde qualificado.")
  })
  
  # Gerar o PDF
  output$download_relatorio <- downloadHandler(
    filename = function() {
      paste("relatorio_", input$nome_utente, "_",Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, paper = "a4", width = 8.27, height = 11.69) # Dimensões A4 em polegadas
      
      grid.newpage()
      
      # Definir margens
      pushViewport(viewport(x = 0.05, y = 0.05, width = 0.9, height = 0.9, just = c("left", "bottom")))
      
      # Título
      grid.text("Relatório de Risco Cardiovascular", x = 0.5, y = 0.95, gp = gpar(fontsize = 16, fontface = "bold"))
      
      risco_guardado <- tryCatch(guardar_risco(), error = function(e) {return(NULL)})
      
      # Informações do paciente
      
      if (!is.null(risco_guardado)) {
        grid.text(paste("Nome do Utente:", input$nome_utente), x = 0, y = 0.85, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Idade:", input$idade), x = 0, y = 0.82, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Sexo:", ifelse(input$sexo == 0, "Masculino", "Feminino")), x = 0, y = 0.79, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Colesterol Total:", risco_guardado[2]), x = 0, y = 0.76, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("HDL:", risco_guardado[3]), x = 0, y = 0.73, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Pressão Arterial Sistólica:", risco_guardado[4]), x = 0, y = 0.70, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Tratamento:", ifelse(input$tratamento == 0, "Não", "Sim")), x = 0, y = 0.67, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Diabético:", ifelse(input$diabetico == 0, "Não", "Sim")), x = 0, y = 0.64, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Fumador:", ifelse(risco_guardado[5] == 0, "Não", "Sim")), x = 0, y = 0.61, just = "left", gp = gpar(fontsize = 12))
        
        grid.text(paste("Estimativa de Risco Cardiovascular:", round(risco_guardado[1] * 100, 2), "%"),
                  x = 0, y = 0.58, just = "left", gp = gpar(fontsize = 14, fontface = "bold", col = "red"))
        
        grid.text(paste("Novo Valor de Colesterol Total:", input$col), x = 0, y = 0.53, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Novo Valor de HDL:", input$HDL), x = 0, y = 0.50, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Novo Valor de Pressão Arterial Sistólica:", input$PAS), x = 0, y = 0.47, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Fumador:", ifelse(input$fumador == 0, "Não", "Sim")), x = 0, y = 0.44, just = "left", gp = gpar(fontsize = 12))
        
        risco <- calcular_risco()
        grid.text(paste("Nova Estimativa de Risco Cardiovascular:", round(risco * 100, 2), "%"), 
                  x = 0, y = 0.41, just = "left", gp = gpar(fontsize = 14, fontface = "bold", col = "red"))
        
        grid.text(paste("Recomendações:"), x = 0, y = 0.36, just = "left", gp = gpar(fontsize = 12, col = "blue"))
        
        if (risco_guardado[2] > 200) {
          grid.text(paste("Colesterol Total Alto: Tente baixar para", input$col,", adotanto um estilo de vida mais saudável.\nExemplo: diminuir as comidas gordurosas, comer mais alimentos como frutas e vegetais e praticar exercício."), x = 0, y = 0.32, just = "left", gp = gpar(fontsize = 11))
        }
        
        if (risco_guardado[4] > 130) {
          grid.text(paste("Pressão Arterial Sistólica Alta: Tente baixar para", input$PAS,", adotando um estilo de vida saudável.\nExemplo: diminuir o sal na comida, comer mais alimentos como frutas e vegetais e praticar exercício."), x = 0, y = 0.26, just = "left", gp = gpar(fontsize = 11))
        }
        if (risco_guardado[5] == 1) {
          grid.text(paste("Fumador: Tente diminuir a quantidade de cigarros que fuma por dia até parar."), x = 0, y = 0.21, just = "left", gp = gpar(fontsize = 11))
        }
        
      } else {
        grid.text(paste("Nome do Utente:", input$nome_utente), x = 0, y = 0.85, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Idade:", input$idade), x = 0, y = 0.82, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Sexo:", ifelse(input$sexo == 0, "Masculino", "Feminino")), x = 0, y = 0.79, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Colesterol Total:", input$col), x = 0, y = 0.76, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("HDL:", input$HDL), x = 0, y = 0.73, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Pressão Arterial Sistólica:", input$PAS), x = 0, y = 0.70, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Tratamento:", ifelse(input$tratamento == 0, "Não", "Sim")), x = 0, y = 0.67, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Diabético:", ifelse(input$diabetico == 0, "Não", "Sim")), x = 0, y = 0.64, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Fumador:", ifelse(input$fumador == 0, "Não", "Sim")), x = 0, y = 0.61, just = "left", gp = gpar(fontsize = 12))
        
        risco <- calcular_risco()
        grid.text(paste("Estimativa de Risco Cardiovascular:", round(risco * 100, 2), "%"), 
                  x = 0, y = 0.58, just = "left", gp = gpar(fontsize = 14, fontface = "bold", col = "red"))
        
        grid.text(paste("Recomendações:"), x = 0, y = 0.53, just = "left", gp = gpar(fontsize = 12, col = "blue"))
        
        if (input$col > 200) {
          grid.text(paste("Colesterol Total Alto: Tente baixar para 200, adotanto um estilo de vida mais saudável.\nExemplo: diminuir as comidas gordurosas, comer mais alimentos como frutas e vegetais e praticar exercício."), x = 0, y = 0.49, just = "left", gp = gpar(fontsize = 11))
        }
        
        if (input$PAS > 130) {
          grid.text(paste("Pressão Arterial Sistólica Alta: Tente baixar para 120 adotando um estilo de vida saudável.\nExemplo: diminuir o sal na comida, comer mais alimentos como frutas e vegetais e praticar exercício."), x = 0, y = 0.43, just = "left", gp = gpar(fontsize = 11))
        }
        if (input$fumador == 1) {
          grid.text(paste("Fumador: Tente diminuir a quantidade de cigarros que fuma por dia até parar."), x = 0, y = 0.38, just = "left", gp = gpar(fontsize = 11))
        }
        else {
          grid.text(paste("Continue a manter os seus bons hábitos!"), x = 0, y = 0.49, just = "left", gp = gpar(fontsize = 11))
        }
      }
      
      # Aviso legal
      aviso_legal <- "Aviso Legal:\nEste relatório destina-se exclusivamente a fins académicos e não constitui um documento médico oficial. \nNão foi revisto pelas autoridades reguladoras e pode conter erros. \nA utilização deste relatório é da inteira responsabilidade do utilizador. \nOs criadores e a universidade afiliada não se responsabilizam pelas decisões tomadas com base no seu conteúdo.\nPara questões médicas, por favor consulte um profissional de saúde qualificado."
      
      grid.text(aviso_legal, x = 0, y = 0.15, just = c("left", "top"), 
                gp = gpar(fontsize = 10), draw = TRUE,
                check.overlap = FALSE, default.units = "npc",
                name = NULL, vp = NULL)
      
      popViewport()
      dev.off()
    }
  )
  
  # Processar dados do arquivo Excel
  observeEvent(input$processar, {
    req(input$arquivo)
    dados <- readxl::read_excel(input$arquivo$datapath)
    colnames(dados) = c("nome_utente", "idade", "col", "HDL", "PAS", "sexo", "tratamento", "fumador", "diabetico", "risco")
    
    # Verificar se as variáveis estão no formato correto
    dados$sexo <- as.numeric(ifelse(dados$sexo == "F" | dados$sexo == 1, 1, 0))  # 1 - F, 0 - M
    dados$tratamento <- as.numeric(ifelse(dados$tratamento == "Sim" | dados$tratamento == 1, 1, 0))  # 1 - S, 0 - N
    dados$fumador <- as.numeric(ifelse(dados$fumador == "Sim" | dados$fumador == 1, 1, 0))  # 1 - S, 0 - N
    dados$diabetico <- as.numeric(ifelse(dados$diabetico == "Sim" | dados$diabetico == 1, 1, 0))  # 1 - S, 0 - N
    
    # Calcular a estimativa de risco para cada paciente
    dados$risco <- mapply(function(sexo, idade, col, HDL, fumador, diabetico, PAS, tratamento) {
      estimativa_risco(sexo, idade, col, HDL, fumador, diabetico, PAS, tratamento)
    }, dados$sexo, dados$idade, dados$col, dados$HDL, dados$fumador, dados$diabetico, dados$PAS, dados$tratamento)
    
    dados_tabela = dados
    dados_tabela$sexo <- ifelse(dados$sexo == 1, "F", "M")
    dados_tabela$tratamento <- ifelse(dados$tratamento == 1, "Sim", "Não")
    dados_tabela$fumador <- ifelse(dados$fumador == 1, "Sim", "Não")
    dados_tabela$diabetico <- ifelse(dados$diabetico == 1, "Sim", "Não")
    dados_tabela$risco <- percent(dados_tabela$risco)
    
    colnames(dados_tabela) = c("Nome de Utente", "Idade", "Colesterol Total", "HDL", "Pressão Arterial Sistólica", "Sexo", "Em Tratamento", "Fumador", "Diabético", "Risco")
    
    output$resumo_estatisticas <- renderTable({
      dados_tabela
    })
    
    output$download_excel <- downloadHandler(
      filename = function() {
        paste("tabela_pacientes_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        write.xlsx(dados_tabela, file)
      })
  })
}

# Rodar o aplicativo Shiny
shinyApp(ui, server)

