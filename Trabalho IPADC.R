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

estimated_risk(0,61,0,180,47,124,1,0)
estimated_risk(1,53,1,161,55,125,0,1)



####################################################################
#INTERFACE SHINY
####################################################################
library(shiny)
library(shinyTime)
library(ggplot2)


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
  
  # Navigation bar
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
          h2("Calculadora do risco cardiovascular"),
          p("Calculadora do risco"),
          
      ),
      

      
      # --- Contact Page ---
      hidden(div(id = "page_calculator",
                 h2("Calculadora"),
                 fluidRow(
                   box(title = "Dados do Paciente", width = 6,
                       textInput("patient_name", "Nome do Utente:"), # Novo campo para o nome do utente
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
                       radioButtons("sex", "Sexo:", choices = c("Masculino" = 0, "Feminino" = 1)),
                       radioButtons("treated", "Em Tratamento:", choices = c("Não" = 0, "Sim" = 1)),
                       radioButtons("smoker", "Fumador:", choices = c("Não" = 0, "Sim" = 1)),
                       htmlOutput("smoker_saved"),
                       radioButtons("diabetic", "Diabético:", choices = c("Não" = 0, "Sim" = 1)),
                       actionButton("calcular", "Calcular Risco"),
                       actionButton("guardar", "Guardar Valores")
                   ),
                   
                   box(title = "Estimativa de Risco", width = 6,
                       htmlOutput("estimated_risk"), 
                       htmlOutput("saved_risk"),
                       htmlOutput("tcl_high"),
                       htmlOutput("sbp_high"),
                       htmlOutput("aviso_legal"),
                       downloadButton("download_relatorio", "Criar PDF")
                   )
                 )
      )),
      
      # --- About Page ---
      hidden(div(id = "page_about",
                 p("Aplicação feita para o projeto de IPADC"),
      )),
      )
)


server <- function(input, output, session) {
  
  
  #––– PAGE NAVIGATION –––
  showPage <- function(pageId) {
    message("Escondendo todas as páginas")
    shinyjs::hide("page_home")
    shinyjs::hide("page_about")
    shinyjs::hide("page_calculator")
    message("Tentando mostrar página: ", pageId)
    shinyjs::show(pageId)
    shinyjs::runjs(sprintf("
      console.log('Tentando mostrar %s');
      var elem = document.getElementById('%s');
      if (elem) {
        elem.style.display = 'block';
        console.log('Elemento %s encontrado e definido como visível');
      } else {
        console.log('Elemento %s NÃO encontrado');
      }", pageId, pageId, pageId, pageId))
  }
  observeEvent(input$nav_home,   { showPage("page_home") })
  observeEvent(input$nav_about,  { showPage("page_about") })
  observeEvent(input$nav_calculator,{ showPage("page_calculator") })
  
  
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
    #sex,age,treated,tcl,hdl,sbp,smoker,diabetic
    risco <- estimated_risk(sex,age,treated,tcl,hdl,sbp,smoker,diabetic)
    estimated_risk(0,61,0,180,47,124,1,0)
    message("Dados: ", sex, " ", age, " ", treated, " ",tcl, " ",hdl," ", sbp, " ", smoker," ", diabetic)
    return(risco)
  })
  
  guardar_risco <- eventReactive(input$save,{
    
    sex <- as.numeric(input$sex)
    age <- as.numeric(input$age)
    tcl_saved <- as.numeric(input$tcl)
    hdl_saved <- as.numeric(input$hdl)
    sbp_saved <- as.numeric(input$sbp)
    treated <- as.numeric(input$treated)
    smoker_saved <- as.numeric(input$smoker)
    diabetic <- as.numeric(input$diabetic)
    
    saved_risk <- estimated_risk(sex,age,treated,tcl_saved,hdl_saved,sbp_saved,smoker_saved,diabetic)
    return(c(saved_risk, tcl_saved, hdl_saved, sbp_saved, smoker_saved))
  })
  
  # Exibir a estimativa de risco
  output$estimated_risk <- renderUI({
    risco <- calculo_risco()
    HTML(paste("<h5>Estimativa atual de risco: <strong>", risco, "%</strong></h3>"))
  })
  
  output$ctcl_high <- renderUI({
    if (as.numeric(input$tcl) > 200){
      HTML(paste("<h6 style=\"color:red;\">Valor de Colesterol Alto!"))
    }
  })
  
  output$sbp_high <- renderUI({
    if (as.numeric(input$sbp) > 130){
      HTML(paste("<h6 style=\"color:red;\">Valor de sbp high!"))
    }
  })
  
  output$saved_risk <- renderUI({
    valores_saved <- guardar_risco()
    HTML(paste("<h5>Estimativa original do risco: <strong>", valores_saved[1], "%</strong></h3>"))
  })
  
  output$tcl_saved <- renderUI({
    valores_saved <- guardar_risco()
    
    HTML(paste("<h5 style=\"color:blue;\"> Valor anterior: <strong>", valores_saved[2]))
  })
  
  output$hdl_saved <- renderUI({
    valores_saved <- guardar_risco()
    
    HTML(paste("<h5 style=\"color:blue;\">Valor anterior: <strong>", valores_saved[3]))
  })
  
  output$sbp_saved <- renderUI({
    valores_saved <- guardar_risco()
    
    HTML(paste("<h5 style=\"color:blue;\">Valor anterior: <strong>", valores_saved[4]))
  })
  
  output$smoker_saved <- renderUI({
    valores_saved <- guardar_risco()
    
    if (valores_saved[5] == 0){
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
      paste("relatorio_", input$patient_name, "_",Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, paper = "a4", width = 8.27, height = 11.69) # Dimensões A4 em polegadas
      
      grid.newpage()
      
      # Definir margens
      pushViewport(viewport(x = 0.05, y = 0.05, width = 0.9, height = 0.9, just = c("left", "bottom")))
      
      # Título
      grid.text("Relatório de Risco Cardiovascular", x = 0.5, y = 0.95, gp = gpar(fontsize = 16, fontface = "bold"))
      
      saved_risk <- tryCatch(guardar_risco(), error = function(e) {return(NULL)})
      
      # Informações do paciente
      
      if (!is.null(saved_risk)) {
        grid.text(paste("Nome do Utente:", input$patient_name), x = 0, y = 0.85, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Idade:", input$age), x = 0, y = 0.82, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Sexo:", ifelse(input$sex == 0, "Masculino", "Feminino")), x = 0, y = 0.79, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Colesterol Total:", saved_risk[2]), x = 0, y = 0.76, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("hdl:", saved_risk[3]), x = 0, y = 0.73, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Pressão Arterial Sistólica:", saved_risk[4]), x = 0, y = 0.70, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Tratamento:", ifelse(input$treated == 0, "Não", "Sim")), x = 0, y = 0.67, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Diabético:", ifelse(input$diabetic == 0, "Não", "Sim")), x = 0, y = 0.64, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Fumador:", ifelse(saved_risk[5] == 0, "Não", "Sim")), x = 0, y = 0.61, just = "left", gp = gpar(fontsize = 12))
        
        grid.text(paste("Estimativa de Risco Cardiovascular:", round(saved_risk[1] * 100, 2), "%"),
                  x = 0, y = 0.58, just = "left", gp = gpar(fontsize = 14, fontface = "bold", col = "red"))
        
        grid.text(paste("Novo Valor de Colesterol Total:", input$tcl), x = 0, y = 0.53, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Novo Valor de hdl:", input$hdl), x = 0, y = 0.50, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Novo Valor de Pressão Arterial Sistólica:", input$sbp), x = 0, y = 0.47, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Fumador:", ifelse(input$smoker == 0, "Não", "Sim")), x = 0, y = 0.44, just = "left", gp = gpar(fontsize = 12))
        
        risco <- calculo_risco()
        grid.text(paste("Nova Estimativa de Risco Cardiovascular:", risco, "%"), 
                  x = 0, y = 0.41, just = "left", gp = gpar(fontsize = 14, fontface = "bold", col = "red"))
        
        grid.text(paste("Recomendações:"), x = 0, y = 0.36, just = "left", gp = gpar(fontsize = 12, col = "blue"))
        
        if (saved_risk[2] > 200) {
          grid.text(paste("Colesterol Total Alto: Tente baixar para", input$tcl,", adotanto um estilo de vida mais saudável.\nExemplo: diminuir as comidas gordurosas, comer mais alimentos como frutas e vegetais e praticar exercício."), x = 0, y = 0.32, just = "left", gp = gpar(fontsize = 11))
        }
        
        if (saved_risk[4] > 130) {
          grid.text(paste("Pressão Arterial Sistólica Alta: Tente baixar para", input$sbp,", adotando um estilo de vida saudável.\nExemplo: diminuir o sal na comida, comer mais alimentos como frutas e vegetais e praticar exercício."), x = 0, y = 0.26, just = "left", gp = gpar(fontsize = 11))
        }
        if (saved_risk[5] == 1) {
          grid.text(paste("Fumador: Tente diminuir a quantidade de cigarros que fuma por dia até parar."), x = 0, y = 0.21, just = "left", gp = gpar(fontsize = 11))
        }
        
      } else {
        grid.text(paste("Nome do Utente:", input$patient_name), x = 0, y = 0.85, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Idade:", input$age), x = 0, y = 0.82, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Sexo:", ifelse(input$sex == 0, "Masculino", "Feminino")), x = 0, y = 0.79, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Colesterol Total:", input$tcl), x = 0, y = 0.76, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("hdl:", input$hdl), x = 0, y = 0.73, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Pressão Arterial Sistólica:", input$sbp), x = 0, y = 0.70, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Tratamento:", ifelse(input$treated == 0, "Não", "Sim")), x = 0, y = 0.67, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Diabético:", ifelse(input$diabetic == 0, "Não", "Sim")), x = 0, y = 0.64, just = "left", gp = gpar(fontsize = 12))
        grid.text(paste("Fumador:", ifelse(input$smoker == 0, "Não", "Sim")), x = 0, y = 0.61, just = "left", gp = gpar(fontsize = 12))
        
        risco <- calculo_risco()
        grid.text(paste("Estimativa de Risco Cardiovascular:", risco, "%"), 
                  x = 0, y = 0.58, just = "left", gp = gpar(fontsize = 14, fontface = "bold", col = "red"))
        
        grid.text(paste("Recomendações:"), x = 0, y = 0.53, just = "left", gp = gpar(fontsize = 12, col = "blue"))
        
        if (input$tcl > 200) {
          grid.text(paste("Colesterol Total Alto: Tente baixar para 200, adotanto um estilo de vida mais saudável.\nExemplo: diminuir as comidas gordurosas, comer mais alimentos como frutas e vegetais e praticar exercício."), x = 0, y = 0.49, just = "left", gp = gpar(fontsize = 11))
        }
        
        if (input$sbp > 130) {
          grid.text(paste("Pressão Arterial Sistólica Alta: Tente baixar para 120 adotando um estilo de vida saudável.\nExemplo: diminuir o sal na comida, comer mais alimentos como frutas e vegetais e praticar exercício."), x = 0, y = 0.43, just = "left", gp = gpar(fontsize = 11))
        }
        if (input$smoker == 1) {
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
}

shinyApp(ui, server)
