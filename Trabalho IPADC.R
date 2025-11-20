#Projeto IPADC


#Função
# tcl = total cholesterol, sbp = systolic blood pressure, treated = treatment for hbp
risco_estimado = function(sex,age,treated,tcl,hdl,sbp,smoker,diabetic) {
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

risco_estimado(0,61,0,180,47,124,1,0)
risco_estimado(1,53,1,161,55,125,0,1)



####################################################################
#INTERFACE SHINY
####################################################################
library(shiny)
library(ggplot2)
library(DT)
library(plotly)
library(medicaldata)
#library(rsconnect)
library(bslib) #não apagar esta library!
library(htmltools)
library(data.table)

library(devtools)

library(ggplot2)
library(DT)
library(hexbin)

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
                       textInput("nome_utente", "Nome do Utente:"), # Novo campo para o nome do utente
                       numericInput("idade", "Idade:", 50, min = 30, max = 120),
                       textInput("n_utente", "Número de utente", '000000000'),
                       numericInput("col", "Colesterol Total:", 200, min = 100, max = 500),
                       htmlOutput("col_guardado"),
                       dateInput(
                         'data',
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
  
}

shinyApp(ui, server)
