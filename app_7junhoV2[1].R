library(shiny)
library(ggplot2)
library(DT)
library(plotly)
library(medicaldata)
#library(rsconnect)
library(bslib) #não apagar esta library!
library(htmltools)
library(data.table)

library(BiocManager)
#install.packages("devtools")
library(devtools)

library(ggplot2)
library(DT)
library(hexbin)
#BiocManager::install("DESeq2")
library(DESeq2)
library(dplyr)
library(factoextra)
library(RColorBrewer)
library(pheatmap)
#BiocManager::install("vsn")
library(vsn)
#BiocManager::install("phyloseq")
#BiocManager::install("genefilter")
#BiocManager::install("biomformat")
#install_github('twbattaglia/btools')
library(btools)
#install_github("dleelab/leedonghyung")
library(leedonghyung)
library(data.table)
library(plotly)
library(ggrepel)
library(Seurat)
library(VennDiagram)
library(reshape2)
library(heatmaply)


setwd("C:\\Users\\Marta\\Documents\\24-25\\analise de dados em bioinformatica\\24-25\\projeto final\\app ana")

#rsconnect::setAccountInfo(name='noodle', token='14B592BDE1D3B2335D649DEF5426CB1E', secret='XNQnAvUpmLRhriQWVhP4EbJJ0scLEnNe4D5W/IHA')

#para resolver problemas de limitação do upload dos ficheiros:
options(shiny.maxRequestSize = 50 * 1024^2)

library(shiny)
library(shinyjs)
library(DT)

library(DESeq2)
library(RColorBrewer)



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
      tags$li(actionLink("nav_home", "Home")),
      tags$li(actionLink("nav_about", "About")),
      tags$li(actionLink("nav_contact", "Contact")),
      tags$li(actionLink("nav_upload", "Upload & Process")),
      tags$li(actionLink("nav_deseq2", "DESeq2 Analysis")),
      tags$li(actionLink("nav_eda", "Exploratory Data Analysis"))
    )
  ),
  
  # Page content container
  div(class = "page-container",
      
      # --- Home Page ---
      div(id = "page_home",
          h2("Home Page"),
          p("Welcome to the RNA-Seq Analysis App!")
      ),
      
      # --- About Page ---
      hidden(div(id = "page_about",
                 h2("About this App:"),
                 p("Application done for the final project in Bioinformatic Data Analysis."),
                 p("In this project, we performed an analysis of RNA-Seq processing outputs from DMSO and drug-treated cancer cells."),
                 p("Application developed with love (and with fingers crossed at times) by group 3 on the Tamoxifen and Mefloquine (TM) Drug."),
                 br(),
                 p("Obrigado Ana por teres feito meia direta para resolver este código. :)"),
      )),
      
      # --- Contact Page ---
      hidden(div(id = "page_contact",
                 h2("Contact"),
                 div(class = "contact-box",
                     p(strong("Developer:"), "Ana Cardoso"),
                     p(strong("Institution:"), " Universidade de Aveiro"),
                     p(strong("Student Number:"), "122994"),
                     p(strong("Email:"), a(href = "mailto:analuisacardoso@ua.pt", "analuisacardoso@ua.pt"))
                 ),
                 div(class = "contact-box",
                     p(strong("Developer:"), "Anabela Madalena"),
                     p(strong("Institution:"), " Universidade de Aveiro"),
                     p(strong("Student Number:"), " 124257"),
                     p(strong("Email:"), a(href = "mailto:anabelamadalena@ua.pt", "anabelamadalena@ua.pt"))
                 ),
                 div(class = "contact-box",
                     p(strong("Developer:"), "Beatriz Lavado"),
                     p(strong("Institution:"), " Universidade de Aveiro"),
                     p(strong("Student Number:"), " 102495"),
                     p(strong("Email:"), a(href = "mailto:beatriznlavado@ua.pt", "beatriznlavado@ua.pt"))
                 ),
                 div(class = "contact-box",
                     p(strong("Developer:"), "Marta Carvalho"),
                     p(strong("Institution:"), " Universidade de Aveiro"),
                     p(strong("Student Number:"), " 107664"),
                     p(strong("Email:"), a(href = "mailto:martasc@ua.pt", "martasc@ua.pt"))
                 ),
      )),
      
      # --- Upload & Process Page ---
      hidden(div(id = "page_upload",
                 h2("Upload Data and Run Processing"),
                 p("Note that, with exception of the first file, only the first 10 lines of each file are represented, for viewing purposes solely."),
                 
                 fileInput("file_geneann", "1. Sample Info (sample_info.txt)", accept = ".txt"),
                 DTOutput("preview_coldata_raw"),
                 
                 fileInput("file_transcounts", "2. Transcript Count Matrix (.csv)", accept = c(".csv", ".tsv")),
                 DTOutput("preview_cts_raw"),
                 
                 fileInput("file_transann", "3. Transcript Annotation (.txt)", accept = ".txt"),
                 DTOutput("preview_annot_raw"),
                 
                 fileInput("file_genecounts", "4. Gene Count Matrix (.csv)", accept = ".csv"),
                 DTOutput("preview_count_raw"),
                 
                 br(),
                 actionButton("run_process", "Run Processing"),
                 br(), br(),
                 
                 # --- Processing Results ---
                 hidden(div(id = "processing_results",
                            hidden(div(id = "post_process_controls",
                                       br(),
                                       htmlOutput("process_status"),
                                       numericInput("low_coverage_cut", "Low Coverage Sample Threshold:", value = 4000000, min = 0, step = 100000),
                                       numericInput("low_count_cut", "Low Count Gene Threshold:", value = 0, min = 0, step = 10),
                                       actionButton("validate_filters", "Validate & Apply Filters"),
                                       textOutput("filter_summary"),
                                       br(),
                                       hidden(div(id = "final_analysis_results",
                                                  h3("Final Filtered Count Matrix"),
                                                  DTOutput("final_cts"),
                                                  h3("Updated Sample Info"),
                                                  p("Please note that from here on, only the DMSO and TM samples are included, since the only count file available was precisely of those treatments."),
                                                  DTOutput("final_coldata"),
                                                  br(),
                                                  h3("DESeq2 analysis ready to begin.")
                                       ))
                            ))
                 ))
      )),
      
      # --- DESeq2 Page ---
      hidden(div(id = "page_deseq2",
                 h2("DESeq2 Differential Expression Analysis"),
                 
                
                 p("Select the experimental condition for comparison against the fixed reference:"),
                 p(strong("Reference condition:"), "DMSO_0"),
                 
                 p("Statistical testing:"),
                 
                 
                 
                 hidden(div(id = "deseq_controls",
                            selectInput("experimental_condition", "Experimental condition:",
                                        choices = NULL),  # dynamic
                            
                            selectInput("padj_method", "p-adjust method:",
                                        choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
                                        selected = "fdr"),  # default
                            
                            helpText("Common choices are 'fdr' or 'BH' for controlling false discovery rate."),
                            
                            
                            numericInput("alpha_threshold", "Adjusted p-value threshold (alpha):",
                                         value = 0.05, min = 0, max = 1, step = 0.01),
                            
                            numericInput("num_top_genes", "Number of top DE genes to show:",
                                         value = 20, min = 5, max = 40, step = 5),
                            
                            
                            actionButton("run_deseq", "Run DESeq2"),
                            
                            br(),
                            h4("While you wait, a lesson:"),
                            
                            p("Under the hood, the code (more specifically, the DESeq2 package) is running a negative binomial distribution, which models the mean count and the dispersion typical of RNA-Seq data - overdispersed and non negative counts that can span a huge range across genes and samples"),
                            p("DESeq2 firstly estimates size factors for normalization, estimates dispersion gene-by-gene, then fits a generalized linear model, and then runs a Wald test"),
                            p("More technical details are out of the scope of this app, but available by searching the Vignette related to DESeq2."),
                            br(),
                            
                            p("Summary table of the metrics obtained:"),
                            
                            div(class = "table-container", tableOutput("deseq_summary")),
                            
                            br(),
                            
                            p("Volcano plot of the adjusted p values."),
                            p("In this plot, a positive log2FC indicates that the statistically significant genes (and thus, that the number of counts of these genes is statistically different from what was defined) in the area are overexpressed on the experimental sample compared to the reference condition."),
                            
                            div(class = "plot-container", plotOutput("volcano_plot", height = "500px")),
                            
                            br(),
                            
                            p("Heatmap of diiferentially expressed genes"),
                            div(class= 'plot-container', plotOutput("gene_heatmap"), height="500px")
                 ))
      )),
      
      # --- Exploratory Analysis Page ---
      hidden(div(id = "page_eda",
                 h2("Exploratory Data Analysis"),
                 
                 p("This section provides exploratory plots for the filtered data."),
                 p("WARNING: this is a time-consuming process, please wait."),
                 actionButton("run_eda", "Generate EDA Plots"),
                 br(), br(),
                 
                 p("Boxplots obrained in raw values, normal transformation, vst, and logistic regression."),
                 div(class = "plot-container", plotOutput("boxplot_raw", height = "500px")),
                 div(class = "plot-container", plotOutput("boxplot_normTransform", height = "500px")),
                 div(class = "plot-container", plotOutput("boxplot_vst", height = "500px")),
                 div(class = "plot-container", plotOutput("boxplot_rlog", height = "500px")),
                 
                 br(),
                 p("Hierarchical clustering and euclidean distance between samples "),
                 div(class = "plot-container", plotOutput("distance_heatmap", height = "500px")),
                 
                 br(),
                 p("Principal Components Analysis (2D and 3D)"),
                 div(class = "plot-container", plotOutput("distance_PCA", height = "500px")),
                 div(class = "plot-container", plotlyOutput("distance_3DPCA", height = "500px")),
                 
                 
                 )
      ))
      
      
      
  )





server <- function(input, output, session) {
  
  # Store raw and processed data
  data <- reactiveValues(
    coldata_raw = NULL,
    cts_raw = NULL
  )
  
  # Store filtered results for DESeq2 and UI
  vals <- reactiveValues(
    filtered_coldata = NULL,
    filtered_cts = NULL
  )
  
  
  #––– PAGE NAVIGATION –––
  showPage <- function(pageId) {
    message("Escondendo todas as páginas")
    shinyjs::hide("page_home")
    shinyjs::hide("page_about")
    shinyjs::hide("page_contact")
    shinyjs::hide("page_upload")
    shinyjs::hide("page_deseq2")
    shinyjs::hide("page_eda")
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
  observeEvent(input$nav_contact,{ showPage("page_contact") })
  observeEvent(input$nav_upload, { showPage("page_upload") })
  observeEvent(input$nav_deseq2, { showPage("page_deseq2") })
  observeEvent(input$nav_eda, { showPage("page_eda") })
  
  
  #––– coldata (sample info) –––
  coldata <- reactive({
    req(input$file_geneann)
    df <- read.delim(input$file_geneann$datapath, sep = "\t")
    rownames(df) <- df[[1]]
    df[, -1, drop = FALSE]
  })
  
  output$preview_coldata_raw <- renderDT({
    req(coldata())
    DT::datatable(coldata(), options = list(scrollX = TRUE))
  })
  
  #––– raw transcript counts –––
  cts_raw <- reactive({
    req(input$file_transcounts)
    read.csv(input$file_transcounts$datapath, row.names = 1)
  })
  observeEvent(cts_raw(), {
    output$preview_cts_raw <- renderDT({
      DT::datatable(head(cts_raw(), 6), options = list(scrollX = TRUE))
    })
  })
  
  #––– raw annotation –––
  annot_raw <- reactive({
    req(input$file_transann)
    read.delim(input$file_transann$datapath, sep = "\t")
  })
  observeEvent(annot_raw(), {
    output$preview_annot_raw <- renderDT({
      DT::datatable(head(annot_raw(), 6), options = list(scrollX = TRUE))
    })
  })

  count_raw <- reactive({
    req(input$file_genecounts)
    read.csv(input$file_genecounts$datapath, row.names = 1)
  })
  observeEvent(count_raw(), {
    output$preview_count_raw <- renderDT({
      DT::datatable(head(count_raw(), 6), options = list(scrollX = TRUE))
    })
  })  
  
  
  #––– Process Button –––
  observeEvent(input$run_process, {
    message("Botão 'Run Processing' clicado")
    req(coldata(), cts_raw(), annot_raw(), count_raw())
    
    message("Dados de entrada disponíveis: coldata, cts, annot, count")
    
    # Initial data
    coldata2 <- coldata()
    cts <- cts_raw()
    annot <- annot_raw()
    
    message("Dimensões de coldata2: ", paste(dim(coldata2), collapse = " x "))
    message("Dimensões de cts: ", paste(dim(cts), collapse = " x "))
    message("Dimensões de annot: ", paste(dim(annot), collapse = " x "))
    
    message("Primeiros 5 rownames de coldata2: ", paste(head(rownames(coldata2), 5), collapse = ", "))
    message("Primeiros 5 colnames de cts antes do merge: ", paste(head(colnames(cts), 5), collapse = ", "))
    
    # Apply grouping filter
    group <- 3
    coldata2 <- coldata2[c(1:9, (9 * group + 1):(9 * group + 9)), ]
    
    # Annotate and filter cts
    annot$new <- paste0(annot$transcript_id, "|", annot$gene_name)
    cts <- merge(annot, cts, by.x = "transcript_id", by.y = 0)
    rownames(cts) <- cts[, 3]
    cts <- cts[, -c(1:3)]
    
    message("Primeiros 5 colnames de cts após merge: ", paste(head(colnames(cts), 5), collapse = ", "))
    
    colnames(cts) <- gsub(".gtf_", "", colnames(cts))
    
    message("Primeiros 5 colnames de cts após gsub: ", paste(head(colnames(cts), 5), collapse = ", "))
    message("Número de colunas em cts antes do filtro: ", ncol(cts))
    message("Número de colunas correspondentes em coldata2: ", sum(colnames(cts) %in% rownames(coldata2)))
    
    cts <- cts[, colnames(cts) %in% rownames(coldata2)]
    
    # Store processed raw data
    data$coldata_raw <- coldata2
    data$cts_raw <- cts
    
    message("Processamento concluído. Dimensões finais: coldata_raw = ", paste(dim(coldata2), collapse = " x "), ", cts_raw = ", paste(dim(cts), collapse = " x "))
    View(coldata2)
    # Show processing UI
    shinyjs::show("processing_results")
    shinyjs::show("post_process_controls")
    
    output$process_status <- renderUI({
      tags$div(style = "color: green; font-weight: bold;", "Data processing complete!")
    })
  })
  
    
  observeEvent(input$validate_filters, {
    req(data$coldata_raw, data$cts_raw, input$low_coverage_cut, input$low_count_cut)
    
    coldata2 <- data$coldata_raw
    cts <- data$cts_raw
    
    # Apply filtering
    low_coverage <- cts[, colSums(cts) < input$low_coverage_cut]
    if (ncol(low_coverage) > 0) {
      remove <- colnames(low_coverage)
      cts <- cts[, !colnames(cts) %in% remove]
      coldata2 <- coldata2[!rownames(coldata2) %in% remove, ]
    }
    
    cts <- cts[rowSums(cts) > input$low_count_cut, ]
    cts <- cts[rowSums(cts) != apply(cts, 1, max), ]
    
    cts <- cts[, order(colnames(cts))]
    coldata2 <- coldata2[order(rownames(coldata2)), ]
    
    View(coldata2)
    # Store filtered data for downstream
    vals$filtered_coldata <- coldata2
    vals$filtered_cts <- cts
    
    # UI updates
    output$filter_summary <- renderText({
      paste("Filtered data: ", nrow(cts), " genes and ", ncol(cts), " samples.")
    })
    
    output$final_cts <- renderDT({
      DT::datatable(head(cts), options = list(scrollX = TRUE))
    })
    
    output$final_coldata <- renderDT({
      DT::datatable(coldata2, options = list(scrollX = TRUE))
    })
    
    shinyjs::show("final_analysis_results")
  })
  
  
  #––– Update dropdown menu with filtered coldata –––
  observe({
    req(vals$filtered_coldata)
    variable_of_interest <- 3
    
    ordered_levels <- c("DMSO_0", "DMSO_9", "DMSO_24", "TM_0", "TM_9", "TM_24")
    vals$filtered_coldata[, variable_of_interest] <- factor(
      vals$filtered_coldata[, variable_of_interest],
      levels = ordered_levels
    )
    levels_list <- levels(vals$filtered_coldata[, variable_of_interest])
    View(levels_list)
    experimental_choices <- levels_list[levels_list != "DMSO_0"]
    
    updateSelectInput(session, "experimental_condition",
                      choices = experimental_choices,
                      selected = experimental_choices[1])
    
    shinyjs::show("deseq_controls")  # Make sure to show controls once data is ready
  })
  
  #––– DESeq2 analysis trigger –––
  observeEvent(input$run_deseq, {
    req(vals$filtered_coldata, vals$filtered_cts, input$experimental_condition)
    
    coldata <- vals$filtered_coldata
    cts <- vals$filtered_cts
    
    variable_of_interest <- 3
    
    coldata[,variable_of_interest] <- factor(coldata[,variable_of_interest],
                                             levels = c("DMSO_0",
                                                        "DMSO_9",
                                                        "DMSO_24",
                                                        "TM_0",
                                                        "TM_9",
                                                        "TM_24"))
    View(coldata)
    reference <- "DMSO_0"
    experimental <- input$experimental_condition
    
    print(paste("Reference:", reference))
    print(paste("Experimental:", experimental))
    
    coldata[,variable_of_interest] <- relevel(coldata[,variable_of_interest], reference)
    
    dds <-
      DESeqDataSetFromMatrix(countData = cts,
                             colData = coldata,
                             design = 
                               formula(
                                 paste0("~",
                                        colnames(coldata)[variable_of_interest])))
    View(dds)
    #TODO: se calhar meter o output da tabela mas vou fazer isso depois.
    
    withProgress(message = "Running DESeq analysis...", {
      dds2 <- DESeq(dds)
    })
    
    comparison_names <- resultsNames(dds2)
    which_comparison <- grep(experimental, comparison_names)
    
    res <- results(
      dds2,
      name = comparison_names[which_comparison],
      pAdjustMethod = input$padj_method,
      lfcThreshold = 1,
      alpha = input$alpha_threshold
    )
    
    

    summary_text <- capture.output(summary(res))
    summary_df <- data.frame(Summary = summary_text)
    
    output$deseq_summary <- renderTable({
      summary_df
    })
    
    
    res.shr <- as.data.frame(res)
    res.shr$ids <- row.names(res.shr)
    setDT(res.shr)[, c("c1", "c2") := tstrsplit(ids, "\\|")]
    
    res.shr$Significant <- ifelse(
      res.shr$padj < 0.05,
      "padj < 0.05",
      "padj > 0.05"
    )
    
    res.shr <- res.shr[!is.na(res.shr$Significant), ]
    res.shr <- res.shr[order(res.shr$padj), ]
    
    print("Triggering Volcano plot")
    output$volcano_plot <- renderPlot({
      top_labels <- head(res.shr[res.shr$padj < 0.05, ], 10)
      
      ggplot(res.shr, aes(x = log2FoldChange, y = -log10(padj))) +
        geom_point(aes(color = Significant)) +
        scale_color_manual(values = c("turquoise4", "grey")) +
        theme_bw(base_size = 12) +
        theme(legend.position = "bottom") +
        ggtitle("Volcano Plot (padj < 0.05)") +
        xlab("log2 fold change") +
        ylab("-log10(padj)") +
        geom_text_repel(
          data = top_labels,
          aes(label = c2),
          size = 3,
          box.padding = unit(0.35, "lines"),
          point.padding = unit(0.3, "lines")
        )
    })
    
    ####PARTE DO HEATMAP DOS GENES
    
    #Order rows by padj values (from lowest to highest)
    o <- order(res$padj, decreasing = FALSE)
    topgenes <- res[o, ] 
    
    #Extract the first 20 genes
    topN <- input$num_top_genes
    top20genes <- rownames(topgenes[1:topN, ])
    
    
    allCounts <- counts(dds2, normalized=TRUE)
    
    #Selects the timepoints in which we are interested
    
    #Subsets the coldata data.frame based on those values
    annotation <- coldata[coldata[,3] %in% c(reference, experimental),] 
    
    #Creates a data.frame based on "allCounts" but comprising only the top 20 genes as rows and the ids of the samples as columns
    top20genesCounts <- as.data.frame(allCounts[top20genes,rownames(annotation)]) 
    
    
    
    output$gene_heatmap <- renderPlot({
      
      heat_colors <- brewer.pal(6, "RdBu") # can be changed
      
      
      annotation[, 3] <- factor(annotation[, 3])
      
      
      pheatmap(top20genesCounts, 
               color = heat_colors, 
               show_rownames = T,
               show_colnames = F,
               annotation = annotation,  
               fontsize = 10,
               cellheight = 10,
               cellwidth = 10,
               scale = "row", #plots z-scores instead of normalized count values after the clustering to improve visualization
               height = 50)
    })
    
    show("ready!")
    
    shinyjs::show("deseq_controls")
    
    
    
  })
  
  
  observeEvent(input$run_eda, {
    print("Botão EDA premido")
    req(vals$filtered_coldata, vals$filtered_cts)
    
    coldata <- vals$filtered_coldata
    cts <- vals$filtered_cts
    variable_of_interest <- 3
    treatment_col <- colnames(coldata)[variable_of_interest]
    coldata[[treatment_col]] <- factor(coldata[[treatment_col]])
    
    dds <- DESeqDataSetFromMatrix(cts, coldata, design = as.formula(paste0("~", treatment_col)))
    vsd <- vst(dds, blind = FALSE)
    rld <- rlog(dds, blind = FALSE)
    ntd <- normTransform(dds)
    treatment <- coldata[[treatment_col]]
    col.plot.data <- RColorBrewer::brewer.pal(length(unique(treatment)), "Set3")[as.integer(treatment)]
    
    # Boxplot - raw
    output$boxplot_raw <- renderPlot({
      boxplot(counts(dds),
              ylim = c(0, max(counts(dds)) * 1.2),
              cex.axis = 0.8,
              las = 3,
              col = col.plot.data,
              outline = TRUE,
              outpch = 21,
              outbg = col.plot.data,
              whisklty = 1,
              boxlty = 0,
              cex.lab = 1.2,
              main = "Raw Counts")
      legend("topright", legend = unique(treatment), fill = unique(col.plot.data), bty = "n", cex = 0.8)
    })
    
    # Boxplot - normTransform
    output$boxplot_normTransform <- renderPlot({
      boxplot(assay(ntd),
              ylim = c(0, max(assay(ntd)) * 1.2),
              cex.axis = 0.8,
              las = 3,
              col = col.plot.data,
              outline = TRUE,
              outpch = 21,
              outbg = col.plot.data,
              whisklty = 1,
              boxlty = 0,
              cex.lab = 1.2,
              main = "NormTransform")
      legend("topright", legend = unique(treatment), fill = unique(col.plot.data), bty = "n", cex = 0.8)
    })
    
    # Boxplot - vst
    output$boxplot_vst <- renderPlot({
      boxplot(assay(vsd),
              ylim = c(0, max(assay(vsd)) * 1.2),
              cex.axis = 0.8,
              las = 3,
              col = col.plot.data,
              outline = TRUE,
              outpch = 21,
              outbg = col.plot.data,
              whisklty = 1,
              boxlty = 0,
              cex.lab = 1.2,
              main = "VST")
      legend("topright", legend = unique(treatment), fill = unique(col.plot.data), bty = "n", cex = 0.8)
    })
    
    # Boxplot - rlog
    output$boxplot_rlog <- renderPlot({
      boxplot(assay(rld),
              ylim = c(0, max(assay(rld)) * 1.2),
              cex.axis = 0.8,
              las = 3,
              col = col.plot.data,
              outline = TRUE,
              outpch = 21,
              outbg = col.plot.data,
              whisklty = 1,
              boxlty = 0,
              cex.lab = 1.2,
              main = "rlog")
      legend("topright", legend = unique(treatment), fill = unique(col.plot.data), bty = "n", cex = 0.8)
    })
    
    # Heatmap
    output$distance_heatmap <- renderPlot({
      sampleDists <- dist(t(assay(vsd)), method = "euclidean")
      sampleDistMatrix <- as.matrix(sampleDists)
      rownames(sampleDistMatrix) <- colnames(vsd)
      colnames(sampleDistMatrix) <- colnames(vsd)
      pheatmap::pheatmap(
        sampleDistMatrix,
        annotation_row = as.data.frame(coldata[, c(2, 3)]),
        clustering_distance_rows = sampleDists,
        clustering_distance_cols = sampleDists,
        col = colorRampPalette(rev(brewer.pal(9, "Spectral")))(255),
        fontsize_row = 10
      )
    })
    
    output$distance_PCA <- renderPlot({
      plotPCA(vsd, intgroup = colnames(coldata)[3]) + # the intgroup used can be changed. se em vez de escolhemos 1, escolhermos o 3, vais ter um PCA de acordo com os treatment time point
        theme_minimal() +
        geom_vline(xintercept = 0) +
        geom_hline(yintercept = 0) +
        #scale_color_manual(values = col.plot) + # change - descomentar isto se selecionarmos 1
        #labs(color = "Treatment") +  # change - descomentar isto também
        geom_text(
          aes(label = colnames(vsd)),
          position = position_jitter(0, 4, seed = 5),
          size = 3.5
        ) +
        scale_x_discrete(expand = c(.4, 2))
      
    })
    
    output$distance_3DPCA <- renderPlotly({
      plot_pca_3d_data <- plotPCA3D(
        vsd,
        intgroup = colnames(coldata)[1], # group factor for coloring
        returnData = TRUE
      )
      
      plot_ly(
        plot_pca_3d_data,
        x = ~PC1,
        y = ~PC2,
        z = ~PC3,
        text = ~name,
        color = ~group,
        colors = col.plot.data
      ) %>%
        add_markers() %>%
        add_text(textposition = "top", showlegend = FALSE)
    })
    shinyjs::show("page_eda")
  })
  
  
  

}

shinyApp(ui, server)
