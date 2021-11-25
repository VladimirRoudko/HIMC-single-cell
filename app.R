library(shiny)
library(plotly)
library(shinydashboard)
library(data.table)
library(tidyverse)
library(DT)
library(odbc)
library(RMariaDB)
library(pool)
library(dbplyr)

source("database.R")
source("visuals.R")

#setwd("~/Documents/data/research/mount.sinai/HIMC/database/development")

psswd <- .rs.askForPassword("Database Password:")  
uid <- .rs.askForPassword("Database userID:") 

con_himc <- dbPool(
  odbc::odbc(),
  drv = RMariaDB::MariaDB(), 
  username = uid,
  password = psswd, 
  host = "data1.hpc.mssm.edu", 
  port = 3306,
  dbname = "himc_data1"
)

#### add column with sample of origin
database <- dbListTables(con_himc)
inputGeneNames <- get_input_gene_list(con_himc)
inputCellTypes <- get_input_cell_list(con_himc)
inputDatasetsSamples <- get_input_dataset_sample_list(con_himc)
inputListDatasets <- inputDatasetsSamples %>% collect() %>% select(dataset) %>% unique()
inputMetaData <- get_input_metadata(con_himc)
inputMetaData <- as_tibble(inputMetaData) %>% spread(term, value)


ui <- dashboardPage(
  dashboardHeader(title='HIMC single cell'), 
  dashboardSidebar(
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tab",
      menuItem("Dashboard", icon = icon("dashboard"), tabName = "dashboard"),
      menuItem("Sample Metadata", icon = icon("clipboard-check"), tabName = "metadata"),
      menuItem("Gene Expression", icon = icon("bar-chart-o"),
               menuSubItem("Dot Plot", tabName = "exprDot"),
               menuSubItem("Density Plot", tabName = "exprDensity"),
               menuSubItem("Box Plot", tabName = "exprBox"),
               menuSubItem("Pairwase Scatter Plot", tabName = "pairwase")
      ),
      menuItem("Cell Fraction", icon = icon("calculator"),
               menuSubItem("Box Plot", tabName = "boxFraction"),
               menuSubItem("Stacked Bar Plot", tabName = "barFraction")
      ),
      menuItem("DE Genes", icon = icon("dna"),
               menuSubItem("Box Plot", tabName = "boxDE"),
               menuSubItem("Table", tabName = "tableDE")
      ),
      menuItem("Functional GSEA", icon = icon("project-diagram"),
               menuSubItem("Box Plot", tabName = "boxGSEA"),
               menuSubItem("Table", tabName = "tableGSEA")
      ),
      menuItem("Survival analysis", icon = icon("project-diagram"), tabName="plotSurvival")
    ),
    br(),
    selectizeInput(
      'dataset', label='1. Select dataset:', choices = inputListDatasets$dataset, multiple = TRUE
    ),
    selectizeInput(
      'sample', label='2. Select sample:', choices = NULL, multiple = TRUE
    ),
    selectizeInput(
      'celltype', label='3. Select cell type:', choices = NULL, multiple = TRUE
    ),
    selectizeInput(
      'gene', label='4. Select gene:', choices = NULL, multiple = TRUE
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem("dashboard", "Dashboard tab content"),
      tabItem("metadata", 
              fluidRow(
                box(title = "Sample Metadata", status = "primary", width = 12, #height ="90vh", 
                    solidHeader = TRUE, collapsible = FALSE,collapsed = FALSE,
                    br(),
                    br(),
                    DTOutput('metaTable',height = "90vh") #,height = "85vh"
                )
              )
      ),
      tabItem("exprDot", 
              fluidRow(
                box(title = "Expression Dot Plot", status = "primary", width = 12, #height ="90vh", 
                    solidHeader = TRUE, collapsible = FALSE,collapsed = FALSE,
                    div(style="display:inline-block;width:20%;text-align: center;",
                        actionButton(inputId = "dotPerCell",label = "Group by cell type")),
                    div(style="display:inline-block;width:30%;text-align: center;",
                        actionButton(inputId = "dotPerGene",label = "Group by gene name")),
                    br(),br(),
                    plotlyOutput('exprDot', height ="90vh")
                )
              )
      ),
      tabItem("exprDensity", 
              fluidRow(
                box(title = "Expression Density Plot", status = "primary", width = 12, #height ="90vh", 
                    solidHeader = TRUE, collapsible = FALSE,collapsed = FALSE,
                    div(style="display:inline-block;width:20%;text-align: center;",
                        actionButton(inputId = "densityPerCell",label = "Group by cell type")),
                    div(style="display:inline-block;width:30%;text-align: center;",
                        actionButton(inputId = "densityPerGene",label = "Group by gene name")),
                    br(),br(),
                    plotlyOutput('exprDensity', height ="90vh")
                )
              )
      ), 
      tabItem("exprBox", 
              fluidRow(
                box(title = "Expression Box Plot", status = "primary", width = 12, #height ="90vh", 
                    solidHeader = TRUE, collapsible = FALSE,collapsed = FALSE,
                    div(style="display:inline-block;width:20%;text-align: center;",
                        actionButton(inputId = "boxPerCell",label = "Group by cell type")),
                    div(style="display:inline-block;width:30%;text-align: center;",
                        actionButton(inputId = "boxPerGene",label = "Group by gene name")),
                    br(),br(),
                    plotlyOutput('exprBox', height ="90vh")
                )
              )
      ),
      tabItem("pairwase", 
              fluidRow(
                box(title = "Pairwase Scatter Plot", status = "primary", width = 12, #height ="90vh", 
                    solidHeader = TRUE, collapsible = FALSE,collapsed = FALSE,
                    # plotlyOutput('dot', height ="90vh")
                    "Here will be plot"
                )
              )
      ),
      tabItem("boxFraction", 
              fluidRow(
                box(title = "Cell Fraction Box Plot", status = "primary", width = 12, #height ="90vh", 
                    solidHeader = TRUE, collapsible = FALSE,collapsed = FALSE,
                    #plotlyOutput('dot', height ="85vh")
                    "Here will be plot"
                )
              )
      ),
      tabItem("barFraction", 
              fluidRow(
                box(title = "Cell Fraction Bar Plot", status = "primary", width = 12, #height ="90vh", 
                    solidHeader = TRUE, collapsible = FALSE,collapsed = FALSE,
                    #plotlyOutput('dot', height ="85vh")
                    "Here will be plot"
                )
              )
      ),
      tabItem("boxDE", 
              fluidRow(
                box(title = "Differentially Expressed Genes Box Plot", status = "primary", width = 12, #height ="90vh", 
                    solidHeader = TRUE, collapsible = FALSE,collapsed = FALSE,
                    #plotlyOutput('dot', height ="85vh")
                    "Here will be plot"
                )
              )
      ),
      tabItem("tableDE", 
              fluidRow(
                box(title = "Differentially Expressed Genes Table", status = "primary", width = 12, #height ="90vh", 
                    solidHeader = TRUE, collapsible = FALSE,collapsed = FALSE,
                    #plotlyOutput('dot', height ="85vh")
                    "Here will be plot"
                )
              )
      ),
      tabItem("boxGSEA", 
              fluidRow(
                box(title = "Functional Network Enrichment Box Plot", status = "primary", width = 12, #height ="90vh", 
                    solidHeader = TRUE, collapsible = FALSE,collapsed = FALSE,
                    #plotlyOutput('dot', height ="85vh")
                    "Here will be plot"
                )
              )
      ),
      tabItem("tableGSEA", 
              fluidRow(
                box(title = "Functional Network Enrichment Table", status = "primary", width = 12, #height ="90vh", 
                    solidHeader = TRUE, collapsible = FALSE,collapsed = FALSE,
                    #plotlyOutput('dot', height ="85vh")
                    "Here will be plot"
                )
              )
      )
    )
  )
)


server <- function(input, output, session) {
  output$res <- renderText({
    paste("You've selected:", input$tabs)
  })
  
  cdata <- session$clientData
  
  v <- reactiveValues(dot = NULL, density = NULL, box = NULL)
  
  reactiveData <- reactive({
    if (is.null(input$sample) | is.null(input$celltype) | is.null(input$gene)) return()
    get_data(input$sample,input$celltype,input$gene, con_himc)
  })
  
  observeEvent(input$tab, {
    if (input$tab == "metadata"){
      output$metaTable <- get_table(inputMetaData)
    }
  })
  
  
  observeEvent(input$dotPerCell,{ 
    if (is.null(input$sample) | is.null(input$celltype) | is.null(input$gene)) return()
    
    data_to_plot <- reactiveData()
    
    if (input$tab == "exprDot"){
      v$dot <- get_dot_per_cell(data_to_plot)
    }
  })
  
  observeEvent(input$dotPerGene,{ 
    if (is.null(input$sample) | is.null(input$celltype) | is.null(input$gene)) return()
    
    data_to_plot <- reactiveData()
    
    if (input$tab == "exprDot"){
      v$dot <- get_dot_per_gene(data_to_plot)
    }
  })
  
  observeEvent(input$densityPerCell,{ 
    ### add option to set Y-limit for the density plot
    
    if (is.null(input$sample) | is.null(input$celltype) | is.null(input$gene)) return()
    
    data_to_plot <- reactiveData()
    
    if (input$tab == "exprDensity"){
      v$density <- get_density_per_cell(data_to_plot)
    }
  })
  
  observeEvent(input$densityPerGene,{ 
    ### add option to set Y-limit for the density plot
    
    if (is.null(input$sample) | is.null(input$celltype) | is.null(input$gene)) return()
    
    data_to_plot <- reactiveData()
    
    if (input$tab == "exprDensity"){
      v$density <- get_density_per_gene(data_to_plot)
    }
  })
  
  observeEvent(input$boxPerCell,{ 
    ### add option to set Y-limit for the box plot
    
    if (is.null(input$sample) | is.null(input$celltype) | is.null(input$gene)) return()
    
    data_to_plot <- reactiveData()
    
    if (input$tab == "exprBox"){
      v$box <- get_box_per_cell(data_to_plot)
    }
  })
  
  observeEvent(input$boxPerGene,{ 
    ### add option to set Y-limit for the box plot
    
    if (is.null(input$sample) | is.null(input$celltype) | is.null(input$gene)) return()
    
    data_to_plot <- reactiveData()
    
    if (input$tab == "exprBox"){
      v$box <- get_box_per_gene(data_to_plot)
    }
  })
  
  ## NB: when working with SQL queries force local evaluation for the reactive variables: add !! before the variable.
  
  outSample <- reactive({
    if (length(input$dataset > 0)) {
      inputDatasetsSamples %>% filter(dataset %in% !!input$dataset) %>% select(sample) %>% collect() %>% arrange(sample)
    }
  })
  
  outCell <- reactive({
    if (length(input$sample > 0)) {
      filter(inputCellTypes, sample %in% !!input$sample) %>% select(celltype) %>% collect() %>% unique()  %>% arrange(celltype)
    }
  })
  
  outGene <- reactive({
    if (length(input$sample > 0)) {
      filter(inputGeneNames, sample %in% !!input$sample) %>% select(gene) %>% collect() %>% unique() %>% arrange(gene)
    }
  })
  
  observe({
    updateSelectizeInput(session, "sample",
                         choices = outSample()$sample, server = TRUE
    )
  })
  observe({
    updateSelectizeInput(session, "celltype",
                         choices = outCell()$celltype, server = TRUE
    )
  })
  observe({
    updateSelectizeInput(session, "gene",
                         choices = outGene()$gene, server = TRUE
    )
  })
  
  output$exprDot <- renderPlotly({
    if (is.null(v$dot)) return()
    ggplotly(v$dot, width = cdata$output_pid_width, height = cdata$output_pid_height)
  })
  
  output$exprDensity <- renderPlotly({
    if (is.null(v$density)) return()
    ggplotly(v$density, width = cdata$output_pid_width, height = cdata$output_pid_height)
  })
  
  output$exprBox <- renderPlotly({
    if (is.null(v$box)) return()
    ggplotly(v$box, width = cdata$output_pid_width, height = cdata$output_pid_height)
  })
}

shinyApp(ui, server)