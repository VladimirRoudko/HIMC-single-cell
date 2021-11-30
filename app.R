library(shiny)
library(plotly)
library(shinydashboard)
library(data.table)
library(tidyverse)
library(DT)

source("database.R")
source("visuals.R")
source("global.R")


ui <- dashboardPage(
  dashboardHeader(title='HIMC single cell'), 
  dashboardSidebar(
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tab",
      menuItem("Metadata Dashboard", icon = icon("dashboard"), tabName = "dashboard"),
      menuItem("Gene Expression", icon = icon("bar-chart-o"),
               menuSubItem("Dot", tabName = "exprDot"),
               menuSubItem("Density", tabName = "exprDensity"),
               menuSubItem("Box", tabName = "exprBox"),
               menuSubItem("Heatmap averages", tabName = "heatmap")
      ),
      menuItem("Cell Counts", icon = icon("calculator"),
               menuSubItem("Dataset overview", tabName = "boxCell"),
               menuSubItem("Selection subsets", tabName = "barCell"),
               menuSubItem("Metadata subsets", tabName = "metaCell")
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
    conditionalPanel(condition = "input.tab != 'dashboard'",
                     selectizeInput(
                       'dataset', label='1. Select dataset:', choices = inputDatasets$dataset, multiple = TRUE
                     ),
                     selectizeInput(
                       'sample', label='2. Select sample:', choices = NULL, multiple = TRUE
                     ),
                     selectizeInput(
                       'celltype', label='3. Select cell type:', choices = NULL, multiple = TRUE
                     ),
                     selectizeInput(
                       'gene', label='4. Select gene:', choices = NULL, multiple = TRUE
                     ))
  ),
  
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(
                infoBoxOutput("datasetInfo",width=3),
                infoBoxOutput("cellInfo",width=3),
                infoBoxOutput("celltypeInfo",width=3),
                infoBoxOutput("geneInfo",width=3)
              ),
              fluidRow(
                column(12,align="center",
                       selectInput('updateInfo', label='Update Dataset Info', choices = inputDatasets %>% select(dataset), multiple = TRUE)
                )
              ),
              fluidRow(
                box(title = "Sample Metadata", status = "primary", width = 12, #height ="90vh", 
                    solidHeader = TRUE, collapsible = FALSE,collapsed = FALSE,
                    br(),
                    DTOutput('metaTable',height = "50vh") #,height = "85vh"
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
      tabItem("heatmap", 
              fluidRow(
                box(title = "Heatmap Plot", status = "primary", width = 12, #height ="90vh", 
                    solidHeader = TRUE, collapsible = FALSE,collapsed = FALSE,
                    # plotlyOutput('dot', height ="90vh")
                    "Here will be plot"
                )
              )
      ),
      tabItem("boxCell", 
              fluidRow(
                box(title = "Cell Fraction Box Plot", status = "primary", width = 12, #height ="90vh", 
                    solidHeader = TRUE, collapsible = FALSE,collapsed = FALSE,
#                    div(style="display:inline-block;width:20%;text-align: center;",
#                        actionButton(inputId = "box",label = "Group by cell type")),
#                    div(style="display:inline-block;width:30%;text-align: center;",
#                        actionButton(inputId = "boxPerGene",label = "Group by gene name")),
                    br(),br(),
                    plotlyOutput('countBox', height ="85vh"),
                    br(),br(),
                    DTOutput('countTable',height = "40vh") #,height = "85vh"
                )
              )
      ),
      tabItem("barCell", 
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
  
  ## exit SQL connection and stop App upon session termination:
  
  session$onSessionEnded(function() {
    cat("closing connection and exiting app")
    poolClose(con_himc)
    stopApp()
  })
  
  cdata <- session$clientData
  
  #### info Dashboard part
  
  updateDatasetInfo <- reactive({
    if (length(input$updateInfo) == 0) {
      tags$p(paste(length(inputDatasets %>% select(dataset) %>% unique() %>% as_vector()),length(inputDatasets %>% select(sample) %>% as_vector()),sep=" / "), style = "font-size: 130%;")
    } else {
      tags$p(paste(length(inputDatasets %>% select(dataset) %>% unique() %>% filter(dataset %in% input$updateInfo) %>% as_vector()),length(inputDatasets %>% filter(dataset %in% input$updateInfo) %>% select(sample)  %>% as_vector()),sep=" / "), style = "font-size: 130%;")
    }
  })
  
  updateCellInfo <- reactive({
    if (length(input$updateInfo) == 0) {
      tags$p(length(inputCellTypes %>%  select(barcodeID) %>% as_vector()), style = "font-size: 130%;")
    } else {
      tags$p(length(inputCellTypes %>% filter(dataset %in% input$updateInfo) %>% select(barcodeID) %>% as_vector()), style = "font-size: 130%;")
    }
  })
  
  updateCelltypeInfo <- reactive({
    if (length(input$updateInfo) == 0) {
      tags$p(length(inputCellTypes %>%  select(celltype) %>% unique() %>% as_vector()), style = "font-size: 130%;")
    } else {
      tags$p(length(inputCellTypes %>% filter(dataset %in% input$updateInfo) %>% select(celltype) %>% unique() %>% as_vector()), style = "font-size: 130%;")
    }
  })
  
  updateGeneInfo <- reactive({
    if (length(input$updateInfo) == 0) {
      tags$p(length(inputGeneNames %>% select(gene) %>% unique() %>% as_vector()), style = "font-size: 130%;")
    } else {
      tags$p(length(inputGeneNames %>% filter(dataset %in% input$updateInfo) %>% select(gene) %>% unique() %>% as_vector()), style = "font-size: 130%;")
    }
  })
  
  output$datasetInfo <- renderInfoBox({
    infoBox(title = tags$p("Dataset / Sample",style = "font-size: 130%;"),
            value = updateDatasetInfo(), 
            icon = icon("vials"), 
            width=3, 
            fill=TRUE,
            color="red"
    )
  })
  
  output$cellInfo <- renderInfoBox({
    infoBox(title = tags$p("Cell",style = "font-size: 130%;"),
            value = updateCellInfo(), 
            icon = icon("calculator"), 
            width=3, 
            fill=TRUE,
            color="yellow"
    )
  })
  
  output$celltypeInfo <- renderInfoBox({
    infoBox(title = tags$p("Celltype",style = "font-size: 130%;"),
            value = updateCelltypeInfo(), 
            icon = icon("project-diagram"), 
            width=3, 
            fill=TRUE,
            color="green"
    )
  })
  
  output$geneInfo <- renderInfoBox({
    infoBox(title = tags$p("Gene",style = "font-size: 130%;"),
            value = updateGeneInfo(), 
            icon = icon("dna"), 
            width=3, 
            fill=TRUE,
            color="blue"
    )
  })
  
  observeEvent(input$tab, {
    if (input$tab == "dashboard"){
      output$metaTable <- get_table(inputMetaData)
    }
  })
  
  
  toListen <- reactive({
    list(input$tab,input$dataset)
  })
  
  observeEvent(toListen(),{
    if(input$tab=="boxCell" & length(input$dataset) > 0){
      data_to_plot <- reactiveCellCountData()
      v$cellbox <- get_box_dataset_cellcount(data_to_plot)
    }
  })
  
  
  
  
  
  ### Expression Data part:
  
  v <- reactiveValues(dot = NULL, density = NULL, box = NULL, cellbox = NULL)
  
  reactiveData <- reactive({
    if (is.null(input$sample) | is.null(input$celltype) | is.null(input$gene)) return()
    get_data(input$sample,input$celltype,input$gene, con_himc)
  })
  
  reactiveCellCountData <- reactive({
    if (length(input$dataset) > 0) {
      get_cell_count_per_dataset(inputCellTypes,input$dataset)
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
      inputDatasets %>% filter(dataset %in% !!input$dataset) %>% select(sample) %>% collect() %>% arrange(sample)
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

  output$countBox <- renderPlotly({
    if (is.null(v$cellbox)) return()
    ggplotly(v$cellbox, width = cdata$output_pid_width, height = cdata$output_pid_height)
  })
  
  output$countBar <- renderPlotly({
    if (is.null(v$cellbar)) return()
    ggplotly(v$cellbar, width = cdata$output_pid_width, height = cdata$output_pid_height)
  })
  
}




shinyApp(ui, server)
