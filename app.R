library(shiny)
library(plotly)
library(shinydashboard)
library(data.table)
library(tidyverse)
library(DT)

source("database.R")
source("visuals.R")
source("global.R")



#################################################################################
################################## DASHBOARD SETUP ########################
ui <- dashboardPage(
  dashboardHeader(title='HIMC single cell'), 
  dashboardSidebar(
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tab",
      menuItem("Metadata Dashboard", icon = icon("dashboard"), tabName = "dashboard"),
      menuItem("Gene Expression", icon = icon("bar-chart-o"),
               menuSubItem("Dot plot", tabName = "exprDot"),
               menuSubItem("Density plot", tabName = "exprDensity"),
               menuSubItem("Box plot", tabName = "exprBox"),
               menuSubItem("Heatmap plot", tabName = "exprHeatmap")
      ),
      menuItem("Cell Frequences", icon = icon("calculator"),
               menuSubItem("Dataset composition", tabName = "datasetComposition"),
               menuSubItem("Sample composition", tabName = "sampleComposition"),
               menuSubItem("Metadata integration", tabName = "barCell"),
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
    
############################## SELECTION FIELDS AND BUTTONS ###################
    
    conditionalPanel(condition = "input.tab != 'dashboard'",
                     selectizeInput(
                       'dataset', label='1. Select dataset:', choices = inputDatasets$dataset, multiple = TRUE
                     )),
    conditionalPanel(condition = "input.tab != 'dashboard' && input.tab != 'datasetComposition'",
                     selectizeInput(
                       'sample', label='2. Select sample:', choices = NULL, multiple = TRUE
                     ),
                     selectizeInput(
                       'celltype', label='3. Select cell type:', choices = NULL, multiple = TRUE
                     )),
    conditionalPanel(condition = "input.tab != 'dashboard' && input.tab != 'datasetComposition' && input.tab != 'sampleComposition'",
                     selectizeInput(
                       'gene', label='4. Select gene:', choices = NULL, multiple = TRUE
                     )),
    conditionalPanel(condition = "input.tab =='exprDot' || input.tab == 'exprBox' || input.tab == 'exprDensity'",
                     fluidRow( column(5, offset = 0, actionButton(inputId = "getExprPerCell", label = "By celltype")),
                               column(5, offset = 0, actionButton(inputId = "getExprPerGene", label = "By gene"))
                               )
                     ),
    conditionalPanel(condition = "input.tab == 'exprHeatmap'",
                     fluidRow(column(5, offset = 0, actionButton(inputId = "getExprHeatUnscaled", label = "Unscaled")),
                              column(5, offset = 0, actionButton(inputId = "getExprHeatScaled", label = "Scaled"))
                     )
    ),
    conditionalPanel(condition = "input.tab == 'barCell' || input.tab == 'datasetComposition'",
                     fluidRow( column(4, offset = 0, actionButton(inputId = "getDatasetCompCount", label = "By count")),
                               column(6, offset = 0, actionButton(inputId = "getDatasetCompFreq", label = "By frequency"))
                       )
    ),
    conditionalPanel(condition = "input.tab == 'sampleComposition'",
                     fluidRow( column(5, offset = 3, actionButton(inputId = "getSampleCompApply", label = "Apply"))
                     )
    )
    
  ),
  
##############################################################################
########################################## TAB SETUP ######################
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
                       selectInput('updateInfo', label='Update Dataset Info', choices = inputDatasets$dataset, multiple = TRUE)
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

############################################ EXPRESSION TABS ###########################
      tabItem("exprDot", 
              fluidRow(
                box(title = "Expression Dot Plot", status = "primary", width = 12, #height ="90vh", 
                    solidHeader = TRUE, collapsible = FALSE,collapsed = FALSE,
                    br(),
                    plotlyOutput('exprDot', height ="90vh")
                )
              )
      ),
      tabItem("exprDensity", 
              fluidRow(
                box(title = "Expression Density Plot", status = "primary", width = 12, #height ="90vh", 
                    solidHeader = TRUE, collapsible = FALSE,collapsed = FALSE,
                    br(),
                    plotlyOutput('exprDensity', height ="90vh")
                )
              )
      ), 
      tabItem("exprBox", 
              fluidRow(
                box(title = "Expression Box Plot", status = "primary", width = 12, #height ="90vh", 
                    solidHeader = TRUE, collapsible = FALSE,collapsed = FALSE,
                    br(),
                    plotlyOutput('exprBox', height ="90vh")
                )
              )
      ),
      tabItem("exprHeatmap", 
              fluidRow(
                box(title = "Heatmap Plot", status = "primary", width = 12, #height ="90vh", 
                    solidHeader = TRUE, collapsible = FALSE,collapsed = FALSE,
                    br(),
                    plotlyOutput('exprHeat', height ="90vh")
                )
              )
      ),

##################################### CELL COUNT TABS ###################################
      tabItem("datasetComposition", 
              fluidRow(
                box(title = "Dataset Composition", status = "primary", width = 12, #height ="90vh", 
                    solidHeader = TRUE, collapsible = FALSE,collapsed = FALSE,
                    br(),
                    plotlyOutput('countBox', height ="85vh"),
                    br(),
                    DTOutput('countTable',height = "40vh")
                )
              )
      ),
      tabItem("sampleComposition", 
              fluidRow(
                box(title = "Sample Composition", status = "primary", width = 12, #height ="90vh", 
                    solidHeader = TRUE, collapsible = FALSE,collapsed = FALSE,
                    br(),
                    plotlyOutput('sampleStats', height ="85vh"),
                    br()
                    
                    #DTOutput('countTable',height = "40vh")
                )
              )
      ),
      
      
      tabItem("barCell", 
              fluidRow(
                box(title = "Cell Fraction Bar Plot", status = "primary", width = 12, #height ="90vh", 
                    solidHeader = TRUE, collapsible = FALSE,collapsed = FALSE,
                    br(),
                    plotlyOutput('countPerGeneBar', height ="85vh"),
                    br(),
                    DTOutput('countPerGeneTable',height = "40vh")
                )
              )
      ),
      
################### DIFFERENTIAL EXPRESSION TAB, OTHER STATISTICS? #######################
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
  
######################################################################################
##############################3## DASHBOARD REACTIVITY TO SELECTION ################
  
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
  
  observeEvent(input$tab,{
    if (input$tab == "dashboard"){
      output$metaTable <- get_table(inputMetaData)
    }
  })
  
  
###############################################################################
############################ EXPRESSION REACTIVITY TO BUTTONS ################
  
  observeEvent(input$getExprPerCell,{
    if (is.null(input$sample) | is.null(input$celltype) | is.null(input$gene)) return()
    
    data_to_plot <- reactiveData()$expression
    
    if (input$tab == "exprDot"){
      v$dot <- get_dot_per_cell(data_to_plot)
    }
    
    if(input$tab == "exprDensity"){
      v$density <- get_density_per_cell(data_to_plot)
    }
    
    if(input$tab == "exprBox"){
      v$box <- get_box_per_cell(data_to_plot)
    }
  })
  
  observeEvent(input$getExprHeatUnscaled,{
    if (is.null(input$sample) | is.null(input$celltype) | is.null(input$gene)) return()
    
    data_to_plot <- reactiveData()$exprMean
    
    if (input$tab == "exprHeatmap"){
      v$heatmap <- get_heatmap_expression_unscaled(data_to_plot)
    }
  })
  
  observeEvent(input$getExprHeatScaled,{
    if (is.null(input$sample) | is.null(input$celltype) | is.null(input$gene)) return()
    
    data_to_plot <- reactiveData()$exprMean
    
    if (input$tab == "exprHeatmap"){
      v$heatmap <- get_heatmap_expression_scaled(data_to_plot)
    }
  })
  
  observeEvent(input$getExprPerGene,{
    if (is.null(input$sample) | is.null(input$celltype) | is.null(input$gene)) return()
    
    data_to_plot <- reactiveData()$expression
    
    if (input$tab == "exprDot"){
      v$dot <- get_dot_per_gene(data_to_plot)
    }
    
    if(input$tab == "exprDensity"){
      v$density <- get_density_per_gene(data_to_plot)
    }
    
    if(input$tab == "exprBox"){
      v$box <- get_box_per_gene(data_to_plot)
    }
  })
  
#######################3###########################################################
###################### CELL COUNT REACTIVITY TO BUTTONS ################
  
  observeEvent(input$getDatasetCompCount,{
    if(input$tab=="barCell" & !is.null(input$sample) & !is.null(input$celltype) & !is.null(input$gene)) {

      data_to_plot <- reactiveData()$countPerGene
      v$cellbar <- get_bar_gene_cellcount(data_to_plot)
      
      data_to_table <- data_to_plot %>% spread(celltype,count) %>% select(-c(frequency,relative_frequency))
      output$countPerGeneTable <- get_count_table(data_to_table)
    }
    
    if(input$tab == "datasetComposition" & length(input$dataset) > 0){
      
      data_to_plot <- reactiveCellCountData()
      v$datasetstats <- get_box_dataset_cellcount(data_to_plot)
      
      data_to_table <- data_to_plot %>% spread(celltype, n) %>% select(-frequency)
      output$countTable <- get_count_table(data_to_table)
    }
  })
  
  observeEvent(input$getDatasetCompFreq,{
    if(input$tab=="barCell" & !is.null(input$sample) & !is.null(input$celltype) & !is.null(input$gene)) {
      
      data_to_plot <- reactiveData()$countPerGene
      v$cellbar <- get_bar_gene_frequency(data_to_plot)
      
      data_to_table <- data_to_plot %>% spread(celltype,frequency) %>% select(-c(count,relative_frequency))
      output$countPerGeneTable <- get_count_table(data_to_table)
    }
    
    if(input$tab == "datasetComposition" & length(input$dataset) > 0){
      
      data_to_plot <- reactiveCellCountData()
      v$datasetstats <- get_box_dataset_frequency(data_to_plot)
      
      data_to_table <- data_to_plot %>% spread(celltype, frequency) %>% select(-n)
      output$countTable <- get_count_table(data_to_table)
    }
  })
  
  observeEvent(input$getSampleCompApply,{
    if(input$tab == "sampleComposition" & length(input$dataset) > 0 & !is.null(input$sample) & !is.null(input$celltype)) {
      
      data_to_plot <- reactiveCellCountData()
      v$samplestats <- get_sample_composition(data_to_plot,input$sample, input$celltype)
    }
  })
  
  
  
  observeEvent(input$barPerGene,{
    if (is.null(input$sample) | is.null(input$celltype) | is.null(input$gene)) return()
    
    if(input$tab=="barCell") {
      data_to_plot <- reactiveData()$countPerGene
      
      v$cellbar <- get_bar_gene_cellcount(data_to_plot)
      
      data_to_table <- data_to_plot %>% spread(celltype,count)
      
      output$countPerGeneTable <- get_count_table(data_to_table)
    }
  })
  
  
######################################################################################
############### GET DATA FROM REACTIVE SQL QUERIES ###################################
  
  v <- reactiveValues(dot = NULL, 
                      density = NULL, 
                      box = NULL, 
                      heatmap = NULL, 
                      datasetstats = NULL, 
                      samplestats = NULL, 
                      cellbar = NULL)
 
 
###### QUERY 1. 
  reactiveData <- reactive({
    if (is.null(input$sample) | is.null(input$celltype) | is.null(input$gene)) return()
    get_data(inputCellTypes, input$sample,input$celltype,input$gene, con_himc)
  })

  
##### QUERY 2. collect cell subset statistics (count, freq, total_count) per selected datasets for ALL celltypes.
  reactiveCellCountData <- reactive({
    if (length(input$dataset) > 0) {
      get_cell_count_per_dataset(inputCellTypes,input$dataset)
    }
  })
  
################################################################################
#################### UPDATE SELECTIONN FIELDS UPON USER REQUEST ################
  
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
  
####################################################################################
############################# REACTIVE VISUALS ###################################
  
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
  
  output$exprHeat <- renderPlotly({
    if (is.null(v$heatmap)) return()
    ggplotly(v$heatmap, width = cdata$output_pid_width, height = cdata$output_pid_height)
  })

  output$countBox <- renderPlotly({
    if (is.null(v$datasetstats)) return()
    ggplotly(v$datasetstats, width = cdata$output_pid_width, height = cdata$output_pid_height)
  })
  
  output$sampleStats <- renderPlotly({
    if (is.null(v$samplestats)) return()
    gp1 <- ggplotly(v$samplestats$p1, tooltip=c("text")) %>% add_annotations(
      text = "<b>Total cell count</b>",
      showlegend = FALSE,
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      yshift = 20,
      showarrow = FALSE,
      font = list(color = 'BLACK',
                  size = 15)
    )
    gp2 <- ggplotly(v$samplestats$p2, tooltip=c("text")) %>% add_annotations(
      text = "<b>Subset Composition</b>",
      showlegend = TRUE,
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      yshift = 20,
      showarrow = FALSE,
      font = list(color = 'BLACK',
                  size = 15)
    )
    gp3 <- ggplotly(v$samplestats$p3, tooltip=c("text")) %>% add_annotations(
      text = "<b>Selected subsets</b>",
      showlegend = FALSE,
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "top",
      yshift = 20,
      showarrow = FALSE,
      font = list(color = 'BLACK',
                  size = 15)
    )
    subplot(gp1, gp2, gp3, nrows=1, shareX = FALSE, shareY = FALSE, titleX = TRUE, width = cdata$output_pid_width, height = cdata$output_pid_height)
  })
  

  
  output$countPerGeneBar <- renderPlotly({
    if (is.null(v$cellbar)) return()
    ggplotly(v$cellbar, width = cdata$output_pid_width, height = cdata$output_pid_height)
  })
  
  
  
  
  
}

shinyApp(ui, server)
