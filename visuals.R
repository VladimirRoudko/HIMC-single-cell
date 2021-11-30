#!/usr/bin/Rscript


#### visuals for metadata as interactive table:

get_table <- function(input_table) {
  table <- renderDT(input_table, 
           options = list(paging = TRUE,    ## paginate the output
                          pageLength = 10,  ## number of rows to output for each page
                          scrollX = TRUE,   ## enable scrolling on X axis
                          scrollY = TRUE,   ## enable scrolling on Y axis
                          autoWidth = TRUE, ## use smart column width handling
                          server = FALSE,   ## use client-side processing
                          dom = 'Bfrtip',
                          columnDefs = list(list(targets = '_all', className = 'dt-center'))
           ),
           selection = 'single', ## enable selection of a single row
           filter = 'bottom',              ## include column filters at the bottom
           rownames = FALSE )
  
  return(table)
}

#### visuals of expression as dot plot:

get_dot_per_cell <- function(data_to_plot){
  plot <- ggplot(data_to_plot, aes(x=gene, y=ncounts, fill = celltype)) +  
    geom_point(aes(x = gene), shape = 21, position = position_jitterdodge(jitter.width = 0.1, jitter.height=0.12, dodge.width=0.9)) +
    ylab("Normalized expression, log2") + xlab("Gene names") +
    coord_flip() + #theme(legend.position="bottom") +
    facet_grid(celltype~ dataset + sample) + theme(panel.spacing = unit(.05, "lines"),
                                                   panel.border = element_rect(color = "black", fill = NA, size = 1), 
                                                   strip.background = element_rect(color = "black", size = 1.2),
                                                   strip.text.x=element_text(margin = margin(0.5,0.2,0.5,0.2, "cm")))
  return(plot)
}

get_dot_per_gene <- function(data_to_plot){
  plot <- ggplot(data_to_plot, aes(x=celltype, y=ncounts, fill = gene)) +  
    geom_point(aes(x = celltype), shape = 21, position = position_jitterdodge(jitter.width = 0.1, jitter.height=0.12, dodge.width=0.9)) +
    ylab("Normalized expression, log2") + xlab("Cell types") +
    coord_flip() + #theme(legend.position="bottom") +
    facet_grid(gene ~ dataset + sample) + theme(panel.spacing = unit(.05, "lines"),
                                              panel.border = element_rect(color = "black", fill = NA, size = 1), 
                                              strip.background = element_rect(color = "black", size = 1.2),
                                              strip.text.x=element_text(margin = margin(0.5,0.2,0.5,0.2, "cm")))
  return(plot)
}


get_density_per_cell <- function(data_to_plot){
  plot <- ggplot(data_to_plot, aes(ncounts, color = celltype, fill = celltype)) +  
    geom_density(alpha = .3)+
    ylim(c(0,5))+
    facet_grid(gene ~ dataset + sample) + theme(panel.spacing = unit(.05, "lines"),
                                                panel.border = element_rect(color = "black", fill = NA, size = 1), 
                                                strip.background = element_rect(color = "black", size = 1.2),
                                                strip.text.x=element_text(margin = margin(0.5,0.2,0.5,0.2, "cm")))
  
  return(plot) 
}

get_density_per_gene <- function(data_to_plot){
  plot <- ggplot(data_to_plot, aes(ncounts, color = gene, fill = gene)) +  
    geom_density(alpha = .3)+
    ylim(c(0,5))+
    facet_grid(celltype ~ dataset + sample) + theme(panel.spacing = unit(.05, "lines"),
                                                    panel.border = element_rect(color = "black", fill = NA, size = 1), 
                                                    strip.background = element_rect(color = "black", size = 1.2),
                                                    strip.text.x=element_text(margin = margin(0.5,0.2,0.5,0.2, "cm")))
  
  return(plot)
}

get_box_per_cell <- function(data_to_plot){
  plot <- ggplot(data_to_plot, aes(x=gene, y=ncounts, fill = celltype)) +  
    geom_boxplot(position=position_dodge(0.3)) +
    coord_flip() +
    #        ylim(c(0,5))+
    facet_grid(celltype ~ dataset + sample) + theme(panel.spacing = unit(.05, "lines"),
                                                    panel.border = element_rect(color = "black", fill = NA, size = 1), 
                                                    strip.background = element_rect(color = "black", size = 1.2),
                                                    strip.text.x=element_text(margin = margin(0.5,0.2,0.5,0.2, "cm")))
  return(plot)
  }

get_box_per_gene <- function(data_to_plot){
  plot <- ggplot(data_to_plot, aes(x=celltype, y=ncounts, fill = gene)) +  
    geom_boxplot(position=position_dodge(0.3)) +
    #  ylab("Normalized expression, log2") + xlab("Gene names") +
    coord_flip() +
    facet_grid(gene ~ dataset + sample) + theme(panel.spacing = unit(.05, "lines"),
                                                panel.border = element_rect(color = "black", fill = NA, size = 1), 
                                                strip.background = element_rect(color = "black", size = 1.2),
                                                strip.text.x=element_text(margin = margin(0.5,0.2,0.5,0.2, "cm")))
  return(plot)
}


