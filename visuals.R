#!/usr/bin/Rscript
library("viridis")
library("scales")

#### visuals for interactive tables:

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

get_count_table <- function(input_table) {
  table <- renderDT(input_table, 
                    options = list(paging = TRUE,    ## paginate the output
                                   pageLength = 5,  ## number of rows to output for each page
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






####################################################################
################ VISUALS ON EXPRESSION TAB ###############

get_dot_per_cell <- function(data_to_plot){
  plot <- ggplot(data_to_plot, aes(x=gene, y=ncounts, fill = celltype)) +  
    geom_point(aes(x = gene), shape = 21, position = position_jitterdodge(jitter.width = 0.1, jitter.height=0.12, dodge.width=0.9)) +
    ylab("Normalized expression, log2") + xlab("Gene names") +
    coord_flip() + #theme(legend.position="bottom") +
    facet_grid(celltype~ dataset + sample) + theme(panel.spacing = unit(.05, "lines"),
                                                   panel.border = element_rect(color = "black", fill = NA, size = 1), 
                                                   strip.background = element_rect(color = "black", size = 1.2),
                                                   strip.text.x=element_text(margin = margin(0.4,0.2,0.4,0.2, "cm")))
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
                                              strip.text.x=element_text(margin = margin(0.4,0.2,0.4,0.2, "cm")))
  return(plot)
}


get_density_per_cell <- function(data_to_plot){
  plot <- ggplot(data_to_plot, aes(ncounts, color = celltype, fill = celltype)) +  
    geom_density(alpha = .3)+
    ylim(c(0,5))+
    facet_grid(gene ~ dataset + sample) + theme(panel.spacing = unit(.05, "lines"),
                                                panel.border = element_rect(color = "black", fill = NA, size = 1), 
                                                strip.background = element_rect(color = "black", size = 1.2),
                                                strip.text.x=element_text(margin = margin(0.4,0.2,0.4,0.2, "cm")))
  
  return(plot) 
}

get_density_per_gene <- function(data_to_plot){
  plot <- ggplot(data_to_plot, aes(ncounts, color = gene, fill = gene)) +  
    geom_density(alpha = .3)+
    ylim(c(0,5))+
    facet_grid(celltype ~ dataset + sample) + theme(panel.spacing = unit(.05, "lines"),
                                                    panel.border = element_rect(color = "black", fill = NA, size = 1), 
                                                    strip.background = element_rect(color = "black", size = 1.2),
                                                    strip.text.x=element_text(margin = margin(0.4,0.2,0.4,0.2, "cm")))
  
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
                                                    strip.text.x=element_text(margin = margin(0.4,0.2,0.4,0.2, "cm")))
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
                                                strip.text.x=element_text(margin = margin(0.4,0.2,0.4,0.2, "cm")))
  return(plot)
}

####################################################################
################# VISUALS ON CELLCOUNT TAB #################3
### Visualize whole dataset composition with major cell types and cell counts per sample
get_box_dataset_cellcount <- function(data_to_plot){
  plot <- ggplot(data_to_plot, aes(x=celltype,y=n, fill=factor(dataset))) +  
    geom_boxplot() + 
    coord_flip() +
    labs(fill = "dataset") + 
    ylab("cell count") +
    geom_point(position=position_jitterdodge(),alpha=0.3) +
    facet_wrap(~dataset) + theme(panel.spacing = unit(.05, "lines"), axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
                                   panel.border = element_rect(color = "black", fill = NA, size = 1), 
                                   strip.background = element_rect(color = "black", size = 1.2),
                                   strip.text.x=element_text(margin = margin(0.2,0.2,0.2,0.2, "cm")))
  
  return(plot)
}


### Visualize whole dataset composition with major cell types and cell frequences per sample
### NB! add split on metadata. Combine with count tab?
get_box_dataset_frequency <- function(data_to_plot){
  plot <- ggplot(data_to_plot, aes(x=celltype,y=frequency, fill=factor(dataset))) +  
    geom_boxplot() + 
    coord_flip() +
    labs(fill = "dataset") + 
    ylab("cell frequency") +
    geom_point(position=position_jitterdodge(),alpha=0.3) +
    facet_wrap(~dataset) + theme(panel.spacing = unit(.05, "lines"), axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
                                 panel.border = element_rect(color = "black", fill = NA, size = 1), 
                                 strip.background = element_rect(color = "black", size = 1.2),
                                 strip.text.x=element_text(margin = margin(0.2,0.2,0.2,0.2, "cm")))
  
  return(plot)
}

### Visualize the  cell counts on gene-positive, gene-negative populations. per gene of interest, absolute counts in sample
### NB! add relative frequency (per cell subset counts)
### NB! add split on metadata. Combine with frequency tab?
get_bar_gene_cellcount <- function(data_to_plot){
  plot <- ggplot(data_to_plot, aes(fill=gene, y=count, x=celltype)) + 
    geom_bar(position="stack", stat="identity") +
    facet_wrap(~dataset+sample) +
    coord_flip() + 
    theme(panel.spacing = unit(.05, "lines"), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5), 
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          strip.background = element_rect(color = "black", size = 0.5),
          strip.text.x=element_text(margin = margin(0.3,0.2,0.3,0.2, "cm")))
  
  return(plot)
}

### Visualize the  cell counts on gene-positive, gene-negative populations. per gene of interest, use frequences
### NB! add relative frequency (per cell subset counts)
### NB! add split on metadata.
get_bar_gene_frequency <- function(data_to_plot){
  plot <- ggplot(data_to_plot, aes(fill=gene, y=frequency, x=celltype)) + 
    geom_bar(position="stack", stat="identity") +
    facet_wrap(~dataset+sample) +
    coord_flip() + 
    theme(panel.spacing = unit(.05, "lines"), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5), 
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          strip.background = element_rect(color = "black", size = 0.5),
          strip.text.x=element_text(margin = margin(0.3,0.2,0.3,0.2, "cm")))
  
  return(plot)
}

### Visualize unscaled, average gene expression per cell subset using heatmap. 
get_heatmap_expression_unscaled <- function(data_to_plot){
  plot <- ggplot(data_to_plot, aes(gene, celltype)) + 
    geom_tile(aes(fill= mean), colour = "black", size=1) +
    scale_fill_viridis_c(option = "D", direction = 1) +
    facet_wrap(~dataset+sample) +
    theme(panel.spacing = unit(.05, "lines"), axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5), 
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          strip.background = element_rect(color = "black", size = 0.5),
          strip.text.x=element_text(margin = margin(0.3,0.2,0.3,0.2, "cm"))) 
  
  return(plot)
}


### Visualize scaled, average gene expression per cell subset using heatmap. 
get_heatmap_expression_scaled <- function(data_to_plot){
  plot <- ggplot(data_to_plot, aes(gene, celltype)) + 
    geom_tile(aes(fill= mean_scaled), colour = "black", size=1) +
    scale_fill_viridis_c(option = "D", direction = 1) +
    facet_wrap(~dataset+sample) +
    theme(panel.spacing = unit(.05, "lines"), axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5), 
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          strip.background = element_rect(color = "black", size = 0.5),
          strip.text.x=element_text(margin = margin(0.3,0.2,0.3,0.2, "cm"))) 
  
  return(plot)
}


### Visualize sample composition on SampleCompotision tab. 
### Use data_to plot derived from  get_cell_count_per_dataset() function
get_sample_composition <- function(data_to_plot, selected_sample, selected_celltype){
  
  p1_data <- data_to_plot %>% select(dataset,sample,sample_name,total_cell) %>% distinct() %>% filter(sample %in% selected_sample)
  
  p1 <- ggplot(p1_data, aes(y=sample_name,x=total_cell,text = paste(
    "count:", ..x..,
    "<br>sample: ", ..y..)
  )) + 
    geom_bar(stat='identity') + xlab("Cell count") +
    theme(
      plot.title = element_text(color="black", size=12, face="bold", hjust=0.5),
      axis.title.y=element_blank(),
      axis.text.x = element_text(angle = 0, color="black"),
      axis.text.y = element_text(angle = 0, color="black")
    ) + scale_x_continuous(labels = label_scientific(digits = 1))
  
  ### define colour schema for celltypes in queried samples
  viridis_pal(option = "D")(length(unique(data_to_plot$celltype)))
  colours <- data.frame(sort(unique(data_to_plot$celltype)),viridis_pal(option = "D", direction = -1)(length(unique(data_to_plot$celltype))))
  colnames(colours) <- c("celltype","color")
  selection <- data_to_plot %>% select(dataset,sample,celltype, frequency) %>% filter(celltype %in% selected_celltype)
  colours_selection <- colours %>% right_join(selection) %>% select(-c(dataset,sample,frequency)) %>% unique()
  
  p2_data <- data_to_plot %>% select(dataset,sample, sample_name,celltype, frequency) %>% filter(sample %in% selected_sample) %>% mutate(celltype = factor(celltype, levels=colours$celltype))
  p3_data <- data_to_plot %>% select(dataset,sample,sample_name,total_cell) %>% filter(sample %in% selected_sample) %>% distinct() %>% left_join(selection) %>% mutate(celltype = factor(celltype, levels=colours_selection$celltype))
  
  p2 <- ggplot(p2_data, aes(x=frequency, y=sample_name, fill=celltype, text = paste(
    "frequency:", ..x..,
    "<br>celltype", ..fill..,
    "<br>sample: ", ..y..))) +
    geom_bar(position="stack", stat='identity', color="black") +
    xlab("Celltype frequencies") + 
    theme(
      plot.title = element_text(color="black", size=12, face="bold", hjust=0.5),
      legend.position='right',
      legend.title = element_text(colour="black", face="bold"),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()
    ) +
    scale_x_continuous(labels = comma) +
    scale_fill_manual(values = colours$color)
  
  p3<- ggplot(p3_data, aes(y=sample_name,x=frequency, fill=celltype, text = paste(
    "frequency:", ..x..,
    "<br>celltype", ..fill..,
    "<br>sample: ", ..y..))) + 
    xlab("Selected celltype frequency") + xlim(0, 1) +
    geom_bar(position="stack", stat='identity', color="black") + 
    theme(
      plot.title = element_text(color="black", size=12, face="bold", hjust=0.5),
      axis.text.x = element_text(angle = 0),
      legend.position='right',
      legend.title = element_text(colour="black", face="bold"),
      axis.title.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.text.y=element_blank()
    ) + 
    scale_fill_manual(values = colours_selection$color)
  
  my_list <- list("p1" = p1,"p2" = p2,"p3" = p3)
  
  return(my_list)
}





