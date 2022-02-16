#!/usr/bin/Rscript

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

get_input_gene_list <- function(my_con_sql){
  tableList <- dbListTables(my_con_sql)
  myGeneTable <- tableList[startsWith(tableList, "gene")]
  mySampleTable <- tableList[startsWith(tableList, "sample")]

  listSample <- con_himc %>% tbl(mySampleTable)
  listGene <- con_himc %>% tbl(myGeneTable) %>% left_join(listSample) #%>% unique() #%>% collect()

  return(listGene)
}

get_input_cell_list <- function(my_con_sql){
  tableList <- dbListTables(my_con_sql)
  myCellTable <- tableList[startsWith(tableList, "celltype")]
  mySampleTable <- tableList[startsWith(tableList, "sample")]
  
  listSample <- con_himc %>% tbl(mySampleTable)
  listCell <- con_himc %>% tbl(myCellTable) %>% left_join(listSample) #%>% unique() #%>% collect()

  return(listCell)
}

get_input_dataset_sample_list <- function(my_con_sql){
  tableList <- dbListTables(my_con_sql)
  mySampleTable <- tableList[startsWith(tableList, "sample")]
  listSample <- con_himc %>% tbl(mySampleTable)
  return(listSample)
}

get_input_metadata <- function(my_con_sql){
  tableList <- dbListTables(my_con_sql)
  myMetaTable <- tableList[startsWith(tableList, "meta")]
  mySampleTable <- tableList[startsWith(tableList, "sample")]
  
  listSample <- con_himc %>% tbl(mySampleTable)
  listMeta <- con_himc %>% tbl(myMetaTable) %>% left_join(listSample)  #%>% unique() %>% collect()

  return(listMeta)
}

get_data <- function(inputCellTypes, samples,celltypes,genes, my_con_sql) {
  
  tableList <- dbListTables(my_con_sql)
  myMetaTable <- tableList[startsWith(tableList, "meta")]
  mySampleTable <- tableList[startsWith(tableList, "sample")]
  myCellTable <- tableList[startsWith(tableList, "celltype")]
  myGeneTable <- tableList[startsWith(tableList, "gene")]
  myExpressionTable <- tableList[startsWith(tableList, "expression")]
  
  selected_sampleid <- con_himc %>% 
    tbl(mySampleTable) %>% 
    filter(sample %in% samples) %>% 
    pull(sampleID) 
  
  selected_samples <- con_himc %>% 
    tbl(mySampleTable) %>% 
    filter(sample %in% samples)

  selected_barcodids_celltypes <- con_himc %>% 
    tbl(myCellTable) %>% 
    filter(celltype %in% celltypes) %>% 
    filter(sampleID %in% selected_sampleid) 

  
  selected_geneid_genes <- con_himc %>% 
    tbl(myGeneTable) %>% 
    filter(gene %in% genes) %>% 
    filter(sampleID %in% selected_sampleid)
  
  selected <- con_himc %>% tbl(myExpressionTable) %>%
    filter(sampleID %in% selected_sampleid) %>%
    left_join(selected_samples) %>%
    right_join(selected_barcodids_celltypes) %>% 
    right_join(selected_geneid_genes) %>%
    select(-geneID) %>% 
    collect() %>% drop_na()
  
  expression_sample <- selected %>% group_by(dataset,sample,celltype,gene) %>% slice_sample(n=200) %>% ungroup()
  
  positive_counts <- selected %>% filter(ncounts > 0) %>% 
    group_by(sampleID,celltype,barcodeID) %>% 
    count(gene) %>% 
    pivot_wider(names_from = gene, values_from = n) %>% 
    group_by(across(-barcodeID)) %>% 
    summarise(N=n()) %>% ungroup() %>% rownames_to_column("ID") %>%
    pivot_longer(!c(ID,sampleID,celltype,N),names_to = "variable",values_to="value") %>% drop_na() %>% 
    mutate(value = variable[as.logical(value)]) %>% 
    spread(variable, value) %>% replace(is.na(.), "zorg") %>%
    pivot_wider(names_from = !c(ID,sampleID,celltype,N),values_from = N) %>%
    rename_with(~str_remove_all(., "_zorg")) %>% rename_with(~str_remove_all(., 'zorg_')) %>%
    pivot_longer(!c(ID,sampleID,celltype),names_to = "variable",values_to="count") %>% drop_na() %>% select(-ID)
  
  selected <- NULL
  
  positive_counts_total <- positive_counts %>% group_by(sampleID,celltype) %>% summarise(N=sum(count)) %>% ungroup()
  
  counts_total <- inputCellTypes %>% filter(sample %in% samples) %>% filter(celltype %in% celltypes) %>%
   group_by(sampleID) %>% count(celltype) %>% ungroup() 
  
  counts_per_gene <- left_join(positive_counts_total,counts_total) %>% mutate(negative = n - N) %>% select(-c(N,n)) %>% 
    pivot_longer(!c(sampleID,celltype),names_to = "variable",values_to="count") %>%
    rbind(positive_counts) %>% arrange(sampleID,celltype) %>% rename(gene=variable) %>% left_join(as.data.frame(selected_samples)) %>% 
    select(-sampleID) %>% relocate(gene, .after = sample) %>% replace(is.na(.), 0) %>% group_by(dataset,sample) %>% mutate(frequency = count / sum(count)) %>% ungroup() %>%
    group_by(dataset,sample,celltype) %>% mutate(relative_frequency = count / sum(count)) %>% ungroup()
  
  expression_averages <- expression_sample %>% 
    group_by(sampleID,celltype,gene) %>% 
    summarize(mean = mean(ncounts, na.rm=TRUE)) %>% 
    ungroup() %>% 
    complete(sampleID,celltype,gene) %>% 
    left_join(as.data.frame(selected_samples)) %>% 
    replace(is.na(.), 0) %>% group_by(dataset,sample) %>% 
    mutate(mean_scaled = scale_this(mean)) %>% replace(is.na(.), -2) %>% 
    mutate(mean_scaled = replace(mean_scaled, mean_scaled > 2, 2)) %>% ungroup()
  
  my_list <- list("expression" = expression_sample,"countPerGene" = counts_per_gene,"exprMean" = expression_averages)
  
  return(my_list)
}


get_cell_count_per_dataset <- function(inputCellTypes,selected_datasets){
  cellcounts <- inputCellTypes %>% 
    filter(dataset %in% selected_datasets) %>% 
    count(dataset,sample,celltype) %>%
    group_by(dataset,sample) %>% 
    mutate(frequency = n / sum(n)) %>%
    mutate(total_cell = sum(n)) %>%
    ungroup() %>% 
    arrange(desc(total_cell))  %>% 
    mutate(sample_name = factor(sample, 
                                levels = sample,
                                labels = paste0(dataset, "\n", sample)))
  
  return(cellcounts)
}









