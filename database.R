#!/usr/bin/Rscript

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

get_data <- function(samples,celltypes,genes, my_con_sql) {
  
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
    right_join(selected_geneid_genes) %>%
    right_join(selected_barcodids_celltypes) %>% 
    left_join(selected_samples) %>%
    select(-c(barcodeID,geneID)) %>% 
    collect() %>%
    group_by(celltype,gene) %>% 
    slice_sample(n=200)
  
  return(selected)
}




