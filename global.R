#!/usr/bin/Rscript

library(odbc)
library(RMariaDB)
library(pool)
library(dbplyr)
library(keyring)

con_himc <- dbPool(
  odbc::odbc(),
  drv = RMariaDB::MariaDB(), 
  username = key_get("sql", "key2"),
  password = key_get("sql", "key1"), 
  host = "data1.hpc.mssm.edu", 
  port = 3306,
  dbname = "himc_data1"
)

inputGeneNames <- get_input_gene_list(con_himc) %>% collect()
inputCellTypes <- get_input_cell_list(con_himc) %>% collect()
inputDatasets <- get_input_dataset_sample_list(con_himc) %>% collect()
inputMetaData <- get_input_metadata(con_himc) %>% as_tibble() %>% spread(term, value)

