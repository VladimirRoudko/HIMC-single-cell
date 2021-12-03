# HIMC-single-cell
Rshiny app connects to SQL database (Minerva)

### Database structure

Database consists from 5 files: "expression", "celltypes", "genes", "metadata" and "samples". 

Table "sample" has columns:
1) sampleID
2) dataset
3) sample 

Table "metadata" has columns:
1) sampleID
2) term
3) value

Table "genes" has columns:
1) sampleID
2) geneID
3) gene

Table "celltypes" has columns:
1) sampleID
2) barcodeID
3) celltype

Table "expression" has columns:
1) sampleID
2) barcodeID
3) geneID
4) ncounts

Each table is indexed by "...ID" columns. Each sample name is unique and associated with only one dataset. Metadata is per sample and can have as many different terms as possible. Single cell expression stores normalized values.

### Start Rhiny app:

Rshiny app deployment requires authentication to SQL server under network.




Dependances, R sessionInfo():

R version 4.1.0 (2021-05-18)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Big Sur 10.16

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib

locale:
en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
stats     
graphics  
grDevices 
utils     
datasets  
methods   
base     

other attached packages:
keyring_1.3.0        
dbplyr_2.1.1         
pool_0.1.6.9000      
RMariaDB_1.1.2       
odbc_1.3.2           
DT_0.18             
forcats_0.5.1        
stringr_1.4.0        
dplyr_1.0.7          
purrr_0.3.4          
readr_2.0.0          
tidyr_1.1.4         
tibble_3.1.6         
tidyverse_1.3.1      
data.table_1.14.2    
shinydashboard_0.7.2 
plotly_4.10.0.9000   
ggplot2_3.3.5       
shiny_1.6.0         





