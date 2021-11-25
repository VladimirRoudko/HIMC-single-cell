# HIMC-single-cell
Rshiny app connected to SQL database (Minerva)

### Database structure

conventions: $NAME - project-sample name. Each project is composed from samples. Each sample name has to be unique, non-repetative from samples in other projects

Each sample is represented by 3 files:

Annotation file: $NAME.annot ....
Expression file: $NAME.expr.gz ....
Metadata file: $NAME.metadata ....

Annotation file: 2 column file, first column - barcode, second column - celltype.

Expression file: multicolumn file, first column - barcode, other columns - genes, values - normalized expression values.

Metadata file: muticolumn file with first column - dataset, second column - sample name, others - meta features.

Metadata file is common for all samples from one dataset

All input files has to be comma separated csv format.

