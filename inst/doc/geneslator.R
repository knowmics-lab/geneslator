## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", warning = FALSE,
message = FALSE)

## ----style, echo=FALSE, results='asis'----------------------------------------
BiocStyle::markdown()

## ----installation, eval=FALSE-------------------------------------------------
# # Install package devtools, if missing
# if (!requireNamespace("devtools", quietly = TRUE)) {
#     install.packages("devtools")
# }
# 
# # Install the development version of geneslator from GitHub
# devtools::install_github("knowmics-lab/geneslator", build_vignettes = TRUE)

## ----load-package-------------------------------------------------------------
library(geneslator)

## ----available-organisms, eval=TRUE-------------------------------------------
# List organisms annotated in geneslator
availableDatabases()

## ----available-organisms-older, eval=TRUE-------------------------------------
# List organisms annotated in geneslator (release December 2025)
availableDatabases(release.version = "2025.12")

## ----available-versions, eval=TRUE--------------------------------------------
# Import human db again. Now cache data will be used to import db
availableVersions()

## ----geneslator-db, eval=TRUE-------------------------------------------------
# Import human annotation db (after downloading it from remote repository)
GeneslatorDb("Homo sapiens")
# Info about the imported human annotation database object
org.Hsapiens.db
# Import mouse annotation database using its Taxonomy ID
GeneslatorDb("10090")
# Info about the imported human annotation database object
org.Mmusculus.db

## ----geneslator-db-cache, eval=TRUE-------------------------------------------
# Import human db again. Now cache data will be used to import db
GeneslatorDb("Homo sapiens")

## ----geneslator-db-older, eval=TRUE-------------------------------------------
# Import yeast annotation db from release 2025.12 (December 2025)
GeneslatorDb("Saccharomyces cerevisiae",release.version = "2025.12")
# Info about the imported human annotation database object
org.Scerevisiae.db

## ----keytypes, eval=TRUE------------------------------------------------------
# Get all columns that can be used as keys in mouse annotation db
geneslator::keytypes(org.Mmusculus.db)

## ----columns, eval=TRUE-------------------------------------------------------
# Get all available types of output values in mouse annotation db
geneslator::columns(org.Mmusculus.db)

## ----keys, eval=TRUE----------------------------------------------------------
# Get the first 10 Entrez IDs in mouse annotation db
head(geneslator::keys(org.Mmusculus.db, keytype = "ENTREZID"), 10)

## ----select-example, eval=TRUE------------------------------------------------
# Map NCBI Gene IDs to gene symbols and Ensembl IDs in Human
genes <- c("1", "2", "9")
result <- geneslator::select(org.Hsapiens.db, keys = genes,
            columns = c("SYMBOL", "ENSEMBL"), keytype = "ENTREZID")
result

## ----mapids-example, eval=TRUE------------------------------------------------
# Convert gene symbols to ENTREZ IDs (first match only)
genes <- c("TP53", "BRCA1", "EGFR")
entrez_ids <- geneslator::mapIds(org.Hsapiens.db, keys = genes, 
            column = "ENTREZID", keytype = "SYMBOL")
entrez_ids

## ----mapids-multi, eval=TRUE--------------------------------------------------
# Get all possible mappings as a list
entrez_list <- geneslator::mapIds(org.Hsapiens.db, keys = genes,
            column = "ENTREZID", keytype = "SYMBOL", multiVals = "list")
entrez_list

## ----aliases, eval=TRUE-------------------------------------------------------
# Map gene symbols to their NCBI gene ids, querying also the ALIAS column 
# if needed
result <- geneslator::select(org.Hsapiens.db, keys = c("BRCAI","PTEN"),
            columns = "ENTREZID", keytype = "SYMBOL")
result

## ----no-aliases, eval=TRUE----------------------------------------------------
# Map gene symbols to their NCBI gene ids, querying only the SYMBOL column 
result <- geneslator::select(org.Hsapiens.db, keys = c("BRCAI","PTEN"),
            columns = "ENTREZID", keytype = "SYMBOL", search.aliases = FALSE)
result

## ----archives, eval=TRUE------------------------------------------------------
# Map NCBI gene id 3 to gene symbol, using both current and old identifiers
result <- geneslator::select(org.Hsapiens.db, keys = "3", columns = "SYMBOL",
            keytype = "ENTREZID")
result

## ----no-archives, eval=TRUE---------------------------------------------------
# Map NCBI gene id 3 to gene symbol, using only current identifiers 
result <- geneslator::select(org.Hsapiens.db, keys = "3", columns = "SYMBOL",
            keytype = "ENTREZID", search.archives = FALSE)
result

## ----orthologs, eval=TRUE-----------------------------------------------------
# Get orthologs of yeast genes CHC1 and NMA2 in worm and fly 
result <- geneslator::select(org.Hsapiens.db, keys = c("CHC1","SCAMP5"),
            columns = c("ORTHOWORM", "ORTHOFLY"), keytype = "SYMBOL")
result

## ----orthologs-single, eval=TRUE----------------------------------------------
result <- geneslator::select(org.Hsapiens.db, keys = c("CHC1","SCAMP5"),
            columns = c("ORTHOWORM", "ORTHOFLY"), keytype = "SYMBOL",
            orthologs.mapping = "single")
result

## ----session-info-------------------------------------------------------------
sessionInfo()

