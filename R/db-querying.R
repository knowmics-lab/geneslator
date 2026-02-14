# ========================================
# File: R/db-querying.R
# ========================================

#' @title Available databases in geneslator
#'
#' @description
#' `availableDatabases` lists all possible annotation databases that can be 
#' queried in the \pkg{geneslator} package. Databases are available as a 
#' GitHub Release at https://github.com/knowmics-lab/geneslator/releases. 
#' Each database refer to a specific organism. 
#'
#' @return `availableDatabases` returns a dataframe which reports, for each
#' annotation database: database name, scientific name and Taxonomy ID of the 
#' organism it refers to and number, release date and MD5 security check of 
#' the most recent version available in the GitHub Release. 
#'
#' @seealso \code{\link{GeneslatorDb}}
#'
#' @examples
#' # Get the list of all databases included in geneslator
#' availableDatabases()
#' @export
availableDatabases <- function() {
    url <- paste0("https://github.com/knowmics-lab/",
    "geneslator/releases/download/GeneslatorDb/databases.json")
    temp.file <- tempfile(fileext = paste0(".json"))
    tryCatch({
        #Download database info file
        utils::download.file(url = url, destfile = temp.file, quiet = TRUE)
    }, error = function(e) {
        msg <- paste0("Failed to retrieve list of annotation databases from ",
        url,"\nCheck internet connection")
        stop(msg) 
    })
    list.db <- jsonlite::fromJSON(temp.file)
    #Clean temp file
    invisible(file.remove(temp.file))
    return(list.db)
}

#' @title GeneslatorDb class
#' 
#' @description
#' The `GeneslatorDb` class is the container for storing annotation databases 
#' in the \pkg{geneslator} package.
#' 
#' @details
#' The `GeneslatorDb` class is the container for storing annotation databases 
#' in the `geneslator` package. It wraps an `OrgDb` object, which represents 
#' the annotation database of a specific organism. 
#' 
#' The constructor method `GeneslatorDb(org)` creates a new `GeneslatorDb` 
#' object for the annotation database of organism `org`. Once created, the 
#' object is exported to the global environment of the user as a variable 
#' having the same name of the annotation database (e.g. `org.Hsapiens.db` for 
#' Human, `org.Mmusculus.db` for Mouse). See [availableDatabases()] for the 
#' list of available databases.  
#' 
#' Annotation databases used by \pkg{geneslator} are stored as SQLite files in 
#' a remote repository at https://github.com/knowmics-lab/geneslator/releases. 
#' When called, the constructor method first look for a copy of the SQLite 
#' file in the R cache folder of the user. If the SQLite file exists and is 
#' updated, the latter is used to create the `GeneslatorDb` object. If an old 
#' version of the SQLite file exists, upon request by the user, the new 
#' version is copied in the R cache before creating the object. If the SQLite 
#' file does not exist, the latter is automatically copied in the 
#' \pkg{geneslator} package cache, before creating the object.
#' 
#' @slot db The annotation database represented as an `OrgDb` object.
#' 
#' @param org A character string specifying the scientific name of the 
#' organism (e.g. "Homo sapiens") or its Taxonomy ID. 
#' See [availableDatabases()] for the list of supported organisms.
#' 
#' @returns A `GeneslatorDb` object.
#' 
#' @examples
#' # Create a GeneslatorDb object for Human
#' # First call: download human db (org.Hsapiens.db), then load it from cache 
#' GeneslatorDb("Homo sapiens")
#' org.Hsapiens.db
#' 
#' # Create a GeneslatorDb object for Human
#' # Second call: load db from local cache
#' GeneslatorDb("Homo sapiens")
#' org.Hsapiens.db
#' 
#' # Create a GeneslatorDb object for Fly (use taxonomy id)
#' GeneslatorDb("7227")
#' org.Dmelanogaster.db
#' 
#' @importFrom AnnotationDbi loadDb
#' @export
GeneslatorDb <- function(org) {
    #Check if annotation database for this organism is available
    list.databases <- availableDatabases()
    if(org %in% list.databases$Organism){
        db.name <- list.databases[list.databases$Organism==org,"Name"]
    } else if(org %in% list.databases$TaxID){
        db.name <- list.databases[list.databases$TaxID==org,"Name"]
    } else {
        stop("Organism '", org, "' not supported.\n",
        "See availableDatabases() to view the complete list.", call. = FALSE)
    }
    db.version <- list.databases[list.databases$Name==db.name,"Version"]
    db.md5 <- list.databases[list.databases$Name==db.name,"MD5"]
    #Get database local path (after downloading it if necessary)
    org.db <- .loadAnnotationDb(db.name, db.version, db.md5)
    #Create object GeneslatorDb
    assign(db.name, methods::new("GeneslatorDb",db=org.db), envir = .GlobalEnv)
}


#' @rdname GeneslatorDb
#' @importClassesFrom AnnotationDbi OrgDb
#' @export
setClass("GeneslatorDb", slots = list(db = "OrgDb"))


#' @title Extract data from the annotation databases of geneslator
#' @name select
#' @aliases select,GeneslatorDb-method
#' @description
#' `select` query annotation databases of \pkg{geneslator} package, by mapping 
#' different types of gene annotation data from several source of data.
#' 
#' @param x A `GeneslatorDb` object returned by [GeneslatorDb()]. 
#' It represents the annotation database to query from.
#' @param keys Values used as keys to retrieve records from the annotation 
#' database.
#' @param columns Columns to return as output of the query. See [columns()] 
#' for more details.
#' @param keytype Column representing the type of values of `keys` parameter.
#' See [keytypes()] for more details. 
#' @param search.aliases When no mapping is found using gene symbol (SYMBOL 
#' column), should `select` perform query using also ALIAS column? (default = 
#' TRUE). This parameter is used only in queries involving SYMBOL column.
#' @param search.archives When no mapping is found using NCBI gene ids 
#' (ENTREZID column) and/or Ensembl gene ids (ENSEMBL column), should `select`
#' perform query using also archived identifiers (columns ENTREZIDOLD and/or 
#' ENSEMBLOLD)? (default = TRUE). This parameter is used only in queries 
#' involving ENTREZID and/or ENSEMBL column.
#' @param orthologs.mapping Return all orthologs (`"multiple"`) or just the 
#' first ortholog (`"single"`) of a gene? (default = `"multiple"`). Used only 
#' in queries where the output columns include `ORTHO` columns (e.g. 
#' ORTHOMOUSE, ORTHOYEAST).
#' @param ... Other arguments. See 
#' \code{\link[AnnotationDbi:AnnotationDb-class]{AnnotationDb}} for more info.
#' 
#' @details `select` collects all possible mappings between values of the 
#' column specified by `keytype` parameter and values of the columns specified 
#' by the `columns` parameter.
#' 
#' @returns 
#' `select` returns a dataframe with all columns specified by 
#' `keytype` and `columns` parameters and one row for each mapping 
#' found between keys and column values.
#' 
#' @seealso \code{\link{availableDatabases}}, \code{\link{keytypes}},
#' \code{\link{columns}}
#' 
#' @examples
#' #Lookup NCBI gene ids for a given list of gene symbols in fly
#' GeneslatorDb("Drosophila melanogaster")
#' geneslator::select(org.Dmelanogaster.db, keys=c("CG14883","GstE2"), 
#' columns="ENTREZID", keytype="SYMBOL")
#' 
#' # Lookup KEGG pathway ids and their relative full names for a given list 
#' # of ensembl gene ids in worm
#' GeneslatorDb("Caenorhabditis elegans")
#' geneslator::select(org.Celegans.db, keys=c("ENSDARG00000013522",
#' "ENSDARG00000103044"), columns=c("KEGGPATH","KEGGPATHNAME"), 
#' keytype="ENSEMBL")
#' 
#' # Lookup mouse orthologs for a list of human gene symbols. 
#' # Ignore aliases and return only the first ortholog found for each gene
#' GeneslatorDb("Homo sapiens")
#' geneslator::select(org.Hsapiens.db, keys=c("BRCA1","PTEN"), 
#' columns="ORTHOMOUSE", keytype="SYMBOL", search.aliases = FALSE, 
#' orthologs.mapping = "single")
#' 
#' # Lookup gene ontologies for a list of entrez ids in arabidopsis. 
#' # Do not use NCBI archive data
#' GeneslatorDb("Arabidopsis thaliana")
#' geneslator::select(org.Athaliana.db, keys=c("820005","831939"), 
#' columns=c("GO","GONAME","GOTYPE"), keytype="ENTREZID", 
#' search.archives = FALSE)
#' 
#' @importMethodsFrom AnnotationDbi select
#' @export
setMethod("select", signature(x = "GeneslatorDb"),
function(x, keys, columns, keytype, search.aliases=TRUE, 
search.archives=TRUE, orthologs.mapping="multiple", ...) {
    #Remove keytype and duplicated columns from list of target columns
    columns <- unique(columns[columns!=keytype])
    #Group columns according to related information in the DB table
    col.groups <- .group_db_columns(columns,search.archives)
    #Set all needed keytypes for the search
    keytype.set <- keytype
    if(keytype %in% c("ENTREZID","ENSEMBL") && search.archives){
        keytype.set <- c(keytype.set,paste0(keytype,"OLD"))
    } else if(keytype=="SYMBOL" && search.aliases){
        keytype.set <- c(keytype.set,"ALIAS")
    } else if(keytype=="ALIAS"){
        keytype.set <- c(keytype.set,"SYMBOL")
    }
    #Perform all searches
    for(j in seq_len(length(col.groups))){
        for(i in seq_len(length(keytype.set))){
            #Solve select query using AnnotationDbi
            query.res <- .solve_query_select(x@db, as.character(keys), 
            col.groups[[j]],keytype.set[i],orthologs.mapping)
            #Differentiate column names if aliases or old ids are used to search
            colnames(query.res)[colnames(query.res)!=keytype.set[i]] <- paste0(
            colnames(query.res)[colnames(query.res)!=keytype.set[i]], 
            " ",LETTERS[i])
            colnames(query.res)[colnames(query.res)==keytype.set[i]] <- keytype
            #Integrate results
            key.res <- if(i == 1) query.res else merge(key.res, query.res)
        }
        #Integrate results
        final.res <- if(j==1) key.res else merge(final.res,key.res)
    }
    #Aggregate results
    for(col in columns){
        ref.cols <- colnames(final.res)[startsWith(colnames(final.res),col)]
        final.res[[col]] <- apply(final.res,1,function(row){
            unique.info <- unname(c(unlist(row[ref.cols])))
            unique.info <- unique(unique.info[!is.na(unique.info)])
            ifelse(length(unique.info)==0,NA,unique.info[1])
        })
    }
    #Warn user if archived ids or aliases have been used for the search
    .check_warnings(final.res,columns,keytype,search.aliases,search.archives)
    #Remove duplicate rows
    final.res <- unique(final.res[,c(keytype,columns)])
    return(as.data.frame(final.res))
})


#' @title Map data from the annotation databases of geneslator
#' @name mapIds
#' @aliases mapIds,GeneslatorDb-method
#' @description
#' `mapIds` maps key values of a column to values of another column in the 
#' annotation databases of \pkg{geneslator} package.
#' 
#' @param x A `GeneslatorDb` object returned by [GeneslatorDb()]. 
#' It represents the annotation database to query from.
#' @param keys Values used as keys to retrieve records from the annotation 
#' database.
#' @param column Column to return as output of the query. See [columns()] 
#' for more details.
#' @param keytype Column representing the type of values of `keys` parameter.
#' See [keytypes()] for more details. 
#' @param search.aliases When no mapping is found using gene symbol (SYMBOL 
#' column), should `select` perform query using also ALIAS column? (default = 
#' TRUE). This parameter is used only in queries involving SYMBOL column.
#' @param search.archives When no mapping is found using NCBI gene ids 
#' (ENTREZID column) and/or Ensembl gene ids (ENSEMBL column), should `select`
#' perform query using also archived identifiers (columns ENTREZIDOLD and/or 
#' ENSEMBLOLD)? (default = TRUE). This parameter is used only in queries 
#' involving ENTREZID and/or ENSEMBL column.
#' @param ... Other arguments. See 
#' \code{\link[AnnotationDbi:AnnotationDb-class]{AnnotationDb}} for more info.
#' @param multiVals What should `mapIds` do when there are multiple output 
#' values that could be returned for a specific input? Options include:
#'
#' | Option | Description |
#' | :--- | :--- |
#' | `first` | Return a vector object containing only the first match found 
#' for each input (default behaviour). |
#' | `asNA` | Return a vector object with `NA` values whenever there are 
#' multiple matches for a given input. |
#' | `filter` | Return a shorter vector object, excluding all inputs for which 
#' multiple matches have been found. |
#' | `list` | Return a list object with all matches found for each input. |
#' | `CharacterList` | Return a `SimpleCharacterList` object with all matches 
#' found for each input. |
#' | `FUN` | Supply a function to the `multiVals` argument for custom 
#' behaviors. |
#'
#' If using `FUN`, the function must take a single argument and return a single
#' value. This function will be applied to all elements and will serve as a 
#' 'rule' for which item to keep when there is more than one match for a given 
#' input. For example, the following function grabs the last element in each 
#' result: `last <- function(x) { x[[length(x)]] }`.
#' 
#' @details 
#' `mapIds` maps each key value to either a single value or a list of 
#' values of the type specified by `column` parameter, depending on the 
#' value of `multiVals` parameter.
#' 
#' @returns 
#' `mapIds` returns either a named vector, where each value is a possible 
#' mapping (if exists) for a given key, or a list of values, where each element 
#' of the list is the vector of all mappings found for a given key. The type of 
#' the return object depends on the value of the `multiVals` parameter.
#' 
#' @seealso \code{\link{availableDatabases}}, \code{\link{keytypes}},
#' \code{\link{columns}}
#' 
#' @examples
#' # Map NCBI gene ids to gene aliases in yeast. 
#' # Return a named vector with 1st mapping found
#' GeneslatorDb("Saccharomyces cerevisiae")
#' geneslator::mapIds(org.Scerevisiae.db, keys=c("856781","1466469"), 
#' column="ALIAS", keytype="ENTREZID")
#' 
#' # Map gene symbols to gene ontologies in mouse. 
#' # Return a list with all possible mappings
#' GeneslatorDb("Mus musculus")
#' geneslator::mapIds(org.Mmusculus.db, keys=c("Grin2a","Rev3l"), column="GO", 
#' keytype="SYMBOL", multiVals="list")
#' 
#' # Map gene symbols to uniprot ids in rat. Apply a custom function to 
#' # return the last mapping found and do not use Ensembl archive data.
#' GeneslatorDb("Rattus norvegicus")
#' last <- function(x){x[[length(x)]]}
#' geneslator::mapIds(org.Rnorvegicus.db, keys=c("ENSRNOG00000003105",
#' "ENSRNOG00000049505"), column="UNIPROT", keytype="ENSEMBL", 
#' multiVals="list", search.archives=FALSE)
#' 
#' # Map gene symbols to reactome pathways in zebrafish.
#' # Return a CharacterList object with all possible mappings
#' GeneslatorDb("Danio rerio")
#' geneslator::mapIds(org.Drerio.db, keys=c("hoxc8a","samhd1"), 
#' column="REACTOMEPATH", keytype="SYMBOL", multiVals="CharacterList")
#' 
#' @importFrom IRanges CharacterList
#' @importMethodsFrom AnnotationDbi mapIds
#' @export
setMethod("mapIds", signature(x = "GeneslatorDb"),
function(x, keys, column, keytype, search.aliases=TRUE, search.archives=TRUE, 
..., multiVals) {
    # Set "multiVals" parameter if unspecified
    if (missing(multiVals)) {
        multiVals <- "first"
    }
    #Set keytype and columns for the search
    keytype.set <- keytype
    if(keytype=="SYMBOL" && search.aliases){
        keytype.set <- c(keytype.set,"ALIAS")
    } else if(keytype=="ALIAS"){
        keytype.set <- c(keytype.set,"SYMBOL")
    } else if(keytype %in% c("ENTREZID","ENSEMBL") && search.archives){
        keytype.set <- c(keytype.set,paste0(keytype,"OLD"))
    }
    column.set <- column
    if(column %in% c("ENTREZID","ENSEMBL") && search.archives){
        column.set <- c(column.set,paste0(column,"OLD"))
    }
    #Initialize final results data
    final.res <- rep(NA,length(keys))
    names(final.res) <- keys
    #Map data keytype by keytype, for each column
    for(kt in keytype.set){
        for(col in column.set){
            query.res <- .solve_query_mapIds(x@db, as.character(keys), col, 
            kt, multiVals)
            final.res <- ifelse(is.na(final.res),query.res,final.res)
        }
    }
    names(final.res) <- keys
    if(!is.function(multiVals) && multiVals=="filter"){
        final.res <- unlist(final.res[lengths(final.res)<2])
    } else if(!is.function(multiVals) && multiVals=="CharacterList"){
        final.res <- CharacterList(final.res)
    }
    return(final.res)
})


#' @title List available columns in the annotation databases of geneslator
#' @name keytypes
#' @aliases keytypes,GeneslatorDb-method
#' @description
#' Functions `keytypes` and `columns` are used to access the complete lists 
#' of input and output columns that can be queried in the annotation databases 
#' of the \pkg{geneslator} package through [mapIds()] and [select()] functions.
#'
#' @param x A `GeneslatorDb` object returned by [GeneslatorDb()]. 
#' It represents the annotation database to query from.
#'
#' @details
#' `keytypes()` lists all possible columns of the annotation database `x` that 
#' can be used as input when querying `x`, i.e., all possible values of the 
#' `keytype` argument in [mapIds()] and [select()] functions.
#'
#' `columns()` lists all possible columns of the annotation database `x` that 
#' can be used as output when querying `x`, i.e., all possible values of the 
#' `column` argument in [mapIds()] and [select()] functions.
#'
#' The following is the complete list of columns defined in the annotation 
#' databases of \pkg{geneslator} package. Some of these columns may be missing 
#' in one or more organisms.
#'
#' | Column | Description |
#' | :--- | :--- |
#' | `SYMBOL` | Official gene symbol |
#' | `ALIAS` | Aliases of a gene |
#' | `GENETYPE` | Biological type of a gene (e.g. 'protein-coding', 'ncRNA') |
#' | `GENENAME` | Full name or description of a gene |
#' | `ENTREZID` | Gene ID in NCBI Gene |
#' | `ENSEMBL` | Gene ID in Ensembl |
#' | `HGNC` | Gene ID in HUGO Gene Nomenclature Committee (Human only) |
#' | `MGI` | Gene ID in Mouse Genome Informatics (Mouse only) |
#' | `RGD` | Gene ID in Rat Genome Database (Rat only) |
#' | `SGD` | Gene ID in Saccharomyces Genome Database (Yeast only) |
#' | `WORMBASE` | Gene ID in WormBase database (Worm only) |
#' | `FLYBASE` | Gene ID in FlyBase database (Fly only) |
#' | `ZFIN` | Gene ID in Zebrafish Information Network (Zebrafish only) |
#' | `TAIR` | Gene ID in The Arabidopsis Information Resource (Arabidopsis 
#' only) |
#' | `UNIPROTKB` | Uniprot IDs of proteins associated to a gene |
#' | `ENTREZIDOLD` | Archived IDs in NCBI Gene |
#' | `ENSEMBLOLD` | Archived IDs in Ensembl |
#' | `ORTHOHUMAN` | Orthologs in Human (absent in Human and Arabidopsis) |
#' | `ORTHOMOUSE` | Orthologs in Mouse (absent in Mouse and Arabidopsis) |
#' | `ORTHORAT` | Orthologs in Rat (absent in Rat and Arabidopsis) |
#' | `ORTHOYEAST` | Orthologs in Yeast (absent in Yeast and Arabidopsis) |
#' | `ORTHOWORM` | Orthologs in Worm (absent in Worm and Arabidopsis) |
#' | `ORTHOFLY` | Orthologs in Fly (absent in Fly and Arabidopsis) |
#' | `ORTHOZEBRAFISH` | Orthologs in Zebrafish (absent in Zebrafish and 
#' Arabidopsis) |
#' | `GO` | IDs of Gene Ontology (GO) terms associated to a gene |
#' | `GONAME` | Names of GO terms associated to a gene |
#' | `GOEVIDENCE` | Evidence codes of GO terms associated to a gene |
#' | `GOTYPE` | Types of GO terms ('BP'=biological process, 'CC'=cellular 
#' component, 'MF'=molecular function) associated to a gene |
#' | `KEGGPATH` | IDs of KEGG pathways associated to a gene |
#' | `KEGGPATHNAME` | Names of KEGG pathways associated to a gene |
#' | `REACTOMEPATH` | IDs of Reactome pathways associated to a gene |
#' | `REACTOMEPATHNAME` | Names of Reactome pathways associated to a gene |
#' | `WIKIPATH` | IDs of Wikipathways pathways associated to a gene |
#' | `WIKIPATHNAME` | Names of Wikipathways pathways associated to a gene |
#'
#' @return `keytypes()` and `columns()` return a character vector of column 
#' names of database `x`.
#'
#' @seealso \code{\link{availableDatabases}}, \code{\link{mapIds}},
#' \code{\link{select}}
#'
#' @examples
#' # Get the list of available keytypes in mouse
#' GeneslatorDb("Mus musculus")
#' geneslator::keytypes(org.Mmusculus.db)
#'
#' # Get the list of available columns that can be mapped to keys in yeast
#' GeneslatorDb("Saccharomyces cerevisiae")
#' geneslator::columns(org.Scerevisiae.db)
#' 
#' @importMethodsFrom AnnotationDbi keytypes
#' @export
setMethod("keytypes", signature(x = "GeneslatorDb"), function(x) {
    # Ottieni i keytypes dal database base
    #all.keys <- getMethod("keytypes", "OrgDb", 
    #where = getNamespace("AnnotationDbi"))(x@db)
    all.keys <- AnnotationDbi::keytypes(x@db)
    valid.keys <- all.keys[all.keys!="GID" & !endsWith(all.keys,"NAME") & 
    !startsWith(all.keys,"GO")]
    valid.keys <- c(valid.keys,"GENENAME","GO")
    valid.keys <- sort(valid.keys)
    return(valid.keys)
})


#' @rdname keytypes
#' @aliases columns
#' @importMethodsFrom AnnotationDbi columns
#' @export
setMethod("columns", signature(x = "GeneslatorDb"), function(x) {
    #all.columns <- getMethod("keytypes", "OrgDb", 
    #where = getNamespace("AnnotationDbi"))(x@db)
    all.columns <- AnnotationDbi::keytypes(x@db)
    valid.columns <- all.columns[all.columns!="GID"]
    valid.columns <- sort(valid.columns)
    return(valid.columns)
})


#' @title List values of a column in the annotation databases of geneslator
#' @name keys
#' @aliases keys,GeneslatorDb-method
#' @description
#' The `keys` function lists of all possible values for a given 
#' column in the annotation database of a specific organism within the 
#' \pkg{geneslator} package.
#'
#' @param x A `GeneslatorDb` object returned by [GeneslatorDb()]. 
#' It represents the annotation database to query from.
#' @param keytype Name of the column from which the list of values should be 
#' extracted. See [keytypes()] for the list of available columns for the 
#' annotation database `x`.
#'
#' @return `keys` returns a character vector of all possible values of the 
#' column `keytype` in database `x`.
#'
#' @seealso [keytypes()], [mapIds()], [select()]
#'
#' @examples
#' # Get the list of all NCBI gene ids present in zebrafish annotation db
#' GeneslatorDb("Danio rerio")
#' geneslator::keys(org.Drerio.db, keytype = "ENTREZID")
#'
#' # Get the list of all KEGG pathways present in rat annotation db
#' GeneslatorDb("Rattus norvegicus")
#' geneslator::keys(org.Rnorvegicus.db, keytype = "KEGGPATH")
#' 
#' @importMethodsFrom AnnotationDbi keys
#' @export
setMethod("keys", signature(x = "GeneslatorDb"), function(x, keytype) {
    #key.values <- getMethod("keys", "OrgDb", 
    #where = getNamespace("AnnotationDbi"))(x@db, keytype)
    key.values <- AnnotationDbi::keys(x@db, keytype)
    key.values <- sort(unique(key.values))
    return(key.values)
})


#' Group columns in annotation db for select() function
#' @keywords internal
#' @noRd
.group_db_columns <- function(columns, search.archives){
    #Group columns according to related information in the DB table
    col.groups <- list()
    i <- 1
    cols.go <- columns[columns %in% c("GO","GOEVIDENCE","GOTYPE","GONAME")]
    if(length(cols.go)>0){
        col.groups[[i]] <- cols.go
        i <- i+1
    }
    cols.kegg <- columns[columns %in% c("KEGGPATH","KEGGPATHNAME")]
    if(length(cols.kegg)>0){
        col.groups[[i]] <- cols.kegg
        i <- i+1
    }
    cols.reactome <- columns[columns %in% c("REACTOMEPATH","REACTOMEPATHNAME")]
    if(length(cols.reactome)>0){
        col.groups[[i]] <- cols.reactome
        i <- i+1
    }
    cols.wiki <- columns[columns %in% c("WIKIPATH","WIKIPATHNAME")]
    if(length(cols.wiki)>0){
        col.groups[[i]] <- cols.wiki
        i <- i+1
    }
    cols.special <- c(cols.go,cols.kegg,cols.reactome,cols.wiki)
    for(col in columns){
        if(!col %in% cols.special){
            col.groups[[i]] <- col
            i <- i+1
            if(col %in% c("ENTREZID","ENSEMBL") && search.archives){
                col.groups[[i]] <- paste0(col,"OLD")
                i <- i+1
            }
        }
    }
    return(col.groups)
}


#' Solve mapIds query using AnnotationDbi
#' @keywords internal
#' @noRd
.solve_query_mapIds <- function(db, keys, column, keytype, multiVals){
    #Solve query using AnnotationDbi
    ans <- tryCatch({
        AnnotationDbi::select(db, keys, column, keytype)
    },
    error = function(e) {
        res <- data.frame(matrix(nrow=length(keys),ncol=2))
        colnames(res) <- c(keytype,column)
        res[[keytype]] <- as.character(keys)
        return(res)
    })
    #Process query results considering multiVals parameter
    if(is.function(multiVals)){
        query.res <- split(ans[[column]],ans[[keytype]])
        query.res <- unlist(lapply(query.res,multiVals))
    } else if(multiVals=="first"){
        ans <- ans[!duplicated(ans[[keytype]]),]
        query.res <- ans[[column]]
        names(query.res) <- ans[[keytype]]
    } else if(multiVals=="list" || multiVals=="filter" 
    || multiVals=="CharacterList"){
        query.res <- split(ans[[column]],ans[[keytype]])[keys]
    } else if(multiVals=="asNA"){
        list.dup.keys <- unique(ans[duplicated(ans[[keytype]]),keytype])
        ans[ans[[keytype]] %in% list.dup.keys,column] <- NA
        ans <- unique(ans)
        query.res <- ans[[column]]
        names(query.res) <- ans[[keytype]]
    }
    return(query.res)
}


#' Solve select query using AnnotationDbi
#' @keywords internal
#' @noRd
.solve_query_select <- function(db, keys, col.group, keytype, 
orthologs.mapping){
    #Run query with specified set of related columns and keytpe
    query.res <- tryCatch({
    AnnotationDbi::select(db, keys, col.group, keytype)
    },
    error = function(e) {
        res <- data.frame(matrix(nrow=length(keys), ncol=length(col.group)+1))
        colnames(res) <- c(keytype,col.group)
        res[[keytype]] <- keys
        return(res)
    })
    #For orthologs search, return only the first mapping found  if mapping 
    #type is single
    if(any(startsWith(col.group,"ORTHO")) && orthologs.mapping=="single"){
        query.res <- query.res[!duplicated(query.res[[keytype]]),]
    }
    #Map unmapped LOC genes in case of mapping SYMBOL-->ENTREZID
    if(keytype=="SYMBOL" && "ENTREZID" %in% col.group){
        query.res[grepl("^LOC[0-9]+",query.res$SYMBOL) & 
        is.na(query.res$ENTREZID),"ENTREZID"] <- gsub("LOC","",
        query.res[grepl("^LOC[0-9]+",query.res$SYMBOL) & 
        is.na(query.res$ENTREZID),"SYMBOL"])
    }
    return(query.res)
}


#' Check if warning user for archives or aliases used in select query
#' @keywords internal
#' @noRd
.check_warnings <- function(final.res, columns, keytype, search.aliases, 
search.archives){
    #If search.aliases=T, check if some ids are mapped with ALIAS and not SYMBOL
    #If search.archives=T, check if some ids have been mapped using archives
    for(col in columns){
        if(keytype=="SYMBOL" & search.aliases){
            #Warn user for SYMBOL --> any queries when using also aliases
            .warning_symbol_aliases(final.res, col)
        } else if(keytype=="ALIAS" & search.archives){
            #Warn user for ALIAS --> any queries when using also archived ids
            .warning_alias_archives(final.res, col)
        } else if(keytype %in% c("ENTREZID","ENSEMBL") & search.archives) {
            #Warn user for ENTREZID/ENSEMBL --> any queries when using also 
            #archived ids
            .warning_geneids_archives(final.res, col)
        } else if(search.archives){
            #Warn user for any --> any queries when using also archived ids
            .warning_any_archives(final.res, col)
        }
    }
}


#' Warn user for SYMBOL --> any queries when using also aliases
#' @keywords internal
#' @noRd
.warning_symbol_aliases <- function(final.res, col){
    if(length(grep("OLD ",colnames(final.res)))>0){
        if(nrow(final.res[(is.na(final.res[[paste0(col," A")]]) & 
        !is.na(final.res[[paste0(col,"OLD A")]])) | 
        (is.na(final.res[[paste0(col," A")]]) & 
        is.na(final.res[[paste0(col,"OLD A")]]) & 
        is.na(final.res[[paste0(col," B")]]) & 
        !is.na(final.res[[paste0(col,"OLD B")]])),])>0){
            warning("One or more gene symbols have been mapped to 
            column ",col," using archives. To disable search 
            using archives, call select() with search.archives=F")
        }
        if(nrow(final.res[is.na(final.res[[paste0(col," A")]]) 
        & is.na(final.res[[paste0(col,"OLD A")]]) & 
        !is.na(final.res[[paste0(col," B")]]),])>0){
            warning("One or more gene symbols have been mapped to 
            column ",col," using aliases. To disable search using 
            aliases, call select() with search.aliases=F")
        }
    } else {
        if(nrow(final.res[(is.na(final.res[[paste0(col," A")]]) & 
        !is.na(final.res[[paste0(col," B")]])),])>0){
            warning("One or more gene symbols have been mapped to 
            column ",col," using aliases. To disable search using 
            aliases, call select() with search.aliases=F")
        }
    }
}


#' Warn user for ALIAS --> any queries when using also archived ids
#' @keywords internal
#' @noRd
.warning_alias_archives <- function(final.res, col){
    if(nrow(final.res[(is.na(final.res[[paste0(col," A")]]) & 
    !is.na(final.res[[paste0(col,"OLD A")]])) | 
    (is.na(final.res[[paste0(col," A")]]) & 
    is.na(final.res[[paste0(col,"OLD A")]]) & 
    is.na(final.res[[paste0(col," B")]]) & 
    !is.na(final.res[[paste0(col,"OLD B")]])),])>0){
        warning("One or more gene aliases have been mapped to column ",
        col," using archive data. To disable search using archives, call 
        select() with search.archives=F")
    }
}


#' Warn user for ENTREZID/ENSEMBL --> any queries when using also archived ids
#' @keywords internal
#' @noRd
.warning_geneids_archives <- function(final.res, col){
    if(length(grep("OLD ",colnames(final.res)))>0){
        if(nrow(final.res[(is.na(final.res[[paste0(col," A")]]) & 
        !is.na(final.res[[paste0(col," B")]])) |
        (is.na(final.res[[paste0(col," A")]]) & 
        is.na(final.res[[paste0(col," B")]]) & 
        !is.na(final.res[[paste0(col,"OLD A")]])) |
        (is.na(final.res[[paste0(col," A")]]) & 
        is.na(final.res[[paste0(col," B")]]) & 
        is.na(final.res[[paste0(col,"OLD A")]]) & 
        !is.na(final.res[[paste0(col,"OLD B")]])),])>0){
            warning("One or more gene ids have been mapped to 
            column ",col," using archive data. To disable search using
            archives, call select() with search.archives=F")
        }
    } else {
        if(nrow(final.res[is.na(final.res[[paste0(col," A")]]) & 
        !is.na(final.res[[paste0(col," B")]]),])>0){
            warning("One or more gene ids have been mapped to column ",
            col," using archive data. To disable search using 
            archives, call select() with search.archives=F")
        }
    }
}


#' Warn user for any --> any queries when using also archived ids
#' @keywords internal
#' @noRd
.warning_any_archives <- function(final.res, col){
    if(nrow(final.res[is.na(final.res[[paste0(col," A")]]) & 
    !is.na(final.res[[paste0(col,"OLD A")]]),])>0){
        warning("One or more keys have been mapped to column ",col,
        " using archive data. To disable search using archives, call 
        select() with search.archives=F")
    }
}
