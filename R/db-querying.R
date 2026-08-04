# ========================================
# File: R/db-querying.R
# ========================================

# Crea un ambiente privato per il pacchetto
.geneslator_cache <- new.env(parent = emptyenv())

#' @importMethodsFrom methods show
#' @importFrom KEGGREST keggConv keggLink keggList
#' @importFrom stats setNames
NULL

#' @title Available database versions in geneslator
#'
#' @description
#' `availableVersions` lists all possible versions of the annotation databases
#' that can be queried in the \pkg{geneslator} package. Databases are updated
#' on a monthly basis and available as different versions of a Zenodo record
#' at \url{https://doi.org/10.5281/zenodo.20448208}.
#' Each release refer to a specific version of the databases. Versions are
#' indicated as `year.month`, where `year` and `month` denote the year and the
#' month of the publication of the release (e.g. '2026.03').
#'
#' @return `availableVersions` returns a character vector with all available
#' versions of the geneslator annotation databases.
#'
#' @seealso \code{\link{GeneslatorDb}}, \code{\link{availableDatabases}}.
#'
#' @examples
#' # List all available versions of geneslator databases
#' availableVersions()
#' @export
availableVersions <- function() {
  if (!curl::has_internet()) {
    msg <- paste0(
      "Failed to retrieve the list of all available versions ",
      "of geneslator annotation databases.\nNo internet connection"
    )
    stop(msg)
  }
  concept.doi <- "10.5281/zenodo.20448208"
  zenodo <- suppressMessages(zen4R::ZenodoManager$new())
  records <- suppressMessages(zenodo$getRecordByConceptDOI(concept.doi))
  db.versions <- suppressMessages(records$getVersions()$version)
  return(db.versions)
}

#' @title Available databases in geneslator
#'
#' @description
#' `availableDatabases` lists all possible annotation databases that can be
#' queried in the \pkg{geneslator} package. Databases are updated
#' on a monthly basis and available as different versions of a Zenodo record
#' at \url{https://doi.org/10.5281/zenodo.20448208}.
#' Each release refer to a specific version of the databases. Versions are
#' indicated as `year.month`, where `year` and `month` denote the year and the
#' month of the publication of the release (e.g. '2026.03').
#' Each database in a release refer to a specific organism.
#'
#' @param release.version Release version of the databases. By default, the
#' most recent version is considered ("latest"). Older versions must be
#' indicated as `year.month`, where `year` and `month` denote the year and the
#' month of the publication of the release (e.g. "2026.03"). See
#' [availableVersions()] for the list of available release versions.
#'
#' @return `availableDatabases` returns a dataframe which reports, for each
#' annotation database: database name, scientific name of the organism,
#' Taxonomy ID of the organism, MD5 security check of the SQLite database
#' file and release version. Database info refer to the release version
#' specified by the `version` parameter.
#'
#' @seealso \code{\link{GeneslatorDb}}, \code{\link{availableVersions}}.
#'
#' @examples
#' # List all databases included in the current geneslator release
#' availableDatabases()
#'
#' # List all databases included in geneslator release version 2025.12
#' availableDatabases("2025.12")
#'
#' @export
availableDatabases <- function(release.version = "latest") {
  cached.list.dbs <- get0(release.version, envir = .geneslator_cache)
  if (is.null(cached.list.dbs)) {
    if (!curl::has_internet()) {
      message(
        "Failed to retrieve list of available annotation ",
        "databases.\nNo internet connection"
      )
      return(NULL)
    }
    tryCatch(
      {
        api.url <- paste0(
          "https://zenodo.org/api/records?q=conceptrecid:",
          "20448208&all_versions=true"
        )
        response <- jsonlite::fromJSON(api.url)
        versions.meta <- response$hits$hits$metadata[, c("doi", "version")]
        versions.meta <- versions.meta[order(versions.meta$version), ]
        if (release.version == "latest") {
          record.doi <- versions.meta[nrow(versions.meta), "doi"]
        } else {
          record.doi <- versions.meta[versions.meta$version == release.version, "doi"]
          if (length(record.doi) == 0) {
            msg <- paste0(
              "Failed to retrieve the list of geneslator data bases ",
              "version ", release.version, "\nVersion ", release.version, " does not exist",
              "\nRun availableVersions() to check available releases of geneslator."
            )
            stop(msg)
          }
        }
        zenodo <- zen4R::ZenodoManager$new()
        record <- suppressMessages(zenodo$getRecordByDOI(record.doi))
      },
      error = function(e) {
        stop("Failed to retrieve list of annotation databases\n")
      }
    )
    db.version <- record$metadata$version
    tryCatch(
      {
        temp.dir <- tempdir()
        record$downloadFiles(
          record = record,
          files = "databases.json", path = temp.dir, quiet = TRUE
        )
        temp.file <- file.path(temp.dir, "databases.json")
      },
      error = function(e) {
        stop("Failed to retrieve list of annotation databases")
      }
    )
    list.databases <- jsonlite::fromJSON(temp.file)
    list.databases <- list.databases[order(list.databases$Organism), ]
    list.databases$Version <- db.version
    list.databases$DOI <- record.doi
    assign(release.version, list.databases, envir = .geneslator_cache)
    invisible(file.remove(temp.file))
    return(list.databases)
  } else {
    return(cached.list.dbs)
  }
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
#' Annotation databases used by \pkg{geneslator} are updated on a monthly basis
#' and available as different versions of a Zenodo record at
#' \url{https://doi.org/10.5281/zenodo.20448208} as SQLite
#' files. Each release refers to a specific version of the databases. Versions
#' are indicated as `year.month`, where `year` and `month` denote the year and
#' the month of the publication of the release (e.g. '2026.03'). Each database
#' in a release refers to a specific organism.
#'
#' The constructor method `GeneslatorDb(org)` creates a new `GeneslatorDb`
#' object for the annotation database of organism `org`. Once created, the
#' object is exported to the global environment of the user as a variable
#' having the same name of the annotation database (e.g. `org.Hsapiens.db` for
#' Human, `org.Mmusculus.db` for Mouse). By default, the constructor method
#' considers the latest release of the database. An older version can be
#' specified through parameter `release.version`. See [availableDatabases()]
#' and [availableVersions()] for the list of available databases and release
#' versions.
#'
#' When called, the constructor method first look for a copy of the SQLite
#' file in the R cache folder of the user. If the SQLite file exists and is
#' up-to-date, the cached copy is used to create the `GeneslatorDb` object.
#' Otherwise, upon request by the user, the database is dowloaded from the
#' remote release and copied in the \pkg{geneslator} package cache, before
#' creating the object.
#'
#' @slot db The annotation database represented as an `OrgDb` object.
#'
#' @param org A character string specifying the scientific name of the
#' organism (e.g. "Homo sapiens") or its Taxonomy ID.
#' See [availableDatabases()] for the list of supported organisms.
#'
#' @param release.version A character string indicating the release version of
#' the annotation database (e.g. "2025-12"). See [availableVersions()] for the
#' list of available releases.
#'
#' @returns A `GeneslatorDb` object.
#'
#' @examples
#' # Create a GeneslatorDb object for Human
#' # First call: download human db (org.Hsapiens.db) from latest release and
#' # save it to R cache
#' GeneslatorDb("Homo sapiens")
#' org.Hsapiens.db
#' # Second call: load db from local cache
#' GeneslatorDb("Homo sapiens")
#' org.Hsapiens.db
#'
#' # Create a GeneslatorDb object for Fly.
#' # Use taxonomy id and release version 2025.12
#' GeneslatorDb("7227", "2025.12")
#' org.Dmelanogaster.db
#'
#' @importFrom AnnotationDbi loadDb
#' @export
GeneslatorDb <- function(org, release.version = "latest") {
  # Check if annotation database for the required organism and release version
  # is available
  list.databases <- availableDatabases(release.version)
  if (is.null(list.databases)) {
    db.version <- release.version
    if (grepl("[0-9]+", org)) {
      org.info <- strsplit(.getOrgFromTaxid(org), " ")[[1]]
    } else {
      org.info <- strsplit(org, " ")[[1]]
    }
    db.name <- paste0("org.", substr(org.info[1], 1, 1), org.info[2], ".db")
    db.md5 <- NULL
    db.doi <- NULL
  } else {
    db.version <- unique(list.databases$Version)
    db.doi <- unique(list.databases$DOI)
    if (org %in% list.databases$Organism) {
      db.name <- list.databases[list.databases$Organism == org, "Name"]
    } else if (org %in% list.databases$TaxID) {
      db.name <- list.databases[list.databases$TaxID == org, "Name"]
    } else {
      stop("Organism '", org, "' not supported.\n",
        "See availableDatabases('", release.version, "') to view the ",
        "complete list.",
        call. = FALSE
      )
    }
    db.md5 <- list.databases[list.databases$Name == db.name, "MD5"]
  }
  is.latest <- FALSE
  if (release.version == "latest") {
    is.latest <- TRUE
  }
  # Get database local path (after downloading it if necessary)
  org.db <- .loadAnnotationDb(db.name, db.version, db.md5, db.doi, is.latest)
  # Create object GeneslatorDb
  assign(db.name, methods::new("GeneslatorDb", db = org.db), envir = .GlobalEnv)
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
#' # Lookup NCBI gene ids for a given list of gene symbols in fly
#' GeneslatorDb("Drosophila melanogaster")
#' geneslator::select(org.Dmelanogaster.db,
#'   keys = c("CG14883", "GstE2"),
#'   columns = "ENTREZID", keytype = "SYMBOL"
#' )
#'
#' # Lookup KEGG pathway ids and their relative full names for a given list
#' # of ensembl gene ids in worm
#' GeneslatorDb("Caenorhabditis elegans")
#' geneslator::select(org.Celegans.db,
#'   keys = c(
#'     "ENSDARG00000013522",
#'     "ENSDARG00000103044"
#'   ), columns = c("KEGGPATH", "KEGGPATHNAME"),
#'   keytype = "ENSEMBL"
#' )
#'
#' # Lookup mouse orthologs for a list of human gene symbols.
#' # Ignore aliases and return only the first ortholog found for each gene
#' GeneslatorDb("Homo sapiens")
#' geneslator::select(org.Hsapiens.db,
#'   keys = c("BRCA1", "PTEN"),
#'   columns = "ORTHOMOUSE", keytype = "SYMBOL", search.aliases = FALSE,
#'   orthologs.mapping = "single"
#' )
#'
#' # Lookup gene ontologies for a list of entrez ids in arabidopsis.
#' # Do not use NCBI archive data
#' GeneslatorDb("Arabidopsis thaliana")
#' geneslator::select(org.Athaliana.db,
#'   keys = c("820005", "831939"),
#'   columns = c("GO", "GONAME", "GOTYPE"), keytype = "ENTREZID",
#'   search.archives = FALSE
#' )
#'
#' @importMethodsFrom AnnotationDbi select
#' @export
setMethod("select", signature(x = "GeneslatorDb"),
function(x, keys, columns, keytype, search.aliases = TRUE,search.archives = TRUE, orthologs.mapping = "multiple", ...) {
    #Check correct format for input keys if SYMBOL or ALIAS
    if(keytype %in% c("SYMBOL","ALIAS")){
      .check.gene.symbols(keys)
    }
    # Remove keytype and duplicated columns from list of target columns
    columns <- unique(columns[columns != keytype])
    # Group columns according to related information in the DB table
    col.groups <- .group_db_columns(columns, search.archives, keytype)
    # Set all needed keytypes for the search
    keytype.set <- keytype
    if (keytype %in% c("ENTREZID", "ENSEMBL") && search.archives) {
      keytype.set <- c(keytype.set, paste0(keytype, "OLD"))
    } else if (keytype == "SYMBOL" && search.aliases) {
      keytype.set <- c(keytype.set, "ALIAS")
    } else if (keytype == "ALIAS") {
      keytype.set <- c(keytype.set, "SYMBOL")
    }
    # Perform all searches
    for (j in seq_len(length(col.groups))) {
      for (i in seq_len(length(keytype.set))) {
        # Solve select query using AnnotationDbi
        if(keytype.set[i]=="KEGGPATH"){
          #Get entrez gene IDs and pathway name if requested
          if("ENTREZID" %in% col.groups[[j]]){
            query.res <- .get.pathway.genes(as.character(keys),"ENTREZID")
          } else if("KEGGPATHNAME" %in% col.groups[[j]]){
            query.res <- .get.pathway.genes(as.character(keys),"KEGGPATHNAME")
          } else {
            query.res.entrez <- .get.pathway.genes(as.character(keys),"ENTREZID")
            list.keys.to.search <- query.res.entrez[!is.na(query.res.entrez$ENTREZID),"ENTREZID"]
            if(length(list.keys.to.search)>0){
              query.res <- .solve_query_select(
                x@db, list.keys.to.search,
                col.groups[[j]], "ENTREZID", orthologs.mapping)
              query.res <- merge(query.res.entrez,query.res)
              query.res <- unique(query.res)
            } else {
              query.res <- data.frame(matrix(nrow = length(keys), ncol = length(col.groups[[j]]) + 1))
              colnames(query.res) <- c(keytype.set[i], col.groups[[j]])
              query.res[[keytype.set[i]]] <- keys
            }
          }
        } else if("KEGGPATH" %in% col.groups[[j]] || "KEGGPATHNAME" %in% col.groups[[j]]){
          if(keytype.set[i]=="ENTREZID"){
            query.res <- .get.genes.pathways(as.character(keys),col.groups[[j]])
          } else {
            #Retrieve entrez IDs for pathway search
            entrez.keytpes <- "ENTREZID"
            fetch.old.entrez <- search.archives && keytype.set[i] != "ENTREZIDOLD"
            if(fetch.old.entrez){
              entrez.keytpes <- c(entrez.keytpes,"ENTREZIDOLD")
            }
            query.res.entrez <- .solve_query_select(
              x@db, as.character(keys),
              entrez.keytpes, keytype.set[i], orthologs.mapping
            )
            if(fetch.old.entrez){
              query.res.entrez$ENTREZID <- ifelse(is.na(query.res.entrez$ENTREZID),query.res.entrez$ENTREZIDOLD,query.res.entrez$ENTREZID)
              query.res.entrez$ENTREZIDOLD <- NULL
            }
            list.keys.to.search <- query.res.entrez[!is.na(query.res.entrez$ENTREZID),"ENTREZID"]
            if(length(list.keys.to.search)>0){
              query.res <- .get.genes.pathways(list.keys.to.search,col.groups[[j]])
              query.res <- merge(query.res.entrez,query.res,all.x=TRUE)
              query.res$ENTREZID <- NULL
              query.res <- unique(query.res)
            } else{
              query.res <- data.frame(matrix(nrow = length(keys), ncol = length(col.groups[[j]]) + 1))
              colnames(query.res) <- c(keytype.set[i], col.groups[[j]])
              query.res[[keytype.set[i]]] <- keys
            }
          }
          # Differentiate column names if aliases or old ids are used to search
          colnames(query.res)[colnames(query.res) != keytype.set[i]] <- paste0(
            colnames(query.res)[colnames(query.res) != keytype.set[i]],
            " ", LETTERS[i]
          )
          colnames(query.res)[colnames(query.res) == keytype.set[i]] <- keytype
        } else {
          query.res <- .solve_query_select(
            x@db, as.character(keys),
            col.groups[[j]], keytype.set[i], orthologs.mapping
          )
          # Differentiate column names if aliases or old ids are used to search
          colnames(query.res)[colnames(query.res) != keytype.set[i]] <- paste0(
            colnames(query.res)[colnames(query.res) != keytype.set[i]],
            " ", LETTERS[i]
          )
          colnames(query.res)[colnames(query.res) == keytype.set[i]] <- keytype
        }
        # Integrate results
        key.res <- if (i == 1) query.res else merge(key.res, query.res)
      }
      # Integrate results
      final.res <- if (j == 1) key.res else merge(final.res, key.res)
    }
    # Aggregate results
    for (col in columns) {
      ref.cols <- colnames(final.res)[colnames(final.res) == col | startsWith(colnames(final.res), paste0(col, " "))]
      final.res[[col]] <- apply(final.res, 1, function(row) {
        unique.info <- unname(c(unlist(row[ref.cols])))
        unique.info <- unique(unique.info[!is.na(unique.info)])
        ifelse(length(unique.info) == 0, NA, unique.info[1])
      })
    }
    # Warn user if archived ids or aliases have been used for the search
    .check_warnings(final.res, columns, keytype, search.aliases, search.archives)
    # Remove duplicate rows
    final.res <- unique(final.res[, c(keytype, columns)])
    return(as.data.frame(final.res))
  }
)


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
#' geneslator::mapIds(org.Scerevisiae.db,
#'   keys = c("856781", "1466469"),
#'   column = "ALIAS", keytype = "ENTREZID"
#' )
#'
#' # Map gene symbols to gene ontologies in mouse.
#' # Return a list with all possible mappings
#' GeneslatorDb("Mus musculus")
#' geneslator::mapIds(org.Mmusculus.db,
#'   keys = c("Grin2a", "Rev3l"), column = "GO",
#'   keytype = "SYMBOL", multiVals = "list"
#' )
#'
#' # Map gene symbols to uniprot ids in rat. Apply a custom function to
#' # return the last mapping found and do not use Ensembl archive data.
#' GeneslatorDb("Rattus norvegicus")
#' last <- function(x) {
#'   x[[length(x)]]
#' }
#' geneslator::mapIds(org.Rnorvegicus.db,
#'   keys = c(
#'     "ENSRNOG00000003105",
#'     "ENSRNOG00000049505"
#'   ), column = "UNIPROT", keytype = "ENSEMBL",
#'   multiVals = "list", search.archives = FALSE
#' )
#'
#' # Map gene symbols to reactome pathways in zebrafish.
#' # Return a CharacterList object with all possible mappings
#' GeneslatorDb("Danio rerio")
#' geneslator::mapIds(org.Drerio.db,
#'   keys = c("hoxc8a", "samhd1"),
#'   column = "REACTOMEPATH", keytype = "SYMBOL", multiVals = "CharacterList"
#' )
#'
#' @importFrom IRanges CharacterList
#' @importMethodsFrom AnnotationDbi mapIds
#' @export
setMethod("mapIds", signature(x = "GeneslatorDb"),
function(x, keys, column, keytype, search.aliases = TRUE, search.archives = TRUE,..., multiVals){
    # Set "multiVals" parameter if unspecified
    if (missing(multiVals)) {
      multiVals <- "first"
    }
    #Check correct format for input keys if SYMBOL or ALIAS
    if(keytype %in% c("SYMBOL","ALIAS")){
      .check.gene.symbols(keys)
    }
    # Set keytype and columns for the search
    keytype.set <- keytype
    if (keytype == "SYMBOL" && search.aliases) {
      keytype.set <- c(keytype.set, "ALIAS")
    } else if (keytype == "ALIAS") {
      keytype.set <- c(keytype.set, "SYMBOL")
    } else if (keytype %in% c("ENTREZID", "ENSEMBL") && search.archives) {
      keytype.set <- c(keytype.set, paste0(keytype, "OLD"))
    }
    column.set <- column
    if (column %in% c("ENTREZID", "ENSEMBL") && search.archives) {
      column.set <- c(column.set, paste0(column, "OLD"))
    }
    # Initialize final results data
    final.res <- rep(NA, length(keys))
    names(final.res) <- keys
    # Map data keytype by keytype, for each column
    for (kt in keytype.set) {
      for (col in column.set) {
        if (kt == "KEGGPATH" || col %in% c("KEGGPATH", "KEGGPATHNAME")) {
          query.res <- .solve_query_mapIds_kegg(
            x@db, as.character(keys), col,
            kt, multiVals
          )
        } else {
          query.res <- .solve_query_mapIds(
            x@db, as.character(keys), col,
            kt, multiVals
          )
        }
        final.res <- ifelse(is.na(final.res), query.res, final.res)
      }
    }
    names(final.res) <- keys
    if (!is.function(multiVals) && multiVals == "filter") {
      final.res <- unlist(final.res[lengths(final.res) < 2])
    } else if (!is.function(multiVals) && multiVals == "CharacterList") {
      final.res <- CharacterList(final.res)
    }
    return(final.res)
  }
)


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
  all.keys <- AnnotationDbi::keytypes(x@db)
  valid.keys <- all.keys[all.keys != "GID" & !endsWith(all.keys, "NAME") &
    !startsWith(all.keys, "GO")]
  valid.keys <- c(valid.keys, "GENENAME", "GO", "KEGGPATH")
  valid.keys <- unique(sort(valid.keys))
  return(valid.keys)
})


#' @rdname keytypes
#' @aliases columns
#' @importMethodsFrom AnnotationDbi columns
#' @export
setMethod("columns", signature(x = "GeneslatorDb"), function(x) {
  all.columns <- AnnotationDbi::keytypes(x@db)
  valid.columns <- all.columns[all.columns != "GID"]
  valid.columns <- c(valid.columns,"KEGGPATH","KEGGPATHNAME")
  valid.columns <- unique(sort(valid.columns))
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
  key.values <- AnnotationDbi::keys(x@db, keytype)
  key.values <- sort(unique(key.values))
  return(key.values)
})


#' Group columns in annotation db for select() function
#' @keywords internal
#' @noRd
.group_db_columns <- function(columns, search.archives, keytype) {
  # Group columns according to related information in the DB table
  col.groups <- list()
  i <- 1
  cols.go <- columns[columns %in% c("GO", "GOEVIDENCE", "GOTYPE", "GONAME")]
  if (length(cols.go) > 0) {
    col.groups[[i]] <- cols.go
    i <- i + 1
  }
  cols.kegg <- columns[columns %in% c("KEGGPATH", "KEGGPATHNAME")]
  if (length(cols.kegg) > 0) {
    col.groups[[i]] <- cols.kegg
    i <- i + 1
  }
  cols.reactome <- columns[columns %in% c("REACTOMEPATH", "REACTOMEPATHNAME")]
  if (length(cols.reactome) > 0) {
    col.groups[[i]] <- cols.reactome
    i <- i + 1
  }
  cols.wiki <- columns[columns %in% c("WIKIPATH", "WIKIPATHNAME")]
  if (length(cols.wiki) > 0) {
    col.groups[[i]] <- cols.wiki
    i <- i + 1
  }
  cols.special <- c(cols.go, cols.kegg, cols.reactome, cols.wiki)
  for (col in columns) {
    if (!col %in% cols.special) {
      col.groups[[i]] <- col
      i <- i + 1
      if(keytype=="KEGGPATH" && col=="ENSEMBL" && search.archives){
        col.groups[[i]] <- paste0(col, "OLD")
        i <- i + 1
      } else if (keytype!="KEGGPATH" && col %in% c("ENTREZID", "ENSEMBL") && search.archives){
        col.groups[[i]] <- paste0(col, "OLD")
        i <- i + 1
      }
    }
  }
  return(col.groups)
}


#' Apply multiVals dispatch logic to a raw keytype/column query result
#' @keywords internal
#' @noRd
.dispatch_multiVals <- function(ans, keys, column, keytype, multiVals) {
  if (is.function(multiVals)) {
    query.res <- split(ans[[column]], ans[[keytype]])
    query.res <- unlist(lapply(query.res, multiVals))
  } else if (multiVals == "first") {
    ans <- ans[!duplicated(ans[[keytype]]), ]
    query.res <- ans[[column]]
    names(query.res) <- ans[[keytype]]
  } else if (multiVals == "list" || multiVals == "filter" ||
             multiVals == "CharacterList") {
    query.res <- split(ans[[column]], ans[[keytype]])[keys]
  } else if (multiVals == "asNA") {
    list.dup.keys <- unique(ans[duplicated(ans[[keytype]]), keytype])
    ans[ans[[keytype]] %in% list.dup.keys, column] <- NA
    ans <- unique(ans)
    query.res <- ans[[column]]
    names(query.res) <- ans[[keytype]]
  }
  return(query.res)
}


#' Solve mapIds query using AnnotationDbi
#' @keywords internal
#' @noRd
.solve_query_mapIds <- function(db, keys, column, keytype, multiVals) {
  # Solve query using AnnotationDbi
  ans <- tryCatch(
    {
      AnnotationDbi::select(db, keys, column, keytype)
    },
    error = function(e) {
      res <- data.frame(matrix(nrow = length(keys), ncol = 2))
      colnames(res) <- c(keytype, column)
      res[[keytype]] <- as.character(keys)
      return(res)
    }
  )
  # Process query results considering multiVals parameter
  .dispatch_multiVals(ans, keys, column, keytype, multiVals)
}


#' Solve mapIds query for KEGG pathway <-> gene conversions
#' @keywords internal
#' @noRd
.solve_query_mapIds_kegg <- function(db, keys, column, keytype, multiVals) {
  keys.chr <- as.character(keys)
  if (keytype == "KEGGPATH") {
    kegg.pathway.ids <- ifelse(grepl("^path:", keys.chr), keys.chr, paste0("path:", keys.chr))
    norm.to.orig <- setNames(keys.chr, kegg.pathway.ids)
    if (column %in% c("ENTREZID", "KEGGPATHNAME")) {
      # Pathway -> Entrez IDs, or pathway -> pathway name
      ans <- .get.pathway.genes(keys.chr, column)
      # Restore KEGGPATH to original user values
      ans$KEGGPATH <- unname(norm.to.orig[ans$KEGGPATH])
      return(.dispatch_multiVals(ans, keys.chr, column, "KEGGPATH", multiVals))
    }
    # Pathway -> other column. Map done through ENTREZ IDs
    pathway.entrez.df <- .get.pathway.genes(keys.chr, "ENTREZID")
    # Restore KEGGPATH to original user values
    pathway.entrez.df$KEGGPATH <- unname(norm.to.orig[pathway.entrez.df$KEGGPATH])
    valid.entrez <- unique(pathway.entrez.df$ENTREZID[!is.na(pathway.entrez.df$ENTREZID)])
    if (length(valid.entrez) == 0) {
      ans <- data.frame(KEGGPATH = keys.chr)
      ans[[column]] <- NA
    } else {
      col.df <- .solve_query_select(db, valid.entrez, column, "ENTREZID", "multiple")
      ans <- merge(pathway.entrez.df, col.df, by = "ENTREZID", all.x = TRUE)
      ans <- ans[, c("KEGGPATH", column)]
    }
    return(.dispatch_multiVals(ans, keys.chr, column, "KEGGPATH", multiVals))
  }
  if (keytype == "ENTREZID") {
    #ENTREZ IDs -> KEGGPATH or KEGGPATHNAME
    ans <- .get.genes.pathways(keys.chr, column)
    colnames(ans)[colnames(ans) == "ENTREZID"] <- keytype
    return(.dispatch_multiVals(ans, keys.chr, column, keytype, multiVals))
  }
  # Other keytype -> Pathway. Map done through ENTREZ IDs
  entrez.map <- .solve_query_mapIds(db, keys.chr, "ENTREZID", keytype, "list")
  key.entrez.df <- do.call(rbind, lapply(keys.chr, function(k) {
    ids <- entrez.map[[k]]
    ids <- ids[!is.na(ids)]
    if (length(ids) == 0) {
      data.frame(KEY = k, ENTREZID = NA_character_)
    } else {
      data.frame(KEY = k, ENTREZID = ids)
    }
  }))
  valid.entrez <- unique(key.entrez.df$ENTREZID[!is.na(key.entrez.df$ENTREZID)])
  if (length(valid.entrez) == 0) {
    ans <- data.frame(KEY = keys.chr)
    ans[[column]] <- NA
  } else {
    pathway.df <- .get.genes.pathways(valid.entrez, column)
    ans <- merge(key.entrez.df, pathway.df, by = "ENTREZID", all.x = TRUE)
    ans <- ans[, c("KEY", column)]
  }
  colnames(ans)[colnames(ans) == "KEY"] <- keytype
  return(.dispatch_multiVals(ans, keys.chr, column, keytype, multiVals))
}


#' Solve select query using AnnotationDbi
#' @keywords internal
#' @noRd
.solve_query_select <- function(db,keys,col.group,keytype,orthologs.mapping){
  # Run query with specified set of related columns and keytpe
  query.res <- tryCatch(
    {
      AnnotationDbi::select(db, keys, col.group, keytype)
    },
    error = function(e) {
      res <- data.frame(matrix(nrow = length(keys), ncol = length(col.group) + 1))
      colnames(res) <- c(keytype, col.group)
      res[[keytype]] <- keys
      return(res)
    }
  )
  # For orthologs search, return only the first mapping found  if mapping
  # type is single
  if (any(startsWith(col.group, "ORTHO")) && orthologs.mapping == "single") {
    query.res <- query.res[!duplicated(query.res[[keytype]]), ]
  }
  # Map unmapped LOC genes in case of mapping SYMBOL-->ENTREZID
  if (keytype == "SYMBOL" && "ENTREZID" %in% col.group) {
    query.res[grepl("^LOC[0-9]+", query.res$SYMBOL) &
      is.na(query.res$ENTREZID), "ENTREZID"] <- gsub(
      "LOC", "",
      query.res[grepl("^LOC[0-9]+", query.res$SYMBOL) &
        is.na(query.res$ENTREZID), "SYMBOL"]
    )
  }
  return(query.res)
}


#' Check if warning user for archives or aliases used in select query
#' @keywords internal
#' @noRd
.check_warnings <- function(final.res,columns,keytype,search.aliases,search.archives) {
  # If search.aliases=T, check if some ids are mapped with ALIAS and not SYMBOL
  # If search.archives=T, check if some ids have been mapped using archives
  for (col in columns) {
    if (keytype == "SYMBOL" & search.aliases) {
      # Warn user for SYMBOL --> any queries when using also aliases
      .warning_symbol_aliases(final.res, col)
    } else if (keytype == "ALIAS" & search.archives) {
      # Warn user for ALIAS --> any queries when using also archived ids
      .warning_alias_archives(final.res, col)
    } else if (keytype %in% c("ENTREZID", "ENSEMBL") & search.archives) {
      # Warn user for ENTREZID/ENSEMBL --> any queries when using also
      # archived ids
      .warning_geneids_archives(final.res, col)
    } else if (search.archives) {
      # Warn user for any --> any queries when using also archived ids
      .warning_any_archives(final.res, col)
    }
  }
}


#' Warn user for SYMBOL --> any queries when using also aliases
#' @keywords internal
#' @noRd
.warning_symbol_aliases <- function(final.res, col) {
  if (length(grep("OLD ", colnames(final.res))) > 0) {
    if (nrow(final.res[(is.na(final.res[[paste0(col, " A")]]) &
      !is.na(final.res[[paste0(col, "OLD A")]])) |
      (is.na(final.res[[paste0(col, " A")]]) &
        is.na(final.res[[paste0(col, "OLD A")]]) &
        is.na(final.res[[paste0(col, " B")]]) &
        !is.na(final.res[[paste0(col, "OLD B")]])), ]) > 0) {
      warning("One or more gene symbols have been mapped to
            column ", col, " using archives. To disable search
            using archives, call select() with search.archives=F")
    }
    if (nrow(final.res[is.na(final.res[[paste0(col, " A")]]) &
      is.na(final.res[[paste0(col, "OLD A")]]) &
      !is.na(final.res[[paste0(col, " B")]]), ]) > 0) {
      warning("One or more gene symbols have been mapped to
            column ", col, " using aliases. To disable search using
            aliases, call select() with search.aliases=F")
    }
  } else {
    if (nrow(final.res[(is.na(final.res[[paste0(col, " A")]]) &
      !is.na(final.res[[paste0(col, " B")]])), ]) > 0) {
      warning("One or more gene symbols have been mapped to
            column ", col, " using aliases. To disable search using
            aliases, call select() with search.aliases=F")
    }
  }
}


#' Warn user for ALIAS --> any queries when using also archived ids
#' @keywords internal
#' @noRd
.warning_alias_archives <- function(final.res, col) {
  if (nrow(final.res[(is.na(final.res[[paste0(col, " A")]]) &
    !is.na(final.res[[paste0(col, "OLD A")]])) |
    (is.na(final.res[[paste0(col, " A")]]) &
      is.na(final.res[[paste0(col, "OLD A")]]) &
      is.na(final.res[[paste0(col, " B")]]) &
      !is.na(final.res[[paste0(col, "OLD B")]])), ]) > 0) {
    warning(
      "One or more gene aliases have been mapped to column ",
      col, " using archive data. To disable search using archives, call
        select() with search.archives=F"
    )
  }
}


#' Warn user for ENTREZID/ENSEMBL --> any queries when using also archived ids
#' @keywords internal
#' @noRd
.warning_geneids_archives <- function(final.res, col) {
  if (length(grep("OLD ", colnames(final.res))) > 0) {
    if (nrow(final.res[(is.na(final.res[[paste0(col, " A")]]) &
      !is.na(final.res[[paste0(col, " B")]])) |
      (is.na(final.res[[paste0(col, " A")]]) &
        is.na(final.res[[paste0(col, " B")]]) &
        !is.na(final.res[[paste0(col, "OLD A")]])) |
      (is.na(final.res[[paste0(col, " A")]]) &
        is.na(final.res[[paste0(col, " B")]]) &
        is.na(final.res[[paste0(col, "OLD A")]]) &
        !is.na(final.res[[paste0(col, "OLD B")]])), ]) > 0) {
      warning("One or more gene ids have been mapped to
            column ", col, " using archive data. To disable search using
            archives, call select() with search.archives=F")
    }
  } else {
    if (nrow(final.res[is.na(final.res[[paste0(col, " A")]]) &
      !is.na(final.res[[paste0(col, " B")]]), ]) > 0) {
      warning(
        "One or more gene ids have been mapped to column ",
        col, " using archive data. To disable search using
            archives, call select() with search.archives=F"
      )
    }
  }
}


#' Warn user for any --> any queries when using also archived ids
#' @keywords internal
#' @noRd
.warning_any_archives <- function(final.res, col) {
  if (nrow(final.res[is.na(final.res[[paste0(col, " A")]]) &
    !is.na(final.res[[paste0(col, "OLD A")]]), ]) > 0) {
    warning(
      "One or more keys have been mapped to column ", col,
      " using archive data. To disable search using archives, call
        select() with search.archives=F"
    )
  }
}


#' Show method for GeneslatorDb
#' @param object A \code{GeneslatorDb} object.
#' @return Invisibly returns \code{NULL}. Called for its side effect of 
#' printing a summary of the object to the console.
#' @exportMethod show
setMethod("show", "GeneslatorDb", function(object) {
  cat("GeneslatorDb object\n")
  cat("Organism:", AnnotationDbi::species(object@db), "\n")
  cat("Columns:", paste(AnnotationDbi::columns(object@db), collapse = ", "), "\n")
})


#' Cache map KEGG pathway IDs <-> KEGG pathway names for an organism (if needed) and return list
#' @keywords internal
#' @noRd
.get.kegg.pathway.list <- function(organism) {
  cache.key <- paste0("kegg_pathway_", organism)
  if (!exists(cache.key, envir = .geneslator_cache)) {
    assign(cache.key, keggList("pathway", organism), envir = .geneslator_cache)
  }
  get(cache.key, envir = .geneslator_cache)
}


#' Cache map NCBI IDs <-> KEGG Gene IDs (if needed) and return map
#' @keywords internal
#' @noRd
.get.kegg.entrez.map <- function(organism) {
  cache.key <- paste0("kegg_entrez_", organism)
  if (!exists(cache.key, envir = .geneslator_cache)) {
    assign(cache.key, keggConv("ncbi-geneid", organism), envir = .geneslator_cache)
  }
  get(cache.key, envir = .geneslator_cache)
}


#' Get pathway names or pathway IDs associated to a list of genes (denoted by Entrez IDs)
#' @keywords internal
#' @noRd
.get.genes.pathways <- function(entrez.ids, columns) {
  
  #Discover organism using only the first entrez ID (small, safe call)
  first.conv <- keggConv("genes", paste0("ncbi-geneid:", entrez.ids[1]))
  if (length(first.conv) == 0) {
    final.df <- data.frame(ENTREZID=as.character(entrez.ids))
    if("KEGGPATH" %in% columns){
      final.df$KEGGPATH <- NA
    }
    if("KEGGPATHNAME" %in% columns){
      final.df$KEGGPATHNAME <- NA
    }
    return(final.df)
  }
  organism <- sub(":.*", "", unname(first.conv)[1])
  
  #Get full organism ncbi-geneid <-> KEGG gene ID map (cached, one download per organism)
  all.conv <- .get.kegg.entrez.map(organism)
  kegg.from.ncbi <- setNames(names(all.conv), sub("^ncbi-geneid:", "", unname(all.conv)))
  
  #Look up locally (no API call) the KEGG gene ID for every requested entrez ID
  kegg.gene.ids <- unname(kegg.from.ncbi[as.character(entrez.ids)])
  valid <- !is.na(kegg.gene.ids)
  
  if(!any(valid)){
    final.df <- data.frame(ENTREZID=as.character(entrez.ids))
    if("KEGGPATH" %in% columns){
      final.df$KEGGPATH <- NA
    }
    if("KEGGPATHNAME" %in% columns){
      final.df$KEGGPATHNAME <- NA
    }
    return(final.df)
  }
  
  ncbi.from.kegg <- setNames(as.character(entrez.ids)[valid], kegg.gene.ids[valid])
  
  #Get pathway IDs from KEGG gene IDs (always needed, even if KEGGPATH isn't in the output)
  links <- keggLink("pathway", kegg.gene.ids[valid])
  if (length(links) == 0) {
    final.df <- data.frame(ENTREZID=as.character(entrez.ids))
    if("KEGGPATH" %in% columns){
      final.df$KEGGPATH <- NA
    }
    if("KEGGPATHNAME" %in% columns){
      final.df$KEGGPATHNAME <- NA
    }
    return(final.df)
  }
  
  final.df <- data.frame(ENTREZID = unname(ncbi.from.kegg[names(links)]))
  
  #Include KEGGPATH column only if requested
  if("KEGGPATH" %in% columns){
    final.df$KEGGPATH <- links
  }
  
  #Get pathway names (if requested)
  if("KEGGPATHNAME" %in% columns){
    clean.pathway.ids <- sub("^path:", "", links)
    all.pathways <- .get.kegg.pathway.list(organism)
    pathway.names <- unname(all.pathways[clean.pathway.ids])
    pathway.names <- sub(" - [^-]+$", "", pathway.names)
    final.df$KEGGPATHNAME <- pathway.names
  }
  
  #Add NA rows for genes with no associated pathways
  missing.ids <- setdiff(as.character(entrez.ids), final.df$ENTREZID)
  if(length(missing.ids) > 0){
    df.missing <- data.frame(ENTREZID = missing.ids)
    if("KEGGPATH" %in% columns){
      df.missing$KEGGPATH <- NA
    }
    if("KEGGPATHNAME" %in% columns){
      df.missing$KEGGPATHNAME <- NA
    }
    final.df <- rbind(final.df, df.missing)
  }
  
  return(final.df)
}


#' Get genes (denoted by Entrez IDs) or pathway names associated to a list of pathway IDs
#' @keywords internal
#' @noRd
.get.pathway.genes <- function(pathway.ids, columns) {
  
  #Ensure pathway IDs have the "path:" prefix required by keggLink
  kegg.pathway.ids <- ifelse(grepl("^path:", pathway.ids), pathway.ids, paste0("path:", pathway.ids))
  
  #Retrieve KEGG organism code directly from the pathway ID (e.g. "hsa" from "path:hsa04115")
  organism <- sub("^path:([a-zA-Z]+)[0-9]+$", "\\1", kegg.pathway.ids[1])
  
  #KEGGPATHNAME requested -> no need to fetch genes, just look up the pathway name
  if("KEGGPATHNAME" %in% columns){
    clean.pathway.ids <- sub("^path:", "", kegg.pathway.ids)
    all.pathways <- .get.kegg.pathway.list(organism)
    pathway.names <- unname(all.pathways[clean.pathway.ids])
    pathway.names <- sub(" - [^-]+$", "", pathway.names)
    return(data.frame(KEGGPATH = kegg.pathway.ids, KEGGPATHNAME = pathway.names))
  }
  
  #ENTREZID requested -> get KEGG gene IDs from pathway IDs and convert them
  links <- keggLink(organism, kegg.pathway.ids)
  if (length(links) == 0) {
    return(data.frame(KEGGPATH = kegg.pathway.ids, ENTREZID = NA))
  }
  kegg.gene.ids <- unname(links)
  
  #Convert KEGG gene IDs into Entrez IDs using the full organism mapping (cached)
  all.conv <- .get.kegg.entrez.map(organism)
  entrez.from.kegg <- setNames(sub("^ncbi-geneid:", "", unname(all.conv)), names(all.conv))
  
  final.df <- data.frame(
    KEGGPATH = names(links),
    ENTREZID = unname(entrez.from.kegg[kegg.gene.ids])
  )
  
  #Add NA rows for pathways with no associated genes
  missing.ids <- setdiff(kegg.pathway.ids, final.df$KEGGPATH)
  if(length(missing.ids) > 0){
    final.df <- rbind(final.df, data.frame(KEGGPATH = missing.ids, ENTREZID = NA))
  }
  
  return(final.df)
}


#' Screen gene symbols for spreadsheet-autoformatting corruption, based on
#' the four detection rules used by Ziemann et al. 2016 (PMID 27552985). 
#' This function flags values whose pattern is consistent with
#' having been silently reformatted by Excel (or similar spreadsheet
#' software) into a date or a floating-point number, and raises a
#' warning listing exactly which elements are suspect.
#' Detection rules: a slash-separated date (e.g. 01/03/2016), a dash-separated 
#' all-numeric date (e.g. 01-03-16), a day-month value (e.g. 1-Mar, 12-Sep), 
#' scientific notation (e.g. 2.31E+13, typical of RIKEN clone IDs or other 
#' long numeric-looking identifiers). If warn=TRUE and at least one element is
#' flagged, a warning is raised summarising the problem.
#' Invisibly, it returns a data.frame with one row per input element with the 
#' original input, a boolean denoting if it was flagged and which rule matched. 
#' Example: check_gene_symbols(c("BRCA1","1-Mar","TP53","12-Sep","2.31E+13"))
#' @keywords internal
#' @noRd
.check.gene.symbols <- function(gene.symbols, warn = TRUE) {
  
  ## ---- the four detection regexes, translated from the awk originals ----
  re.date.slash <- "^\\s*[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}\\s*$"
  re.date.dash  <- "^\\s*[0-9]{1,2}-[0-9]{1,2}-[0-9]{2,4}\\s*$"
  re.day.month  <- "^\\s*[0-9]{1,2}-[A-Za-z]{3}(-[0-9]{2,4})?\\s*$"
  re.sci        <- "^\\s*[0-9]\\.[0-9]{2}[Ee]\\+[0-9]{2}\\s*$"
  
  gene.symbols <- as.character(gene.symbols)
  n.symbols <- length(gene.symbols)
  rule <- rep(NA_character_, n.symbols)
  is.valid <- !is.na(gene.symbols) & nzchar(trimws(gene.symbols))
  rule[is.valid & grepl(re.day.month, gene.symbols, perl = TRUE)] <- "day_month"
  rule[is.valid & is.na(rule) & grepl(re.date.slash, gene.symbols, perl = TRUE)] <- "date_slash"
  rule[is.valid & is.na(rule) & grepl(re.date.dash, gene.symbols, perl = TRUE)] <- "date_dash"
  rule[is.valid & is.na(rule) & grepl(re.sci, gene.symbols, perl = TRUE)] <- "sci_notation"
  flagged <- !is.na(rule)
  result <- data.frame(
    value   = gene.symbols,
    flagged = flagged,
    rule    = rule,
    stringsAsFactors = FALSE
  )
  if (warn && any(flagged)) {
    n.flagged <- sum(flagged)
    rule.tbl  <- table(rule[flagged])
    rule.summary <- paste(sprintf("%s: %d", names(rule.tbl), rule.tbl), collapse = ", ")
    idx.preview <- which(flagged)[seq_len(min(10, n.flagged))]
    preview <- paste(sprintf("[%d] '%s' (%s)", idx.preview,
                             gene.symbols[idx.preview], rule[idx.preview]),
                     collapse = "; ")
    if (n.flagged > 10) preview <- paste0(preview, "; ... (", n.flagged - 10, " more)")
    warning(
      sprintf(
        "%d of %d value(s) look like they were reformatted by Excel/spreadsheet autocorrect (%s).
These may be corrupted gene symbols (e.g. MARCH1/SEPT12/DEC1 -> day-month, or long numeric IDs -> scientific notation).
Flagged: %s",
        n.flagged, n.symbols, rule.summary, preview
      ),
      call. = FALSE
    )
  }
  invisible(result)
}