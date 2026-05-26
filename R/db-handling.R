# ========================================
# File: R/db-handling.R
# ========================================

#' @importFrom utils read.table write.table
NULL

#' Load from local cache annotation database
#' If annotation db is not present, download it automatically before loading
#' If annotation db is not up to date, eventually download it before loading
#' @keywords internal
#' @noRd
.loadAnnotationDb <- function(db.name, remote.version, remote.md5, is.latest) {
    cache.dir <- tools::R_user_dir("geneslator", which = "cache")
    db.file <- if (is.latest) {
    list.files(cache.dir,pattern=paste0(db.name,".*_latest"),full.names=TRUE)
    } else {
    list.files(cache.dir,pattern=paste0(db.name,"_",remote.version,".sqlite"), 
    full.names = TRUE)
    }
    if(!is.na(db.file)){
        if(is.latest){
            local.version <- strsplit(db.file,".db_|_latest")[[1]][2]
            if (curl::has_internet() && local.version!=remote.version) {
                message("Available update for ",db.name," database")
                message("Local version: ", local.version)
                message("Available version: ", remote.version)
                if(interactive()){
                    response <- readline("Do you want to update it? (y/n): ")
                    if(tolower(trimws(response)) == "y"){
                        .downloadAnnotationDb(db.name,cache.dir,remote.version,
                        remote.md5,is.latest,is.update=TRUE)
                    } else {
                        message("Use existing local version.")
                    }
                }
            } else {
                if(exists(db.name)){
                    DBI::dbDisconnect(AnnotationDbi::dbconn(get(db.name)@db))
                }
                message("Loaded database found in cache: ", db.file)
            }
        } else {
            if(exists(db.name)) DBI::dbDisconnect(
            AnnotationDbi::dbconn(get(db.name)@db))
            message("Loaded database found in cache: ", db.file)
        }
    } else {
        message("Database not found in cache")
        if(!curl::has_internet()){
            msg <- paste0("Failed to download database ",db.name," from ",
            "remote repository.\nNo internet connection")
            stop(msg)
        }
        .downloadAnnotationDb(db.name,cache.dir,remote.version,remote.md5,
        is.latest,is.update=FALSE)
    }
    db.file <- ifelse(is.latest,list.files(cache.dir,pattern=paste0(db.name,
    ".*_latest"),full.names = TRUE),list.files(cache.dir,pattern = paste0(
    db.name,"_",remote.version,".sqlite"),full.names = TRUE))
    org.db <- suppressPackageStartupMessages(AnnotationDbi::loadDb(db.file))
    return(org.db)
}

#' Download annotation database from remote repository on GitHub
#' @keywords internal
#' @noRd
.downloadAnnotationDb <- function(db.name,cache.dir,remote.version,
remote.md5, is.latest, is.update) {
    #URL of remote repository
    url <- paste0("https://github.com/knowmics-lab/",
    "geneslator-data/releases/download/",remote.version,"/",db.name,".sqlite")
    message("========================================")
    message("Download database ", db.name)
    message("Version: ", remote.version)
    message("========================================")
    message("This can take few minutes...")
    #Increase timeout for download to 10 minutes
    options(timeout = 3600)
    tryCatch({
        #Download annotation database
        temp.file <- tempfile(fileext=".sqlite")
        utils::download.file(url = url, mode = "wb", quiet = FALSE,
        destfile = temp.file, method = "auto")
        #Check file integrity
        local.md5 <- tools::md5sum(temp.file)
        if (local.md5!=remote.md5) {
            file.remove(temp.file)
            stop("Incomplete download of annotation db file")
        }
        if(is.latest){
            local.file.name <- paste0(cache.dir,"/",db.name,"_",remote.version,
            "_latest.sqlite")
        } else {
            local.file.name <- paste0(cache.dir,"/",db.name,"_",remote.version,
            ".sqlite")
        }
        if(exists(db.name)){
            DBI::dbDisconnect(AnnotationDbi::dbconn(get(db.name)@db))
        }
        if(is.latest && is.update){
            file.remove(list.files(cache.dir,pattern=paste0(db.name,
            ".*_latest"),full.names = TRUE))
        }
        file.rename(temp.file, local.file.name)
        message("Download completed successfully!")
        message("File: ", local.file.name)
    }, error = function(e) {
        #Clean data in case of error
        invisible(file.remove(temp.file))
        msg <- sprintf("Failed to download database: %s\nURL: 
        %s\nDetails: %s", db.name, url, e$message)
        stop(msg, call. = FALSE) 
    })
}

#' Map Taxonomy ID to organism
#' @keywords internal
#' @noRd
.getOrgFromTaxid <- function(taxid)
{
    org.list <- c("Homo sapiens","Mus musculus","Rattus norvegicus",
    "Danio rerio","Drosophila melanogaster","Caenorhabditis elegans",
    "Saccharomyces cerevisiae","Arabidopsis thaliana")
    names(org.list) <- c("9606","10090","10116","7955","7227","6239","559292",
    "3702")
    return(org.list[as.character(taxid)])
}
