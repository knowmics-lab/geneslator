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
.loadAnnotationDb <- function(db.name,remote.version,remote.md5,doi,is.latest){
    cache.dir <- tools::R_user_dir("geneslator", which = "cache")
    if (!dir.exists(cache.dir)) { dir.create(cache.dir, recursive = TRUE) }
    db.file <- if (is.latest) {
    list.files(cache.dir,pattern=paste0(db.name,".*_latest"),full.names=TRUE)
    } else {
    list.files(cache.dir,pattern=paste0(db.name,"_",remote.version,".sqlite"), 
    full.names = TRUE)
    }
    if(length(db.file)>0){
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
                        remote.md5,doi,is.latest,is.update=TRUE)
                    } else {
                        message("Use existing local version.")
                    }
                }
            } else {
if(exists(db.name)){DBI::dbDisconnect(AnnotationDbi::dbconn(get(db.name)@db))
                }
                message("Loaded database found in cache: ", db.file)
            }
        } else {
if(exists(db.name)) DBI::dbDisconnect(AnnotationDbi::dbconn(get(db.name)@db))
            message("Loaded database found in cache: ", db.file)
        }
    } else {
        message("Database not found in cache")
        if(!curl::has_internet()){
            msg <- paste0("Failed to download database ",db.name," from ",
            "remote repository.\nNo internet connection")
            stop(msg)
        }
        .downloadAnnotationDb(db.name,cache.dir,remote.version,remote.md5,doi,
        is.latest,is.update=FALSE)
    }
    db.file <- if (is.latest) {
    list.files(cache.dir,pattern=paste0(db.name,".*_latest"),full.names=TRUE)
    } else {
    list.files(cache.dir,pattern=paste0(db.name,"_",remote.version,".sqlite"),
    full.names=TRUE)
    }
    if(length(db.file)== 0||is.na(db.file[1])) {
    stop("Database file not found in cache after download: ",cache.dir)}
    db.file <- db.file[1]
    org.db <- suppressPackageStartupMessages(AnnotationDbi::loadDb(db.file))
    return(org.db)
}

#' Download annotation database from remote repository on GitHub
#' @keywords internal
#' @noRd
.downloadAnnotationDb <- function(db.name,cache.dir,remote.version,
remote.md5, db.doi, is.latest, is.update) {
    message("========================================")
    message("Download database ", db.name)
    message("Version: ", remote.version)
    message("========================================")
    message("This can take few minutes...")
    #Increase timeout for download to 10 minutes
    options(timeout = 3600)
    tryCatch({
        #Access Zenodo data by DOI
        zenodo <- zen4R::ZenodoManager$new()
        record <- suppressMessages(zenodo$getRecordByDOI(db.doi))
        #Download annotation database
        file.url <- paste0("https://zenodo.org/records/",record$id,"/files/",
        db.name,".sqlite")
        temp.file <- file.path(tempdir(), paste0(db.name, ".sqlite"))
        utils::download.file(url=file.url,destfile=temp.file,mode="wb",
        quiet=FALSE,method="auto")
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
        if(!file.copy(temp.file,local.file.name,overwrite=TRUE)) {
        stop("Failed to move db file to cache directory: ",local.file.name)
        }
        file.remove(temp.file)
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
