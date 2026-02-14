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
.loadAnnotationDb <- function(db.name, remote.version, remote.md5) {
    #Check if annotation db is present in local package cache
    cache.dir <- tools::R_user_dir("geneslator", which = "cache")
    db.file <- paste0(cache.dir,"/",db.name,".sqlite")
    if(file.exists(db.file)){
        #Annotation db is present in local package cache
        #Check if it is updated
        db.version.file <- paste0(cache.dir,"/",db.name,".version")
        local.version <- read.table(db.version.file)$V1
        if(local.version!=remote.version) {
            #Old db version. Ask if downloading new version
            message("Available update for ",db.name," database")
            message("Local version: ", local.version)
            message("Available version: ", remote.version)
            if (interactive()) {
                response <- readline("Do you want to update it now? (y/n): ")
                if (tolower(trimws(response)) == "y") {
                    .downloadAnnotationDb(db.name,cache.dir,remote.version,
                    remote.md5)
                } else {
                    message("Use existing local version.")
                }
            } else {
                message("Non-interactive mode: use local version.")
            }
        } else {
            if(exists(db.name)){
                DBI::dbDisconnect(AnnotationDbi::dbconn(get(db.name)@db))
            }
            message("Loaded database found in cache: ", db.file)
        }
    } else {
        #Database not in cache. Dowload automatically last version
        message("Database not found in cache: ", db.file)
        .downloadAnnotationDb(db.name,cache.dir,remote.version,remote.md5)
    }
    #Load annotation database as OrgDb object
    org.db <- suppressPackageStartupMessages(AnnotationDbi::loadDb(db.file))
    return(org.db)
}

#' Download annotation database from remote repository on GitHub
#' @keywords internal
#' @noRd
.downloadAnnotationDb <- function(db.name,cache.dir,remote.version,
remote.md5) {
    #URL of remote repository
    url <- paste0("https://github.com/knowmics-lab/",
    "geneslator/releases/download/GeneslatorDb/",db.name,".sqlite")
    message("========================================")
    message("Download database ", db.name)
    message("Version: ", remote.version)
    message("========================================")
    message("This can take few minutes...")
    #Increase timeout for download to 10 minutes
    options(timeout = 600)
    tryCatch({
        #Download annotation database
        temp.file <- tempfile(fileext=".sqlite")
        utils::download.file(url = url, mode = "wb", quiet = FALSE,
        destfile = temp.file, method = "auto")
        #Check file integrity
        local.md5 <- tools::md5sum(temp.file)
        if (local.md5!=remote.md5) {
            file.remove(temp.file)
            stop("Downloaded annotation db file incomplete or wrong")
        }
        local.file.name <- paste0(cache.dir,"/",db.name,".sqlite")
        if(exists(db.name)){
            DBI::dbDisconnect(AnnotationDbi::dbconn(get(db.name)@db))
        }
        file.rename(temp.file, local.file.name)
        #Update version file
        write.table(remote.version, paste0(cache.dir,"/",db.name,".version"), 
        col.names=FALSE, quote=FALSE, row.names=FALSE)
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
