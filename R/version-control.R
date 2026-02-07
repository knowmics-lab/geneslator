#' @importFrom utils read.table write.table
NULL

#' Get version of remote annotation database
#' @keywords internal
#' @noRd
.get_remote_db_version <- function(organism) {
    url <- paste0("https://github.com/knowmics-lab/geneslator/releases/download/",
    "GeneslatorDb/",organism,".version")
    return(read.table(url(url))$V1)
}

#' Get version of local annotation database
#' @keywords internal
#' @noRd
.get_local_db_version <- function(organism) {
    cache_dir <- tools::R_user_dir("geneslator", which = "cache")
    version_file <- file.path(cache_dir, paste0(organism, ".version"))
    if (!file.exists(version_file)) {
        return(NULL)
    }
    tryCatch({
        return(read.table(version_file)$V1)
    }, error = function(e) {
        return(NULL)
    })
}

#' Save version of local annotation database
#' @keywords internal
#' @noRd
.save_local_db_version <- function(organism, version_info) {
    cache_dir <- tools::R_user_dir("geneslator", which = "cache")
    version_file <- file.path(cache_dir, paste0(organism, ".version"))
    write.table(version_info, version_file, col.names=FALSE, quote=FALSE, 
                row.names=FALSE)
}

#' Check if annotation database needs to be updated 
#' @keywords internal
#' @noRd
.needs_update <- function(organism, force_refresh = FALSE) {
    #Get remote versions
    remote_version <- .get_remote_db_version(organism)
    #Get local version
    local_version <- .get_local_db_version(organism)
    #There is no local copy of the database, database is not in cache
    if(is.null(local_version)) {
        return(list(needs_update = FALSE, reason = "not_cached"))
    }
    #Compare versions
    if (local_version != remote_version) {
        return(list(needs_update = TRUE, reason = "version_mismatch",
        local_version = local_version,
        online_version = remote_version))
    }
    return(list(needs_update = FALSE, reason = "up_to_date"))
}

#' Update a specific annotation database
#' @keywords internal
#' @noRd
.update_database <- function(organism) {
    list.organisms <- availableOrganisms()
    if (!organism %in% list.organisms) {
        stop("Organism not valid: ", organism, call. = FALSE)
    }
    message("Updating database for ", organism, "...")
    #Remove old local copy of the annotation database
    cache_dir <- tools::R_user_dir("geneslator", which = "cache")
    db_file <- file.path(cache_dir, paste0(organism, ".sqlite"))
    version_file <- file.path(cache_dir, paste0(organism, ".version"))
    if (file.exists(db_file)) {
        file.remove(db_file)
    }
    if (file.exists(version_file)) {
        file.remove(version_file)
    }
    #Download new version of the annotation database
    .get_annotation_db(organism, force_download = TRUE, check_version = FALSE)
    message("Database ", organism, " updated correctly!")
    invisible(NULL)
}
