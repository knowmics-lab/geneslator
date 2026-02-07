#' @title Available organisms in geneslator
#'
#' @description
#' `availableOrganisms` lists all possible organisms that can be queried in the 
#' \pkg{geneslator} package.
#'
#' @return `availableOrganisms` returns a character vector.
#'
#' @seealso \code{\link{GeneslatorDb}}
#'
#' @examples
#' # Get the list of all organisms supported by geneslator
#' availableOrganisms()
#' @export
availableOrganisms <- function() {
    #url <- paste0("https://api.github.com/repos/",
    #"GMicale/geneslator/releases/latest")
    #tags <- jsonlite::fromJSON(url)
    #latest.organisms <- gsub(".sqlite","",tags$assets$name)
    latest.organisms <- c("Human","Mouse","Rat","Yeast","Worm","Fly",
    "Zebrafish","Arabidopsis")
    return(latest.organisms)
}

#' Get local path of annotation database
#' @keywords internal
#' @noRd
.get_annotation_db <- function(organism, force_download = FALSE, 
check_version = TRUE) {
    list.organism <- availableOrganisms()
    if (!organism %in% list.organism) {
        stop("Organism '", organism, "' not supported.\n",
        "See availableOrganisms() to view the complete list.", call. = FALSE)
    }
    #Check if annotation database needs to be updated
    cache_dir <- tools::R_user_dir("geneslator", which = "cache")
    if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE)
    }
    db_filename <- paste0(organism,".sqlite")
    db_path <- file.path(cache_dir, db_filename)
    if (check_version && file.exists(db_path) && !force_download) {
        update_info <- .needs_update(organism, force_refresh = FALSE)
        version_info <- update_info$online_version
        if (update_info$needs_update) {
            message("Available update for ",organism," database")
            message("Local version: ", update_info$local_version)
            message("Available version: ", update_info$online_version)
            if (interactive()) {
                response <- readline("Do you want to update it now? (s/n): ")
                if (tolower(trimws(response)) == "s") {
                    force_download <- TRUE
                } else {
                    message("Use existing local version.")
                }
            } else {
                message("Non-interactive mode: use local version.")
            }
        }
    }
    #If annotation file does not exist, prepare for download
    if(!file.exists(db_path)){
        message("Database not found in cache: ", db_path)
        message("Downloading it...")
        version_info <- .get_remote_db_version(organism)
    }
    #If annotation file exists and download is not forced, return file path
    if (file.exists(db_path) && !force_download) {
        message("Database found in cache: ", db_path)
        return(db_path)
    }
    #Download annotation file and save information about database version
    .download_annotation_db(organism, db_path, version_info)
    .save_local_db_version(organism, version_info)
    return(db_path)
}

#' Download annotation database from remote repository on GitHub
#' @keywords internal
#' @noRd
.download_annotation_db <- function(organism, dest_path, version_info) {
    #URL of remote repository
    url <- paste0(
    "https://github.com/GMicale/geneslator/releases/download/GeneslatorDb/",
    organism,".sqlite")
    message("========================================")
    message("Download database for ", organism)
    message("Version: ", version_info)
    message("========================================")
    message("This can take few minutes...")
    message("")
    #Create a temporary file for a secure download
    temp_file <- paste0(dest_path, ".tmp")
    #Increase timeout for download to 10 minutes
    original_timeout <- getOption("timeout")
    options(timeout = 600)
    tryCatch({
        #Download with progress bar
        utils::download.file(url = url, destfile = temp_file, mode = "wb",
        quiet = FALSE, method = "auto")
        #Check file integrity
        if (!file.exists(temp_file) || file.size(temp_file) == 0) {
            stop("Downloaded file is empty or does not exists")
        }
        #Move file to final destination
        file.rename(temp_file, dest_path)
        message("\nDownload completed successfully!")
        message("File: ", dest_path)
    }, error = function(e) {
        #Clean data in case of error
        if (file.exists(temp_file)) {
            file.remove(temp_file)
        }
        msg <- sprintf("Failed to download database for organism: %s\nURL: 
        %s\nDetails: %s", organism, url, e$message)
        stop(msg, call. = FALSE) 
    })
}
