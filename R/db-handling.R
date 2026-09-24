# ========================================
# File: R/db-handling.R
# ========================================

#' @importFrom utils read.table write.table download.file
#' @importFrom methods new is
#' @importFrom jsonlite fromJSON
#' @importFrom DBI dbDisconnect
#' @importFrom curl has_internet
#' @importFrom zen4R ZenodoManager
#' @importFrom BiocFileCache BiocFileCache bfcquery bfcadd bfcpath bfcremove
NULL

#' Load from local cache annotation database
#' If annotation db is not present, download it automatically before loading
#' If annotation db is not up to date, eventually download it before loading
#' @keywords internal
#' @noRd
.loadAnnotationDb <- function(db.name, remote.version, remote.md5, doi, is.latest) {
  bfc <- BiocFileCache::BiocFileCache(ask = FALSE)
  hits <- if (is.latest) {
    BiocFileCache::bfcquery(bfc, paste0(db.name, ".*_latest"), field = "rname")
  } else {
    BiocFileCache::bfcquery(bfc, paste0(db.name, "_", remote.version), 
                            field = "rname", exact = TRUE)
  }
  if (nrow(hits) > 0) {
    if (is.latest) {
      local.version <- hits$rname
      local.version <- strsplit(local.version, ".db_|_latest")[[1]][2]
      if (curl::has_internet() && local.version != remote.version) {
        message("Available update for ", db.name, " database")
        message("Local version: ", local.version)
        message("Available version: ", remote.version)
        if (interactive()) {
          response <- readline("Do you want to update it? (y/n): ")
          if (tolower(trimws(response)) == "y") {
            BiocFileCache::bfcremove(bfc, hits$rid)
            .downloadAnnotationDb(bfc, db.name, remote.version, remote.md5, doi, is.latest)
          } else {
            message("Use existing local version.")
          }
        }
      } else {
        if (exists(db.name)) {
          DBI::dbDisconnect(AnnotationDbi::dbconn(get(db.name)@db))
        }
        message(
          "Loaded database found in cache: ",
          BiocFileCache::bfcpath(bfc, hits$rid)
        )
      }
    } else {
      if (exists(db.name)) {
        DBI::dbDisconnect(AnnotationDbi::dbconn(get(db.name)@db))
      }
      message(
        "Loaded database found in cache: ",
        BiocFileCache::bfcpath(bfc, hits$rid)
      )
    }
  } else {
    message("Database not found in cache")
    if (!curl::has_internet()) {
      stop(
        "Failed to download database ", db.name,
        " from remote repository.\nNo internet connection"
      )
    }
    .downloadAnnotationDb(bfc, db.name, remote.version, remote.md5, doi, is.latest)
  }
  # Retrieve path from cache
  hits <- if (is.latest) {
    BiocFileCache::bfcquery(bfc, paste0(db.name, ".*_latest"), field = "rname")
  } else {
    BiocFileCache::bfcquery(bfc, paste0(db.name, "_", remote.version), 
                            field = "rname", exact = TRUE)
  }
  if (nrow(hits) == 0) {
    stop("Database file not found in cache after download.")
  }
  db.file <- BiocFileCache::bfcpath(bfc, hits$rid)
  org.db <- suppressPackageStartupMessages(AnnotationDbi::loadDb(db.file))
  return(org.db)
}

#' Download annotation database from remote repository on Zenodo
#' @keywords internal
#' @noRd
.downloadAnnotationDb <- function(bfc, db.name, remote.version, remote.md5, db.doi, is.latest) {
  message("========================================")
  message("Download database ", db.name)
  message("Version: ", remote.version)
  message("========================================")
  message("This can take few minutes...")
  options(timeout = 3600)
  tryCatch(
    {
      # Access Zenodo data by DOI
      zenodo <- zen4R::ZenodoManager$new()
      record <- suppressMessages(zenodo$getRecordByDOI(db.doi))
      file.url <- paste0(
        "https://zenodo.org/records/", record$id, "/files/",
        db.name, ".sqlite"
      )
      cache.name <- if (is.latest) {
        paste0(db.name, "_", remote.version, "_latest")
      } else {
        paste0(db.name, "_", remote.version)
      }
      #Download file
      temp.file <- file.path(tempdir(), paste0(cache.name, ".sqlite"))
      utils::download.file(url = file.url, destfile = temp.file, 
                           mode = "wb", quiet = FALSE, method = "auto")
      #Check file integrity
      local.md5 <- tools::md5sum(temp.file)
      if (local.md5 != remote.md5) {
        file.remove(temp.file)
        stop("Incomplete download of annotation db file")
      }
      #Clean old versions before adding new file
      if(is.latest){
        existing <- BiocFileCache::bfcquery(bfc, paste0(db.name, ".*_latest"), field = "rname")
        if (nrow(existing) > 0) {
          BiocFileCache::bfcremove(bfc, existing$rid)
        }
      }
      #Add file to BiocFileCache
      db.file <- BiocFileCache::bfcadd(bfc, rname = cache.name, fpath = temp.file, action = "copy")
      file.remove(temp.file)
      message("Download completed successfully!")
      message("File: ", db.file)
    },
    error = function(e) {
      stop("Failed to download database: ", db.name, "\nDetails: ", e$message, call. = FALSE)
    }
  )
}

#' Map Taxonomy ID to organism
#' @keywords internal
#' @noRd
.getOrgFromTaxid <- function(taxid) {
  org.list <- c(
    "Apis mellifera","Arabidopsis thaliana","Bos taurus","Brassica napus","Brassica oleracea",
    "Caenorhabditis elegans","Canis lupus familiaris","Danio rerio","Drosophila melanogaster",
    "Escherichia coli K12 MG1655","Gallus gallus","Homo sapiens","Lupinus angustifolius","Macaca mulatta",
    "Mus musculus","Oryza sativa","Phaseolus vulgaris","Rattus norvegicus","Saccharomyces cerevisiae",
    "Schizosaccharomyces pombe","Solanum lycopersicum","Vitis vinifera","Xenopus laevis","Xenopus tropicalis",
    "Zea mays"
  )
  names(org.list) <- c(
    "7460","3702","9913","3708","109376","6239","9615","7955","7227","511145","9031","9606","3871","9544",
    "10090","39947","3885","10116","559292","284812","4081","29760","8355","8364","4577"
  )
  return(org.list[as.character(taxid)])
}
