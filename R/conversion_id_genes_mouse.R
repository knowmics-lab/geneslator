#' Convert Gene IDs using Mouse Database
#'
#' This function filters an SQLite dataframe for mouse genes based on a list of values and selects specific columns.
#'
#' @param list_ensembl A list of Ensembl gene IDs to filter.
#' @param keyid The column in the dataframe to use for filtering (e.g., "ENSEMBL").
#' @param outputid A vector of column names to return.
#' @return A dataframe containing the filtered and selected results.
#' @details This function connects to the SQLite database specific for mouse genes (`mgeneslator.sqlite`) 
#'          stored in the `extdata` directory of the package. It checks for the table existence and 
#'          performs filtering and selection based on the provided parameters.
#' @examples
#' \dontrun{
#' # Example usage
#' list_ensembl <- c("ENSMUSG00000064337", "ENSMUSG00000029580")
#' keyid <- "ENSEMBL"
#' outputid <- c("SYMBOL", "ENTREZID")
#' 
#' result <- conversion_id_genes_mouse(list_ensembl, keyid, outputid)
#' print(result)
#' }
#' @export

conversion_id_genes_mouse<-function (list_ensembl, keyid, outputid) 
{
  library(dplyr)
  library(dbplyr)
  library(RSQLite)
  table_name <- "mgeneslator"
  sqlite_path_mouse <- system.file("extdata", "mgeneslator.sqlite", 
                             package = "Geneslator")
  if (!file.exists(sqlite_path_mouse)) {
    stop("The SQLite database file does not exist: ", sqlite_path_mouse)
  }
  conn <- DBI::dbConnect(SQLite(), sqlite_path_mouse)
  on.exit(dbDisconnect(conn), add = TRUE)
  tables <- dbListTables(conn)
  if (!table_name %in% tables) {
    stop("Table ", table_name, " does not exist in the database.")
  }
  table <- tbl(conn, table_name)
  keyid_sym <- rlang::sym(keyid)
  outputid_syms <- rlang::syms(outputid)
  filtered_table <- table %>% dplyr::filter(!!keyid_sym %in% list_ensembl) %>% 
    dplyr::select(!!!outputid_syms) %>% collect()
  return(filtered_table)
}