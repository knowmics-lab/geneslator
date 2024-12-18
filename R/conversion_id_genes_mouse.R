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


conversion_id_genes_mouse <- function(list_ensembl, keyid, outputid) {
  library(RSQLite)
  library(dplyr)
  
  # Percorso al database mouse
  sqlite_path_mouse <- system.file("extdata", "mgeneslator.sqlite", package = "Geneslator")
  
  # Verifica che il file esista
  if (!file.exists(sqlite_path_mouse)) {
    stop("Il database mgeneslator.sqlite non è stato trovato.")
  }
  
  # Connessione al database
  conn <- dbConnect(SQLite(), sqlite_path_mouse)
  on.exit(dbDisconnect(conn), add = TRUE)
  
  # Nome della tabella
  table_name <- "mgeneslator" 
  
  # Verifica se la tabella esiste
  tables <- dbListTables(conn)
  if (!(table_name %in% tables)) {
    stop(paste("La tabella", table_name, "non esiste nel database."))
  }
  
  # Riferimento alla tabella
  table <- tbl(conn, table_name)
  
  # Filtra i dati in base ai valori in list_ensembl
  filtered_table <- table %>%
    filter(!!sym(keyid) %in% list_ensembl) %>%
    select(all_of(outputid)) %>%
    collect()
  
  return(filtered_table)
}
