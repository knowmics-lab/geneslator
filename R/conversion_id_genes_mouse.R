#' Convert Gene IDs using Mouse Database
#'
#' This function filters an SQLite dataframe for mouse genes based on a list of values and selects specific columns.
#'
#' @param list_ensembl A vector of Ensembl gene IDs to filter.
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
  library(dplyr)
  # Define the name of the table in the SQLite database
  table_name <- "mgeneslator"
  
  # Get the path to the SQLite database included in the package
  sqlite_path_mouse <- system.file("extdata", "mgeneslator.sqlite", package = "Geneslator")
  
  # Check if the SQLite database file exists
  if (sqlite_path_mouse == "") {
    stop("The SQLite database file was not found. Ensure it is included in the package under inst/extdata/")
  }
  
  # Connect to the SQLite database
  conn <- DBI::dbConnect(RSQLite::SQLite(), sqlite_path_mouse)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)  # Ensure the connection is closed after execution
  
  # Check if the specified table exists in the database
  tables <- DBI::dbListTables(conn)
  if (!table_name %in% tables) {
    stop("Table '", table_name, "' does not exist in the database.")
  }
  
  # Load the table using dbplyr
  table <- dplyr::tbl(conn, table_name)
  
  # Check if the keyid column exists in the database
  db_columns <- colnames(table)
  if (!(keyid %in% db_columns)) {
    stop("Column '", keyid, "' not found in the database table.")
  }
  
  # Check if the requested output columns exist in the database
  missing_cols <- setdiff(outputid, db_columns)
  if (length(missing_cols) > 0) {
    stop("The following columns do not exist in the database: ", paste(missing_cols, collapse = ", "))
  }
  
  # Apply filtering and select the requested columns
  filtered_table <- table %>%
    dplyr::filter(.data[[keyid]] %in% list_ensembl) %>%
    dplyr::select(dplyr::all_of(outputid)) %>%
    dplyr::collect()
  
  return(filtered_table)
}
