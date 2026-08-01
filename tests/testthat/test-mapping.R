library(testthat)

context("Testing GeneslatorDb methods")

skip_if_no_internet <- function() {
  skip_if_not(curl::has_internet(), "No internet connection available")
}

test_that("GeneslatorDb function is correct", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  expect_s4_class(org.Hsapiens.db, "GeneslatorDb")
  expect_s4_class(org.Hsapiens.db@db, "OrgDb")
  expect_equal(AnnotationDbi::species(org.Hsapiens.db@db), "Homo sapiens")
})

test_that("availableDatabases returns the correct data", {
  skip_if_no_internet()
  db_list <- availableDatabases()
  expect_type(db_list, "list")
  expect_true("Homo sapiens" %in% db_list$Organism)
  expect_true(all(c("Name", "Organism", "TaxID", "MD5", "Version", "DOI") %in% colnames(db_list)))
})

test_that("keytypes and columns methods return valid identifiers", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  kt <- geneslator::keytypes(org.Hsapiens.db)
  cols <- geneslator::columns(org.Hsapiens.db)
  expect_type(kt, "character")
  expect_true("SYMBOL" %in% kt)
  expect_true("GO" %in% kt)
  # Verifica che GID sia stato rimosso come da logica in mappingFunctions.R
  expect_false("GID" %in% kt)
  expect_type(cols, "character")
  expect_true("ENTREZID" %in% cols)
  # KEGGPATH/KEGGPATHNAME devono essere esposte come colonne interrogabili
  expect_true(all(c("KEGGPATH", "KEGGPATHNAME") %in% cols))
})

test_that("select method handles basic queries and returns correct mappings", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  keys_test <- c("TP53", "BRCA1")
  res <- geneslator::select(org.Hsapiens.db,
    keys = keys_test,
    columns = c("ENTREZID", "ENSEMBL"),
    keytype = "SYMBOL"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(colnames(res), c("SYMBOL", "ENTREZID", "ENSEMBL"))
  expect_equal(nrow(res), 2)
  # Verifica valori attesi (mapping stabili e noti)
  expect_equal(res$ENTREZID[res$SYMBOL == "TP53"], "7157")
  expect_equal(res$ENTREZID[res$SYMBOL == "BRCA1"], "672")
  expect_equal(res$ENSEMBL[res$SYMBOL == "TP53"], "ENSG00000141510")
  expect_equal(res$ENSEMBL[res$SYMBOL == "BRCA1"], "ENSG00000012048")
})

test_that("select correctly maps unmapped LOC-style symbols to Entrez IDs", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  res_loc <- geneslator::select(org.Hsapiens.db,
    keys = "LOC12345",
    columns = "ENTREZID",
    keytype = "SYMBOL"
  )
  # Il fallback per simboli "LOC<id>" non trovati deve sempre attivarsi,
  # indipendentemente dal contenuto del database (logica locale, non di rete)
  expect_equal(nrow(res_loc), 1)
  expect_equal(res_loc$SYMBOL, "LOC12345")
  expect_equal(res_loc$ENTREZID, "12345")
})

test_that("mapIds method returns expected formats and values", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  keys_test <- c("TP53", "BRCA1")
  res_first <- geneslator::mapIds(org.Hsapiens.db,
    keys = keys_test,
    column = "ENTREZID",
    keytype = "SYMBOL",
    multiVals = "first"
  )
  expect_type(res_first, "character")
  expect_equal(length(res_first), length(keys_test))
  expect_equal(names(res_first), keys_test)
  expect_equal(unname(res_first["TP53"]), "7157")
  expect_equal(unname(res_first["BRCA1"]), "672")

  # Test multiVals = "list"
  res_list <- geneslator::mapIds(org.Hsapiens.db,
    keys = keys_test,
    column = "ENTREZID",
    keytype = "SYMBOL",
    multiVals = "list"
  )
  expect_type(res_list, "list")
  expect_equal(names(res_list), keys_test)
  expect_true("7157" %in% res_list[["TP53"]])
})

test_that("Error handling in select with non-existent keys", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  fake_keys <- "FAKE_GENE"
  res <- geneslator::select(org.Hsapiens.db,
    keys = fake_keys,
    columns = "ENTREZID", keytype = "SYMBOL"
  )
  expect_equal(nrow(res), 1)
  expect_equal(res$SYMBOL, "FAKE_GENE")
  expect_true(is.na(res$ENTREZID))
})
