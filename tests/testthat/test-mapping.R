library(testthat)

context("Testing GeneslatorDb methods")

test_that("GeneslatorDb function is correct", {
  GeneslatorDb("Homo sapiens")
  expect_s4_class(org.Hsapiens.db, "GeneslatorDb")
  expect_s4_class(org.Hsapiens.db@db, "OrgDb")
})

test_that("availableDatabases returns the correct data", {
  db_list <- availableDatabases()
  expect_type(db_list, "list")
  expect_true("Homo sapiens" %in% db_list$Organism)
})

test_that("keytypes and columns methods return valid identifiers", {
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
})

test_that("select method handles basic queries and aliases", {
  GeneslatorDb("Homo sapiens")
  keys_test <- c("TP53", "BRCA1")
  res <- geneslator::select(org.Hsapiens.db, 
                keys = keys_test, 
                columns = c("ENTREZID", "ENSEMBL"), 
                keytype = "SYMBOL")
  expect_s3_class(res, "data.frame")
  expect_equal(colnames(res), c("SYMBOL", "ENTREZID", "ENSEMBL"))
  expect_true(nrow(res) >= 2)
})

test_that("select method handles search.aliases logic", {
  GeneslatorDb("Homo sapiens")
  res_loc <- geneslator::select(org.Hsapiens.db, 
                    keys = "LOC12345", 
                    columns = "ENTREZID", 
                    keytype = "SYMBOL")
  if(any(res_loc$SYMBOL == "LOC12345" & is.na(res_loc$ENTREZID))){
    expect_true(any(res_loc$ENTREZID == "12345", na.rm = TRUE))
  }
})

test_that("mapIds method returns expected formats", {
  GeneslatorDb("Homo sapiens")
  keys_test <- c("TP53", "BRCA1")
  res_first <- geneslator::mapIds(org.Hsapiens.db, 
                      keys = keys_test, 
                      column = "ENTREZID", 
                      keytype = "SYMBOL", 
                      multiVals = "first")
  expect_type(res_first, "character")
  expect_equal(length(res_first), length(keys_test))
  expect_equal(names(res_first), keys_test)
  # Test multiVals = "list"
  res_list <- geneslator::mapIds(org.Hsapiens.db, 
                     keys = keys_test, 
                     column = "ENTREZID", 
                     keytype = "SYMBOL", 
                     multiVals = "list")
  expect_type(res_list, "list")
})

test_that("Error handling in select with non-existent keys", {
  GeneslatorDb("Homo sapiens")
  fake_keys <- "FAKE_GENE"
  res <- geneslator::select(org.Hsapiens.db, keys = fake_keys, 
  columns = "ENTREZID", keytype = "SYMBOL")
  expect_true(all(is.na(res$ENTREZID)))
})
