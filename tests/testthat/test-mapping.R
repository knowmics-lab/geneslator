library(testthat)

context("Testing GeneslatorDb methods")

test_that("GeneslatorDb function is correct", {
  human.db <- GeneslatorDb("Human")
  expect_s4_class(human.db, "GeneslatorDb")
  expect_s4_class(human.db@db, "OrgDb")
})

test_that("availableOrganisms returns the correct data", {
  org_list <- availableOrganisms()
  expect_type(org_list, "character")
  expect_true("Human" %in% org_list)
})

test_that("keytypes and columns methods return valid identifiers", {
  human.db <- GeneslatorDb("Human")
  kt <- geneslator::keytypes(human.db)
  cols <- geneslator::columns(human.db)
  expect_type(kt, "character")
  expect_true("SYMBOL" %in% kt)
  expect_true("GO" %in% kt)
  # Verifica che GID sia stato rimosso come da logica in mappingFunctions.R
  expect_false("GID" %in% kt)
  expect_type(cols, "character")
  expect_true("ENTREZID" %in% cols)
})

test_that("select method handles basic queries and aliases", {
  human.db <- GeneslatorDb("Human")
  keys_test <- c("TP53", "BRCA1")
  res <- geneslator::select(human.db, 
                keys = keys_test, 
                columns = c("ENTREZID", "ENSEMBL"), 
                keytype = "SYMBOL")
  expect_s3_class(res, "data.frame")
  expect_equal(colnames(res), c("SYMBOL", "ENTREZID", "ENSEMBL"))
  expect_true(nrow(res) >= 2)
})

test_that("select method handles search.aliases logic", {
  human.db <- GeneslatorDb("Human")
  res_loc <- geneslator::select(human.db, 
                    keys = "LOC12345", 
                    columns = "ENTREZID", 
                    keytype = "SYMBOL")
  if(any(res_loc$SYMBOL == "LOC12345" & is.na(res_loc$ENTREZID))){
    expect_true(any(res_loc$ENTREZID == "12345", na.rm = TRUE))
  }
})

test_that("mapIds method returns expected formats", {
  human.db <- GeneslatorDb("Human")
  keys_test <- c("TP53", "BRCA1")
  res_first <- geneslator::mapIds(human.db, 
                      keys = keys_test, 
                      column = "ENTREZID", 
                      keytype = "SYMBOL", 
                      multiVals = "first")
  expect_type(res_first, "character")
  expect_equal(length(res_first), length(keys_test))
  expect_equal(names(res_first), keys_test)
  # Test multiVals = "list"
  res_list <- geneslator::mapIds(human.db, 
                     keys = keys_test, 
                     column = "ENTREZID", 
                     keytype = "SYMBOL", 
                     multiVals = "list")
  expect_type(res_list, "list")
})

test_that("Error handling in select with non-existent keys", {
  human.db <- GeneslatorDb("Human")
  fake_keys <- "FAKE_GENE"
  res <- geneslator::select(human.db, keys = fake_keys, columns = "ENTREZID", keytype = "SYMBOL")
  expect_true(all(is.na(res$ENTREZID)))
})
