# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(geneslator.package)

test_check("geneslator.package")

# tests/testthat/test_conversion_id_genes.R
test_that("conversion_id_genes works correctly", {
  result <- conversion_id_genes(list_ensembl = c("ENSG00000139618"), keyid = "ENSEMBL", outputid = c("SYMBOL"))
  expect_true(nrow(result) > 0)
  expect_equal(colnames(result), c("SYMBOL"))
})

