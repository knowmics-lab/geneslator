library(testthat)

context("Testing KEGG pathway queries in GeneslatorDb")

skip_if_no_internet <- function() {
  skip_if_not(curl::has_internet(), "No internet connection available")
}

# TP53 (Homo sapiens): ENTREZID = "7157", noto membro stabile della pathway
# "path:hsa04115" ("p53 signaling pathway"), usato qui come riferimento
# biologico stabile su cui ancorare le asserzioni.

test_that("select retrieves KEGGPATH and KEGGPATHNAME given an Entrez ID", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  # search.archives non specificato: usa il default TRUE, quindi questo test
  # copre anche la regressione del merge ENTREZID/ENTREZIDOLD (in precedenza
  # poteva restituire un risultato vuoto o incrociato in modo scorretto).
  res <- geneslator::select(org.Hsapiens.db,
                            keys = "7157",
                            columns = c("KEGGPATH", "KEGGPATHNAME"),
                            keytype = "ENTREZID"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(colnames(res), c("ENTREZID", "KEGGPATH", "KEGGPATHNAME"))
  expect_true(all(res$ENTREZID == "7157"))
  expect_true("path:hsa04115" %in% res$KEGGPATH)
  # Il suffisso con il nome dell'organismo deve essere stato rimosso
  expect_false(any(grepl("Homo sapiens", res$KEGGPATHNAME)))
  p53.name <- res$KEGGPATHNAME[res$KEGGPATH == "path:hsa04115"]
  expect_equal(p53.name, "p53 signaling pathway")
})

test_that("select returns only KEGGPATH when KEGGPATHNAME is not requested", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  res <- geneslator::select(org.Hsapiens.db,
                            keys = "7157",
                            columns = "KEGGPATH",
                            keytype = "ENTREZID"
  )
  expect_equal(colnames(res), c("ENTREZID", "KEGGPATH"))
  expect_true("path:hsa04115" %in% res$KEGGPATH)
})

test_that("select returns only KEGGPATHNAME when KEGGPATH is not requested", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  res <- geneslator::select(org.Hsapiens.db,
                            keys = "7157",
                            columns = "KEGGPATHNAME",
                            keytype = "ENTREZID"
  )
  expect_equal(colnames(res), c("ENTREZID", "KEGGPATHNAME"))
  expect_true("p53 signaling pathway" %in% res$KEGGPATHNAME)
})

test_that("select handles genes with no associated KEGG pathways", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  # ID Entrez fittizio, non associabile a nessun gene reale
  res <- geneslator::select(org.Hsapiens.db,
                            keys = "999999999",
                            columns = c("KEGGPATH", "KEGGPATHNAME"),
                            keytype = "ENTREZID"
  )
  expect_equal(nrow(res), 1)
  expect_equal(res$ENTREZID, "999999999")
  expect_true(is.na(res$KEGGPATH))
  expect_true(is.na(res$KEGGPATHNAME))
})

test_that("select converts a non-ENTREZID keytype before querying KEGG pathways", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  res <- geneslator::select(org.Hsapiens.db,
                            keys = "TP53",
                            columns = c("KEGGPATH", "KEGGPATHNAME"),
                            keytype = "SYMBOL"
  )
  expect_equal(colnames(res), c("SYMBOL", "KEGGPATH", "KEGGPATHNAME"))
  expect_true(all(res$SYMBOL == "TP53"))
  expect_true("path:hsa04115" %in% res$KEGGPATH)
  expect_equal(res$KEGGPATHNAME[res$KEGGPATH == "path:hsa04115"], "p53 signaling pathway")
})

test_that("select with keytype=SYMBOL and default search.aliases=TRUE still finds KEGG pathways (regression: SYMBOL/ALIAS merge)", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  res <- geneslator::select(org.Hsapiens.db,
                            keys = c("TP53", "BRCA1"),
                            columns = c("KEGGPATH", "KEGGPATHNAME"),
                            keytype = "SYMBOL",
                            search.aliases = TRUE
  )
  # Regressione: prima il merge tra la ricerca via SYMBOL e quella via ALIAS
  # poteva azzerare del tutto il risultato (0 righe) o incrociare righe di
  # geni diversi in modo scorretto.
  expect_true(nrow(res) > 0)
  expect_true("path:hsa04115" %in% res$KEGGPATH[res$SYMBOL == "TP53"])
  expect_false("path:hsa04115" %in% res$KEGGPATH[res$SYMBOL == "BRCA1"])
})

test_that("select retrieves Entrez IDs of genes given a KEGG pathway ID", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  res <- geneslator::select(org.Hsapiens.db,
                            keys = "hsa04115",
                            columns = "ENTREZID",
                            keytype = "KEGGPATH"
  )
  expect_s3_class(res, "data.frame")
  expect_equal(colnames(res), c("KEGGPATH", "ENTREZID"))
  expect_true("7157" %in% res$ENTREZID)
  expect_true(nrow(res) > 1) # la pathway ha più geni associati
  # Nessun gene deve comparire più di una volta (regressione bug prodotto cartesiano)
  expect_equal(nrow(res), length(unique(res$ENTREZID)))
})

test_that("select retrieves only KEGGPATHNAME given a KEGG pathway ID, without duplicating rows", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  res <- geneslator::select(org.Hsapiens.db,
                            keys = "hsa04115",
                            columns = "KEGGPATHNAME",
                            keytype = "KEGGPATH"
  )
  # Regressione: prima restituiva un data.frame vuoto (0 righe)
  expect_equal(nrow(res), 1)
  expect_equal(colnames(res), c("KEGGPATH", "KEGGPATHNAME"))
  expect_equal(res$KEGGPATHNAME, "p53 signaling pathway")
})

test_that("select retrieves both ENTREZID and KEGGPATHNAME given a KEGG pathway ID", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  res <- geneslator::select(org.Hsapiens.db,
                            keys = "hsa04115",
                            columns = c("ENTREZID", "KEGGPATHNAME"),
                            keytype = "KEGGPATH"
  )
  expect_true(all(res$KEGGPATHNAME == "p53 signaling pathway"))
  expect_true("7157" %in% res$ENTREZID)
  expect_equal(nrow(res), length(unique(res$ENTREZID)))
})

test_that("select accepts KEGG pathway IDs with or without the 'path:' prefix", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  res_no_prefix <- geneslator::select(org.Hsapiens.db,
                                      keys = "hsa04115", columns = "ENTREZID", keytype = "KEGGPATH"
  )
  res_prefix <- geneslator::select(org.Hsapiens.db,
                                   keys = "path:hsa04115", columns = "ENTREZID", keytype = "KEGGPATH"
  )
  expect_equal(sort(res_no_prefix$ENTREZID), sort(res_prefix$ENTREZID))
})

test_that("select handles KEGG pathway IDs with no associated genes", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  # ID di pathway plausibile ma inesistente
  res <- geneslator::select(org.Hsapiens.db,
                            keys = "hsa99999",
                            columns = "ENTREZID",
                            keytype = "KEGGPATH"
  )
  expect_equal(nrow(res), 1)
  expect_true(is.na(res$ENTREZID))
})

test_that("select retrieves ENSEMBL ids for a KEGG pathway with search.archives=FALSE", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  res <- geneslator::select(org.Hsapiens.db,
                            keys = "hsa04115",
                            columns = "ENSEMBL",
                            keytype = "KEGGPATH",
                            search.archives = FALSE
  )
  expect_equal(colnames(res), c("KEGGPATH", "ENSEMBL"))
  expect_true("ENSG00000141510" %in% res$ENSEMBL) # TP53
  # Nessun gene deve comparire più di una volta
  expect_equal(nrow(res), length(unique(res$ENSEMBL)))
})

test_that("select retrieves ENSEMBL ids for a KEGG pathway with search.archives=TRUE, coalescing with ENSEMBLOLD per gene", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  res <- geneslator::select(org.Hsapiens.db,
                            keys = "hsa04115",
                            columns = "ENSEMBL",
                            keytype = "KEGGPATH",
                            search.archives = TRUE
  )
  expect_equal(colnames(res), c("KEGGPATH", "ENSEMBL"))
  expect_true("ENSG00000141510" %in% res$ENSEMBL) # TP53
  # Nessun gene deve comparire più di una volta: verifica di regressione
  # per il bug del prodotto cartesiano tra ENSEMBL/ENSEMBLOLD
  expect_equal(nrow(res), length(unique(res$ENSEMBL)))
})

# ---------------------------------------------------------------------------
# mapIds() - stesse conversioni, tramite l'helper .solve_query_mapIds_kegg()
# ---------------------------------------------------------------------------

test_that("mapIds retrieves KEGGPATH ids for a gene given its Entrez ID", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  res <- geneslator::mapIds(org.Hsapiens.db,
                            keys = "7157",
                            column = "KEGGPATH",
                            keytype = "ENTREZID",
                            multiVals = "list"
  )
  expect_type(res, "list")
  expect_equal(names(res), "7157")
  expect_true("path:hsa04115" %in% res[["7157"]])
})

test_that("mapIds retrieves KEGGPATHNAME for a gene given its Entrez ID", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  res <- geneslator::mapIds(org.Hsapiens.db,
                            keys = "7157",
                            column = "KEGGPATHNAME",
                            keytype = "ENTREZID",
                            multiVals = "list"
  )
  expect_true("p53 signaling pathway" %in% res[["7157"]])
})

test_that("mapIds converts a non-ENTREZID keytype before querying KEGG pathways (default search.aliases=TRUE)", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  res <- geneslator::mapIds(org.Hsapiens.db,
                            keys = c("TP53", "BRCA1"),
                            column = "KEGGPATH",
                            keytype = "SYMBOL",
                            multiVals = "list"
  )
  expect_equal(names(res), c("TP53", "BRCA1"))
  expect_true("path:hsa04115" %in% res[["TP53"]])
  expect_false("path:hsa04115" %in% res[["BRCA1"]])
})

test_that("mapIds retrieves Entrez IDs of genes given a KEGG pathway ID", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  res <- geneslator::mapIds(org.Hsapiens.db,
                            keys = "hsa04115",
                            column = "ENTREZID",
                            keytype = "KEGGPATH",
                            multiVals = "list"
  )
  expect_type(res, "list")
  expect_true("7157" %in% res[["hsa04115"]])
})

test_that("mapIds retrieves the pathway name given a KEGG pathway ID", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  res <- geneslator::mapIds(org.Hsapiens.db,
                            keys = "hsa04115",
                            column = "KEGGPATHNAME",
                            keytype = "KEGGPATH",
                            multiVals = "first"
  )
  expect_equal(unname(res["hsa04115"]), "p53 signaling pathway")
})

test_that("mapIds accepts KEGG pathway IDs with or without the 'path:' prefix", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  res_no_prefix <- geneslator::mapIds(org.Hsapiens.db,
                                      keys = "hsa04115", column = "ENTREZID", keytype = "KEGGPATH",
                                      multiVals = "list"
  )
  res_prefix <- geneslator::mapIds(org.Hsapiens.db,
                                   keys = "path:hsa04115", column = "ENTREZID", keytype = "KEGGPATH",
                                   multiVals = "list"
  )
  expect_equal(sort(res_no_prefix[["hsa04115"]]), sort(res_prefix[["path:hsa04115"]]))
})

test_that("mapIds handles genes with no associated KEGG pathways", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  res <- geneslator::mapIds(org.Hsapiens.db,
                            keys = "999999999",
                            column = "KEGGPATH",
                            keytype = "ENTREZID",
                            multiVals = "first"
  )
  expect_true(is.na(unname(res["999999999"])))
})

test_that("mapIds handles KEGG pathway IDs with no associated genes", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  res <- geneslator::mapIds(org.Hsapiens.db,
                            keys = "hsa99999",
                            column = "ENTREZID",
                            keytype = "KEGGPATH",
                            multiVals = "first"
  )
  expect_true(is.na(unname(res["hsa99999"])))
})

test_that("mapIds retrieves an arbitrary column (e.g. SYMBOL) for genes of a KEGG pathway", {
  skip_if_no_internet()
  GeneslatorDb("Homo sapiens")
  res <- geneslator::mapIds(org.Hsapiens.db,
                            keys = "hsa04115",
                            column = "SYMBOL",
                            keytype = "KEGGPATH",
                            multiVals = "list"
  )
  expect_type(res, "list")
  expect_true("TP53" %in% res[["hsa04115"]])
})
