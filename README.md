# geneslator <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/knowmics-lab/geneslator/branch/main/graph/badge.svg)](https://app.codecov.io/gh/knowmics-lab/geneslator?branch=main)
[![License](https://img.shields.io/badge/license-Artistic--2.0-blue.svg)](https://opensource.org/licenses/Artistic-2.0)
<!-- badges: end -->

**geneslator** is a comprehensive R package for gene identifier conversion and genome annotation across multiple model organisms. The package integrates data from several cross-organism databases and organism-specific resources within a single, coherent framework.

## Key Features

- **Multiple database integration**: Integrates data from cross-organism databases (NCBI, Ensembl, UniProt, Alliance of Genome Resources, GO, KEGG, Reactome, Wikipathways) and organism-specific resources (HGNC, MGI, RGD, SGD, WormBase, Flybase, ZFIN, TAIR)
- **Archive search**: Supports searching using both current and archived gene identifiers in NCBI and Ensembl databases
- **Alias resolution**: Supports automatic disambiguation between symbols and aliases in annotations involving gene symbols
- **Automatic download**: Annotation databases are automatically downloaded when needed and cached locally
- **Version management**: Independent versioning system for databases with automatic update checks

## Supported Organisms

- Human (*Homo sapiens*)
- Mouse (*Mus musculus*)
- Rat (*Rattus norvegicus*)
- Yeast (*Saccharomyces cerevisiae*)
- Worm (*Caenorhabditis elegans*)
- Fly (*Drosophila melanogaster*)
- Zebrafish (*Danio rerio*)
- Arabidopsis (*Arabidopsis thaliana*)

## Installation

### From Bioconductor (when available)

```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("geneslator")
```

### Development version from GitHub

```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("knowmics-lab/geneslator")
```

## Usage examples

```r
library(geneslator)

# Check available organisms
availableOrganisms()

# Import human annotation database (download database automatically if needed)
human.db <- GeneslatorDb("Human")

# Get gene symbols, full names and NCBI Gene IDs from Ensembl IDs using select()
select(human.db, keys = c("ENSG00000141510", "ENSG00000012048", "ENSG00000139618"),
       columns = c("SYMBOL", "GENENAME", "ENTREZID"), keytype = "ENSEMBL")

# Convert Ensembl IDs to gene symbols using mapIds()
mapIds(gdb, keys = c("ENSG00000139618", "ENSG00000141510"), column = "SYMBOL",
       keytype = "ENSEMBL")

# Get mouse orthologs for human genes
select(human.db, keys = c("TP53", "BRCA1", "EGFR"), columns = c("ORTHOMOUSE"),
       keytype = "SYMBOL")

# Get GO annotations for a set of genes
select(human.db, keys = c("7157", "672"), columns = c("SYMBOL", "GO", "GONAME"),
       keytype = "ENTREZID")

# Get KEGG pathways for a set of genes
select(human.db, keys = c("TP53", "BRCA1"), columns = c("KEGGPATH", "KEGGPATHNAME"),
       keytype = "SYMBOL")
```

## Available Columns

Available columns for queries include (some may not be present in all organisms):

**Basic Identifiers:**
- `SYMBOL` - Official gene symbol
- `ALIAS` - Gene aliases
- `GENENAME` - Full gene name or description
- `GENETYPE` - Biological type of gene (e.g. 'protein-coding', 'ncRNA')
- `ENTREZID` - Gene ID in NCBI Gene
- `ENSEMBL` - Gene ID in Ensembl
- `UNIPROTKB` - Uniprot IDs of proteins associated with the gene

**Organism-Specific Identifiers:**
- `HGNC` - Gene ID in HUGO Gene Nomenclature Committee (Human only)
- `MGI` - Gene ID in Mouse Genome Informatics (Mouse only)
- `RGD` - Gene ID in Rat Genome Database (Rat only)
- `SGD` - Gene ID in Saccharomyces Genome Database (Yeast only)
- `WORMBASE` - Gene ID in WormBase database (Worm only)
- `FLYBASE` - Gene ID in FlyBase database (Fly only)
- `ZFIN` - Gene ID in Zebrafish Information Network (Zebrafish only)
- `TAIR` - Gene ID in The Arabidopsis Information Resource (Arabidopsis only)

**Archived Identifiers:**
- `ENTREZIDOLD` - Archived IDs in NCBI Gene
- `ENSEMBLOLD` - Archived IDs in Ensembl

**Orthologs:**
- `ORTHOHUMAN` - Orthologs in Human (absent in Human and Arabidopsis)
- `ORTHOMOUSE` - Orthologs in Mouse (absent in Mouse and Arabidopsis)
- `ORTHORAT` - Orthologs in Rat (absent in Rat and Arabidopsis)
- `ORTHOYEAST` - Orthologs in Yeast (absent in Yeast and Arabidopsis)
- `ORTHOWORM` - Orthologs in Worm (absent in Worm and Arabidopsis)
- `ORTHOFLY` - Orthologs in Fly (absent in Fly and Arabidopsis)
- `ORTHOZEBRAFISH` - Orthologs in Zebrafish (absent in Zebrafish and Arabidopsis)

**Functional Annotations:**
- `GO` - IDs of Gene Ontology (GO) terms associated with the gene
- `GONAME` - Names of GO terms associated with the gene
- `GOEVIDENCE` - Evidence codes of GO terms
- `GOTYPE` - Types of GO terms ('BP'=biological process, 'CC'=cellular component, 'MF'=molecular function)
- `KEGGPATH` - IDs of KEGG pathways associated with the gene
- `KEGGPATHNAME` - Names of KEGG pathways associated with the gene
- `REACTOMEPATH` - IDs of Reactome pathways associated with the gene
- `REACTOMEPATHNAME` - Names of Reactome pathways associated with the gene
- `WIKIPATH` - IDs of Wikipathways pathways associated with the gene
- `WIKIPATHNAME` - Names of Wikipathways pathways associated with the gene

For a complete list, use:
```r
columns(gdb)
keytypes(gdb)
```

## Database Management

Annotation databases are automatically downloaded from [<geneslator GitHub release page>](<https://github.com/knowmics-lab/geneslator/releases>) 
when needed and cached locally.

When you import an annotation database in geneslator:
- If the database is not present in the local cache, it is automatically downloaded
- If the database is present but a newer version is available, you will be asked if you want to update it
- Files are saved in the R cache directory (visible with `tools::R_user_dir("geneslator", "cache")`)

```r
# Import human database for the first time: the database is downloaded and saved in cache
gdb <- GeneslatorDb("Human")

# Import human database again: use file saved in local cache
gdb <- GeneslatorDb("Human")
```

## Documentation

For more information and detailed tutorials, see:

- [Package vignette](https://knowmics-lab.github.io/geneslator)
- [Function documentation](https://knowmics-lab.github.io/geneslator/reference)
- [GitHub Issues](https://github.com/knowmics-lab/geneslator/issues)

## Citation

If you use geneslator in your work, please cite:

```r
citation("geneslator")
```

Micale G, Cavallaro G, Privitera GF (2026). geneslator: A Comprehensive Gene Identifier Conversion Tool. R package version 0.99.0. https://github.com/knowmics-lab/geneslator

## License

This package is released under the Artistic-2.0 license. See the [LICENSE](LICENSE) file for details.

## Authors

- **Giovanni Micale** - *Author and maintainer* - [ORCID](https://orcid.org/0000-0002-4953-026X)
- **Giulia Cavallaro** - *Author* - [ORCID](https://orcid.org/0009-0000-1212-8368)
- **Grete Francesca Privitera** - *Author* - [ORCID](https://orcid.org/0000-0003-1807-4780)

University of Catania

## Support

- **Issues**: https://github.com/knowmics-lab/geneslator/issues
- **Documentation**: https://knowmics-lab.github.io/geneslator
- **Email**: giovanni.micale@unict.it

## References

- NCBI Gene: https://www.ncbi.nlm.nih.gov/gene
- Ensembl: https://www.ensembl.org
- UniProt: https://www.uniprot.org
- Gene Ontology: http://geneontology.org
- KEGG: https://www.kegg.jp
- Reactome: https://reactome.org
- WikiPathways: https://www.wikipathways.org
- Alliance of Genome Resources: https://www.alliancegenome.org
- AnnotationDbi: Pages H, Carlson M, Falcon S, Li N (2024). AnnotationDbi: Manipulation of SQLite-based annotations in Bioconductor.
