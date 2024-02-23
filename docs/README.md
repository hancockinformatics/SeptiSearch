# SeptiSearchTutorial

Welcome to SeptiSearchTutorial, the GitHub repository holding the documentation
for SeptiSearch, an interactive Shiny app providing access to manually-curated
molecular sepsis data from current publications. Visit the main site at
[septisearch.ca](https://septisearch.ca).

The SeptiSearch tutorial docs were created using JustTheDocs; more information 
is available [here](https://just-the-docs.github.io/just-the-docs/).

## SeptiSearch overview
The SeptiSearch app is designed to allow easy access to curated sepsis gene 
sets. Multiple tabs allow the exploration and use of the data in a number of 
ways:

- **Explore the Database** makes it easy to search all curated gene sets by
keyword or molecule, filter with select criteria, and view all the molecules in
a gene set
- **Visualize the Database** plots the most common molecules, and provides
filters to see which occur most frequently based on a number of attributes
- **Perform Pathway Enrichment** allows users to upload their own list of genes,
or use one of the curated sepsis gene sets, to test for enriched pathways/terms
using ReactomePA and EnirchR
- **Test for Enriched Sepsis Gene Sets** can be used to test your own expression
data (e.g. counts from RNA-Seq) for dysregulation of the curated sepsis gene
sets

## License
This project uses the MIT license, available
[here](https://github.com/hancockinformatics/SeptiSearchTutorial/blob/main/LICENSE).
