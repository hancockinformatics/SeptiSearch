[<img src="www/septisearch.svg" height="100px">](https://septisearch.ca)

Welcome to SeptiSearch, an interactive Shiny app providing access to
manually-curated molecular sepsis data from current publications. Visit us 
at [septisearch.ca](https://septisearch.ca).

## Usage
SeptiSeach allows users to access our manually curated data in a number of ways:

- **Explore the Database** makes it easy to search the curated publications in 
the database by keyword, and view the associated molecules from a particular
gene set
- **Visualize the Database** graphically displays the most cited
molecules in the database, and provides an easy way to view all entries for a
specific molecule
- **Perform Pathway Enrichment** allows users to upload their own list of genes
and test them for enriched pathways/terms using ReactomePA and EnirchR
- **Test for Enriched Sepsis Gene Sets** makes it easy to check your own
expression data (e.g. counts from RNA-Seq) for dysregulation of our curated
sepsis gene sets

## Tutorial
For more details on how to use SeptiSearch, please see our tutorial hosted via 
Github pages:
https://hancockinformatics.github.io/SeptiSearch/

## Dependencies
SeptiSearch uses the following R packages:

- [Shiny](https://shiny.rstudio.com/)
- [ShinyJS](https://deanattali.com/shinyjs/)
- [DT](https://rstudio.github.io/DT/)
- [Tidyverse](https://www.tidyverse.org/)
- [Plotly](https://plotly.com/r/)
- [biomaRt](https://bioconductor.org/packages/biomaRt/)
- [ReactomePA](https://bioconductor.org/packages/ReactomePA)
- [enrichR](https://cran.r-project.org/package=enrichR)
- [GSVA](https://github.com/rcastelo/GSVA)
- [pheatmap](https://cran.r-project.org/package=pheatmap)

## Contributors
A big thank you to all of the SeptiSearch beta testers from the Hancock Lab:
Morgan Alford, Andy An, Melanie Dostert, Evan Haney, and Felix Santana.

Travis Blimkie is the main developer of the Shiny app. Jasmine Tam performed all
of the data gathering and curation, while Arjun Baghela served as the supervisor
for the project.

## License
This project uses the GNU General Public License v3.0, available
[here](https://github.com/hancockinformatics/SeptiSearch/blob/master/LICENSE).

<br>

[<img src="www/hancock-lab-logo.svg" height="40px">](http://cmdr.ubc.ca/bobh/)
