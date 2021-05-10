<img src="www/septisearch.svg" height="100px">

Welcome to SeptiSearch, an interactive Shiny app providing access to
manually-curated molecular data on sepsis from current publications.

## Usage
SeptiSeach allows users to access our manually curated data in a number of ways, 
including:

- **Explore Data in a Table** to browse our entire collection, search for 
particular molecules, and filter the data using a number of criteria such as 
omics type, case condition, and infection source
- **Explore Data by Study** makes it easy to search the curated publications in
our dataset by keyword, and view the associated molecules for a study of 
interest
- **Visualize Molecule Occurrence** graphically displays the most cited molecules
in our data set, and provides an easy way to view all entries for a specific 
molecule

Finally, the **Perform Enrichment Tests** tab allows users to upload their own 
list of genes and test them for enriched pathways using ReactomePA and EnirchR.

## Availability
Currently, the app is not hosted on any website or service. If you wish to try
it out, you can download this repository and run it locally. The following R
packages are required to run the app:

- [Shiny](https://shiny.rstudio.com/)
- [ShinyJS](https://deanattali.com/shinyjs/)
- [DT](https://rstudio.github.io/DT/)
- [The tidyverse](https://www.tidyverse.org/)
- [Plotly](https://plotly.com/r/)
- [biomaRt](https://bioconductor.org/packages/biomaRt/)
- [ReactomePA](https://bioconductor.org/packages/ReactomePA)
- [enrichR](https://cran.r-project.org/package=enrichR)

The data for the app is currently only available to contributors and data 
curators, outside of this repository.

## License
This project uses the GNU General Public License v3.0, available
[here](https://github.com/hancockinformatics/curation/blob/master/LICENSE).

## Contributors
Travis Blimkie is the main developer of the Shiny app. Jasmine Tam performed all
of the data gathering and curation, while Arjun Baghela served as the supervisor
for the project.
