# Setup -------------------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(dplyr)
  library(bslib)
})

septisearch_theme <- bs_theme(
  preset = "cosmo",
  base_font = font_collection(
    font_google(family = "Noto Serif", local = FALSE),
    "serif"
  )
)

app_version <- gsub(
  x = grep(x = readLines("DESCRIPTION"), pattern = "Version", value = TRUE),
  pattern = "Version\\: ",
  replacement = ""
)

references_table <- tibble(
  name = c(
    "biomaRt",
    "DT",
    "enrichR",
    "GSVA",
    "pheatmap",
    "plotly",
    "ReactomePA",
    "shiny",
    "shinyjs",
    "tidyverse"
  ),
  link = c(
    "https://bioconductor.org/packages/biomaRt/",
    "https://rstudio.github.io/DT/",
    "https://cran.r-project.org/package=enrichR",
    "https://github.com/rcastelo/GSVA",
    "https://cran.r-project.org/package=pheatmap",
    "https://plotly.com/r/",
    "https://bioconductor.org/packages/ReactomePA",
    "https://shiny.rstudio.com/",
    "https://deanattali.com/shinyjs/",
    "https://www.tidyverse.org/"
  ),
  description = c(
    "An R package to access BioMart databases",
    "An R interface to the DataTables JavaScript library",
    "Access the Ma'ayan Lab's gene set enrichment services",
    "Gene Set Variation Analysis for microarray and RNA-Seq data",
    "Easy and robust heatmap visualizations",
    "Interactive plots and visualizations",
    "Perform pathway enrichment analysis using Reactome data",
    "Create beautiful web apps with R",
    "Extend Shiny functionality using JavaScript",
    "A suite of packages for data manipulation"
  )
)


# UI ----------------------------------------------------------------------

septisearch_ui <- page_navbar(
  id = "navbar",
  window_title = "SeptiSearch",
  theme = septisearch_theme,
  header = tags$head(
    useShinyjs(),
    includeHTML("google_analytics.html"),
    tags$script(src = "js/client.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/user.css")
  ),


  # * Home ----------------------------------------------------------------

  nav_panel(
    value = "home_tab",
    icon = icon("house"),
    title = "SeptiSearch",

    div(
      class = "my-3",
      div(
        class = "p-5 textbg-body-tertiary rounded-3",
        h1("Welcome", class = "text-body-emphasis"),
        hr(),
        HTML(
          "<p class='mx-auto fs-5 text-muted'>",
          "Welcome to SeptiSearch! Here you can browse, explore, and download ",
          "curated molecular results derived from transcriptomic sepsis ",
          "studies. The database and app currently catalogs over 20,000 ",
          "unique molecules from more than 70 publications.</p>"
        ),

        HTML(
          "<p class='mx-auto fs-5 text-muted'>To get started, select one of ",
          "the tabs at the top of the page, described below:",

          "<ul class='fs-5 text-muted'>",
          "<li><em>Explore the Database</em> makes it easy to search all ",
          "curated gene sets by keyword or molecule, filter with select ",
          "criteria, and view all the molecules in a gene set</li> ",

          "<li><em>Visualize the Database</em> plots the most common ",
          "molecules, and provides filters to see which occur most ",
          "frequently based on a number of attributes</li>",

          "<li><em>Perform Pathway Enrichment</em> allows users to upload ",
          "their own list of genes, or use one of the curated sepsis gene ",
          "sets, to test for enriched pathways/terms using ",
          "<a href='https://bioconductor.org/packages/ReactomePA/'>",
          "ReactomePA</a> and ",
          "<a href='https://maayanlab.cloud/Enrichr/'>enrichR</a></li>",

          "<li><em>Test for Enriched Sepsis Gene Sets</em> can be used to ",
          "test your own expression data (e.g. counts from RNA-Seq) for ",
          "dysregulation of the curated sepsis gene sets</li>",
          "</ul></p>"
        ),

        HTML(
          "<p class='mx-auto fs-5 text-muted'>",
          "SeptiSearch was created by Travis Blimkie, Jasmine Tam & Arjun ",
          "Baghela from the <a href='https://cmdr.ubc.ca/bobh/'>Hancock ",
          "Lab</a> at the University of British Columbia, and is published ",
          "in Frontiers in Immunology ",
          "(<a href='https://doi.org/10.3389/fimmu.2023.1135859'>doi:",
          "10.3389/fimmu.2023.1135859</a>). If you'd like to learn more, ",
          "or to report bugs or issues, click the button below to visit our ",
          "<em>About</em> page."
        ),

        div(
          class = "d-inline-flex gap-2 mb-5",
          actionButton(
            inputId = "get_started",
            label = "Initializing app...",
            class = "btn-lg disabled",
            title = "Please wait while the app loads...",
            icon =  icon(
              name = "spinner",
              class = "fa fa-spin"
            )
          ),
          actionButton(
            inputId = "learn_more",
            label = "Learn more",
            class = "btn-lg btn-hidden",
            title = "Visit our About page!"
          )
        )
      )
    )
  ),


  # * Explore -------------------------------------------------------------

  nav_panel(
    id = "explore_tab",
    icon = icon("table"),
    title = "Explore the database",

    layout_sidebar(
      sidebar = sidebar()
    )
  ),


  # * Visualize -----------------------------------------------------------

  nav_panel(
    id = "viz_tab",
    icon = icon("chart-bar"),
    title = "Visualize the database",

    layout_sidebar(
      sidebar = sidebar()
    )
  ),


  # * Enrich --------------------------------------------------------------

  nav_panel(
    id = "enrich_tab",
    icon = icon("calculator"),
    title = "Perform pathway enrichment",

    layout_sidebar(
      sidebar = sidebar()
    )
  ),


  # * GSVA ----------------------------------------------------------------

  nav_panel(
    id = "gsva_tab",
    icon = icon("laptop-code"),
    title = "Test for enriched sepsis gene sets",

    layout_sidebar(
      sidebar = sidebar()
    )
  ),


  # * About ---------------------------------------------------------------

  nav_panel(
    id = "about_tab",
    icon = icon("circle-info"),
    title = "About",

    div(
      class = "my-3",
      div(
        class = "p-5 textbg-body-tertiary rounded-3",
        h1("About"),
        hr(),

        HTML(
          "<p>SeptiSearch is a Shiny app in which you can browse, explore, ",
          "and download curated molecular gene sets derived from ",
          "transcriptomic sepsis studies. The app currently allows access to ",
          "over 20,000 unique molecules from over 70 publications. It was ",
          "created by Travis Blimkie, Jasmine Tam & Arjun Baghela from the ",
          "<a href='https://cmdr.ubc.ca/bobh/'>Hancock Lab</a> at the ",
          "University of British Columbia, and is published in Frontiers in ",
          "Immunology (<a href='https://doi.org/10.3389/fimmu.2023.1135859'>",
          "doi: 10.3389/fimmu.2023.1135859</a>). The last update to the data ",
          "was performed in November 2022. Travis is the main developer for ",
          "the Shiny app and handles maintenance & updates. Jasmine performed ",
          "all the signature curation from datasets in peer-reviewed ",
          "research articles and publicly available pre-prints. Arjun served ",
          "as the supervisor for the project."
        ),

        HTML(
          "<p>Gene Sets (i.e. the Gene Set Name column) are defined based on ",
          "a number of columns/fields from each study, such that one study ",
          "may have multiple gene sets. For example, if one study compares ",
          "two groups of sick patients (e.g. severe and mild sepsis) to the ",
          "same group of healthy controls, that study would have two gene ",
          "sets. The fields used to determine the Gene Sets are: Timepoint, ",
          "Case and Control Condition, Tissue, and Gene Set Type.</p>"
        ),

        h2("Tutorial"),
        HTML(
          "<p>A tutorial is available which provides detailed insturctions ",
          "for using SeptiSearch and its different functions, hosted on a ",
          "GitHub Pages site: ",
          "<a href='https://hancockinformatics.github.io/SeptiSearch/'>",
          "https://hancockinformatics.github.io/SeptiSearch/</a></p>"
        ),

        h2("Reporting problems"),
        HTML(
          "<p>If you encounter a problem or bug with the app, please submit ",
          "an issue at the ",
          "<a href='https://github.com/hancockinformatics/SeptiSearch'>Github ",
          "page</a>. Include with your issue details on the problem so we can ",
          "reproduce it, and any inputs if relevant (e.g. your list of genes ",
          "submitted to the <i>Perform Pathway Enrichment</i> tab).</p>"
        ),

        h2("Funding"),
        HTML(
          "We would like to acknowledge and thank the ",
          "<a href='https://cihr-irsc.gc.ca/e/193.html'>Canadian Institutes ",
          "of Health Research (CIHR)</a> for providing the funding for this ",
          "project.</p>"
        ),

        h2("Database details & app methods"),
        h4("Tissue Class definitions"),
        HTML(
          "<p>The Tissue Class column contains a controlled vocabulary to ",
          "describe the type of tissue in which a study was performed: ",
          "'Blood' denotes sampling was done on whole blood; 'Blood Cells' ",
          "indicates a specific cell type was isolated from blood; 'Lung ",
          "Cells' means particular cells or tissue samples were extracted ",
          "from the lung; 'Other Cells' is used to describe any remaining ",
          "entries not fitting those mentioned previously. We recommend ",
          "checking the original source if you require more detailed ",
          "information.</p>"
        ),

        h4("Perform Pathway Enrichment"),
        HTML(
          "<p>Input gene mapping between ID types is performed using data ",
          "obtained via the ",
          "<a href='https://bioconductor.org/packages/biomaRt/'>biomaRt</a> ",
          "package. Biological pathway/term enrichment is performed using ",
          "<a href='https://bioconductor.org/packages/ReactomePA'>",
          "ReactomePA</a> and <a href='https://maayanlab.cloud/Enrichr/'> ",
          "enrichR</a>. The following resources are searched using enrichR: ",
          "MSigDB's Hallmark collection, and the three main GO databases ",
          "(Biological Process, Cellular Component & Molecular Function). For ",
          "both methods, the results are filtered using an adjusted p-value ",
          "threshold of 0.05.</p>"
        ),

        h4("Test for Enriched Sepsis Gene Sets"),
        HTML(
          "<p>Gene Set Variation Analysis is performed using the ",
          "<a href='https://github.com/rcastelo/GSVA'>GSVA</a> package, and ",
          "the heatmap visualization is created with ",
          "<a href='https://github.com/raivokolde/pheatmap'>pheatmap</a>. ",
          "Specified parameters include the <em>gsva</em> method and a ",
          "<em>Gaussian</em> kernel. Genes with zero variance across all ",
          "samples are removed prior to the analysis. Example data for GSVA ",
          "represents a subset of the GEO record ",
          "<a href='https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE65682'>",
          "GSE65682</a>.</p>"
        ),

        h2("References"),
        HTML(
          "<p>SeptiSearch is written in R, and uses the following packages ",
          "and resources:</p>"
        ),
        div(wrap_list(references_table))
      )
    )
  ),


  # * Right-side items ----------------------------------------------------

  nav_spacer(),

  nav_item(a(
    icon("github"),
    href = "https://github.com/hancockinformatics/SeptiSearch",
    target = "_blank",
    rel = "noopener noreferrer"
  )),

  nav_item(tagList(
    div(class = "vr d-none d-sm-flex h-100 mx-sm-2 text-white"),
    hr(class = "d-lg-none my-2 text-white-50")
  )),

  nav_item(app_version, style = "color: var(--bs-nav-link-color)")
)


# Server ------------------------------------------------------------------

septisearch_server <- function(input, output, session) {

  source("scripts/functions.R")
  source("scripts/deferred.R")

  observeEvent(input$sessionInitialized, {
    runjs("handlers.initGetStarted();")
  }, ignoreInit = TRUE, once = TRUE)

  tabEnrich_pkg_load_indicator <- reactiveVal(0)

  observe({
    # When we switch to the Enrichment tab, load enrichR
    if (all(
      as.character(req(input$navbar)) == "enrich_tab",
      tabEnrich_pkg_load_indicator() == 0
    )) {
      tabEnrich_pkg_load_indicator(1)
      require(enrichR)
    }
  })

}

shinyApp(septisearch_ui, septisearch_server)
