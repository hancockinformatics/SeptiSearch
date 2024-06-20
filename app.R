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
  ),
  primary = "#4582ec",
  secondary = "#373a3c",
  info = "#5bc0de",
  success = "#3fad46",
  warning = "#f0ad4e",
  danger = "#d9534f"
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
    extendShinyjs(script = "functions.js", functions = c("resetClick")),
    tags$script(src = "js/client.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/user.css"),
    includeHTML("google_analytics.html")
  ),


  # * Home ----------------------------------------------------------------

  nav_panel(
    value = "home_tab",
    icon = icon("house"),
    title = "SeptiSearch",

    div(
      class = "my-3",
      div(
        class = "p-5 textbg-body-tertiary rounded-3 mx-auto fs-5",
        h1("Welcome", class = "text-body-emphasis"),
        hr(),
        HTML(
          "<p>Welcome to SeptiSearch! Here you can browse, explore, and ",
          "download curated molecular results derived from transcriptomic ",
          "sepsis studies. The database and app currently catalogs over ",
          "20,000 unique molecules from more than 70 publications.</p>"
        ),

        HTML(
          "<p>To get started, select one of the tabs at the top of the page, ",
          "described below:",

          "<ul>",
          "<li><b>Explore the Database</b> makes it easy to search all ",
          "curated gene sets by keyword or molecule, filter with select ",
          "criteria, and view all the molecules in a gene set</li> ",

          "<li><b>Visualize the Database</b> plots the most common ",
          "molecules, and provides filters to see which occur most ",
          "frequently based on a number of attributes</li>",

          "<li><b>Perform Pathway Enrichment</b> allows users to upload ",
          "their own list of genes, or use one of the curated sepsis gene ",
          "sets, to test for enriched pathways using ",
          "<a href='https://bioconductor.org/packages/ReactomePA/'>",
          "ReactomePA</a> and ",
          "<a href='https://maayanlab.cloud/Enrichr/'>enrichR</a></li>",

          "<li><b>Test for Enriched Sepsis Gene Sets</b> can be used to ",
          "test your own expression data (e.g. counts from RNA-Seq) for ",
          "dysregulation of the curated sepsis gene sets</li>",
          "</ul></p>"
        ),

        HTML(
          "<p>SeptiSearch was created by Travis Blimkie, Jasmine Tam & Arjun ",
          "Baghela from the <a href='https://cmdr.ubc.ca/bobh/'>REW Hancock ",
          "Lab</a> at the University of British Columbia, and is published ",
          "in Frontiers in Immunology ",
          "(<a href='https://doi.org/10.3389/fimmu.2023.1135859'>doi:",
          "10.3389/fimmu.2023.1135859</a>). If you'd like to learn more, ",
          "or to report bugs or issues, click the button below to visit our ",
          "<b>About</b> page."
        ),

        div(
          class = "d-inline-flex gap-2 mb-5",
          actionButton(
            inputId = "get_started",
            class = "btn-lg disabled",
            icon = icon(name = "spinner", class = "fa fa-spin"),
            label = "Initializing app..."
          ),
          actionButton(
            inputId = "learn_more",
            class = "btn-lg btn-hidden",
            label = "Learn more"
          )
        )
      )
    )
  ),


  # * Explore -------------------------------------------------------------

  nav_panel(
    value = "explore_tab",
    icon = icon("table"),
    title = "Explore the database",

    layout_sidebar(
      sidebar = sidebar(
        id = "explore_tab_sidebar",
        title = "Explore the database",
        open = "always",
        width = "25%",

        HTML(
          "<p>Browse the database by Gene Set, where one publication can ",
          "contain multiple sets (e.g. different patient groups were included). ",
          "To the right, the top table shows all sets along with some key ",
          "information, such as the type of study and number of molecules in ",
          "the set. You can search the articles by title, view only COVID or ",
          "non-COVID studies, or filter for gene sets containing specific ",
          "molecules.</p>"
        ),
        HTML(
          "<p>By clicking on one or more rows in the top table, another table ",
          "with all the molecules in those sets will appear below. You can ",
          "download this second table via the button which appears at the ",
          "bottom of this sidebar.</p>"
        ),

        textAreaInput(
          inputId = "tabExplore_title_input",
          label = "Search article titles",
          placeholder = "E.g. 'Endotypes'",
          height = 41,
          resize = "none"
        ),

        radioButtons(
          inputId = "tabExplore_covid_radio_input",
          label = "Type of study to include",
          choices = c(
            "All studies" = "all_studies",
            "Non-COVID only" = "noncovid_only",
            "COVID only" = "covid_only"
          ),
          selected = "all_studies"
        ),

        textAreaInput(
          inputId = "tabExplore_molecule_input",
          label = HTML("Search for specific molecules"),
          placeholder = "S100A9\nGYG1\nSTAT4\nTLR5\n...",
          height = 150,
          resize = "vertical"
        ),

        HTML(
          "<p>When you select a single gene set from the top table, the ",
          "button below will switch to the <b>Perform Pathway Enrichment</b> ",
          "tab, allowing you to easily test that gene set for significantly ",
          "enriched pathways.</p>"
        ),

        disabled(actionButton(
          inputId = "tabExplore_send_button",
          class = "btn-primary",
          icon = icon("calculator"),
          label = "Perform pathway enrichment on this gene set"
        )),

        uiOutput("tabExplore_clicked_study_download_button"),

        hr(),
        actionButton(
          inputId = "tabExplore_reset",
          class = "btn-warning",
          icon = icon("rotate-left"),
          label = "Reset this page"
        )
      ),

      # Main section
      uiOutput("tabExplore_grouped_render"),
      uiOutput("tabExplore_clicked_render")
    )
  ),


  # * Visualize -----------------------------------------------------------

  nav_panel(
    value = "viz_tab",
    icon = icon("chart-bar"),
    title = "Visualize the database",

    layout_sidebar(
      sidebar = sidebar(
        id = "viz_tab_sidebar",
        title = "Visualize the database",
        open = "always",
        width = "25%",

        HTML(
          "<p>The plot on the right displays the most common molecules in the ",
          "database. You can hover over the bars with your cursor to see the ",
          "molecule's name and its number of entries.</p>"
        ),
        HTML(
          "<p>The inputs below will automatically filter the data displayed ",
          "in the plot. For example, you can see which molecules are most ",
          "common in whole blood using the <b>Tissue Class</b> input.</p>"
        ),
        HTML(
          "<p>The plot to the right, generated with ",
          "<a href='https://plotly.com/r/'>Plotly</a>, is interactive. ",
          "Clicking on a bar will show a table containing all entries for ",
          "that molecule, and you can download this table using the button ",
          "which appears at the bottom of the sidebar. You can also zoom in ",
          "using your cursor to select an area. Other controls can be toggled ",
          "using the icons at the top-right corner of the plot.</p>"
        ),

        radioButtons(
          inputId = "tabViz_covid_radio_input",
          label = "Type of study to include",
          choices = c(
            "All studies" = "all_studies",
            "Non-COVID only" = "noncovid_only",
            "COVID only" = "covid_only"
          ),
          selected = "all_studies"
        ),

        uiOutput("tabViz_select_inputs"),
        uiOutput("tabViz_clicked_table_download_button"),

        hr(),
        actionButton(
          inputId = "tabViz_reset",
          class = "btn-warning",
          icon = icon("rotate-left"),
          label = "Reset this page"
        )
      ),
      uiOutput("tabViz_plot_panel"),
      uiOutput("tabViz_clicked_table_panel")
    )
  ),


  # * Enrich --------------------------------------------------------------

  nav_panel(
    value = "enrich_tab",
    icon = icon("calculator"),
    title = "Perform pathway enrichment",

    layout_sidebar(
      sidebar = sidebar(
        id = "enrich_tab_sidebar",
        title = "Perform pathway enrichment",
        open = "always",
        width = "25%",

        p(
          "Paste a list of genes into the space below (one per line) to ",
          "test for enriched pathways/terms using ReactomePA and enrichR. ",
          "Input genes may be either Ensembl, Entrez, or HGNC identifiers. ",
          "You can also use the following link to ",
          actionLink(
            inputId = "tabEnrich_load_example",
            label = "load example data",
            .noWS = "after"
          ),
          ". Results are automatically filtered using the adjusted p-value ",
          "provided by each tool. For more details on these methods, please ",
          "see the ",
          actionLink(
            inputId = "tabEnrich_about",
            label = "About page",
            .noWS = "after"
          ),
          "."
        ),

        textAreaInput(
          inputId = "tabEnrich_pasted_input",
          label = "Enter your query molecules below:",
          placeholder = "One per line...",
          height = 200,
          resize = "none"
        ),

        HTML(
          "<p>Once you've entered your genes or loaded the example data, use ",
          "the <b>1. Perform gene ID mapping</b> button to complete the first ",
          "step. Then you can <b>2. Submit genes for pathway enrichment</b>; ",
          "this step may take some time to complete, so please be patient.</p>"
        ),

        disabled(actionButton(
          inputId = "tabEnrich_map_button",
          class = "btn-primary",
          icon = icon("signs-post"),
          label = "1. Perform gene ID mapping"
        )),

        disabled(actionButton(
          inputId = "tabEnrich_submit_button",
          class = "btn-primary",
          icon = icon("circle-right"),
          label = "2. Submit genes for pathway enrichment"
        )),

        uiOutput("tabEnrich_mapping_info"),
        uiOutput("tabEnrich_ReactomePA_download_button"),
        uiOutput("tabEnrich_enrichR_download_button"),
        div(id = "tabEnrich_placeholder_div")
      ),

      uiOutput("tabEnrich_results_header"),
      uiOutput("tabEnrich_result_tabgroup_ui")
    )
  ),


  # * GSVA ----------------------------------------------------------------

  nav_panel(
    value = "gsva_tab",
    icon = icon("laptop-code"),
    title = "Test for enriched sepsis gene sets",

    layout_sidebar(
      sidebar = sidebar(
        id = "gsva_tab_sidebar",
        title = "Test for enriched sepsis gene sets",
        open = "always",
        width = "25%",

        p(
          "Here you can upload transformed counts from RNA-Seq or mircoarray ",
          "experiments to run Gene Set Variation Analysis (GSVA) using the ",
          "curated sepsis gene sets. GSVA looks for dysregulation of the ",
          "specified gene sets - here derived from sepsis studies - to ",
          "identify patterns of expression among your samples. For more ",
          "details on the GSVA method, please refer to the relevant section ",
          "in the ",
          actionLink(inputId = "tabGSVA_about", label = "About"), "page."
        ),

        p(
          "We also provide example expression data, along with ",
          "corresponding metadata, for you to try out. This data represents a ",
          "subset of the GEO record ",
          a(
            href = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE65682",
            "GSE65682"
          ),
          "and can be loaded using",
          actionLink(
            inputId = "tabGSVA_load_example_data",
            label = "this link",
            .noWS = "after"
          ),
          "."
        ),

        HTML("<p><b>Input requirements for GSVA:</b></p>"),
        HTML(
          "<ul>",
          "<li>Must be a comma-separated plaintext file (.csv)</li>",
          "<li>The first column must contain Ensembl gene IDs</li>",
          "<li>The remaining columns should correspond to your samples</li>",
          "<li>Counts must be normalized/transformed as is appropriate for ",
          "your data (e.g. DESeq2's <a href='https://www.bioconductor.org/",
          "packages/devel/bioc/vignettes/DESeq2/inst/doc/DESeq2.html'>VST</a> ",
          "method); raw data will not be accepted.</li>",
          "</ul>"
        ),

        fileInput(
          inputId = "tabGSVA_matrix_input",
          label = NULL,
          buttonLabel = list(icon("upload"), "Upload expression data..."),
          accept = "csv"
        ),

        tags$label("Optional: Upload sample metadata"),

        p(
          "You may also upload metadata for your samples, which will be added ",
          "as annotations to the final heatmap to indicate groups or ",
          "variables for your samples (e.g. control and treatment). The first ",
          "column must contain sample names, matching to the columns from the ",
          "expression matrix provided above. All remaining columns will ",
          "become annotation rows on the final heatmap."
        ),

        fileInput(
          inputId = "tabGSVA_metadata_input",
          label = NULL,
          buttonLabel = list(icon("upload"), "Upload sample metadata..."),
          accept = "csv"
        ),

        disabled(actionButton(
          inputId = "tabGSVA_submit_button",
          class = "btn-primary",
          icon = icon("circle-right"),
          label = "Submit expression data for GSVA"
        )),

        uiOutput("tabGSVA_result_downloadbutton")
      ),

      div(id = "tabGSVA_placeholder_div"),
      uiOutput("tabGSVA_result_UI"),
      uiOutput("tabGSVA_heatmap_UI")
    )
  ),


  # * About ---------------------------------------------------------------

  nav_panel(
    value = "about_tab",
    icon = icon("circle-info"),
    title = "About",

    div(
      class = "my-3",
      div(
        class = "p-5 textbg-body-tertiary rounded-3 fs-5",
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


  # Setup -----------------------------------------------------------------

  source("scripts/deferred.R")

  observeEvent(input$sessionInitialized, {
    runjs("handlers.initGetStarted();")
  }, ignoreInit = TRUE, once = TRUE)

  # When we switch to the Enrichment tab, load enrichR
  tabEnrich_pkg_load_indicator <- reactiveVal(0)
  observe({
    if (all(
      as.character(req(input$navbar)) == "enrich_tab",
      tabEnrich_pkg_load_indicator() == 0
    )) {
      tabEnrich_pkg_load_indicator(1)
      require(enrichR)
    }
  })

  observeEvent(
    input$get_started,
    nav_select("navbar", selected = "explore_tab")
  )

  observeEvent(
    input$learn_more,
    nav_select("navbar", selected = "about_tab")
  )


  # Explore ---------------------------------------------------------------

  observeEvent(input$tabExplore_reset, {
    shinyjs::reset("explore_tab_sidebar", asis = FALSE)
    selectRows(proxy = dataTableProxy("tabExplore_grouped_DT"), selected = NULL)
    output$tabExplore_clicked_DT <- NULL
    tabExplore_clicked_row_studylabel(NULL)
    tabExplore_clicked_row_info(NULL)
    disable("tabExplore_send_button")
    tabExplore_users_molecules(NULL)
  })


  # * Title text search ---------------------------------------------------

  tabExplore_title_search <- reactiveVal()
  observeEvent(input$tabExplore_title_input, {
    input$tabExplore_title_input %>% tabExplore_title_search()
  }, ignoreInit = TRUE)

  # Set up reactive value to store input molecules from the user
  tabExplore_users_molecules <- reactiveVal()
  observeEvent(input$tabExplore_molecule_input, {
    input$tabExplore_molecule_input %>%
      str_split(., pattern = " |\n") %>%
      unlist() %>%
      # The next step prevents the inclusion of an empty string, if the user
      # starts a new line but doesn't type anything
      str_subset(., pattern = "^$", negate = TRUE) %>%
      tabExplore_users_molecules()
  }, ignoreInit = TRUE)


  # Based on molecules the user searches, get the "Gene Set Name" of articles
  # which contain that molecule(s). This needs to be wrapped in a conditional
  # since we get an error for trying to filter with NULL or an empty line.
  tabExplore_studylabel_with_user_molecules <- reactive({
    if (!all(
      is.null(tabExplore_users_molecules()) |
      tabExplore_users_molecules() == ""
    )) {
      full_data %>%
        filter(
          str_detect(
            string  = str_to_lower(Molecule),
            pattern = str_to_lower(
              paste0(tabExplore_users_molecules(), collapse = "|")
            )
          )
        ) %>%
        pull(`Gene Set Name`)
    }
  })


  # * Filter grouped table ------------------------------------------------

  tabExplore_filtered_table <- reactive({
    full_data %>% filter(

      # Search article titles
      conditional_filter(
        !all(is.null(tabExplore_title_search()) |
               tabExplore_title_search() == ""),
        str_detect(Title, regex(tabExplore_title_search(), ignore_case = TRUE))
      ),

      # Search for specific molecules, when the input is not NULL - it should
      # only be NULL when initializing the app
      conditional_filter(
        !is.null(tabExplore_studylabel_with_user_molecules()),
        `Gene Set Name` %in% unique(tabExplore_studylabel_with_user_molecules())
      ),

      # Filter for all/COVID only/non-COVID only studies
      conditional_filter(
        input$tabExplore_covid_radio_input == "covid_only",
        `Covid Study` == "COVID"
      ),

      conditional_filter(
        input$tabExplore_covid_radio_input == "noncovid_only",
        `Covid Study` == "Non-COVID"
      )
    )
  })

  tabExplore_grouped_table <- reactive({
    tabExplore_filtered_table() %>%
      dplyr::select(
        Title,
        `Gene Set Name`,
        Year,
        PMID,
        Link,
        `Transcriptomic Type`,
        `Covid Study`,
        `Gene Set Length`
      ) %>%
      distinct(`Gene Set Name`, .keep_all = TRUE) %>%
      mutate(PMID = case_when(
        !is.na(PMID) ~ paste0("<a href='", Link, "'>", PMID, "</a>"),
        TRUE ~ paste0("<a href='", Link, "'>Pre-Print</a>")
      )) %>%
      arrange(`Gene Set Name`) %>%
      dplyr::select(!Link) %>%
      dplyr::rename("Link" = PMID)
  })


  # * Render grouped table ------------------------------------------------

  tabExplore_grouped_table_container <- htmltools::withTags(table(
    class = "display",
    thead(tr(
      th("Title"),
      th(
        "Gene Set Name",
        title = paste0(
          "Unique identifier for each gene set. See the About page for more ",
          "details."
        )
      ),
      th("Year"),
      th("Link"),
      th(
        "Transcriptomic Type",
        title = "Type of transcriptomic study performed (Array or RNA-Seq)."
      ),
      th("Covid Study"),
      th("Gene Set Length")
    ))
  ))

  observeEvent({
    input$tabExplore_title_input
    input$tabExplore_covid_radio_input
    input$tabExplore_molecule_input
  }, {
    if (!null_or_nrow0(tabExplore_grouped_table())) {
      output$tabExplore_grouped_DT <- DT::renderDataTable(
        tabExplore_grouped_table(),
        container = tabExplore_grouped_table_container,
        rownames = FALSE,
        escape = FALSE,
        selection = "multiple",
        server = TRUE,
        options = list(
          dom = "tip",
          scrollX = TRUE,
          columnDefs = list(list(targets = 0, render = ellipsis_render(70)))
        )
      )

      output$tabExplore_grouped_render <- renderUI(
        tagList(
          DT::dataTableOutput("tabExplore_grouped_DT"),
          hr(),
          h3(paste0(
            "Click one or more rows in the table above to see all molecules ",
            "from those gene sets."
          ))
        )
      )
    } else {
      output$tabExplore_grouped_render <- renderUI(
        tagList(
          h3(paste0(
            "No results were found which match your search criteria. You can ",
            "use the 'Reset this page' button at the bottom of the sidebar to ",
            "restore the input fields."
          ))
        )
      )
    }
  })


  # * Create clicked table ------------------------------------------------

  tabExplore_clicked_row_studylabel <- reactiveVal(NULL)
  tabExplore_clicked_row_info <- reactiveVal(NULL)
  tabExplore_clicked_table <- reactiveVal(NULL)

  observeEvent(input$tabExplore_grouped_DT_rows_selected, {
    # The "Gene Set Name", used to filter the main table for the study the user
    # selected
    tabExplore_grouped_table()[input$tabExplore_grouped_DT_rows_selected, 2] %>%
      pull(1) %>%
      tabExplore_clicked_row_studylabel()


    # Gather the info for each clicked row/paper and format it for use in naming
    # the download file
    tabExplore_clicked_row_info({
      clicked_genesetnames <-
        tabExplore_grouped_table()[input$tabExplore_grouped_DT_rows_selected, 2] %>%
        pull(1) %>%
        str_trim()

      paste(clicked_genesetnames, collapse = "_")
    })
  }, ignoreNULL = FALSE)

  tabExplore_clicked_table <- reactive({
    if (is.null(tabExplore_clicked_row_studylabel())) {
      return(NULL)
    } else {

      # This conditional allows to set a custom order for the clicked table. In
      # the event the user has searched for a molecule, that molecule(s) will be
      # moved to the top of the table via factor levels. If we haven't searched
      # for a molecule, we need to manually output a list with the same
      # structure as what's created by `set_top_molecules()`.
      if (!all(
        is.null(tabExplore_users_molecules()),
        tabExplore_users_molecules() == ""
      )) {
        full_data %>%
          filter(`Gene Set Name` %in% tabExplore_clicked_row_studylabel()) %>%
          dplyr::select(
            !c(Title, Year, Link, PMID, `Gene Set Length`, Tissue)
          ) %>%
          set_top_molecules(df = ., top = tabExplore_users_molecules())
      } else {
        tabExplore_temp_data <- full_data %>%
          filter(`Gene Set Name` %in% tabExplore_clicked_row_studylabel()) %>%
          dplyr::select(
            !c(Title, Year, Link, PMID, `Gene Set Length`, Tissue)
          ) %>%
          arrange(`Gene Set Name`, Molecule)

        list("df" = tabExplore_temp_data, "top_w_partial" = NULL)
      }
    }
  })


  # * Enable the Send button ----------------------------------------------

  tabExplore_send_geneset_indicator <- reactiveVal(0)

  observeEvent(input$tabExplore_grouped_DT_rows_selected, {
    if (length(tabExplore_clicked_row_studylabel()) == 1) {
      tabExplore_send_geneset_indicator(1)
      enable("tabExplore_send_button")
      runjs(paste0(
        "document.getElementById('tabExplore_send_button').setAttribute(",
        "'title', 'Click here to test this gene set for enriched pathways');"
      ))

    } else {
      tabExplore_send_geneset_indicator(0)
      shinyjs::addClass("tabExplore_send_button", class = "disabled")
      runjs(paste0(
        "document.getElementById('tabExplore_send_button').setAttribute(",
        "'title', 'Select one gene set to enable this feature');"
      ))
    }
  }, ignoreNULL = FALSE)


  # * Render clicked table ------------------------------------------------

  tabExplore_clicked_table_container <- htmltools::withTags(table(
    class = "display",
    thead(tr(
      th("Molecule"),
      th(
        "Gene Set Name",
        title = paste0(
          "Unique identifier for each gene set. See the About page for more ",
          "details."
        )
      ),
      th(
        "Transcriptomic Type",
        title = "Type of transcriptomic study performed for the gene set."
      ),
      th(
        "Gene Set Type",
        title = paste0(
          "Whether the associated study was deriving a signature, or ",
          "performing differential expression/abundance analysis."
        )
      ),
      th(
        "Tissue Class",
        title = paste0(
          "Type of tissue in which the study was performed. See the About ",
          "page for details."
        )
      ),
      th("Timepoint"),
      th("Age Group"),
      th("No. Patients"),
      th("Covid Study"),
      th(
        "Case Condition",
        title = paste0(
          "Condition of interest for the study, compared to the Control ",
          "Condition."
        )
      ),
      th(
        "Control Condition",
        title = paste0(
          "Reference condition for the study, to which patients from the ",
          "Case Condition are compared."
        )
      )
    ))
  ))

  observeEvent(input$tabExplore_grouped_DT_rows_selected, {
    s <- input$tabExplore_grouped_DT_rows_selected

    if (length(s)) {

      if (all(
        !is.null(tabExplore_users_molecules()),
        length(tabExplore_users_molecules() == 0)
      )) {

        j <- tabExplore_clicked_table()$df %>%
          filter(Molecule %in% tabExplore_clicked_table()$top_w_partial) %>%
          nrow()

        output$tabExplore_clicked_DT <- DT::renderDataTable(
          tabExplore_clicked_table()$df,
          container = tabExplore_clicked_table_container,
          rownames = FALSE,
          escape = FALSE,
          selection = list(mode = "multiple", selected = c(1:j)),
          options = list(dom = "ftip", scrollX = TRUE)
        )
      } else {
        output$tabExplore_clicked_DT <- DT::renderDataTable(
          tabExplore_clicked_table()$df,
          container = tabExplore_clicked_table_container,
          rownames= FALSE,
          escape = FALSE,
          selection = list(mode = "multiple"),
          options = list(dom = "ftip", scrollX = TRUE)
        )
      }

    } else {
      output$tabExplore_clicked_DT <- NULL
    }
  }, ignoreNULL = FALSE)

  output$tabExplore_test_clicked_row_data <-
    renderPrint(tabExplore_clicked_row_studylabel())

  output$tabExplore_clicked_render <- renderUI(
    tagList(
      br(),
      # verbatimTextOutput("tabExplore_test_clicked_row_data"),
      DT::dataTableOutput("tabExplore_clicked_DT"),
      br()
    )
  )


  # * Download clicked study ----------------------------------------------

  output$tabExplore_clicked_study_download_handler <- downloadHandler(
    filename = function() {
      paste0(
        "septisearch_download_",
        tabExplore_clicked_row_info(),
        ".txt"
      )
    },
    content = function(filename) {
      readr::write_tsv(
        x = tabExplore_clicked_table()$df,
        file = filename
      )
    }
  )

  output$tabExplore_clicked_study_download_button <- renderUI({
    s <- input$tabExplore_grouped_DT_rows_selected

    if ( !length(s) ) {
      return(NULL)
    } else {
      return(tagList(
        downloadButton(
          outputId = "tabExplore_clicked_study_download_handler",
          label = "Download gene set-specific table",
          class = "btn btn-success"
        )
      ))
    }
  })


  # Visualize -------------------------------------------------------------

  observeEvent(input$tabViz_reset, {
    shinyjs::reset(id = "viz_tab_sidebar", asis = FALSE)
    js$resetClick()
  })

  output$tabViz_select_inputs <- renderUI({
    list(
      selectInput(
        inputId = "tabViz_agegroup_input",
        label = div(
          "Age Group",
          icon(
            "circle-question",
            title = "These groups are based on the sources' description"
          )
        ),
        choices = levels(full_data$`Age Group`),
        multiple = TRUE
      ),

      selectInput(
        inputId = "tabViz_tissue_input",
        label = div(
          "Tissue Class",
          icon(
            "circle-question",
            title = "The About page contains descriptions for each Tissue Class"
          )
        ),
        choices = levels(full_data$`Tissue Class`),
        multiple = TRUE
      ),

      selectInput(
        inputId = "tabViz_timepoint_input",
        label = div(
          "Timepoint",
          icon(
            "circle-question",
            title = "Timepoints are determined from the gene set's source"
          )
        ),
        choices = levels(full_data$Timepoint),
        multiple = TRUE
      )
    )
  })


  # * Apply filters -------------------------------------------------------

  tabViz_filtered_table <- reactive({
    full_data %>% filter(
      # Tissue Class
      conditional_filter(
        length(input$tabViz_tissue_input) != 0,
        `Tissue Class` %in% input$tabViz_tissue_input
      ),

      # Time point
      conditional_filter(
        length(input$tabViz_timepoint_input) != 0,
        Timepoint %in% input$tabViz_timepoint_input
      ),

      # Covid status of studies
      conditional_filter(
        input$tabViz_covid_radio_input == "covid_only",
        `Covid Study` == "COVID"
      ),

      conditional_filter(
        input$tabViz_covid_radio_input == "noncovid_only",
        `Covid Study` == "Non-COVID"
      ),

      # Age Group
      conditional_filter(
        length(input$tabViz_agegroup_input) != 0,
        `Age Group` %in% input$tabViz_agegroup_input
      )
    )
  })


  # * Plotly --------------------------------------------------------------

  tabViz_plot_table <- reactive({

    table_v1 <- tabViz_filtered_table() %>%
      count(Molecule, sort = TRUE, name = "total_count") %>%
      head(200)

    table_v2 <- tabViz_filtered_table() %>%
      count(Molecule, `Covid Study`, sort = TRUE, name = "specific_count")

    table_v3 <- left_join(table_v1, table_v2, by = "Molecule", multiple = "all")

    table_v3 %>%
      mutate(Molecule = factor(Molecule, levels = table_v1$Molecule))
  })


  # Make the plot via plotly, primarily to make use of the "hovertext" feature.
  # The hovertext automatically changes when filtering for COVID/Non-COVID
  # studies, to not repeat the same information.
  output$tabViz_plot_object <- plotly::renderPlotly({
    plotly::plot_ly(
      data = tabViz_plot_table(),
      x = ~Molecule,
      y = ~specific_count,
      type = "bar",
      color = ~`Covid Study`,
      colors = c("COVID" = "#f0ad4e", "Non-COVID" = "#4582ec"),
      hoverinfo = "text",
      hovertext = ~if_else(
        total_count != specific_count,
        paste0(
          "<b>", Molecule, " total: </b>", total_count, "<br>",
          "<b>", `Covid Study`, " only: </b>", specific_count
        ),
        paste0(
          "<b>", Molecule, ": </b>", specific_count
        )
      )
    ) %>%
      plotly::style(
        hoverlabel = list(
          bgcolor = "white",
          bordercolor = "black",
          font_family = "serif"
        )
      ) %>%
      plotly::layout(
        font = list(family = "Georgia", size = 16, color = "black"),
        margin = list(b = 150, t = 25),
        barmode = "stack",
        xaxis = list(
          title = "",
          tickfont = list(size = 12),
          tickangle = "45",
          zeroline = TRUE,
          showline = TRUE,
          mirror = TRUE,
          automargin = TRUE
        ),
        yaxis = list(
          title = "<b>Frequency</b>",
          tick = "outside",
          ticklen = 3,
          zeroline = TRUE,
          showline = TRUE,
          mirror = TRUE,
          automargin = TRUE
        )
      )
  })

  # Create the table holding the data for the molecule/time point based on the
  # user clicking on a bar in plotly output
  tabViz_clicked_molecule_table <- reactive({
    d <- plotly::event_data("plotly_click", priority = "event")
    if (is.null(d)) {
      return(NULL)
    } else {
      filter(tabViz_filtered_table(), Molecule == d$x)
    }
  })

  # Grab the molecule name for later use in naming the download file
  tabViz_clicked_molecule_info <- reactive({
    d <- plotly::event_data("plotly_click", priority = "event")
    if (is.null(d)) {
      return(NULL)
    } else {
      list(molecule = d$x)
    }
  })


  # * Create clicked table ------------------------------------------------

  tabViz_clicked_molecule_table_for_DT <- reactive({
    if ( !is.null(tabViz_clicked_molecule_table()) ) {
      tabViz_clicked_molecule_table() %>%
        mutate(
          Link = case_when(
            !is.na(PMID) ~ paste0("<a href='", Link, "'>", PMID, "</a>"),
            TRUE ~ paste0("<a href='", Link, "'>Pre-Print</a>")
          )
        ) %>%
        dplyr::select(
          Molecule,
          `Gene Set Name`,
          Link,
          `Transcriptomic Type`,
          `Gene Set Type`,
          `Tissue Class`,
          Timepoint,
          `Age Group`,
          `No. Patients`,
          `Covid Study`,
          `Case Condition`,
          `Control Condition`,
        )
    }
  })

  tabViz_table_container <- htmltools::withTags(table(
    class = "display",
    thead(tr(
      th("Molecule"),
      th(
        "Gene Set Name",
        title = paste0(
          "Unique identifier for each gene set. See the About page for more ",
          "details."
        )
      ),
      th("Link"),
      th(
        "Transcriptomic Type",
        title = "Type of transcriptomic study performed for the gene set."
      ),
      th(
        "Gene Set Type",
        title = paste0(
          "Whether the associated study was deriving a signature, or ",
          "performing differential expression/abundance analysis."
        )
      ),
      th(
        "Tissue Class",
        title = paste0(
          "Type of tissue in which the study was performed. See the About ",
          "page for details."
        )
      ),
      th("Timepoint"),
      th("Age Group"),
      th("No. Patients"),
      th("Covid Study"),
      th(
        "Case Condition",
        title = paste0(
          "Condition of interest for the study, compared to the Control ",
          "Condition."
        )
      ),
      th(
        "Control Condition",
        title = paste0(
          "Reference condition for the study, to which patients from the ",
          "Case Condition are compared."
        )
      )
    ))
  ))

  output$tabViz_clicked_plot_table <- DT::renderDataTable(
    expr = {
      if (!is.null(tabViz_clicked_molecule_table_for_DT())) {
        tabViz_clicked_molecule_table_for_DT()
      } else {
        NULL
      }
    },
    container = tabViz_table_container,
    rownames = FALSE,
    escape = FALSE,
    selection = "none",
    options = list(dom = "ftip", scrollX = TRUE)
  )

  # Simple render for testing/debugging purposes; see next chunk to enable it's
  # display
  output$testclick <- renderPrint({
    d <- plotly::event_data("plotly_click")
    if (is.null(d)) {
      "Click to see the values:"
    } else {
      d
    }
  })


  # * Render plot and table UI --------------------------------------------

  output$tabViz_plot_panel <- renderUI({
    tagList(
      h3(
        "Click a bar in the plot to see all database entries for that molecule"
      ),
      h4(paste0(
        "Number of gene sets matching filters: ",
        length(unique(tabViz_filtered_table()$`Gene Set Name`))
      )),

      if (nrow(tabViz_plot_table()) > 0) {
        div(
          plotly::plotlyOutput(
            outputId = "tabViz_plot_object",
            inline = TRUE,
            height = "400px",
            width = "98%"
          ) %>% shinycssloaders::withSpinner(type = 8)
        )
      } else {
        message(
          "\n==INFO: No matching molecules were found for the provided ",
          "criteria\n"
        )

        HTML(paste0(
          "<br><p style='margin-left: 40px; font-size: 20px;'>No molecules were
          found that matched your search criteria. You can use the <i>Restore
          defaults</i> button at the bottom of the sidebar to reset the page, or
          try altering some of your filters. If you think this is an error,
          please visit our <a href=
          'https://github.com/hancockinformatics/SeptiSearch'>Github page</a> to
          open an issue.</p>"
        ))
      },
      # verbatimTextOutput("testclick"),
      br()
    )
  })

  # Render the "clicked" table and the surrounding UI, but only if the table is:
  #   1. Not null (i.e. the user has clicked something)
  #   2. Actually contains data (> 0 rows)
  # This second case is violated when (for example) you click on a bar/molecule
  # then apply a filter that would remove that molecule from the data, creating
  # a table with 0 rows
  output$tabViz_clicked_table_panel <- renderUI({
    if ( !is.null(tabViz_clicked_molecule_table()) ) {
      if ( nrow(tabViz_clicked_molecule_table()) > 0 ) {
        return(
          tagList(
            h4(paste0(
              "Viewing entries for ",
              tabViz_clicked_molecule_info()[["molecule"]],
              ":"
            )),
            div(
              DT::dataTableOutput("tabViz_clicked_plot_table"),
              style = "font-size: 14px"
            )
          )
        )
      }
    } else {
      return(NULL)
    }
  })


  # * Download clicked table ----------------------------------------------

  output$tabViz_clicked_table_download_handler <- downloadHandler(
    filename = function() {
      paste0(
        "septisearch_download_",
        tabViz_clicked_molecule_info()$molecule,
        ".txt"
      )
    },
    content = function(filename) {
      readr::write_tsv(
        x = dplyr::select(tabViz_clicked_molecule_table(), !Link),
        file = filename
      )
    }
  )

  # Render the UI for the download (just the button and an "hr"). The label for
  # the button contains the name of the clicked molecule, which is trimmed and
  # appended with an ellipsis if its too long.
  output$tabViz_clicked_table_download_button <- renderUI({
    if (is.null(tabViz_clicked_molecule_table())) {
      return(NULL)
    } else {
      return(tagList(
        br(),
        downloadButton(
          outputId = "tabViz_clicked_table_download_handler",
          class = "btn-success",
          label = paste0(
            "Download entries for ",
            if_else(
              condition = str_length(tabViz_clicked_molecule_info()$molecule) <= 25,
              true = tabViz_clicked_molecule_info()$molecule,
              false = paste0(
                str_sub(tabViz_clicked_molecule_info()$molecule, end = 22),
                "..."
              )
            )
          )
        )
      ))
    }
  })


  # Enrichment ------------------------------------------------------------

  observeEvent(
    input$tabEnrich_about,
    nav_select("navbar", "about_tab")
  )

  tabEnrich_input_genes <- reactiveVal()
  tabEnrich_input_genes_table <- reactiveVal()
  tabEnrich_test_result <- reactiveVal()

  tabEnrich_example_data_indicator <- reactiveVal(0)

  observeEvent(input$tabEnrich_reset, {
    message("\n==INFO: Tab 'enrich_tab' has been reset...")

    shinyjs::reset("enrich_tab_sidebar", asis = FALSE)

    tabEnrich_input_genes(NULL)
    tabEnrich_input_genes_table(NULL)
    tabEnrich_test_result(NULL)
    tabEnrich_example_data_indicator(0)
    tabEnrich_mapped_genes(NULL)

    disable("tabEnrich_map_button")
    disable("tabEnrich_submit_button")

    output$tabEnrich_results_header <- NULL
    output$tabEnrich_result_tabgroup_ui <- NULL

    removeUI(selector = "#tabEnrich_reset_button_div")
  })


  # * Example data ------------------------------------------------------

  observeEvent(input$tabEnrich_load_example, {
    message("\n==INFO: Example data successfully loaded...")

    tabEnrich_example_data_indicator(1)
    tabEnrich_input_genes(tabEnrich_example_data)
    enable("tabEnrich_map_button")

    showModal(modalDialog(
      title = "Example data successfully loaded.",
      HTML(
        "<p>The example list of 1,117 Ensembl genes has been loaded. You ",
        "can now click <b>1. Perform gene ID mapping</b> to find the ",
        "corresponding Entrez and HGNC identifiers for these genes. Then ",
        "you'll be able to use the <b>2. Submit genes for pathway ",
        "enrichment</b> button to test the example genes for over-",
        "represented pathways."
      ),
      footer = modalButton("OK"),
      easyClose = TRUE
    ))
  })


  # * Import Explore tab data -------------------------------------------

  observeEvent(input$tabExplore_send_button, {
    message("\n==INFO: Loaded selected gene set from Explore tab...")

    nav_select("navbar", selected = "enrich_tab")

    # Fill in the textAreaInput box with the clicked gene set, being sure to
    # collapse the vector to one with genes separated by "\n" to ensure it can
    # be parsed correctly
    updateTextAreaInput(
      session = session,
      inputId = "tabEnrich_pasted_input",
      value = tabExplore_clicked_table()$df %>%
        pull(1) %>%
        as.character() %>%
        sort() %>%
        paste(collapse = "\n")
    )

    enable("tabEnrich_map_button")
    runjs(paste0(
      "document.getElementById('tabEnrich_map_button').setAttribute(",
      "'title', 'Click here to map your genes');"
    ))
  })


  # * Parse molecule input ----------------------------------------------

  observeEvent(input$tabEnrich_pasted_input, {
    tabEnrich_example_data_indicator(0)

    input$tabEnrich_pasted_input %>%
      str_split(., pattern = " |\n") %>%
      unlist() %>%
      str_subset(., pattern = "^$", negate = TRUE) %>% # Remove empty lines
      as.character() %>%
      tabEnrich_input_genes()
  })

  # Enable the Map button once we have some input from the user
  observeEvent({
    input$tabEnrich_load_example
    input$tabEnrich_pasted_input
  }, {
    if (length(tabEnrich_input_genes()) > 0) {
      message("\n==INFO: Input detected, enabling 'Map' button...")
      enable("tabEnrich_map_button")

      runjs(paste0(
        "document.getElementById('tabEnrich_map_button').setAttribute(",
        "'title', 'Click here to map your genes');"
      ))
    }
  })


  # * Map genes ---------------------------------------------------------

  tabEnrich_mapped_genes <- reactiveVal()

  observeEvent(input$tabEnrich_map_button, {
    req(tabEnrich_input_genes())

    map_genes(gene_list  = tabEnrich_input_genes()) %>%
      tabEnrich_mapped_genes()
  })

  # If the mapping returns some genes (i.e. input is valid) then enable the
  # second button to run the enrichment tests
  observeEvent(input$tabEnrich_map_button, {
    if ( !is.null(tabEnrich_mapped_genes()) ) {
      message("\n==INFO: Gene mapping complete, enabling 'Submit' button...")
      enable("tabEnrich_submit_button")

      runjs(paste0(
        "document.getElementById('tabEnrich_submit_button').setAttribute(",
        "'title', 'Click here to test your genes for enriched pathways');"
      ))

      showModal(modalDialog(
        title = "Input gene mapping complete!",
        HTML(paste(
          "Your",
          length(tabEnrich_input_genes()),
          attr(tabEnrich_mapped_genes(), "id_type"),
          "genes were successfully mapped. You can now proceed with testing",
          "them for enriched pathways/terms using the <b>2. Submit genes for",
          "pathway enrichment</b> button.",
          collapse = " "
        )),
        footer = modalButton("OK"),
        easyClose = TRUE
      ))
    } else {
      message("ERROR: There was a problem with gene mapping...")
      showModal(modalDialog(
        title = "Input error!",
        paste0(
          "There was a problem mapping your genes; please ensure they are ",
          "either Ensembl, HGNC, or Entrez IDs (one per line) and try again."
        ),
        footer = modalButton("OK")
      ))
    }
  })


  # * Perform enrichment tests ------------------------------------------

  observeEvent(input$tabEnrich_submit_button, {

    showModal(modalDialog(
      title = span(
        div(
          icon(name = "spinner", class = "fa fa-spin"),
          "Enrichment testing in progress...",
        )
      ),
      paste0(
        "We are currently testing your ",
        length(tabEnrich_input_genes()),
        " ",
        attr(tabEnrich_mapped_genes(), "id_type"),
        " input genes. Please wait for your results to appear on this page; ",
        "note it may take up to 30 seconds to run the enrichment tests."
      ),
      footer = NULL
    ))

    test_enrichment(tabEnrich_mapped_genes()) %>%
      tabEnrich_test_result()
  })

  # Take the initial results objects and tidy it for display
  tabEnrich_test_result_clean <- reactive({
    req(tabEnrich_test_result())

    if (!any(map_lgl(tabEnrich_test_result(), ~is.null(.x)))) {
      list(
        ReactomePA = tabEnrich_test_result()$ReactomePA %>%
          mutate(across(where(is.numeric), ~signif(.x, digits = 3))) %>%
          janitor::clean_names("title", abbreviations = c("BG", "ID")) %>%
          dplyr::rename("P Value" = Pvalue, "Adjusted P Value" = `P Adjust`),

        enrichR = tabEnrich_test_result()$enrichR %>%
          mutate(across(where(is.numeric), ~signif(.x, digits = 3))) %>%
          janitor::clean_names("title", abbreviations = "P")
      )
    } else {
      return(NULL)
    }
  })


  # * Output results tables ---------------------------------------------

  tabEnrich_ReactomePA_container <- htmltools::withTags(table(
    class = "display",
    thead(tr(
      th(
        "ID",
        title = "Reactome ID for the pathway which links to the relevant page."
      ),
      th(
        "Description",
        title = "Name and description of the pathway."
      ),
      th(
        "Shared Genes",
        title = paste0(
          "Overlap of input genes and genes in a pathway (i.e. shared or ",
          "common genes)."
        )
      ),
      th(
        "Genes in Pathway",
        title = "Total number of genes annotated to a particular pathway."
      ),
      th(
        "Gene Ratio",
        title = paste0(
          "Ratio of shared genes divided by the total number of genes in ",
          "a pathway."
        )
      ),
      th(
        "P Value",
        title = "Statistical significance of the result."
      ),
      th(
        "Adjusted P Value",
        title = paste0(
          "Statistical significance of the result, adjusted for multiple ",
          "testing."
        )
      )
    ))
  ))

  tabEnrich_enrichR_container <- htmltools::withTags(table(
    class = "display",
    thead(tr(
      th(
        "Database",
        title = "Source of the term."
      ),
      th(
        "Term",
        title = paste0(
          "Pathway, gene set or GO term being tested, which links to the ",
          "relevant page."
        )
      ),
      th(
        "P Value",
        title = "Statistical significance of the gene set or GO term."
      ),
      th(
        "Adjusted P Value",
        title = paste0(
          "Statistical significance of the gene set or GO term, adjusted ",
          "for multiple testing."
        )
      )
    ))
  ))

  observeEvent(input$tabEnrich_submit_button, {

    # Header for the results section
    output$tabEnrich_results_header <- renderUI(
      tagList(
        h1("Pathway Enrichment Results"),
        p(
          "Use the buttons below to see your results from each method, and ",
          "check the bottom of the sidebar for download links."
        )
      )
    )

    # For each subsequent chunk, if there were no significant results (0 rows,
    # but no errors) then simply display a message instead of an empty table
    output$tabEnrich_result_tabgroup_ui <- renderUI(
      div(
        tabsetPanel(
          id = "tabEnrich_result_tabgroup_ui",
          tabPanel(
            title = "ReactomePA",
            uiOutput("tabEnrich_result_ReactomePA_ui")
          ),
          tabPanel(
            title = "enrichR",
            uiOutput("tabEnrich_result_enrichR_ui")
          )
        )
      )
    )

    # ReactomePA. The "ID" column is modified to link to the respective
    # Reactome page.
    if (nrow(tabEnrich_test_result_clean()$ReactomePA) > 0) {
      output$tabEnrich_result_ReactomePA <- DT::renderDataTable(
        datatable(
          tabEnrich_test_result_clean()$ReactomePA %>%
            mutate(
              ID = paste0(
                "<a href='https://reactome.org/content/detail/", ID, "'>",
                ID,
                "</a>"
              )
            ),
          container = tabEnrich_ReactomePA_container,
          rownames = FALSE,
          escape = FALSE,
          selection = "none",
          options = list(
            dom = "ftip",
            columnDefs = list(list(targets = 1, render = ellipsis_render(65)))
          )
        )
      )
      output$tabEnrich_result_ReactomePA_ui <- renderUI(tagList(
        br(),
        dataTableOutput("tabEnrich_result_ReactomePA")
      ))
    } else {
      output$tabEnrich_result_ReactomePA_ui <- renderUI(tagList(
        br(),
        h4("No significant results found.")
      ))
    }


    # enrichR. The "Term" column is modified in the DT output to create links
    # to the respective page for each term.
    if ( nrow(tabEnrich_test_result_clean()$enrichR) > 0 ) {
      output$tabEnrich_result_enrichR <- DT::renderDataTable(
        datatable(
          tabEnrich_test_result_clean()$enrichR %>%
            mutate(
              Term = case_when(
                Database == "MSigDB_Hallmark_2020" ~ paste0(
                  "<a href='https://www.gsea-msigdb.org/gsea/msigdb/cards/HALLMARK_",
                  str_replace_all(
                    string = str_to_upper(Term),
                    c("-" = "", " +" = "_", "/" = "_",
                      "TNFALPHA" = "TNFA", "TGFBETA" = "TGF_BETA",
                      "PPEROXISOME" = "PEROXISOME", "WNTBETA" = "WNT_BETA")
                  ),
                  "'>",
                  Term,
                  "</a>"
                ),
                str_detect(Database, "^GO_") ~ paste0(
                  "<a href='http://amigo.geneontology.org/amigo/term/",
                  str_extract(Term, "GO\\:[0-9]{5,9}"),
                  "'>",
                  Term,
                  "</a>"
                )
              )
            ),
          container = tabEnrich_enrichR_container,
          rownames = FALSE,
          escape = FALSE,
          selection = "none",
          options = list(dom = "ftip")
        )
      )
      output$tabEnrich_result_enrichR_ui <- renderUI(tagList(
        br(),
        dataTableOutput("tabEnrich_result_enrichR")
      ))
    } else {
      output$tabEnrich_result_enrichR_ui <- renderUI(tagList(
        br(),
        h4("No significant results found.")
      ))
    }
  })

  # Once the mapping is finished, remove the modal dialog box
  observeEvent(input$tabEnrich_submit_button, {
    if (!any(map_lgl(tabEnrich_test_result_clean(), ~is.null(.x)))) {
      removeModal()
    }
  })


  # * Download results --------------------------------------------------

  output$tabEnrich_mapping_info <- renderUI({
    if (any(
      is.null(tabEnrich_test_result_clean()$ReactomePA),
      is.null(tabEnrich_test_result_clean()$enrichR)
    )) {
      return(NULL)
    } else {
      tagList(
        hr(),
        h5("Mapping results"),
        make_mapping_success_message(isolate(tabEnrich_mapped_genes())),
        h5("Enrichment results"),
        make_enrichment_success_message(isolate(tabEnrich_test_result_clean())),
        hr()
      )
    }
  })

  # First the button for ReactomePA...
  output$tabEnrich_ReactomePA_download_handler <- downloadHandler(
    filename = function() {
      if (tabEnrich_example_data_indicator() == 1) {
        "septisearch_ReactomePA_result_example_data.txt"
      } else if (tabExplore_send_geneset_indicator() == 1) {
        paste0(
          "septisearch_ReactomePA_result_",
          str_replace(tabExplore_clicked_row_studylabel(), " ", "_"),
          ".txt"
        )
      } else {
        "septisearch_ReactomePA_result_users_genes.txt"
      }
    },
    content = function(filename) {
      readr::write_tsv(
        x = tabEnrich_test_result_clean()$ReactomePA,
        file = filename
      )
    }
  )

  observeEvent(input$tabEnrich_submit_button, {
    output$tabEnrich_ReactomePA_download_button <- renderUI({
      if (null_or_nrow0(tabEnrich_test_result_clean()$ReactomePA)) {
        return(NULL)
      } else {
        return(
          downloadButton(
            outputId = "tabEnrich_ReactomePA_download_handler",
            class = "btn-success",
            label = "Download ReactomePA results"
          )
        )
      }
    })
  })


  # ...and a second button for enrichR
  output$tabEnrich_enrichR_download_handler <- downloadHandler(
    filename = function() {

      if (tabEnrich_example_data_indicator() == 1) {
        "septisearch_enrichR_result_example_data.txt"
      } else if (tabExplore_send_geneset_indicator() == 1) {
        paste0(
          "septisearch_enrichR_result_",
          str_replace(tabExplore_clicked_row_studylabel(), " ", "_"),
          ".txt"
        )
      } else {
        "septisearch_enrichR_result_users_genes.txt"
      }
    },
    content  = function(filename) {
      readr::write_tsv(
        x = tabEnrich_test_result_clean()$enrichR,
        file = filename
      )
    }
  )

  observeEvent(input$tabEnrich_submit_button, {
    output$tabEnrich_enrichR_download_button <- renderUI({
      if ( null_or_nrow0(tabEnrich_test_result_clean()$enrichR) ) {
        return(NULL)
      } else {
        return(
          downloadButton(
            outputId = "tabEnrich_enrichR_download_handler",
            class = "btn-success",
            label = "Download enrichR results"
          )
        )
      }
    })
  })


  # * Reset button --------------------------------------------------------

  observeEvent(input$tabEnrich_submit_button, {
    insertUI(
      selector = "#tabEnrich_placeholder_div",
      where = "afterEnd",
      ui = tagList(
        div(
          id = "tabEnrich_reset_button_div",
          hr(),
          actionButton(
            inputId = "tabEnrich_reset",
            class = "btn-warning",
            icon = icon("rotate-left"),
            label = "Reset this page",
            width = "100%"
          )
        )
      )
    )
  })


  # GSVA ------------------------------------------------------------------

  observeEvent(
    input$tabGSVA_about,
    nav_select("navbar", "about_tab")
  )

  # Define initial reactive values for inputs
  tabGSVA_expr_input_1 <- reactiveVal()
  tabGSVA_example_indicator <- reactiveVal(0)

  tabGSVA_meta_input_1 <- reactiveVal(NULL)
  tabGSVA_meta_input_2 <- reactiveVal(NULL)


  # * Load example or user data -------------------------------------------

  observeEvent(input$tabGSVA_load_example_data, {
    message("\n==INFO: Loading example expression data...")
    tabGSVA_expr_input_1(tabGSVA_example_data$expr)

    message("\n==INFO: Loading example metadata...")
    tabGSVA_meta_input_1(as.data.frame(tabGSVA_example_data$meta))

    tabGSVA_example_indicator(1)
  })


  observeEvent(input$tabGSVA_matrix_input, {
    message("\n==INFO: Loading expression data from user...")
    tabGSVA_expr_input_1(read.csv(input$tabGSVA_matrix_input$datapath))
  })


  # * Process expression data ---------------------------------------------

  tabGSVA_expr_input_2 <- reactive({
    req(tabGSVA_expr_input_1())

    if ( str_detect(tabGSVA_expr_input_1()[1, 1], pattern = "^ENSG") ) {

      if ( is.double(as.matrix(tabGSVA_expr_input_1()[, -1])) ) {
        gsva_temp_data <- tabGSVA_expr_input_1() %>% as.data.frame()

        rownames(gsva_temp_data) <- gsva_temp_data[, 1]
        gsva_temp_data <- gsva_temp_data[, -1]

        if (tabGSVA_example_indicator() == 1) {
          showModal(modalDialog(
            title = span("Example data loaded.", style = "color: #3fad46;"),
            HTML(
              "The example expression data and matching metadata has been ",
              "successfully loaded; you can now use the <b>Submit expression ",
              "data for GSVA</b> button to proceed with the analysis."
            ),
            footer = modalButton("Continue"),
            easyClose = TRUE
          ))
        } else {
          showModal(modalDialog(
            title = span("Input Success!", style = "color: #3fad46;"),
            HTML(
              "Your data was successfully uploaded and parsed. Please ensure ",
              "it looks correct in the preview table before proceeding (note ",
              "not all genes/samples are displayed). You may also upload ",
              "metadata for your samples (e.g. treatment type, disease ",
              "status, etc)."
            ),
            footer = modalButton("Continue"),
            easyClose = TRUE
          ))
        }

        return(gsva_temp_data)

      } else {
        message("ERROR: GSVA input detected as raw counts!")

        showModal(modalDialog(
          title = span("Input Error!", style = "color:red;"),
          paste0(
            "Your data appears to not be normalized/transformed. Please ",
            "ensure you apply the proper transformation to your data before ",
            "attempting GSVA."
          ),
          footer = modalButton("OK")
        ))
        return(NULL)
      }

    } else {
      message("ERROR: Unspecified error!")

      showModal(modalDialog(
        title = span("Input Error!", style = "color:red;"),
        HTML(
          "There was an unspecified problem with your input; please ensure it",
          "meets all of the stated criteria, then try again. If the problem ",
          "persists you can open an issue at our ",
          "<a href='https://github.com/hancockinformatics/SeptiSearch/issues'>",
          "Github page</a>."
        ),
        footer = modalButton("OK")
      ))
      return(NULL)
    }
  })

  # Create a preview of the user's input data
  tabGSVA_user_input_max_cols <- reactive({
    req(tabGSVA_expr_input_2())

    if (ncol(tabGSVA_expr_input_2()) >= 7) {
      return(7)
    } else {
      return(ncol(tabGSVA_expr_input_2()))
    }
  })

  output$tabGSVA_input_preview_table <- DT::renderDataTable(
    tabGSVA_expr_input_2()[1:5, 1:tabGSVA_user_input_max_cols()],
    rownames = TRUE,
    options = list(dom = "t")
  )

  observeEvent(tabGSVA_expr_input_1(), {
    req(tabGSVA_expr_input_2())
    insertUI(
      selector = "#tabGSVA_placeholder_div",
      where = "afterEnd",
      ui = tagList(div(
        id = "tagGSVA_input_data_preview_div",
        h3("Input data preview"),
        HTML(
          "<p>The table below shows the <i>first few</i> rows and columns of ",
          "your data. Ensembl gene IDs should fill the rownames, while each ",
          "column corresponds to a sample. If the data looks OK, you can ",
          "proceed using the <b>Submit expression data for GSVA </b> button ",
          "at the bottom of the sidebar.</p>"
        ),
        br(),
        dataTableOutput("tabGSVA_input_preview_table")
      ))
    )
  })


  # * Process metadata ----------------------------------------------------

  observeEvent(input$tabGSVA_metadata_input, {
    message("\n==INFO: Loading metadata from user...")
    read.csv(input$tabGSVA_metadata_input$datapath) %>%
      tabGSVA_meta_input_1()
  })

  observeEvent(tabGSVA_meta_input_1(), {
    if ( !is.null(tabGSVA_meta_input_1()) ) {
      if (all(tabGSVA_meta_input_1()[, 1] %in% colnames(tabGSVA_expr_input_2()))) {

        gsva_temp_metadata <- tabGSVA_meta_input_1()

        rownames(gsva_temp_metadata) <- tabGSVA_meta_input_1()[, 1]
        gsva_temp_metadata <- gsva_temp_metadata[, -1]

        message("\n==INFO: Successfully parsed GSVA metadata...")
        tabGSVA_meta_input_2(gsva_temp_metadata)
      } else {
        message(paste0(
          "\nERROR: Problem detected with GSVA metadata (non-matching sample ",
          "names)..."
        ))

        showModal(modalDialog(
          title = span("Input Error!", style = "color:red;"),
          HTML(
            "There was a problem matching the samples from your metadata ",
            "(rows) to the columns of your expression data. Please ensure all ",
            "samples match between the two files, without any missing or ",
            "extra samples, then try again."
          ),
          footer = modalButton("OK")
        ))
        tabGSVA_meta_input_2(NULL)
      }
    } else {
      tabGSVA_meta_input_2(NULL)
    }
  })


  # * Run GSVA ------------------------------------------------------------

  observeEvent(tabGSVA_expr_input_1(), {
    req(tabGSVA_expr_input_2())

    message("\n==INFO: Expression input OK, enabling submission...")
    enable("tabGSVA_submit_button")

    if (!is.null(tabGSVA_meta_input_2())) {
      runjs(paste0(
        "document.getElementById('tabGSVA_submit_button').setAttribute(",
        "'title', 'Click here to run GSVA');"
      ))
    } else {
      runjs(paste0(
        "document.getElementById('tabGSVA_submit_button').setAttribute(",
        "'title', 'Upload optional metadata, or click here to run GSVA');"
      ))
    }
  })

  observeEvent(tabGSVA_meta_input_1(), {
    req(tabGSVA_meta_input_2())

    message("\n==INFO: Updating tooltip r.e. metadata...")

    if (!is.null(tabGSVA_expr_input_2())) {
      runjs(paste0(
        "document.getElementById('tabGSVA_submit_button').setAttribute(",
        "'title', 'Click here to run GSVA');"
      ))
    } else {
      runjs(paste0(
        "document.getElementById('tabGSVA_submit_button').setAttribute(",
        "'title', 'Upload your expression data, then click here to run GSVA');"
      ))
    }
  })


  # Remove the input preview, show a modal dialog, and run GSVA
  tabGSVA_result_1 <- reactiveVal()

  observeEvent(input$tabGSVA_submit_button, {
    message("\n==INFO: Running GSVA:")

    removeUI("#tagGSVA_input_data_preview_div")

    showModal(modalDialog(
      title = div(
        icon(name  = "spinner", class = "fa fa-spin"),
        "Running GSVA...",
      ),
      HTML(
        "Your input expression data is currently being analyzed. Please ",
        "wait for your results to appear. Note that if you submitted data ",
        "containing a large number of samples, it will take some time to ",
        "analyze; please be patient."
      ),
      footer = NULL
    ))

    perform_gsva(
      expr = tabGSVA_expr_input_2(),
      gene_sets = full_data_gsva_tab_genesets,
      metadata  = tabGSVA_meta_input_2()
    ) %>% tabGSVA_result_1()
  })

  # Remove modal dialog once we have some results to show
  observeEvent(input$tabGSVA_submit_button, {
    if (!is.null(tabGSVA_result_1())) {
      message("\n==INFO: GSVA has completed, rendering results...")
      removeModal()
    }
  })


  # * Render results table ------------------------------------------------

  tabGSVA_result_summary <- reactive({

    # Summary table that is displayed above the heatmap
    list(
      "summary_tbl" = left_join(
        tabGSVA_result_1()[["gsva_res_df"]],
        full_data_gsva_tab,
        by = "Gene Set Name"
      ) %>%
        dplyr::select(
          `Gene Set Name`,
          `No. Genes in Set`,
          `No. Shared Genes`,
          Title
        ),

      # Results from GSVA plus the gene set info columns; this is what the user
      # can download.
      "gsva_res_df" = left_join(
        tabGSVA_result_1()[["gsva_res_df"]],
        full_data_gsva_tab,
        by = "Gene Set Name"
      ) %>%
        dplyr::select(
          `Gene Set Name`,
          `No. Genes in Set`,
          `No. Shared Genes`,
          Title,
          everything()
        ),

      "gsva_res_plt" = tabGSVA_result_1()[["gsva_res_plt"]]
    )
  })

  # Define a table "container" so that we can have title elements (hover text)
  # on column names to explain each column.
  tabGSVA_table_container <- htmltools::withTags(table(
    class = "display",
    thead(tr(
      th(
        "Gene Set Name",
        title = "Name of the sepsis signature/gene set"
      ),
      th(
        "No. Genes in Set",
        title = "Number of molecules in the gene set"
      ),
      th(
        "No. Shared Genes",
        title = "Number of molecules from the set present in the input data"
      ),
      th(
        "Title",
        title = "Title of the article on which the gene set is based"
      )
    ))
  ))

  output$tabGSVA_result_DT <- DT::renderDataTable(
    datatable(
      tabGSVA_result_summary()[["summary_tbl"]],
      container = tabGSVA_table_container,
      rownames  = FALSE,
      selection = "none",
      options   = list(
        dom = "ftip",
        columnDefs = list(
          list(targets = 3, render = ellipsis_render(95))
        )
      )
    )
  )

  output$tabGSVA_result_UI <- renderUI({
    req(tabGSVA_result_1())
    tagList(
      h3("Summary table of GSVA results:"),
      dataTableOutput("tabGSVA_result_DT")
    )
  })


  # * Render the heatmap --------------------------------------------------

  observeEvent(input$tabGSVA_submit_button, {
    if (!is.null(tabGSVA_result_summary()[["gsva_res_plt"]])) {
      output$tabGSVA_heatmap_UI <- renderUI(
        tagList(
          h3("Heatmap of GSVA results:"),
          div(
            title = paste0(
              "To save the heatmap as a PNG, right click anywhere on this ",
              "image and select \"Save Image...\""),
            renderPlot(
              tabGSVA_result_summary()[["gsva_res_plt"]],
              height = 1700,
              alt = "Heatmap of GSVA results."
            )
          )
        )
      )
    }
  })


  # * Download results ----------------------------------------------------

  observeEvent(input$tabGSVA_submit_button, {
    if ( !is.null(tabGSVA_result_summary()[["gsva_res_df"]]) ) {
      output$tabGSVA_result_downloadhandler <- downloadHandler(
        filename = function() {
          if (tabGSVA_example_indicator() == 1) {
            "septisearch_GSVA_example_data_result.csv"
          } else {
            paste0(
              "septisearch_",
              tools::file_path_sans_ext(input$tabGSVA_matrix_input$name),
              "_GSVA_result.csv"
            )
          }
        },
        content = function(filename) {
          readr::write_csv(
            x = tabGSVA_result_summary()[["gsva_res_df"]],
            file = filename
          )
        }
      )

      output$tabGSVA_result_downloadbutton <- renderUI(
        tagList(
          hr(),
          tags$label("GSVA results"),
          p(
            "Your GSVA was run successfully! To the right is a table
            summarizing the results, and below that is a heatmap visualizing
            the GSVA output. You can use the button below to download the full
            results table as a CSV file."
          ),
          downloadButton(
            outputId = "tabGSVA_result_downloadhandler",
            class = "btn-success",
            label = "Download full table of GSVA results"
          )
        )
      )
    } else {
      output$tabGSVA_result_downloadbutton <- renderUI(
        tagList(
          hr(),
          HTML(
            "<p>There was a problem in running your data through GSVA. Please ",
            "ensure your input meets all of the criteria listed above, then ",
            "refresh the page, reupload your data, and try again. If the ",
            "problem persists, you can submit an issue at our ",
            "<a href='https://github.com/hancockinformatics/SeptiSearch'>",
            "Github page</a>.</p>"
          )
        )
      )
    }
  })

}

shinyApp(septisearch_ui, septisearch_server)
