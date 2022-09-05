# 1. App setup ------------------------------------------------------------

message(paste0(
  "\n=========== SeptiSearch Start ============\n",
  "Loading packages and sourcing functions...\n"
))

# Loads the data files, import custom functions, and minimum calls to library()
# to get the app to appear. Most packages are loaded in "deferred.R", which is
# sourced at the beginning of the `server()` call, so the app's UI appears more
# quickly while the remaining packages load in the background.
library(shinyjs)




# 2. UI sections ----------------------------------------------------------

ui <- fluidPage(

  # Specify that all links should open in a new tab. The "rel" specification is
  # security-related, to prevent the new tab from being able to access/influence
  # the original tab.
  HTML("<base target='_blank' rel='noopener noreferrer'>"),

  # Select the Bootswatch3 theme "Readable": https://bootswatch.com/3/readable
  theme = "css/readablebootstrap.css",

  # Head linking to custom CSS tweaks and favicons
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/user.css"),

    tags$link(
      rel   = "icon",
      type  = "image/svg",
      sizes = "32x32",
      href  = "/favicon_32x32.svg"
    ),

    tags$link(
      rel   = "icon",
      type  = "image/svg",
      sizes = "16x16",
      href  = "/favicon_16x16.svg"
    )
  ),

  # Enable shinyjs usage (tab reset buttons)
  useShinyjs(),

  # Using shinyjs to allow the reset buttons to also reset "plotly_click" by
  # setting it to NULL (initial value). Check the "www" directory for the
  # indicated file & function.
  extendShinyjs(
    script    = "functions.js",
    functions = c("resetClick")
  ),


  ### Begin the navbarPage that serves as the basis for the app
  navbarPage(
    id          = "navbar",
    position    = "static-top",
    windowTitle = "SeptiSearch",

    # Custom nested divs for the title, so we can have our custom logo on the
    # left, and the Github logo (which links to the Github page) on the right
    # side of the navbar. See "user.css" for the custom changes being applied
    # to the Github image.
    title = div(
      # strong("SeptiSearch"),
      HTML(
        "<img src='septisearch_S.svg' height='50' alt='S'
        title='This is our logo!'
        style='font-weight: bold;'>"
      ),

      # Custom div containing the Github logo for the right side of the navbar
      div(
        id = "img-id",
        HTML(paste0(
          "<a href='https://github.com/hancockinformatics/SeptiSearch'> ",
          "<img src='github.svg' title='Visit SeptiSearch on Github to browse ",
          "the code or submit an issue.' alt='Github'> </a>"
        ))
      )
    ),



    # |- 2.a Home ---------------------------------------------------------

    tabPanel(
      value = "home_tab",
      icon  = icon("house"),
      title = span("SeptiSearch", title = "The homepage for SeptiSearch."),

      # Add a div to inform users that the site will be down for maintenance
      # div(
      #   class = "jumbotron",
      #   style = paste0(
      #     "background-color: #d9534f; color: white; ",
      #     "text-align: center;"
      #   ),
      #   p(
      #     "SeptiSearch may be temporarily unavailable from August 31st to
      #     September 7th while updates are being performed. We apologize for any
      #     inconvenience.",
      #     style = "margin-bottom: 0;"
      #   )
      # ),

      div(
        class = "jumbotron",

        h1("Welcome"),
        hr(),

        div(
          class = "logoWrapper-home",

          p(HTML(
            "Welcome to <span style='color:#4582ec;'><b>SeptiSearch</b></span>!
            Here you can browse, explore, and download curated molecular results
            derived from sepsis studies. The app currently catalogs over 25,000
            unique molecules from more than 100 publications."
          )),

          p(HTML(
            "To get started, select one of the tabs at the top of the page,
            described below:

            <ul style='font-size: 24px;'>

            <li><em>Explore the Database</em> is the easiest way to
            explore the curated data, organized by publication</li>

            <li><em>Visualize the Database</em> displays the most
            cited molecules in the database, and allows easy viewing of all
            entries for any molecule of interest</li>

            <li><em>Perform Pathway Enrichment</em> allows you to upload a list
            of genes and test for enriched pathways/biological terms using
            <a href='https://bioconductor.org/packages/ReactomePA/'>ReactomePA
            </a> and <a href='https://maayanlab.cloud/Enrichr/'>enrichR</a></li>

            <li><em>Test for Enriched Sepsis Gene Sets</em> makes it easy to
            upload your own expression data to determine if it's enriched for
            any of the curated sepsis gene sets</li>

            </ul>"
          )),

          p(HTML(
            "SeptiSearch was created by Travis Blimkie, Jasmine Tam & Arjun
            Baghela from the <a href='http://cmdr.ubc.ca/bobh/'>Hancock Lab</a>
            at the University of British Columbia. If you'd like to learn more,
            or to report bugs or issues, click the button below to visit our
            <em>About</em> page."
          )),

          br(),

          div(
            # Button that includes a spinner to show the app is loading. Gets
            # removed once all packages have been lazy loaded, and replaced with
            # "Get Started" button. See "www/js/client.js" for details.
            actionButton(
              inputId = "get_started",
              label   = "Initializing app...",
              class   = "btn btn-primary btn-lg disabled",
              title   = "Please wait while the app loads...",
              icon    =  icon(
                name  = "spinner",
                class = "fa fa-spin"
              )
            ),

            HTML("&nbsp;&nbsp;&nbsp;"),

            # Provide a direct link to the "About" page. This is also hidden
            # until the app has finished lazy loading packages.
            actionButton(
              inputId = "learn_more",
              label   = "Learn more",
              class   = "btn btn-primary btn-lg btn-hidden",
              title   = "Visit our About page!"
            )
          )
        )
      ),


      # Place the wordcloud below the jumbotron and centered horizontally. The
      # latter is achieved via a CSS class in "www/css/user.css".
      div(HTML(paste0(
        "<img src='wordcloud.svg' class='center'
        title='Here&#39;s the 90 most common molecules in the database!'>"
      ))),

      br(),
      br(),

      # Include the lab logo in the bottom left corner, below the wordcloud
      div(
        style = "float: left; padding-bottom: 10px;",
        HTML(
          "<a href='http://cmdr.ubc.ca/bobh/'><img src='hancock-lab-logo.svg'
          title='Visit the Hancock Lab website!'> </a>"
        )
      ),

      # Include the CIHR logo in the bottom right corner
      div(
        style = "float: right; padding-bottom: 10px;",
        HTML(
          "<a href='https://cihr-irsc.gc.ca/e/193.html'><img src='cihr_logo.svg'
          title='This work was funded by the CIHR.'> </a>"
        )
      )
    ),


    # |- 2.b Explore the Database -----------------------------------------

    tabPanel(
      value = "study_tab",
      icon  = icon("table"),

      title = span(
        "Explore the Database",
        title = paste0(
          "Browse and search the entire data collection, organized by ",
          "study/article."
        )
      ),

      sidebarLayout(
        sidebarPanel = sidebarPanel(
          id    = "study_tab_sidebar",
          width = 3,

          h4("Explore the Database", style = "margin-top: 0"),
          p(
            "Here you can browse the database by Study Label (a unique name for
            each gene set based on the author), where one publication may
            contain multiple sets (e.g. different patient groups were included).
            To the right, the top table shows each set included in the database,
            and the number of molecules it contains. You can search the articles
            by title, or filter the studies to those containing specific
            molecules (case sensitive)."
          ),

          p(
            "By clicking on one or more rows in the top table, another table
            with all the molecules in those sets will appear below. You can
            download this second table via the button which appears at the
            bottom of this sidebar."
          ),

          hr(),

          # Input for the user to search article titles
          textAreaInput(
            inputId     = "tabStudy_title_input",
            label       = "Search article titles",
            placeholder = "E.g. 'COVID-19'",
            height      = 41,
            resize      = "none",
          ),

          # Radio buttons for selecting type of studies to include r.e. Covid
          radioButtons(
            inputId = "tabStudy_covid_radio_input",
            label = "Covid inclusion:",
            choices = c(
              "All studies"    = "all_studies",
              "COVID only"     = "covid_only",
              "Non-COVID only" = "noncovid_only"
            ),
            selected = "all_studies"
          ),

          # This is the new input for user molecules.
          textAreaInput(
            inputId     = "tabStudy_molecule_input",
            label       = "Search for specific molecules",
            placeholder = "S100A9\nGYG1\nSTAT4\nTLR5\n...",
            height      = 150,
            resize      = "vertical"
          ),

          # UI for the download button
          uiOutput("tabStudy_clicked_study_download_button"),
          hr(),

          # Reset button for the tab (from shinyjs) - note this mostly relies on
          # normal R/Shiny code to work, since it's resetting DT stuff which
          # doesn't respond to the shinyjs reset button (i.e. we have to
          # manually reset the state of variables/DT tables).
          actionButton(
            class   = "btn-info",
            style   = "width: 170px",
            inputId = "tabStudy_reset",
            icon    = icon("rotate-left"),
            label   = "Restore defaults"
          )
        ),

        mainPanel = mainPanel(
          width = 9,
          uiOutput("tabStudy_grouped_render"),
          uiOutput("tabStudy_clicked_render")
        )
      )
    ),


    # |- 2.c Visualize the Database ---------------------------------------

    tabPanel(
      value = "viz_tab",
      icon  = icon("chart-bar"),

      title = span(
        "Visualize the Database",
        title = paste0(
          "See the most-cited molecules and easily view & download all of ",
          "their entries."
        )
      ),

      sidebarLayout(
        sidebarPanel = sidebarPanel(
          id    = "viz_tab_sidebar",
          width = 3,

          h4("Visualize the Database", style = "margin-top: 0"),

          p(
            "The plot on the right displays the most common molecules in the
            database. You can hover over the bars with your cursor to see the
            molecule's name and its number of entries."
          ),

          p(HTML(
            "The inputs below will automatically filter the data displayed in
            the plot. For example, you can see which molecules are most common
            in 'Whole Blood' using the <b>Tissue</b> input."
          )),

          p(
            "Click on any bar in the plot to bring up a table containing all
            occurrences of that molecule, and download this molecule-specific
            table using the button that appears at the bottom of the sidebar."
          ),

          hr(),

          # Just like the Table tab, we're building all of these inputs in the
          # server section so we don't have to repeat the same code many times
          uiOutput("tabViz_select_inputs"),
          hr(),

          # Dynamically render the download button, to download the table only
          # when there is something to actually download.
          uiOutput("tabViz_clicked_table_download_button"),

          # Reset button for the tab
          actionButton(
            class   = "btn-info",
            style   = "width: 170px",
            inputId = "tabViz_reset",
            icon    = icon("rotate-left"),
            label   = "Restore defaults"
          )
        ),

        mainPanel = mainPanel(
          width = 9,
          uiOutput("tabViz_plot_panel"),
          uiOutput("tabViz_clicked_table_panel")
        )
      )
    ),




    # |- 2.e Perform Pathway Enrichment -----------------------------------

    tabPanel(
      value = "enrich_tab",
      icon  = icon("calculator"),

      title = span(
        "Perform Pathway Enrichment",
        title = paste0(
          "Submit your own genes to be tested for enriched Reactome pathways, ",
          "MSigDB Hallmark gene sets, and GO terms."
        )
      ),

      sidebarLayout(
        sidebarPanel = sidebarPanel(
          id = "enrich_tab_sidebar",
          width = 3,

          h4("Perform Pathway Enrichment", style = "margin-top: 0"),

          p(
            HTML(
              "Paste a list of genes into the space below (one per line) to
              test for enriched pathways/terms using <a href=
              'https://bioconductor.org/packages/ReactomePA'>ReactomePA</a> and
              <a href='https://maayanlab.cloud/Enrichr/'>enrichR</a>. Input
              genes may be either Ensembl, Entrez, or HGNC identifiers. You can
              also use the button below to <b>Load example data</b>. Results
              are automatically filtered using the adjusted p-value provided by
              each tool. For more details on these methods, please see the "
            ),
            actionLink(inputId = "tabEnrich_about", label = "About"),
            "page."
          ),

          # Load example data to test out the tab's functionality
          actionButton(
            inputId = "tabEnrich_load_example",
            label   = "Load example data",
            class   = "btn btn-info btn-tooltip",
            title   = "Click here to load a example list of Ensembl genes.",
          ),
          br(),
          br(),

          # Button to load some example data, which injects the list into the
          # proper input object and triggers a modal dialog with some info about
          # the example genes.
          textAreaInput(
            inputId     = "tabEnrich_pasted_input",
            label       = "Enter your query molecules below:",
            placeholder = "One per line...",
            height      = 200,
            resize      = "none"
          ),

          p(HTML(
            "Once you've entered your genes or loaded the example data, use the
            <b>1. Perform gene ID mapping</b> button to complete the first step.
            Then you can <b>2. Submit genes for pathway enrichment</b>; this
            step may take some time to complete, so please be patient."
          )),

          br(),

          # Button to trigger mapping of input genes. Added as separate step to
          # make input validation easier.
          disabled(
            actionButton(
              inputId = "tabEnrich_map_button",
              label   = div(
                HTML("<b>1.</b> Perform gene ID mapping"),
                HTML("&nbsp;"), # Horizontal spacer
                icon("signs-post")
              ),
              class   = "btn btn-primary btn-tooltip",
              title   = paste0(
                "Paste your genes above or load the example gene list, then ",
                "click here to perform the mapping step"
              )
            )
          ),

          br(),
          br(),

          disabled(
            actionButton(
              inputId = "tabEnrich_submit_button",
              label = div(
                HTML("<b>2.</b> Submit genes for pathway enrichment"),
                HTML("&nbsp;"), # Horizontal spacer
                icon("circle-right")
              ),
              class = "btn btn-primary btn-tooltip",
              title = "Once you've mapped your genes, click here to test them"
            )
          ),

          # Render UI for success message and buttons to download enrichment
          # results
          uiOutput("tabEnrich_mapping_info"),
          uiOutput("tabEnrich_reactomepa_download_button"),
          uiOutput("tabEnrich_enrichR_download_button")
        ),

        mainPanel = mainPanel(
          width = 9,
          uiOutput("tabEnrich_results_header"),
          uiOutput("tabEnrich_result_tabgroup_ui")
        )
      )
    ),




    # |- 2.d Test for Enriched Sepsis Gene Sets ---------------------------

    tabPanel(
      value = "gsva_tab",
      icon = icon("laptop-code"),

      title = span(
        "Test for Enriched Sepsis Gene Sets",
        title = paste0(
          "Upload an RNA-Seq expression table to test for enrichment of the ",
          "curated sepsis gene sets in your own data."
        )
      ),

      sidebarLayout(
        sidebarPanel = sidebarPanel(
          id = "gsva_tab_sidebar",
          width = 3,

          h4("Test for Enriched Sepsis Gene Sets", style = "margin-top: 0"),

          p(
            "Here you can upload transformed counts from RNA-Seq or mircoarray
            experiments to run Gene Set Variation Analysis (GSVA) using the
            curated sepsis gene sets. GSVA looks for dysregulation of the
            specified gene sets - here derived from sepsis studies - to identify
            patterns of expression among your samples. For more details on the
            GSVA method, please refer to the relevant section in the ",
            actionLink(inputId = "tabGSVA_about", label = "About"), "page."
          ),

          p(HTML(
            "We also provide example expression data, along with corresponding
            metadata, for you to try out. This data represents a subset of the
            GEO record <a href=
            'https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE65682'>
            GSE65682</a>, and can be loaded using the button below."
          )),

          # Load example data (expression and meta)
          actionButton(
            inputId = "tabGSVA_load_example_data",
            label   = "Load example data",
            class   = "btn btn-info btn-tooltip",
            title   = paste0(
              "Click here to load an example expression set and corresponding ",
              "metadata."
            )
          ),

          br(),
          br(),

          tags$label("Input requirements for GSVA:"),

          tags$ul(
            tags$li("Must be a comma-separated plaintext file (.csv)"),
            tags$li("The first column must contain Ensembl gene IDs"),
            tags$li("The remaining columns should correspond to your samples"),
            tags$li(HTML(paste0(
              "Counts must be normalized/transformed as is appropriate for ",
              "your data (e.g. DESeq2's <a href='https://www.bioconductor.org/",
              "packages/devel/bioc/vignettes/DESeq2/inst/doc/DESeq2.html'>VST",
              "</a> method); raw data will not be accepted."
            ))),
          ),

          fileInput(
            inputId     = "tabGSVA_matrix_input",
            label       = NULL,
            buttonLabel = list(icon("upload"), "Upload expression data..."),
            accept      = "csv"
          ),

          tags$label("Optional: Upload sample metadata"),

          p(
            "You may also upload metadata for your samples, which will be added
            as annotations to the final heatmap to indicate groups or variables
            for your samples (e.g. control and treatment). The first column must
            contain sample names, matching to the columns from the expression
            matrix provided above. All remaining columns will become annotation
            rows on the final heatmap."
          ),

          fileInput(
            inputId = "tabGSVA_metadata_input",
            label = NULL,
            buttonLabel = list(icon("upload"), "Upload sample metadata..."),
            accept = "csv"
          ),

          disabled(
            actionButton(
              inputId = "tabGSVA_submit_button",
              label   = div(
                "Submit expression data for GSVA",
                HTML("&nbsp;"), # Horizontal spacer
                icon("circle-right")
              ),
              class   = "btn btn-primary btn-tooltip",
              title   = paste0(
                "Upload your expression data and optional metadata, then ",
                "click here to perform GSVA"
              )
            )
          ),

          uiOutput("tabGSVA_result_downloadbutton")
        ),

        mainPanel = mainPanel(
          width = 9,
          # This empty div serves as an anchor for an `insertUI()` call
          div(id = "tabGSVA_placeholder_div"),
          uiOutput("tabGSVA_result_UI"),
          uiOutput("tabGSVA_heatmap_UI")
        )
      )
    ),




    # |- 2.f About --------------------------------------------------------

    tabPanel(
      value = "about_tab",
      icon  = icon("circle-info"),
      title = span("About", title = "Learn more about SeptiSearch."),

      div(
        class = "jumbotron",
        style = "padding-bottom: 6px;",

        h1("About"),
        hr(),

        div(
          class = "logoWrapper-about",

          p(
            actionLink(
              inputId = "tabAbout_home1",
              label = span(
                "SeptiSearch", style = "color: #4582ec; font-weight: bold"
              )
            ),
            HTML(
              " is a Shiny app in which you can browse, explore, and download
              curated molecular gene sets derived from sepsis studies. The app
              currently allows access to over 25,000 unique molecules from over
              100 publications. It was created by Travis Blimkie, Jasmine Tam &
              Arjun Baghela from the <a href='http://cmdr.ubc.ca/bobh/'>Hancock
              Lab</a> at the University of British Columbia. The last update to
              the data was performed on September 20th, 2021. Travis is the main
              developer for the Shiny app and handles maintenance & updates.
              Jasmine performed all the signature curation from datasets in
              peer-reviewed research articles and publicly available pre-prints.
              Arjun served as the supervisor for the project."
            )
          ),

          br(),

          h2(strong("Tutorial")),
          p(HTML(
            "A tutorial is available which provides detailed insturctions
            for using SeptiSearch and its different functions, hosted on the
            GitHub repository: <a href=
            'https://hancockinformatics.github.io/SeptiSearch/'>
            https://hancockinformatics.github.io/SeptiSearch/</a>"
          )),

          br(),

          h2(strong("Reporting problems")),
          p(HTML(
            "If you encounter a problem or bug with the app, please submit an
            issue at the <a href=
            'https://github.com/hancockinformatics/SeptiSearch'>Github page</a>.
            Include with your issue details on the problem so we can reproduce
            it, and any inputs if relevant (e.g. your list of genes submitted to
            the <i>Perform Pathway Enrichment</i> tab)."
          )),

          br(),

          h2(strong("Funding")),
          p(HTML(
            "We would like to acknowledge and thank the
            <a href='https://cihr-irsc.gc.ca/e/193.html'>Canadian Institutes of
            Health Research (CIHR)</a> for providing the funding for this
            project."
          )),

          br(),

          h2("App method details"),

          h3(strong("Perform Pathway Enrichment")),
          p(HTML(
            "Input gene mapping between ID types is performed using data
            obtained via the <a href=
            'https://bioconductor.org/packages/biomaRt/'>biomaRt</a> package.
            Biological pathway/term enrichment is performed using
            <a href='https://bioconductor.org/packages/ReactomePA'>
            ReactomePA</a> and <a href='https://maayanlab.cloud/Enrichr/'>
            enrichR</a>. The following resources are searched using enrichR:
            MSigDB's Hallmark collection, and the three main GO databases
            (Biological Process, Cellular Component & Molecular Function). For
            both methods, the results are filtered using an adjusted p-value
            threshold of 0.05."
          )),

          h3(strong("Test for Enriched Sepsis Gene Sets")),
          p(HTML(
            "Gene Set Variation Analysis is performed using the
            <a href='https://github.com/rcastelo/GSVA'>GSVA</a> package, and the
            heatmap visualization is created with <a href=
            'https://github.com/raivokolde/pheatmap'>pheatmap</a>. Specified
            parameters include the <em>gsva</em> method and a <em>Gaussian</em>
            kernel. Genes with zero variance across all samples are removed
            prior to the analysis. Example data for GSVA represents a subset of
            the GEO record <a href=
            'https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE65682'>
            GSE65682</a>."
          )),

          br(),

          h2(strong("References")),

          p(
            actionLink(
              inputId = "tabAbout_home2",
              label = span(
                "SeptiSearch", style = "color: #4582ec; font-weight: bold"
              )
            ),
            HTML(
              "is written in R, and uses the following packages & resources:"
            ),
            style = "margin-bottom: 0;"
          ),

          div(
            class = "row",
            style = "margin-left: 15px; margin-right: 0;",

            div(
              class = "column",
              tags$dl(
                tags$dt(
                  a(
                    "Shiny",
                    href = "https://shiny.rstudio.com/",
                    style = "font-size: 20px;"
                  ),
                  tags$dd("Create beautiful web apps with R.")
                ),

                tags$dt(
                  a(
                    "ShinyJS",
                    href = "https://deanattali.com/shinyjs/",
                    style = "font-size: 20px;"
                  ),
                  tags$dd("Extend Shiny functionality using JavaScript.")
                ),

                tags$dt(
                  a(
                    "Tidyverse",
                    href = "https://www.tidyverse.org/",
                    style = "font-size: 20px;"
                  ),
                  tags$dd("A suite of packages for data manipulation.")
                ),

                tags$dt(
                  a(
                    "DT",
                    href = "https://rstudio.github.io/DT/",
                    style = "font-size: 20px;"
                  ),
                  tags$dd(HTML(
                    "An R interface to the <em>DataTables</em> JavaScript ",
                    "library."
                  ))
                ),

                tags$dt(
                  a(
                    "Plotly",
                    href = "https://plotly.com/r/",
                    style = "font-size: 20px;"
                  ),
                  tags$dd("Interactive plots and visualizations.")
                )
              )
            ),

            div(
              class = "column",
              tags$dl(
                tags$dt(
                  a(
                    "biomaRt",
                    href = "https://bioconductor.org/packages/biomaRt/",
                    style = "font-size: 20px;"
                  ),
                  tags$dd("An R package to access BioMart databases.")
                ),

                tags$dt(
                  a(
                    "ReactomePA",
                    href = "https://bioconductor.org/packages/ReactomePA",
                    style = "font-size: 20px;"
                  ),
                  tags$dd("Perform pathway enrichment analysis using Reactome
                          data.")
                ),

                tags$dt(
                  a(
                    "enrichR",
                    href = "https://cran.r-project.org/package=enrichR",
                    style = "font-size: 20px;"
                  ),
                  tags$dd("Access the Ma'ayan Lab's gene set enrichment
                          services from R.")
                ),

                tags$dt(
                  a(
                    "GSVA",
                    href = "https://github.com/rcastelo/GSVA",
                    style = "font-size: 20px;"
                  ),
                  tags$dd("Gene Set Variation Analysis for microarray and
                          RNA-Seq data.")
                ),

                tags$dt(
                  a(
                    "pheatmap",
                    href = "https://cran.r-project.org/package=pheatmap",
                    style = "font-size: 20px;"
                  ),
                  tags$dd("Easy and robust heatmap visualizations.")
                )
              )
            )
          )
        )
      ),

      # Display the current app version in bottom-right page corner
      div(
        br(),
        br(),
        div(
          class = "p-ver",
          gsub(
            x = readLines("DESCRIPTION")[3],
            pattern = "^Version\\: ",
            replacement = "v"
          )
        ),
        br()
      )
    )
  ),
  tags$script(src = "js/client.js")
)




# 3. Server ---------------------------------------------------------------

server <- function(input, output, session) {

  source("scripts/deferred.R")
  observeEvent(input$sessionInitialized, {
    runjs("handlers.initGetStarted();")
  }, ignoreInit = TRUE, once = TRUE)

  observe({
    if (is.character(req(input$navbar))) {
      message(paste0(
        "\n\n==INFO: Switching to tab '", input$navbar, "'..."
      ))
    }

    # If we've switched to the Enrichment tab, also load enrichR now
    if (as.character(req(input$navbar)) == "enrich_tab") {
      message("\n==INFO: Loading required package 'enrichR'...")
      require(enrichR)
      message("\tDone.")
    }
  })



  # 3.a Home --------------------------------------------------------------

  observeEvent(input$get_started, {
    updateNavbarPage(
      session = session,
      inputId = "navbar",
      selected = "study_tab"
    )
  })

  # "Learn More" button that takes you to the About page
  observeEvent(input$learn_more, {
    updateNavbarPage(
      session  = session,
      inputId  = "navbar",
      selected = "about_tab"
    )
  }, ignoreInit = TRUE)


  observeEvent(input$tabAbout_home1, {
    updateNavbarPage(
      session  = session,
      inputId  = "navbar",
      selected = "home_tab"
    )
  }, ignoreInit = TRUE)

  observeEvent(input$tabAbout_home2, {
    updateNavbarPage(
      session  = session,
      inputId  = "navbar",
      selected = "home_tab"
    )
  }, ignoreInit = TRUE)



  # 3.b Explore the Database ----------------------------------------------


  # |- 3.b.1 Parse and store user's inputs --------------------------------

  # Simple text search for article titles
  tabStudy_title_search <- reactiveVal()
  observeEvent(input$tabStudy_title_input, {
    input$tabStudy_title_input %>% tabStudy_title_search()
  }, ignoreInit = TRUE)

  # Input for Covid selection from radio button
  # tabStudy_covid_selection() <- reactiveVal()
  # observeEvent(input$tabStudy_covid_radio_input, {
  #   input$tabStudy_covid_radio_input %>% tabStudy_covid_selection()
  # }, ignoreInit = TRUE)


  # Set up reactive value to store input molecules from the user
  tabStudy_users_molecules <- reactiveVal()
  observeEvent(input$tabStudy_molecule_input, {
    input$tabStudy_molecule_input %>%
      str_split(., pattern = " |\n") %>%
      unlist() %>%
      # The next step prevents the inclusion of an empty string, if the user
      # starts a new line but doesn't type anything
      str_subset(., pattern = "^$", negate = TRUE) %>%
      tabStudy_users_molecules()
  }, ignoreInit = TRUE)


  # Based on molecules the user searches, get the "Study Labels" of articles
  # which contain that molecule(s). This needs to be wrapped in a conditional
  # since we get an error for trying to filter with NULL or an empty line.
  tabStudy_studylabel_with_user_molecules <- reactive({

    if (!all(
      is.null(tabStudy_users_molecules()) | tabStudy_users_molecules() == "")
    ) {
      full_data %>% filter(
        str_detect(Molecule, paste0(tabStudy_users_molecules(), collapse = "|"))
      ) %>%
        pull(`Study Label`)
    }
  })


  # |- 3.b.2 Filter the grouped table -------------------------------------

  tabStudy_filtered_table <- reactive({

    full_data %>% filter(

      # User search for words in titles
      conditional_filter(
        !all(is.null(tabStudy_title_search()) | tabStudy_title_search() == ""),
        str_detect(Title, regex(tabStudy_title_search(), ignore_case = TRUE))
      ),

      # Molecule searching
      conditional_filter(
        !all(
          is.null(tabStudy_studylabel_with_user_molecules()) |
            tabStudy_studylabel_with_user_molecules() == ""
        ),
        `Study Label` %in% tabStudy_studylabel_with_user_molecules()
      ),

      conditional_filter(
        input$tabStudy_covid_radio_input == "covid_only",
        `Covid Study` == "COVID"
      ),

      conditional_filter(
        input$tabStudy_covid_radio_input == "noncovid_only",
        `Covid Study` == "Non-COVID"
      )
    )
  })

  tabStudy_grouped_table <- reactive({
    tabStudy_filtered_table() %>%
      dplyr::select(
        Title,
        `Study Label`,
        Year,
        PMID,
        Link,
        `Transcriptomic Type`,
        `Covid Study`,
        `Gene Set Length`
      ) %>%
      distinct(`Study Label`, .keep_all = TRUE) %>%
      mutate(PMID = case_when(
        !is.na(PMID) ~ paste0(
          "<a target='_blank' rel='noopener noreferrer' href='",
          Link, "'>", PMID, "</a>"
        ),
        TRUE ~ paste0(
          "<a target='_blank' rel='noopener noreferrer' href='",
          Link, "'>Pre-Print</a>"
        )
      )) %>%
      arrange(`Study Label`) %>%
      dplyr::select(-Link) %>%
      dplyr::rename("Link" = PMID)
  })


  # |- 3.b.3 Render grouped table -----------------------------------------

  output$tabStudy_grouped_DT <- DT::renderDataTable(
    tabStudy_grouped_table(),
    rownames  = FALSE,
    escape    = FALSE,
    selection = "multiple",
    options   = list(
      dom     = "tip",
      scrollX = TRUE,
      columnDefs = list(
        list(targets = 0, render = ellipsis_render(75))
      )
    )
  )

  output$tabStudy_grouped_render <- renderUI(
    tagList(
      DT::dataTableOutput("tabStudy_grouped_DT"),
      hr(),
      h3(paste0(
        "Click one or more rows in the table above to see all molecules from ",
        "those gene sets."
      ))
    )
  )


  # |- 3.b.4 Create clicked table -----------------------------------------

  tabStudy_clicked_row_studylabel <- reactiveVal()
  tabStudy_clicked_row_info <- reactiveVal()

  observeEvent(input$tabStudy_grouped_DT_rows_selected, {

    # The "Study Label", used to filter the main table for the study the user
    # selected
    tabStudy_clicked_row_studylabel(
      tabStudy_grouped_table()[input$tabStudy_grouped_DT_rows_selected, 2] %>%
        pull(1)
    )

    # Gather the info for each clicked row/paper and format it for use in naming
    # the download file
    tabStudy_clicked_row_info({
      clicked_authors <-
        tabStudy_grouped_table()[input$tabStudy_grouped_DT_rows_selected, 2] %>%
        pull(1) %>%
        str_trim()

      clicked_pmid <-
        tabStudy_grouped_table()[input$tabStudy_grouped_DT_rows_selected, 4] %>%
        pull(1) %>%
        str_extract(., "[0-9]{8}") %>%
        replace(is.na(.), "")

      map2(
        clicked_authors,
        clicked_pmid,
        ~paste0(.x, "_", .y)
      ) %>%
        str_remove(., "_$") %>%
        paste(., collapse = "_")
    })
  })

  tabStudy_clicked_table <- reactive({
    if (is.null(tabStudy_clicked_row_studylabel())) {
      return(NULL)
    } else {
      # The "PMID" and "Link" columns are initially both included, and we drop
      # one or the other when displaying or downloading the data
      full_data %>%
        filter(`Study Label` %in% tabStudy_clicked_row_studylabel()) %>%
        dplyr::select(-c(Title, Year, Link, PMID, `Gene Set Length`)) %>%
        arrange(`Study Label`, Molecule)
    }
  })


  # |- 3.b.5 Render clicked table -----------------------------------------

  observeEvent(input$tabStudy_grouped_DT_rows_selected, {
    output$tabStudy_clicked_DT <- DT::renderDataTable(
      tabStudy_clicked_table(),
      rownames  = FALSE,
      escape    = FALSE,
      selection = "none",
      options   = list(
        dom     = "ftip",
        scrollX = TRUE
      )
    )
  })

  output$tabStudy_test_clicked_row_data <-
    renderPrint(tabStudy_clicked_row_info())

  output$tabStudy_clicked_render <- renderUI(
    tagList(
      br(),
      # verbatimTextOutput("tabStudy_test_clicked_row_data"),
      DT::dataTableOutput("tabStudy_clicked_DT"),
      br()
    )
  )

  # Allow the user to "reset" the page to its original/default state. All the
  # values need to be reset manually; the shinyjs reset function doesn't seem to
  # apply to DT functions/objects
  observeEvent(input$tabStudy_reset, {
    shinyjs::reset("study_tab_sidebar", asis = FALSE)
    selectRows(proxy = dataTableProxy("tabStudy_grouped_DT"), selected = NULL)
    output$tabStudy_clicked_DT <- NULL
    tabStudy_clicked_row_studylabel(NULL)
    tabStudy_clicked_row_info(NULL)
  })


  # |- 3.b.6 Download clicked study data ----------------------------------

  # The "filename" argument needs to be inside the function() call to properly
  # update when the clicked row changes (i.e. to make the file name reactive)
  output$tabStudy_clicked_study_download_handler <- downloadHandler(
    filename = function() {
      paste0(
        "septisearch_download_",
        tabStudy_clicked_row_info(),
        ".txt"
      )
    },
    content = function(filename) {
      readr::write_tsv(
        x    = tabStudy_clicked_table(),
        file = filename
      )
    }
  )


  # Render the UI for the download
  output$tabStudy_clicked_study_download_button <- renderUI({
    if (is.null(tabStudy_clicked_table())) {
      return(NULL)
    } else {
      return(tagList(
        br(),
        p(strong("Download the table for the selected gene sets:")),
        downloadButton(
          outputId = "tabStudy_clicked_study_download_handler",
          label    = "Download gene set-specific table",
          class    = "btn btn-success",
          style    = "width: 100%;"
        )
      ))
    }
  })




  # 3.c Visualize the Database --------------------------------------------


  # |- 3.c.1 Create input objects -----------------------------------------

  # Set up a named list, with columns as entries, and the corresponding input
  # ID as the names. This will be used for creating and later updating the
  # selectInput() objects
  tabViz_cols <- c("Tissue", "Timepoint")

  tabViz_input_ids <-
    paste0("tabViz_", janitor::make_clean_names(tabViz_cols), "_input")

  tabViz_cols_input_ids <- set_names(
    tabViz_cols,
    tabViz_input_ids
  )

  # The tooltips that are displayed for each input's label. For now these have
  # to be set up manually, paying attention to the order, as we're using map2 to
  # assign them to the correct inputs. The string "&#39;" is the HTML code used
  # for an apostrophe.
  tabViz_cols_input_tooltips <- list(
    "Type of tissue used in the study, e.g. whole blood",
    "Time at which samples were collected for analysis"
  )

  # Create the inputs for the sidebar, using our custom function to reduce
  # repetitive code along with the list created above
  output$tabViz_select_inputs <- renderUI({
    map2(
      .x = tabViz_cols_input_ids,
      .y = tabViz_cols_input_tooltips,
      ~create_selectInput(column_name = .x, tab = "tabViz", tooltip = .y)
    )
  })


  # |- 3.c.2 Start with filters ---------------------------------------------

  # All the filtering steps use the `conditional_filter()` function, so we
  # don't need step-wise filtering, while also keeping the whole thing reactive
  tabViz_filtered_table <- reactive({

    full_data %>% filter(

      # Tissue
      conditional_filter(
        length(input$tabViz_tissue_input) != 0,
        Tissue %in% input$tabViz_tissue_input
      ),

      # Time point
      conditional_filter(
        length(input$tabViz_timepoint_input) != 0,
        Timepoint %in% input$tabViz_timepoint_input
      )
    )
  })


  # Here we update the selectInput() objects created earlier, so only valid/
  # present values are shown as possible options. For e.g., if you select
  # "Transcriptomics" in the "Omic Type" filter, then you won't see "Metabolite"
  # under "Molecule Type," since there are no entries matching those criteria.

  # NOTE we need to specify the `selected` argument; if left as NULL, then the
  # input gets cleared by updateSelectInput(), essentially negating/removing the
  # user's filter immediately after they apply it.

  # This code was disabled, as having it meant you could only have one entry per
  # field (i.e. it disabled the 'multiple' argument of 'selectInput()'). Keeping
  # this code in here, in case we want to revert at some point.

  # observe({
  #   tabViz_cols_input_ids %>% imap(
  #     ~updateSelectInput(
  #       session = session,
  #       inputId = .y,
  #       choices = unique(not_NA(tabViz_filtered_table()[[.x]])),
  #       selected = input[[.y]]
  #     )
  #   )
  # })


  # |- 3.c.3 Plotly -------------------------------------------------------

  # Creating a table to plot the top 50 molecules based on the number of
  # citations
  tabViz_plot_table <- reactive({

    table_v1 <- tabViz_filtered_table() %>%
      count(Molecule, sort = TRUE, name = "count") %>%
      tidyr::drop_na(Molecule) %>%
      head(50)

    molecule_order <- table_v1 %>%
      group_by(Molecule) %>%
      summarise(total_count = sum(count)) %>%
      arrange(desc(total_count)) %>%
      pull(1)

    table_v2 <- table_v1 %>%
      mutate(Molecule = factor(Molecule, levels = molecule_order))

    return(table_v2)
  })


  # Make the plot via plotly, primarily to make use of the "hover text" feature.
  # Adding the `customdata` variable here allows us to access this information
  # when a user clicks on a bar, in addition to the x value (gene/protein name).
  output$tabViz_plot_object <- plotly::renderPlotly({
    suppressWarnings({
      plotly::plot_ly(
        data      = tabViz_plot_table(),
        x         = ~Molecule,
        y         = ~count,
        type      = "bar",
        marker    = list(color = "#4582ea"),
        hoverinfo = "text",
        hovertext = ~paste0(
          "<b>", Molecule, ":</b> ", count
        )
      ) %>%
        plotly::style(
          hoverlabel = list(
            bgcolor     = "white",
            bordercolor = "black",
            font_family = "serif"
          )
        ) %>%
        plotly::layout(
          font       = list(family = "Georgia", size = 16, color = "black"),
          margin     = list(b = 150, t = 25),
          showlegend = FALSE,

          xaxis = list(
            title      = "",
            tickfont   = list(size = 12),
            tickangle  = "45",
            zeroline   = TRUE,
            showline   = TRUE,
            mirror     = TRUE,
            automargin = TRUE
          ),

          yaxis = list(
            title      = "<b>Number of Citations</b>",
            tick       = "outside",
            ticklen    = 3,
            zeroline   = TRUE,
            showline   = TRUE,
            mirror     = TRUE,
            automargin = TRUE
          )
        )
    })
  })


  # Create the table holding the data for the molecule/time point based on the
  # user clicking on a bar in plotly output
  tabViz_clicked_molecule_table <- reactive({
    d <- plotly::event_data("plotly_click", priority = "event")
    if (is.null(d)) {
      return(NULL)
    } else {
      tabViz_filtered_table() %>%
        filter(Molecule == d$x)
    }
  })


  # Grab the molecule name and time point for later use in naming the download
  # file
  tabViz_clicked_molecule_info <- reactive({
    d <- plotly::event_data("plotly_click", priority = "event")
    if (is.null(d)) {
      return(NULL)
    } else {
      list(
        molecule = d$x
      )
    }
  })


  # |- 3.c.4 Create clicked table -----------------------------------------

  # Render the table with PMIDs as hyperlinks
  tabViz_clicked_molecule_table_for_DT <- reactive({
    if ( !is.null(tabViz_clicked_molecule_table()) ) {
      tabViz_clicked_molecule_table() %>%
        mutate(
          Link = case_when(
            !is.na(PMID) ~ paste0(
              "<a target='_blank' rel='noopener noreferrer' href='",
              Link, "'>", PMID, "</a>"
            ),
            TRUE ~ paste0(
              "<a target='_blank' rel='noopener noreferrer' href='",
              Link, "'>Pre-Print</a>"
            )
          )
        ) %>%
        dplyr::select(
          Molecule,
          `Study Label`,
          Tissue,
          Timepoint,
          `Age Group`,
          Observations,
          `Case Condition`,
          `Control Condition`,
          `Covid Study`,
          `Transcriptomic Type`,
          `Gene Set Type`
        )
    }
  })

  output$tabViz_clicked_plot_table <- DT::renderDataTable(
    expr = {
      if ( !is.null(tabViz_clicked_molecule_table_for_DT()) ){
        tabViz_clicked_molecule_table_for_DT()
      } else {
        NULL
      }
    },
    rownames  = FALSE,
    escape    = FALSE,
    selection = "none",
    options   = list(
      dom     = "ftip",
      scrollX = TRUE
    )
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


  # |- 3.c.5 Render plot and table UI ---------------------------------------

  # Rendering the plot and surrounding UI. Provide a brief message if the user's
  # filters don't match any molecules, instead of an empty plot. Uncomment the
  # `verbatimTextOutput` line to see the information from the `plotly_click`
  # event.
  output$tabViz_plot_panel <- renderUI({
    tagList(
      h3(
        "Click a bar in the plot to see all databse entries for that molecule"
      ),

      if ( nrow(tabViz_plot_table()) > 0 ) {
        div(
          style = "display: block; overflow: auto;",
          plotly::plotlyOutput(
            outputId = "tabViz_plot_object",
            inline   = TRUE,
            height   = "400px",
            width    = "98%"
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
            ),
            br()
          )
        )
      }
    } else {
      return(NULL)
    }
  })


  # |- 3.c.6 Download clicked table ---------------------------------------

  # Download handler for the table generated when a user clicks on one of the
  # bars in the plot. Fed into the `renderUI()` chunk below so it only appears
  # when there is data to download.
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
        x    = dplyr::select(tabViz_clicked_molecule_table(), -Link),
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

        p(strong("Save the table for the selected molecule:")),

        downloadButton(
          outputId = "tabViz_clicked_table_download_handler",
          label    = paste0(
            "Download entries for ",
            if_else(
              condition = str_length(tabViz_clicked_molecule_info()$molecule) <= 25,
              true = tabViz_clicked_molecule_info()$molecule,
              false = paste0(
                str_sub(tabViz_clicked_molecule_info()$molecule, end = 22),
                "..."
              )
            )
          ),
          class = "btn btn-success",
          style = "width: 100%;"
        ),
        hr()
      ))
    }
  })


  # Allow the user to "reset" the page to its original/default state, using both
  # the default shinyjs function and our own JS, sourced from "www/functions.js"
  observeEvent(input$tabViz_reset, {
    shinyjs::reset(id = "viz_tab_sidebar", asis = FALSE)
    js$resetClick()
  })




  # 3.d Perform Pathway Enrichment ----------------------------------------

  # Linking to the About page for more details on the enrichment methods
  observeEvent(input$tabEnrich_about, {
    updateNavbarPage(
      session  = session,
      inputId  = "navbar",
      selected = "about_tab"
    )
  }, ignoreInit = TRUE)


  # Define reactive values
  tabEnrich_input_genes <- reactiveVal()
  tabEnrich_input_genes_table <- reactiveVal()
  tabEnrich_test_result <- reactiveVal()

  tabEnrich_example_data_indicator <- reactiveVal(0)


  # |- 3.d.1 Load example data --------------------------------------------

  observeEvent(input$tabEnrich_load_example, {

    tabEnrich_example_data_indicator(1)

    tabEnrich_input_genes(tabEnrich_example_data)

    message("\n==INFO: Example data successfully loaded.")

    showModal(modalDialog(
      title = span(
        "Example data successfully loaded.",
        style = "color: #3fad46;"
      ),
      HTML(paste0(
        "The example list of 1,117 Ensembl genes has been loaded. You can now
        click <br><b>1. Perform gene ID mapping</b> to find the corresponding
        Entrez and HGNC identifiers for these genes. Then you'll be able to use
        the <b>2. Submit genes for pathway enrichment</b> button to test the
        example genes for over-represented pathways."
      )),
      footer = modalButton("OK"),
      easyClose = TRUE
    ))

  })


  # |- 3.d.2 Parse molecule input -----------------------------------------

  # Note that input ID's need to be coerced to character to prevent mapping
  # issues when using Entrez IDs (which are interpreted as numeric)
  observeEvent(input$tabEnrich_pasted_input, {
    input$tabEnrich_pasted_input %>%
      str_split(., pattern = " |\n") %>%
      unlist() %>%
      str_subset(., pattern = "^$", negate = TRUE) %>% # Remove empty lines
      as.character() %>%
      tabEnrich_input_genes()
  })

  # Place the input genes into a tibble
  tabEnrich_input_genes_table <- reactive({
    return(
      tibble::tibble("input_genes" = as.character(tabEnrich_input_genes()))
    )
  })

  # Enable the Map button once we have some input from the user
  observeEvent({
    input$tabEnrich_load_example
    input$tabEnrich_pasted_input
  }, {
    if ( nrow(tabEnrich_input_genes_table()) > 0 ) {
      message("\n==INFO: Input detected, enabling 'Map' button...")
      enable("tabEnrich_map_button")

      runjs(paste0(
        "document.getElementById('tabEnrich_map_button').setAttribute(",
        "'title', 'Click here to map your genes');"
      ))
    }
  })


  # |- 3.d.3 Map genes ----------------------------------------------------

  tabEnrich_mapped_genes <- reactiveVal()

  observeEvent(input$tabEnrich_map_button, {
    req(tabEnrich_input_genes(), tabEnrich_input_genes_table())
    map_genes(
      gene_list  = tabEnrich_input_genes(),
      gene_table = tabEnrich_input_genes_table()
    ) %>% tabEnrich_mapped_genes()
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
        title = span(
          "Input gene mapping complete!",
          style = "color: #3fad46;"
        ),
        HTML(paste(
          "Your",
          nrow(tabEnrich_input_genes_table()),
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
        title = span("Input Error.", style = "color:red;"),
        paste0(
          "There was a problem mapping your genes; please ensure they are ",
          "either Ensembl, HGNC, or Entrez IDs (one per line) and try again."
        ),
        footer = modalButton("OK")
      ))
    }
  })


  # |- 3.d.4 Perform enrichment tests -------------------------------------

  observeEvent(input$tabEnrich_submit_button, {

    # Create modal dialog to say the tests are running
    showModal(modalDialog(
      title = span(
        div(
          icon(
            name  = "spinner",
            class = "fa fa-spin"
          ),
          "Enrichment testing in progress...",
        ),
        style = "color: #4582ec;"
      ),
      paste0(
        "We are currently testing your ",
        nrow(tabEnrich_input_genes_table()),
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

    if ( !any(map_lgl(tabEnrich_test_result(), ~is.null(.x))) ) {
      list(
        ReactomePA = tabEnrich_test_result()$ReactomePA %>%
          mutate(across(where(is.numeric), signif, digits = 3)) %>%
          janitor::clean_names("title", abbreviations = c("BG", "ID")) %>%
          dplyr::rename("P Value" = Pvalue, "Adjusted P Value" = `P Adjust`),

        enrichR = tabEnrich_test_result()$enrichR %>%
          mutate(across(where(is.numeric), signif, digits = 3)) %>%
          janitor::clean_names("title", abbreviations = "P")
      )
    } else {
      return(NULL)
    }
  })


  # |- 3.d.5 Output results tables ----------------------------------------

  tabEnrich_reactomepa_container <- htmltools::withTags(table(
    class = "display",
    thead(tr(
      th(
        "ID",
        title = "Reactome ID for the pathway."
      ),
      th(
        "Description",
        title = "Name and description of the pathway."
      ),
      th(
        "Shared Genes",
        title = paste0("Overlap of input genes and genes in a pathway (i.e. ",
                       "shared or common genes).")
      ),
      th(
        "Genes in Pathway",
        title = "Total number of genes annotated to a particular pathway."
      ),
      th(
        "Gene Ratio",
        title = paste0("Ratio of shared genes divided by the total number of ",
                       "genes in a pathway.")
      ),
      th(
        "P Value",
        title = "Statistical significance of the result."
      ),
      th(
        "Adjusted P Value",
        title = paste0("Statistical significance of the result, adjusted for ",
                       "multiple testing.")
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
        title = "Pathway, gene set or GO term being tested."
      ),
      th(
        "P Value",
        title = "Statistical significance of the gene set or GO term."
      ),
      th(
        "Adjusted P Value",
        title = paste0("Statistical significance of the gene set or GO term, ",
                       "adjusted for multiple testing.")
      )
    ))
  ))

  # tabEnrich_result_tabgroup_ui

  observeEvent(input$tabEnrich_submit_button, {

    ### Header for the results section
    output$tabEnrich_results_header <- renderUI(
      tagList(
        h1("Pathway Enrichment Results"),
        p(
          style = "font-size: 20px;",
          "Use the buttons below to see your results, and check the bottom of
          the sidebar for download links."
        ),
        br()
      )
    )

    # For each subsequent chunk, if there were no significant results (0 rows,
    # but no errors) then simply display a message instead of a blank.

    output$tabEnrich_result_tabgroup_ui <- renderUI(
      tabsetPanel(
        id = "tabEnrich_result_tabgroup_ui",
        type = "pills",
        tabPanel(
          title = "ReactomePA",
          uiOutput("tabEnrich_result_reactomepa_ui")
        ),

        tabPanel(
          title = "enrichR",
          uiOutput("tabEnrich_result_enrichR_ui")
        )
      )
    )

    ### ReactomePA
    if ( nrow(tabEnrich_test_result_clean()$ReactomePA) > 0 ) {
      output$tabEnrich_result_reactomepa <- DT::renderDataTable(
        datatable(
          tabEnrich_test_result_clean()$ReactomePA,
          container = tabEnrich_reactomepa_container,
          rownames = FALSE,
          selection = "none",
          options = list(
            dom = "ftip",
            columnDefs = list(
              list(targets = 1, render = ellipsis_render(65))
            )
          )
        )
      )
      output$tabEnrich_result_reactomepa_ui <- renderUI(
        tagList(
          br(),
          dataTableOutput("tabEnrich_result_reactomepa")
        )
      )
    } else {
      output$tabEnrich_result_reactomepa_ui <- renderUI(
        tagList(
          h4("No significant results found.")
        )
      )
    }


    ### enrichR
    if ( nrow(tabEnrich_test_result_clean()$enrichR) > 0 ) {
      output$tabEnrich_result_enrichR <- DT::renderDataTable(
        datatable(
          tabEnrich_test_result_clean()$enrichR,
          container = tabEnrich_enrichR_container,
          rownames = FALSE,
          selection = "none",
          options  = list(
            dom = "ftip"
          )
        )
      )
      output$tabEnrich_result_enrichR_ui <- renderUI(
        tagList(
          br(),
          dataTableOutput("tabEnrich_result_enrichR")
        )
      )
    } else {
      output$tabEnrich_result_enrichR_ui <- renderUI(
        tagList(
          h4("No significant results found.")
        )
      )
    }
  })

  # Once the mapping is finished, remove the modal dialog box
  observeEvent(input$tabEnrich_submit_button, {
    if ( !any(map_lgl(tabEnrich_test_result_clean(), ~is.null(.x))) ) {
      removeModal()
    }
  })


  # |- 3.d.6 Download results ---------------------------------------------

  # Provide some info to the user regarding the number of unique input genes,
  # and how they mapped to the other ID types. The UI elements are constructed
  # conditionally based on the input ID type using the custom function
  # `make_success_message` and a few conditionals.
  output$tabEnrich_mapping_info <- renderUI({
    if (any(
      is.null(tabEnrich_test_result_clean()$ReactomePA),
      is.null(tabEnrich_test_result_clean()$enrichR)
    )) {
      return(NULL)
    } else {
      tagList(
        hr(),
        tags$label("Mapping results"),
        make_success_message(mapped_data = isolate(tabEnrich_mapped_genes())),

        tags$label("Enrichment results"),

        p(paste0(
          "With your input genes we found ",

          if_else(
            condition = nrow(tabEnrich_test_result_clean()$ReactomePA) > 0,
            true = paste0(
              nrow(tabEnrich_test_result_clean()$ReactomePA),
              " pathways from ReactomePA"
            ),
            false = NULL
          ),

          if_else(
            condition = all(
              nrow(tabEnrich_test_result_clean()$ReactomePA) > 0,
              nrow(tabEnrich_test_result_clean()$enrichR) > 0
            ),
            true = " and ",
            false = NULL
          ),

          if_else(
            condition = nrow(tabEnrich_test_result_clean()$enrichR) > 0,
            true = paste0(
              nrow(tabEnrich_test_result_clean()$enrichR),
              " terms from enrichR."
            ),
            false = NULL
          ),
          " Use the buttons below to download your results as a tab-delimited ",
          "text file."
        ))
      )
    }
  })


  # First the button for ReactomePA...
  output$tabEnrich_reactomepa_download_handler <- downloadHandler(
    filename = function() {

      if (tabEnrich_example_data_indicator() == 1) {
        "septisearch_reactomePA_result_example_data.txt"
      } else {
        "septisearch_reactomePA_result.txt"
      }
    },
    content  = function(filename) {
      readr::write_tsv(
        x    = tabEnrich_test_result_clean()$ReactomePA,
        file = filename
      )
    }
  )

  observeEvent(input$tabEnrich_submit_button, {
    output$tabEnrich_reactomepa_download_button <- renderUI({
      if (is.null(tabEnrich_test_result_clean()$ReactomePA)) {
        return(NULL)
      } else {
        return(tagList(
          hr(),
          downloadButton(
            outputId = "tabEnrich_reactomepa_download_handler",
            label    = "Download ReactomePA results",
            class    = "btn btn-success",
            style    = "width: 100%;"
          )
        ))
      }
    })
  })


  # ...and a second button for enrichR
  output$tabEnrich_enrichR_download_handler <- downloadHandler(
    filename = function() {

      if (tabEnrich_example_data_indicator() == 1) {
        "septisearch_enrichR_result_example_data.txt"
      } else {
        "septisearch_enrichR_result.txt"
      }
    },
    content  = function(filename) {
      readr::write_tsv(
        x    = tabEnrich_test_result_clean()$enrichR,
        file = filename
      )
    }
  )

  observeEvent(input$tabEnrich_submit_button, {
    output$tabEnrich_enrichR_download_button <- renderUI({
      if (is.null(tabEnrich_test_result_clean()$enrichR)) {
        return(NULL)
      } else {
        return(
          tagList(
            hr(),
            downloadButton(
              outputId = "tabEnrich_enrichR_download_handler",
              label    = "Download enrichR results",
              class    = "btn btn-success",
              style    = "width: 100%;"
            )
          )
        )
      }
    })
  })




  # 3.d Test for Enriched Sepsis Gene Sets --------------------------------

  # Linking to the About page for more details on the enrichment methods
  observeEvent(input$tabGSVA_about, {
    updateNavbarPage(
      session  = session,
      inputId  = "navbar",
      selected = "about_tab"
    )
  }, ignoreInit = TRUE)

  # Define initial reactive values for inputs
  tabGSVA_expr_input_1 <- reactiveVal()
  tabGSVA_example_indicator <- reactiveVal(0)

  tabGSVA_meta_input_1 <- reactiveVal(NULL)
  tabGSVA_meta_input_2 <- reactiveVal(NULL)


  # |- 3.d.1 Loading example data -----------------------------------------

  observeEvent(input$tabGSVA_load_example_data, {

    # First the expression data
    message("\n==INFO: Loading example expression data...")
    tabGSVA_expr_input_1(tabGSVA_example_data$expr)

    # Then the metadata
    message("\n==INFO: Loading example metadata...")
    tabGSVA_meta_input_1(as.data.frame(tabGSVA_example_data$meta))

    tabGSVA_example_indicator(1)
  })


  # |- 3.d.2 Load user's expression data ----------------------------------

  # Note we need to use read.csv() here so that we can check if the input data
  # is normalized (double) or raw (integer); `read_csv()` treats everything as
  # a double. Here we also provide messages to the user about their input.
  observeEvent(input$tabGSVA_matrix_input, {
    message("\n==INFO: Loading expression data from user...")
    tabGSVA_expr_input_1(read.csv(input$tabGSVA_matrix_input$datapath))
  })


  # |- 3.d.3 Process input (user's or example) ----------------------------

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
            HTML(paste0(
              "The example expression data and matching metadata has been
              successfully loaded; you can now use the <b>Submit expression data
              for GSVA</b> button to proceed with the analysis."
            )),
            footer = modalButton("Continue"),
            easyClose = TRUE
          ))
        } else {
          showModal(modalDialog(
            title = span("Input Success!", style = "color: #3fad46;"),
            HTML(paste0(
              "Your data was successfully uploaded and parsed. Please ensure it
              looks correct in the preview table before proceeding (note not all
              genes/samples are displayed). You may also upload metadata for
              your samples (e.g. treatment type, disease status, etc)."
            )),
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
            "Your data appears to not be normalized/transformed. Please ensure
            you apply the proper transformation to your data before attempting
            GSVA."
          ),
          footer = modalButton("OK")
        ))
        return(NULL)
      }

    } else {
      message("ERROR: Unspecified error!")

      showModal(modalDialog(
        title = span("Input Error!", style = "color:red;"),
        paste0(
          "There was an unspecified problem with your input; please ensure it
          meets all of the stated criteria, then try again."
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
        HTML(paste0(
          "<p style = 'font-size: 20px;'>The table below shows the <i>first
          few</i> rows and columns of your data. Ensembl gene IDs should fill
          the rownames, while each column corresponds to a sample. If the data
          looks OK, you can proceed using the <b>Submit expression data for GSVA
          </b> button at the bottom of the sidebar.</p>"
        )),
        br(),
        dataTableOutput("tabGSVA_input_preview_table")
      ))
    )
  })


  # |- 3.d.4 Load and parse metadata --------------------------------------

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
          paste0(
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


  # |- 3.d.5 Run GSVA -----------------------------------------------------

  # Enable the submission button when we have a non-NULL input, and update
  # tooltips for the submission button accordingly.
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
      title = span(
        div(
          icon(
            name  = "spinner",
            class = "fa fa-spin"
          ),
          "Running GSVA...",
        ),
        style = "color: #4582ec;"
      ),
      paste0(
        "Your input expression data is currently being analyzed. Please wait
        for your results to appear. Note that if you submitted data containing
        a large number of samples, it will take some time to analyze; please be
        patient."
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
    if ( !is.null(tabGSVA_result_1()) ) {
      removeModal()
    }
  })


  # |- 3.d.6 Render the results to the user -------------------------------

  tabGSVA_result_summary <- reactive({

    # Summary table that is displayed above the heatmap
    list(
      "summary_tbl" = left_join(
        tabGSVA_result_1()[["gsva_res_df"]],
        full_data_gsva_tab,
        by = c("Gene Set Name" = "Study Label")
      ) %>%
        dplyr::select(
          `Gene Set Name`,
          `No. Genes in Set`,
          `No. Shared Genes`,
          "Article Title" = Title
        ),

      # Results from GSVA plus the gene set info columns; this is what the user
      # can download
      "gsva_res_df" = left_join(
        tabGSVA_result_1()[["gsva_res_df"]],
        full_data_gsva_tab,
        by = c("Gene Set Name" = "Study Label")
      ) %>%
        dplyr::select(
          `Gene Set Name`,
          `No. Genes in Set`,
          `No. Shared Genes`,
          "Article Title" = Title,
          everything()
        ),

      "gsva_res_plt" = tabGSVA_result_1()[["gsva_res_plt"]]
    )
  })

  # Define a table "container" so that we can have title elements (hover text)
  # on column names to explain what the columns are.
  tabGSVA_table_container <- htmltools::withTags(table(
    class = "display",
    thead(tr(
      th(
        "Gene Set Name",
        title = "Name of the sepsis signature/gene set & PMID (if available)."
      ),
      th(
        "No. Genes in Set",
        title = "Number of genes/molecules in the gene set."
      ),
      th(
        "No. Shared Genes",
        title = "Number of genes from the set present in the input data."
      ),
      th(
        "Article Title",
        title = "Title of the article on which the gene set is based."
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


  # |- 3.d.7 Render heatmap -----------------------------------------------

  observeEvent(input$tabGSVA_submit_button, {
    if ( !is.null(tabGSVA_result_summary()[["gsva_res_plt"]]) ) {
      output$tabGSVA_heatmap_UI <- renderUI(
        tagList(
          br(),
          br(),
          h3("Heatmap of GSVA results:"),
          br(),
          div(
            title = paste0(
              "To save the heatmap as a PNG, right click anywhere on this ",
              "image and select \"Save Image...\""),
            renderPlot(
              tabGSVA_result_summary()[["gsva_res_plt"]],
              height = 1700,
              alt = "Heatmap of GSVA results."
            )
          ),
          br(),
        )
      )
    }
  })


  # |- 3.d.8 Download results ---------------------------------------------

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
            x    = tabGSVA_result_summary()[["gsva_res_df"]],
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
            label    = "Download full table of GSVA results",
            class    = "btn btn-success",
            style    = "width: 100%;"
          )
        )
      )
    } else {
      output$tabGSVA_result_downloadbutton <- renderUI(
        tagList(
          hr(),
          p(HTML(
            "There was a problem in running your data through GSVA. Please
            ensure your input meets all of the criteria listed above, then
            refresh the page, reupload your data, and try again. If the
            problem persists, you can submit an issue at our
            <a href='https://github.com/hancockinformatics/SeptiSearch'>
            Github page</a>."
          ))
        )
      )
    }
  })

} #server close




# 4. Run the app ----------------------------------------------------------

shinyApp(ui, server)
