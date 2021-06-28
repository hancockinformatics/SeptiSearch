
# TODO
#' - Add year to Study tab table - column not in full data spreadsheet...
#' - Make title truncated+hover-able in GSVA summary table if space is needed
#' - Fix tab titles and add more descriptive tooltips - better?
#' - User can upload metadata to be included in GSVA pheatmap??




# 1. Load packages, data, and functions -----------------------------------

library(shiny)
library(shinyjs)

message("Loading additional packages and sourcing functions...")
source("scripts/global.R", local = TRUE)




# 2. UI sections ----------------------------------------------------------

ui <- fluidPage(

  # Select the Bootswatch3 theme "Readable": https://bootswatch.com/3/readable
  theme = "css/readablebootstrap.css",

  # Head linking to custom CSS tweaks and favicons
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/user.css"),

    tags$style(type = "text/css", "body {padding-top: 75px;}"),

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
    position    = "fixed-top",
    windowTitle = "SeptiSearch",

    # Custom nested divs for the title, so we can have a custom title and the
    # Github logo on the right side of the navbar, linking to the Github page.
    # See "user.css" for the custom changes being applied to the image.
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
          "<a href='https://github.com/hancockinformatics/curation'> ",
          "<img src='github.svg' title='Visit SeptiSearch on Github to browse ",
          "the code or submit an issue.' alt='Github'> </a>"
        ))
      )
    ),



    # * 2.a Home ----------------------------------------------------------

    tabPanel(
      value = "home_tab",
      icon  = icon("home"),
      title = span("SeptiSearch", title = "The homepage for SeptiSearch."),

      div(
        class = "jumbotron",

        h1("Welcome"),
        hr(),

        div(class = "logoWrapper-home",

          p(HTML(
            "Welcome to <span style='color:#4582ec;'><b>SeptiSearch</b></span>!
            Here you can browse, explore, and download curated molecular results
            derived from sepsis studies. The app currently allows access to over
            24,000 unique molecules from 90 publications."
          )),

          p(HTML(
            "To get started, select one of the tabs above. <em>Explore the
            Collection by Study</em> is the easiest way to explore our
            curated data, based on the included publications. <em>Visualize the
            Top-Occurring Molecules</em> displays the most cited molecules in
            our dataset, and allows easy viewing of all entries for any molecule
            of interest. In the <em>Perform GSVA with Sepsis Signatures</em>
            tab you can upload your own expression data to determine if it's
            enriched for any of our curated molecular signatures. Finally,
            <em>Perform Pathway Enrichment</em> allows you to upload a list of
            genes and test for enriched pathways/biological terms using <a href=
            'https://bioconductor.org/packages/ReactomePA/'>ReactomePA</a>
            and <a href='https://maayanlab.cloud/Enrichr/'>enrichR</a>."
          )),

          p(HTML(
            "<span style='color:#4582ec;'><b>SeptiSearch</b></span> was
            created by Travis Blimkie, Jasmine Tam & Arjun Baghela from the
            <a href='http://cmdr.ubc.ca/bobh/'>REW Hancock Lab</a> at the
            University of British Columbia. If you'd like to learn more about
            <span style='color:#4582ec;'><b>SeptiSearch</b></span>, or find
            where to report bugs or issues, click the button below to visit
            our <em>About</em> page."
          )),

          br(),

          # Provide a direct link to the "About" page
          actionButton(
            inputId = "learn_more",
            label   = "Learn more",
            class   = "btn btn-primary btn-lg",
            title   = "Visit our About page!"
          )
        )
      ),


      # Place the wordcloud below the jumbotron and centered horizontally. The
      # latter is achieved via a CSS class in "www/css/user.css".
      div(HTML(paste0(
        "<img src='wordcloud.svg' class='center'
        title='Here&#39;s the 90 most common molecules in our database!'>"
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


    # * 2.b Explore Data by Study -----------------------------------------

    tabPanel(
      value = "study_tab",
      icon  = icon("university"),
      title = span(
        "Explore the Collection by Study",
        title = paste0(
          "Browse and search the entire data collection, organized by ",
          "study/article."
        )
      ),

      sidebarLayout(
        sidebarPanel = sidebarPanel(
          id    = "study_tab_sidebar",
          width = 3,

          h4("Explore the Collection by Study", style = "margin-top: 0"),
          p(
            "Here you can browse our collection by study/article. To the
            right, the top table shows each study included in our collection
            and the number of molecules in that study. You can search the
            articles by title, filter the studies to those containing specific
            molecules, or restrict the entries to a particular type of omics
            data."
          ),

          p(
            "By clicking on a row in the top table, another table with all
            the molecules in that study will appear below. You can download
            this study-specific table via the button which will appear
            further down in this section."
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

          # This is the new input for user molecules.
          textAreaInput(
            inputId     = "tabStudy_molecule_input",
            label       = "Search for specific molecules",
            placeholder = "Enter one molecule per line.",
            height      = 82,
            resize      = "vertical"
          ),

          # Omic type
          selectInput(
            inputId  = "tabStudy_omic_type_input",
            label    = "Omic Type",
            choices  = unique(not_NA(full_data$`Omic Type`)),
            multiple = TRUE
          ),

          # Filter for PMID
          # textAreaInput(
          #   inputId     = "tabStudy_pmid_input",
          #   label       = "Filter for a particular PMID",
          #   placeholder = "E.g. 32788292",
          #   height      = 41,
          #   resize      = "none"
          # ),

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
            icon    = icon("undo"),
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


    # * 2.c Visualize Molecule Occurrence ---------------------------------

    tabPanel(
      value = "viz_tab",
      icon  = icon("chart-bar"),
      title = span(
        "Visualize the Top-Occurring Molecules",
        title = paste0(
          "See our most-cited molecules and easily view & download all of ",
          "their entries."
        )
      ),

      sidebarLayout(
        sidebarPanel = sidebarPanel(
          id    = "viz_tab_sidebar",
          width = 3,

          h4("Visualize the Top-Occurring Molecules", style = "margin-top: 0"),

          p(
            "The plot on the right displays the 50 most common molecules in
            our collection. You can hover over the bars with your cursor to
            see the molecule's name and how many entries it has in our
            database."
          ),

          p(HTML(
            "The inputs below will filter the data and change what is
            displayed in the plot. For example, you can see the top
            metabolites using the <b>Molecule Type</b> input."
          )),

          p(
            "You can click on any bar in the plot to bring up a table
            containing all the occurrences of that molecule, and can
            download the molecule-specific table using the button below."
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
            icon    = icon("undo"),
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




    # * 2.d Perform GSVA  ---------------------------------------------------

    tabPanel(
      value = "gsva_tab",
      icon = icon("laptop-code"),
      title = span(
        "Perform GSVA with Sepsis Signatures",
        title = paste0(
          "Upload an RNA-Seq expression table to test for enrichment of our ",
          "curated sepsis signatures in your own data."
        )
      ),

      sidebarLayout(
        sidebarPanel = sidebarPanel(
          id = "gsva_tab_sidebar",
          width = 3,

          h4("Perform GSVA with Sepsis Signatures", style = "margin-top: 0"),

          p(
            "Here you can upload transformed counts from RNA-Seq to run Gene
            Set Variation Analysis (GSVA) using our curated signatures. In GSVA,
            your data is examined for dysregulation of specified sets of genes,
            to identify patterns of expression among your samples for the
            provided gene sets - here, the sepsis signatures that have been
            curated. For more details on the GSVA method, refer to the ",
            actionLink(inputId = "tabGSVA_about", label = "About"), "page."
          ),

          tags$label(
            "Inputs for GSVA must meet ALL of the following requirements:"
          ),

          tags$ul(
            tags$li("Must be a comma-separated plaintext file (.csv)"),
            tags$li("Samples should be columns, with genes as rows"),
            tags$li("The first column must contain Ensembl gene IDs"),
            tags$li(
              "Counts must be normalized/transformed as is appropriate for your
              data; raw counts will not be accepted."
            ),
          ),

          fileInput(
            inputId = "tabGSVA_matrix_input",
            label = NULL,
            buttonLabel = list(icon("upload"), "Browse..."),
            accept = "csv"
          ),

          tags$label("Optional: Upload sample metadata"),
          p(
            "You may also upload metadata for your samples, which will be added
            as annotations to the resulting heatmap to indicate groups or
            variables in your data (e.g. control and treatment designations).
            The first column must contain sample names (matching to columns from
            the matrix input). All remaining columns will become annotations on
            the heatmap."
          ),

          fileInput(
            inputId = "tabGSVA_metadata_input",
            label = NULL,
            buttonLabel = list(icon("upload"), "Browse..."),
            accept = "csv"
          ),

          disabled(
            actionButton(
              inputId = "tabGSVA_submit_button",
              label   = div(
                "Submit expression data for GSVA",
                HTML("&nbsp;"), # Horizontal spacer
                icon("arrow-alt-circle-right")
              ),
              class   = "btn btn-primary btn-tooltip",
              title   = paste0(
                "Upload your expression data, then click here to perform GSVA."
              )
            )
          ),

          uiOutput("tabGSVA_result_downloadbutton")
        ),

        mainPanel = mainPanel(
          width = 9,
          # This div only exists to serve as an anchor for an insertUI() call
          div(id = "tabGSVA_placeholder_div"),
          # uiOutput("tabGSVA_input_preview_ui")
          uiOutput("tabGSVA_result_UI"),
          uiOutput("tabGSVA_heatmap_UI")
        )
      )
    ),




    # * 2.e Perform Enrichment --------------------------------------------

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

          p(HTML(
            "Paste a list of genes into the field below (one per line) to
            test for enriched pathways using ReactomePA and enrichR. Input
            genes may be either Ensembl, Entrez, or HGNC identifiers.
            Results are automatically filtered using the adjusted p-value
            provided by each tool."
          )),

          p(
            "For more details on these methods, please see our ",
            actionLink(inputId = "tabEnrich_about", label = "About"),
            "page."
          ),
          br(),

          textAreaInput(
            inputId     = "tabEnrich_pasted_input",
            label       = "Enter your query molecules below:",
            placeholder = "ADAP2\nHK1\nLAIR1\nTRIM7\n...",
            height      = 200,
            resize      = "none"
          ),

          p(HTML(
            "Once you've entered your genes above, hit the <b>Submit genes</b>
            button to test for enriched pathways. Note that this may take some
            time to complete; please be patient."
          )),

          disabled(
            actionButton(
              inputId = "tabEnrich_submit_button",
              label   = div(
                "Submit genes for pathway enrichment",
                HTML("&nbsp;"), # Horizontal spacer
                icon("arrow-alt-circle-right")
              ),
              class   = "btn btn-primary btn-tooltip",
              title   = "Paste your genes above, then click here to test them."
            )
          ),

          # Render UI for success message and buttons to download enrichment
          # results
          uiOutput("tabEnrich_mapping_info"),
          uiOutput("tabEnrich_reactomepa_download_button"),
          uiOutput("tabEnrich_enrichr_download_button")
        ),

        mainPanel = mainPanel(
          width = 9,
          h1("Your enrichment results will be displayed below"),
          p(HTML(
            "Please allow up to 30 seconds after hitting the <b>Submit</b>
            button for results to appear."
          )),
          uiOutput("tabEnrich_result_reactomepa_ui"),
          uiOutput("tabEnrich_result_enrichr_ui")
        )
      )
    ),




    # * 2.f About ---------------------------------------------------------

    tabPanel(
      value = "about_tab",
      icon  = icon("info-circle"),
      title = span("About", title = "Learn more about SeptiSearch."),

      div(
        class = "jumbotron",
        style = "padding-bottom: 6px;",

        h1("About"),
        hr(),

        div(
          class = "logoWrapper-about",

          p(HTML(
            "<span style='color:#4582ec;'><b>SeptiSearch</b></span> is a Shiny
            app in which you can browse, explore, and download curated molecular
            signatures derived from sepsis studies. The app currently allows
            access to over 24,000 unique molecules from 90 publications. It was
            created by Travis Blimkie, Jasmine Tam & Arjun Baghela from the
            <a href='http://cmdr.ubc.ca/bobh/'>REW Hancock Lab</a> at the
            University of British Columbia. Travis is the main developer for
            the Shiny app, and handles maintenance & updates. Jasmine performed
            all the signature curation from publicly available articles and
            datasets. Arjun served as the supervisor for the project."
          )),

          br(),

          h3(strong("Reporting problems")),
          p(HTML(
            "If you encounter a problem or bug with the app, please submit an
            issue at the <a href=
            'https://github.com/hancockinformatics/curation'>Github page</a>.
            Include with your issue details on the problem so we can reproduce
            it, and any inputs if relevant (e.g. for the <i>Perform Enrichment
            Tests</i> tab)."
          )),

          br(),

          h3(strong("Funding")),
          p(HTML(
            "We would like to acknowledge and thank the
            <a href='https://cihr-irsc.gc.ca/e/193.html'>Canadian Institutes of
            Health Research (CIHR)</a> for providing the funding for this
            project."
          )),

          br(),

          h3(strong("Perform Pathway Enrichment")),
          p(HTML(
            "Input gene mapping between ID types is performed using data
            obtained via the <a href=
            'https://bioconductor.org/packages/biomaRt/'>biomaRt</a> R package.
            Pathway enrichment is performed using
            <a href='https://bioconductor.org/packages/ReactomePA'>
            ReactomePA</a> and <a href='https://maayanlab.cloud/Enrichr/'>
            enrichR</a>. For both methods, the results are filtered using an
            adjusted p-value threshold of 0.05. The following resources are
            searched using enrichR: MSigDB's Hallmark collection, and the three
            main GO databases (Biological Process, Cellular Component &
            Molecular Function)."
          )),

          br(),

          h3(strong("Use SeptiSearch Signatures for GSVA")),
          p(HTML(
            "Gene Set Variation Analysis is performed using the
            <a href='https://github.com/rcastelo/GSVA'>GSVA</a> package.
            Specified parameters include the <em>gsva</em> method and a
            <em>Gaussian</em> kernel. Genes with zero variance across all
            samples are removed prior to analysis. The heatmap visualization is
            created with <a href='https://github.com/raivokolde/pheatmap'>
            pheatmap</a>."
          )),

          br(),

          h3(strong("References")),

          p(
            HTML(
              "<span style='color:#4582ec;'><b>SeptiSearch</b></span> is
              written in R, and uses the following packages & resources:"
            ),
            style = "margin-bottom: 0;"
          ),

          tags$dl(
            tags$dt(
              a(href = "https://shiny.rstudio.com/", "Shiny"),
              tags$dd("Create beautiful web apps with R.")
            ),

            tags$dt(
              a(href = "https://deanattali.com/shinyjs/", "ShinyJS"),
              tags$dd("Extend Shiny functionality using JavaScript.")
            ),

            tags$dt(
              a(href = "https://www.tidyverse.org/", "Tidyverse"),
              tags$dd("A suite of packages for data manipulation.")
            ),

            tags$dt(
              a(href = "https://rstudio.github.io/DT/", "DT"),
              tags$dd(HTML(
                "An R interface to the <em>DataTables</em> JavaScript library."
              ))
            ),

            tags$dt(
              a(href = "https://plotly.com/r/", "Plotly"),
              tags$dd("Interactive plots and visualizations.")
            ),

            tags$dt(
              a(
                href = "https://bioconductor.org/packages/biomaRt/",
                "biomaRt"
              ),
              tags$dd("An R package to access BioMart databases.")
            ),

            tags$dt(
              a(
                href = "https://bioconductor.org/packages/ReactomePA",
                "ReactomePA"
              ),
              tags$dd("Perform pathway enrichment analysis using Reactome
                      data.")
            ),

            tags$dt(
              a(
                href = "https://cran.r-project.org/package=enrichR",
                "enrichR"
              ),
              tags$dd("Access the Ma'ayan Lab's gene set enrichment services
                      from R.")
            ),

            tags$dt(
              a(
                href = "https://github.com/rcastelo/GSVA",
                "GSVA"
              ),
              tags$dd("Gene Set Variation Analysis for microarray and RNA-Seq
                      data.")
            ),

            tags$dt(
              a(
                href = "https://cran.r-project.org/package=pheatmap",
                "pheatmap"
              ),
              tags$dd("Easy and robust heatmap visualizations.")
            )
          )
        )
      )
    )
  )
)




# 3. Server ---------------------------------------------------------------

server <- function(input, output, session) {




  # 3.a Home --------------------------------------------------------------

  # "Learn More" button that takes you to the About page
  observeEvent(input$learn_more, {
    updateNavbarPage(
      session  = session,
      inputId  = "navbar",
      selected = "about_tab"
    )
  }, ignoreInit = TRUE)




  # 3.b Explore Data by Study ---------------------------------------------


  # * 3.b.1 Parse and store user's inputs ---------------------------------

  # Simple text search for article titles
  tabStudy_title_search <- reactiveVal()
  observeEvent(input$tabStudy_title_input, {
    input$tabStudy_title_input %>% tabStudy_title_search()
  }, ignoreInit = TRUE)


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


  # Based on molecules the user searches, get the titles of articles which
  # contain that molecule(s), otherwise the number of molecules in the grouped
  # table isn't calculated properly. Needs to be wrapped in the conditional
  # since we get an error for trying to filter with NULL or an empty line.
  tabStudy_titles_with_user_molecules <- reactive({

    if (!all(
      is.null(tabStudy_users_molecules()) | tabStudy_users_molecules() == "")
    ) {
      full_data %>% filter(
        str_detect(Molecule, paste0(tabStudy_users_molecules(), collapse = "|"))
      ) %>%
        pull(Title)
    }
  })


  # * 3.b.2 Filter the grouped table --------------------------------------

  tabStudy_filtered_table <- reactive({

    full_data %>% filter(

      # Molecule searching
      conditional_filter(
        !all(
          is.null(tabStudy_titles_with_user_molecules()) |
            tabStudy_titles_with_user_molecules() == ""
        ),
        Title %in% tabStudy_titles_with_user_molecules()
      ),

      # Omic Type
      conditional_filter(
        length(input$tabStudy_omic_type_input) != 0,
        `Omic Type` %in% input$tabStudy_omic_type_input
      ),

      # User search for words in titles
      conditional_filter(
        !all(is.null(tabStudy_title_search()) | tabStudy_title_search() == ""),
        str_detect(Title, regex(tabStudy_title_search(), ignore_case = TRUE))
      )
    )
  })

  tabStudy_grouped_table <- reactive({
    tabStudy_filtered_table() %>%
      dplyr::select(
        Title,
        Author,
        PMID,
        Link,
        `Omic Type`,
        Platform,
        Molecule
      ) %>%
      group_by(across(c(-Molecule))) %>%
      summarise(`No. Molecules` = n(), .groups = "keep") %>%
      mutate(PMID = case_when(
        !is.na(PMID) ~ paste0(
          "<a target='_blank' href='", Link, "'>", PMID, "</a>"
        ),
        TRUE ~ paste0(
          "<a target='_blank' href='", Link, "'>Pre-Print</a>"
        )
      )) %>%
      ungroup() %>%
      dplyr::select(-Link) %>%
      dplyr::rename("Link" = PMID)
  })


  # * 3.b.3 Render grouped table ------------------------------------------

  output$tabStudy_grouped_DT <- DT::renderDataTable(
    tabStudy_grouped_table(),
    rownames  = FALSE,
    escape    = FALSE,
    selection = "single",
    options   = list(
      dom     = "tip",
      scrollX = TRUE,
      columnDefs = list(list(
        targets = 4,
        render  = ellipsis_render(25)
      ))
    )
  )

  output$tabStudy_grouped_render <- renderUI(
    tagList(
      DT::dataTableOutput("tabStudy_grouped_DT"),
      hr(),
      h3(
        "Click a row in the table above to see all molecules from that study."
      )
    )
  )


  # * 3.b.4 Create clicked table ------------------------------------------

  tabStudy_clicked_row_title  <- reactiveVal(NULL)
  tabStudy_clicked_row_author <- reactiveVal(NULL)
  tabStudy_clicked_row_pmid   <- reactiveVal(NULL)

  observeEvent(input$tabStudy_grouped_DT_rows_selected, {
    # The title, used to filter the main table for the specific study the user
    # selected
    tabStudy_grouped_table() %>%
      extract2(input$tabStudy_grouped_DT_rows_selected, 1) %>%
      tabStudy_clicked_row_title()

    # The author, used to name the downloaded study-specific table
    tabStudy_grouped_table() %>%
      extract2(input$tabStudy_grouped_DT_rows_selected, 2) %>%
      str_remove_all(., "\\.") %>%
      str_replace_all(., " ", "_") %>%
      tabStudy_clicked_row_author()

    # PMID, also used to name the downloaded file
    tabStudy_clicked_row_pmid({
      temp_id <- tabStudy_grouped_table() %>%
        extract2(input$tabStudy_grouped_DT_rows_selected, 3) %>%
        str_extract(., "[0-9]{8}") %>%
        replace(is.na(.), "")

      if (temp_id != "") {
        paste0("_", temp_id)
      } else {
        temp_id
      }
    })
  })

  output$tabStudy_test_clicked_row_title <-
    renderPrint(tabStudy_clicked_row_title())

  tabStudy_clicked_table <- reactive({
    if (is.null(tabStudy_clicked_row_title())) {
      return(NULL)
    } else {
      full_data %>%
        filter(Title == tabStudy_clicked_row_title()) %>%
        dplyr::select(
          Molecule,
          `Molecule Type`,
          Tissue,
          Timepoint,
          `Case Condition`,
          `Control Condition`,
          Infection,
          `Sex (M/F)`,
          `Age Group`
        )
    }
  })


  # * 3.b.5 Render clicked table ------------------------------------------

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

  output$tabStudy_clicked_render <- renderUI(
    tagList(
      br(),
      # verbatimTextOutput("tabStudy_test_clicked_row_title"),
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
    tabStudy_clicked_row_title(NULL)
    tabStudy_clicked_row_author(NULL)
    tabStudy_clicked_row_pmid(NULL)
  })


  # * 3.b.6 Download clicked study data -----------------------------------

  # The filename needs to be inside the function() call to properly update when
  # the clicked row changes (i.e. to make the filename reactive)
  output$tabStudy_clicked_study_download_handler <- downloadHandler(
    filename = function() {
      paste0(
        "septisearch_download_",
        tabStudy_clicked_row_author(),
        tabStudy_clicked_row_pmid(),
        ".txt"
      )
    },
    content = function(filename) {
      write_tsv(
        x    = tabStudy_clicked_table(),
        file = filename
      )
    }
  )


  # Render the UI for the download (just the button and an "br").
  output$tabStudy_clicked_study_download_button <- renderUI({
    if (is.null(tabStudy_clicked_table())) {
      return(NULL)
    } else {
      return(tagList(
        br(),
        p(strong("Download the table for the selected study:")),
        downloadButton(
          outputId = "tabStudy_clicked_study_download_handler",
          label    = "Download study-specific table",
          class    = "btn btn-success",
          style    = "width: 100%;"
        )
      ))
    }
  })




  # 3.c Visualize Molecule Occurrence -------------------------------------

  output$tabViz_select_inputs <- renderUI({
    tabViz_columns <- colnames(full_data_viz_tab) %>%
      str_subset(., "^Molecule$|PMID|Link|Author", negate = TRUE)

    tabViz_columns %>%
      map(~create_selectInput(column_name = ., tab = "tabViz"))
  })


  # * 3.c.1 Start with filters ----------------------------------------------

  # All the filtering steps make use of the custom `conditional_filter()`
  # function, so we don't need step-wise filtering, while keeping it reactive.
  tabViz_filtered_table <- reactive({
    full_data_viz_tab %>% filter(

      # Filter on omic type
      conditional_filter(
        length(input$tabViz_omic_type_input != 0),
        `Omic Type` %in% input$tabViz_omic_type_input
      ),

      # Molecule Type
      conditional_filter(
        length(input$tabViz_molecule_type_input) != 0,
        `Molecule Type` %in% input$tabViz_molecule_type_input
      ),

      # Tissue
      conditional_filter(
        length(input$tabViz_tissue_input) != 0,
        Tissue %in% input$tabViz_tissue_input
      ),

      # Timepoint
      conditional_filter(
        length(input$tabViz_timepoint_input) != 0,
        Timepoint %in% input$tabViz_timepoint_input
      ),

      # Case condition
      conditional_filter(
        length(input$tabViz_case_condition_input) != 0,
        `Case Condition` %in% input$tabViz_case_condition_input
      ),

      # Control Condition
      conditional_filter(
        length(input$tabViz_control_condition_input) != 0,
        `Control Condition` %in% input$tabViz_control_condition_input
      ),

      # Infection
      conditional_filter(
        length(input$tabViz_infection_input) != 0,
        Infection %in% input$tabViz_infection_input
      ),

      # Age group
      conditional_filter(
        length(input$tabViz_age_group_input) != 0,
        `Age Group` %in% input$tabViz_age_group_input
      )
    )
  })


  # Creating a table to plot the top 50 molecules based on the number of
  # citations
  tabViz_plot_table <- reactive({
    tabViz_filtered_table() %>%
      group_by(Molecule, Timepoint) %>%
      summarize(count = n(), .groups = "drop") %>%
      arrange(desc(count)) %>%
      mutate(Molecule = fct_inorder(Molecule)) %>%
      drop_na(Molecule, Timepoint) %>%
      head(50)
  })


  # * 3.c.2 Plotly --------------------------------------------------------

  # Make the plot via plotly, primarily to make use of the "hover text" feature.
  # Adding the `customdata` variable here allows us to access this information
  # when a user clicks on a bar, in addition to the x value (gene/protein name).
  output$tabViz_plot_object <- renderPlotly({
    plot_ly(
      data       = tabViz_plot_table(),
      x          = ~Molecule,
      y          = ~count,
      color      = ~Timepoint,
      customdata = tabViz_plot_table()$Timepoint,
      type       = "bar",
      hoverinfo  = "text",
      text       = ~paste0(
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
        title      = "<b>Top molecules based on citations</b>",
        margin     = list(t = 50),
        showlegend = TRUE,
        legend     = list(title = list(text = "<b>Timepoint</b>")),

        xaxis = list(
          title     = "",
          tickfont  = list(size = 12),
          tickangle = "45",
          zeroline  = TRUE,
          showline  = TRUE,
          mirror    = TRUE
        ),

        yaxis = list(
          title    = "<b>Number of Citations</b>",
          tick     = "outside",
          ticklen  = 3,
          zeroline = TRUE,
          showline = TRUE,
          mirror   = TRUE
        )
      )
  })


  # Create the table holding the data for the molecule/time point based on the
  # user clicking on a bar in plotly output
  tabViz_clicked_molecule_table <- reactive({
    d <- event_data("plotly_click", priority = "event")
    if (is.null(d)) {
      return(NULL)
    } else {
      tabViz_filtered_table() %>%
        filter(Molecule == d$x, Timepoint == d$customdata)
    }
  })


  # Grab the molecule name and time point for later use in naming the the
  # download file
  tabViz_clicked_molecule_info <- reactive({
    d <- event_data("plotly_click", priority = "event")
    if (is.null(d)) {
      return(NULL)
    } else {
      list(
        molecule  = d$x,
        timepoint = str_replace_all(
          d$customdata,
          pattern = " ",
          replacement = "_"
        )
      )
    }
  })


  # * 3.c.3 Render table --------------------------------------------------

  # Render the table with PMIDs as hyperlinks
  tabViz_clicked_molecule_table_for_DT <- reactive({
    if ( !is.null(tabViz_clicked_molecule_table()) ) {
      tabViz_clicked_molecule_table() %>%
        mutate(PMID = case_when(
          !is.na(PMID) ~ paste0(
            "<a target='_blank' href='", Link, "'>", PMID, "</a>"
          ),
          TRUE ~ paste0(
            "<a target='_blank' href='", Link, "'>Pre-Print</a>"
          )
        )) %>%
        ungroup() %>%
        dplyr::select(-c(Link, Molecule)) %>%
        dplyr::rename("Link" = PMID)
    }
  })

  output$tabViz_clicked_plot_table <- DT::renderDataTable(
    tabViz_clicked_molecule_table_for_DT(),
    rownames  = FALSE,
    escape    = FALSE,
    selection = "none",
    options   = list(
      dom     = "tip",
      paging  = FALSE,
      scrollX = TRUE
    )
  )

  # Simple render for testing/debugging purposes; see next chunk to enable it's
  # display
  output$testclick <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) {
      "Click to see the values:"
    } else {
      d
    }
  })

  # Rendering the plot and surrounding UI
  # Uncomment the `verbatimTextOutput` line to see the information from the
  # `plotly_click` event.
  output$tabViz_plot_panel <- renderUI({
    tagList(
      plotlyOutput("tabViz_plot_object", inline = TRUE, height = "300px"),
      # verbatimTextOutput("testclick"),
      h3("Click a bar to see all entries for that molecule & timepoint"),
      br()
    )
  })



  # Render the "clicked" table and the surrounding UI
  output$tabViz_clicked_table_panel <- renderUI({
    if ( !is.null(tabViz_clicked_molecule_table()) ) {
      return(
        tagList(
          h4(paste0(
            "Viewing entries for ",
            tabViz_clicked_molecule_info()[["molecule"]],
            " at the timepoint: ",
            str_to_sentence(str_replace_all(
              tabViz_clicked_molecule_info()[["timepoint"]],
              pattern = "_",
              replacement = " "
            ))
          )),
          div(
            DT::dataTableOutput("tabViz_clicked_plot_table"),
            style = "font-size: 13px"
          ),
          br()
        )
      )
    }
  })


  # * 3.c.4 Download clicked table ----------------------------------------

  # Download handler for the table generated when a user clicks on one of the
  # bars in the plot. Fed into the `renderUI()` chunk below so it only appears
  # when there is data to download.
  output$tabViz_clicked_table_download_handler <- downloadHandler(
    filename = function() {
      paste0(
        "septisearch_download_",
        tabViz_clicked_molecule_info()$molecule, "_",
        tabViz_clicked_molecule_info()$timepoint, ".txt"
      )
    },
    content = function(filename) {
      write_tsv(
        x    = tabViz_clicked_molecule_table(),
        file = filename
      )
    }
  )

  # Render the UI for the download (just the button and an "hr").
  output$tabViz_clicked_table_download_button <- renderUI({
    if (is.null(tabViz_clicked_molecule_table())) {
      return(NULL)
    } else {
      return(tagList(
        p(strong("Download the table for the chosen molecule:")),
        downloadButton(
          outputId = "tabViz_clicked_table_download_handler",
          label    = "Download plot table",
          class    = "btn btn-success",
          style    = "width: 100%;"
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




  # 3.d Perform GSVA ------------------------------------------------------

  # Linking to the About page for more details on the enrichment methods
  observeEvent(input$tabGSVA_about, {
    updateNavbarPage(
      session  = session,
      inputId  = "navbar",
      selected = "about_tab"
    )
  }, ignoreInit = TRUE)


  # * 3.d.1 Read, reformat, and preview input -----------------------------

  tabGSVA_user_input_0 <- reactiveVal()

  # We need to use read.csv() here so that we can check if the input data is
  # normalized (double) or raw (integer) - `read_csv()` treats everything as a
  # double. Here we also provide messages to the user about their input.
  observeEvent(input$tabGSVA_matrix_input, {
    read.csv(input$tabGSVA_matrix_input$datapath) %>%
      tabGSVA_user_input_0()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  tabGSVA_user_input_1 <- reactive({
    req(tabGSVA_user_input_0())

    if ( str_detect(tabGSVA_user_input_0()[1, 1], pattern = "^ENSG") ) {

      if ( is.double(as.matrix(tabGSVA_user_input_0()[, -1])) ) {
        gsva_temp_data <- tabGSVA_user_input_0() %>% as.data.frame()

        rownames(gsva_temp_data) <- gsva_temp_data[, 1]
        gsva_temp_data <- gsva_temp_data[, -1]

        showModal(modalDialog(
          title = span("Input Success!", style = "color: #3fad46;"),
          paste0(
            "Your data was successfully uploaded and parsed. Please ensure it
            looks correct in the preview table before proceeding (note not all
            genes/samples are displayed)."
          ),
          footer = modalButton("Continue"),
          easyClose = TRUE
        ))
        return(gsva_temp_data)

      } else {
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

  # Creating a preview of the user's input data
  tabGSVA_user_input_max_cols <- reactive({
    req(tabGSVA_user_input_1())

    if (ncol(tabGSVA_user_input_1()) >= 7) {
      return(7)
    } else {
      return(ncol(tabGSVA_user_input_1()))
    }
  })

  output$tabGSVA_input_preview_table <- renderDataTable(
    tabGSVA_user_input_1()[1:5, 1:tabGSVA_user_input_max_cols()],
    rownames = TRUE,
    options = list(dom = "t")
  )

  observeEvent(input$tabGSVA_matrix_input, {
    req(tabGSVA_user_input_1())

    insertUI(
      selector = "#tabGSVA_placeholder_div",
      where    = "afterEnd",
      ui       = tagList(div(
        id = "tagGSVA_input_data_preview_div",
        h3("Input data preview"),
        dataTableOutput("tabGSVA_input_preview_table")
      ))
    )
  })


  # * 3.d.3 Parse metadata input ------------------------------------------

  tabGSVA_meta_input_1 <- reactiveVal(NULL)
  observeEvent(input$tabGSVA_metadata_input, {
    read.csv(input$tabGSVA_metadata_input$datapath) %>%
      tabGSVA_meta_input_1()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)


  tabGSVA_meta_input_2 <- reactiveVal(NULL)
  observeEvent(input$tabGSVA_metadata_input, {

    if ( !is.null(tabGSVA_meta_input_1()) ) {
      if ( all(tabGSVA_meta_input_1()[, 1] %in% colnames(tabGSVA_user_input_1())) ) {

        gsva_temp_metadata <- tabGSVA_meta_input_1()

        rownames(gsva_temp_metadata) <- tabGSVA_meta_input_1()[, 1]
        gsva_temp_metadata <- gsva_temp_metadata[, -1]

        message("Successfully read metadata...")
        tabGSVA_meta_input_2(gsva_temp_metadata)
      } else {
        message("Problem detected with metadata (non-matching sample names)...")

        showModal(modalDialog(
          title = span("Input Error!", style = "color:red;"),
          paste0(
            "There was a problem matching the samples from your metadata ",
            "(rows) to the columns of your expression data. Please ensure all ",
            "samples match between the two files, without any missing or extra ",
            "samples, then try again."
          ),
          footer = modalButton("OK")
        ))

        tabGSVA_meta_input_2(NULL)
      }
    } else {
      tabGSVA_meta_input_2(NULL)
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)


  # * 3.d.2 Run GSVA ------------------------------------------------------

  # Enable the submission button when we have a non-NULL input
  observeEvent(input$tabGSVA_matrix_input, {
    req(tabGSVA_user_input_1())
    message("Input OK, enabling submission...")
    enable("tabGSVA_submit_button")
  })

  # Remove the input preview, show a modal dialog and run GSVA
  tabGSVA_result_1 <- reactiveVal()
  observeEvent(input$tabGSVA_submit_button, {
    removeUI("#tagGSVA_input_data_preview_div")

    message("Running GSVA...")

    showModal(modalDialog(
      title = span("Running GSVA.", style = "color: #4582ec;"),
      paste0(
        "Your input expression data is currently being analyzed. Please wait
        for your results to appear. Note that if you submitted data containing
        a large number of samples, it will take some time to analyze; please be
        patient."
      ),
      footer = NULL
    ))

    perform_gsva(
      expr = tabGSVA_user_input_1(),
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


  # * 3.d.3 Render the results to the user --------------------------------

  tabGSVA_result_summary <- reactive({
    # Summary table that is displayed above the heatmap
    list(
      "summary_tbl" = left_join(
        tabGSVA_result_1()[["gsva_res_df"]],
        full_data_gsva_tab,
        by = c("Gene Set Name" = "study_label")
      ) %>%
        dplyr::select(
          `Gene Set Name`,
          `Gene Set Length`,
          `No. Shared Genes`,
          Title
        ),

      # Results from GSVA plus the gene set info columns - this is what the user
      # can download.
      "gsva_res_df" = left_join(
        tabGSVA_result_1()[["gsva_res_df"]],
        full_data_gsva_tab,
        by = c("Gene Set Name" = "study_label")
      ) %>%
        dplyr::select(
          `Gene Set Name`,
          `Gene Set Length`,
          `No. Shared Genes`,
          Title,
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
        title = "Name of the sepsis signature/gene set."
      ),
      th(
        "Gene Set Length",
        title = "Number of genes/molecules in the gene set."
      ),
      th(
        "No. Shared Genes",
        title = "Number of genes from the set present in the input data."
      ),
      th(
        "Title",
        title = "Title of the article on which the gene set is based."
      )
    ))
  ))

  output$tabGSVA_result_DT <- renderDataTable(
    datatable(
      tabGSVA_result_summary()[["summary_tbl"]],
      container = tabGSVA_table_container,
      rownames  = FALSE,
      selection = "none",
      options   = list(dom = "tip")
    )
  )

  output$tabGSVA_result_UI <- renderUI({
    req(tabGSVA_result_1())

    tagList(
      h3("Summary table of GSVA results:"),
      dataTableOutput("tabGSVA_result_DT")
    )
  })


  # * 3.d.4 Render heatmap ------------------------------------------------

  observeEvent(input$tabGSVA_submit_button, {
    if ( !is.null(tabGSVA_result_summary()[["gsva_res_plt"]]) ) {
      output$tabGSVA_heatmap_UI <- renderUI(
        tagList(
          br(),
          br(),
          h3("Heatmap of GSVA results:"),
          renderPlot(
            tabGSVA_result_summary()[["gsva_res_plt"]],
            height = 1400,
            alt = "Heatmap of GSVA results."
          ),
          br(),
        )
      )
    }
  })


  # * 3.d.5 Download results ----------------------------------------------

  observeEvent(input$tabGSVA_submit_button, {
    if ( !is.null(tabGSVA_result_summary()[["gsva_res_df"]]) ) {
      output$tabGSVA_result_downloadhandler <- downloadHandler(
        filename = function() {
          paste0(
            "septisearch_",
            tools::file_path_sans_ext(input$tabGSVA_matrix_input$name),
            "_GSVA_result.csv"
          )
        },
        content = function(filename) {
          write_csv(
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
            <a href='https://github.com/hancockinformatics/curation'>
            Github page</a>."
          ))
        )
      )
    }
  })




  # 3.e Perform Enrichment ------------------------------------------------

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


  # * 3.e.1 Parse molecule input ------------------------------------------

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


  # Place the input genes into a tibble so we can map with `left_join()`
  tabEnrich_input_genes_table <- reactive({
    return(
      tibble("input_genes" = as.character(tabEnrich_input_genes()))
    )
  })


  # Enable the submission button once we have some input from the user. Note
  # we're aren't checking if the input is "valid" yet...
  observeEvent(input$tabEnrich_pasted_input, {
    if ( nrow(tabEnrich_input_genes_table()) > 0 ) {
      enable("tabEnrich_submit_button")
    }
  })


  # * 3.e.2 Map genes -----------------------------------------------------

  tabEnrich_mapped_genes <- reactive({
    req(tabEnrich_input_genes(), tabEnrich_input_genes_table())
    map_genes(
      gene_list  = tabEnrich_input_genes(),
      gene_table = tabEnrich_input_genes_table()
    )
  })


  # * 3.e.3 Perform enrichment tests --------------------------------------

  observeEvent(input$tabEnrich_submit_button, {

    # Create modal dialog to say the tests are running
    showModal(modalDialog(
      title = span(
        "Enrichment testing in progress.",
        style = "color: #4582ec;"
      ),
      paste0(
        "We are currently mapping and testing your ",
        nrow(tabEnrich_input_genes_table()),
        " ",
        attr(tabEnrich_mapped_genes(), "id_type"),
        " input genes. Your results will appear on this page shortly, ",
        "please wait..."
      ),
      footer = NULL
    ))

    test_enrichment(tabEnrich_mapped_genes()) %>%
      tabEnrich_test_result()
  })


  # Take the initial results objects and tidy them up for display
  tabEnrich_test_result_clean <- reactive({
    req(tabEnrich_test_result())

    list(
      ReactomePA = tabEnrich_test_result()$ReactomePA %>%
        dplyr::select(-c(gene_id, qvalue)) %>%
        mutate(across(where(is.numeric), signif, digits = 3)) %>%
        clean_names("title", abbreviations = c("BG", "ID")) %>%
        dplyr::rename("P Value" = Pvalue),

      EnrichR = tabEnrich_test_result()$EnrichR %>%
        dplyr::select(-c(old_p_value, old_adjusted_p_value, genes)) %>%
        mutate(across(where(is.numeric), signif, digits = 3)) %>%
        clean_names("title", abbreviations = "P")
    )
  })


  # * 3.e.4 Output results tables -----------------------------------------

  observeEvent(input$tabEnrich_submit_button, {

    ### ReactomePA
    output$tabEnrich_result_reactomepa <- renderDataTable(
      tabEnrich_test_result_clean()$ReactomePA,
      rownames = FALSE,
      options  = list(
        dom = "tip"
      )
    )
    output$tabEnrich_result_reactomepa_ui <-renderUI(
      tagList(
        h3("ReactomePA:"),
        dataTableOutput("tabEnrich_result_reactomepa"),
        hr()
      )
    )


    ### EnrichR
    output$tabEnrich_result_enrichr <- renderDataTable(
      tabEnrich_test_result_clean()$EnrichR,
      rownames = FALSE,
      options  = list(
        dom = "tip"
      )
    )
    output$tabEnrich_result_enrichr_ui <- renderUI(
      tagList(
        h3("EnrichR:"),
        dataTableOutput("tabEnrich_result_enrichr"),
        br()
      )
    )
  })

  # Once the mapping is finished, remove the notification message
  observeEvent(input$tabEnrich_submit_button, {
    if ( !is.null(tabEnrich_test_result_clean()$ReactomePA) ) {
      removeModal()
    }
  })


  # * 3.e.5 Download results ----------------------------------------------

  # Provide some info to the user regarding the number of unique input genes,
  # and how they mapped to the other ID types. The UI elements are constructed
  # conditionally based on the input ID type using the custom function
  # `make_success_message`.
  output$tabEnrich_mapping_info <- renderUI({
    if (
      is.null(tabEnrich_test_result_clean()$ReactomePA) &&
      is.null(tabEnrich_test_result_clean()$EnrichR)
    ) {
      return(NULL)
    } else {
      return(tagList(
        hr(),
        tags$label("Mapping results"),
        make_success_message(
          mapped_data = isolate(tabEnrich_mapped_genes())
        ),
        tags$label("Enrichment results"),
        p(
          "With your input genes, we found ",
          span(
            style = "color: #4582ec;",
            nrow(tabEnrich_test_result_clean()$ReactomePA)
          ),
          " pathways from ReactomePA, ",
          "and ",
          span(
            style = "color: #4582ec;",
            nrow(tabEnrich_test_result_clean()$EnrichR)
          ),
          " terms from enrichR. Use the buttons below to download your ",
          "results as a tab-delimited text file."
        )
      ))
    }
  })


  # First button for ReactomePA...
  output$tabEnrich_reactomepa_download_handler <- downloadHandler(
    filename = "septisearch_reactomePA_result.txt",
    content  = function(filename) {
      write_tsv(
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
          br(),
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


  # ...and a second for EnrichR
  output$tabEnrich_enrichr_download_handler <- downloadHandler(
    filename = "septisearch_enrichR_result.txt",
    content  = function(filename) {
      write_tsv(
        x    = tabEnrich_test_result_clean()$EnrichR,
        file = filename
      )
    }
  )

  observeEvent(input$tabEnrich_submit_button, {
    output$tabEnrich_enrichr_download_button <- renderUI({
      if (is.null(tabEnrich_test_result_clean()$EnrichR)) {
        return(NULL)
      } else {
        return(tagList(
          br(),
          downloadButton(
            outputId = "tabEnrich_enrichr_download_handler",
            label    = "Download EnrichR results",
            class    = "btn btn-success",
            style    = "width: 100%;"
          )
        ))
      }
    })
  })
} #server close




# 4. Run the app ----------------------------------------------------------

shinyApp(ui, server)
