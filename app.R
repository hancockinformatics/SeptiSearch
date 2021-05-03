
# 1. Load packages, data, and functions -----------------------------------

library(shiny)
library(shinyjs)
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
      strong("SeptiSearch"),
      # HTML(
      #   "<img src='septisearch.svg' height='45' alt='SeptiSearch'>"
      # ),

      # Custom div containing the Github logo for the right side of the navbar
      div(
        id = "img-id",
        HTML(paste0(
          "<a href='https://github.com/hancockinformatics/curation'> ",
          "<img src = 'github.svg'> </a>"
        ))
      )
    ),



    # * 2.a Home ----------------------------------------------------------

    tabPanel(
      value = "home_tab",
      icon  = icon("home"),
      title = "Home",

      div(
        class = "jumbotron",

        h1("Welcome"),
        hr(),

        div(class = "logoWrapper",

          p(HTML(paste0(
            "Welcome to <span style='color:#4582ec;'><b>SeptiSearch",
            "</b></span>! Here you can browse, explore, and download curated ",
            "molecular results derived from sepsis studies. The app currently ",
            "allows access to over 14,500 unique molecules from more than 70 ",
            "different publications."
          ))),

          p(HTML(
            "To get started, select one of the tabs above. ",
            "<span style='color:#4582ec;'>Explore Data in a Table</span> will ",
            "let you browse our entire collection, with the ability to filter ",
            "the data in various ways and search for specific molecules. ",
            "<span style='color:#4582ec;'>Explore Data by Study</span> is the ",
            "easiest way to explore our collection based on the publications ",
            "we've curated. <span style='color:#4582ec;'>Visualize Molecule ",
            "Occurence</span> displays the most cited molecules in our ",
            "dataset, and allows easy viewing of all entries for any molecule ",
            "of interest. Finally, <span style='color:#4582ec;'>Perform ",
            "Enrichment Tests</span> allows you to upload a list of genes and ",
            "test for enriched pathways/GO terms using <a href=",
            "'https://bioconductor.org/packages/ReactomePA/'>ReactomePA</a> ",
            "and <a href='https://maayanlab.cloud/Enrichr/'>enrichR</a>."
          )),

          p(HTML(
            "If you'd like to know more about <span style='color:#4582ec;'>",
            "<b>SeptiSearch</b></span>, or find where to report bugs or ",
            "issues, click the button below to visit our <span style=",
            "'color:#4582ec;'>About</span> page."
          )),

          br(),

          # Provide a direct link to the "About" page
          actionButton(
            inputId = "learn_more",
            label   = "Learn more",
            class   = "btn btn-primary btn-lg"
          )
        )
      ),


      # Place the wordcloud below the jumbotron and centered horizontally. The
      # latter is achieved via a CSS class in "www/css/user.css".
      div(HTML("<img src='wordcloud.svg' class='center'>")),


      # Separate div to include the lab logo in the bottom-left corner, below
      # the wordcloud
      div(
        style = "position: relative; bottom: 0; padding-bottom: 10px;",
        HTML(
          "<a href='http://cmdr.ubc.ca/bobh/'>",
          "<img src='hancock-lab-logo.svg'> </a>"
        )
      )
    ),



    # * 2.b Explore Data in a Table ---------------------------------------

    tabPanel(
      value = "table_tab",
      icon  = icon("table"),
      title = "Explore Data in a Table",

      sidebarLayout(
        sidebarPanel = sidebarPanel(
          id    = "tabTable_sidebar",

          # Making the sidebarPanel a bit narrower (default is 4) to accommodate
          # our table. Note this plus the width of the main panel must equal 12.
          width = 3,

          h4("Explore Data in a Table", style = "margin-top: 0"),
          p(
            "Search our database for any molecules using the box below, ",
            "entering one gene/protein/metabolite per line. The other fields ",
            "allow you to filter the table based on the values in any of the ",
            "columns displayed."
          ),

          p(HTML(
            "You can also download the currently viewed table using the ",
            "button below. The <b>Restore defaults</b> button will reset any ",
            "searches or filters that have been applied to the data."
          )),
          hr(),

          # Area for the user to input their own genes to filter the data
          textAreaInput(
            inputId     = "pasted_molecules",
            label       = "Search for specific molecules",
            placeholder = "One per line...",
            height      = 82
          ),

          # All of the selectInput bits are created in the server section, so we
          # can make them using map() and create_selectInput() instead of
          # repeating the same code many times
          uiOutput("tabTable_select_inputs"),
          hr(),

          # UI for the download button
          p(strong(
            "Download the current table (tab-delimited):"
          )),

          downloadButton(
            outputId = "full_table_download_handler",
            label    = "Download the data",
            class    = "btn btn-success"
          ),

          hr(),

          # Reset button for the tab (from shinyjs)
          actionButton(
            class   = "btn-info",
            style   = "width: 170px",
            inputId = "tabTable_reset",
            icon    = icon("undo"),
            label   = "Restore defaults"
          )
        ),

        mainPanel = mainPanel(
          width = 9,
          uiOutput("table_molecules_render"),
        )
      )
    ),


    # * 2.c Explore Data by Study -----------------------------------------

    tabPanel(
      value = "study_tab",
      icon  = icon("university"),
      title = "Explore Data by Study",

      sidebarLayout(
        sidebarPanel = sidebarPanel(
          id    = "by_study_tab",
          width = 3,

          h4("Explore Data by Study", style = "margin-top: 0"),
          p(
            "Here you can browse our collection by study/article. To the ",
            "right, the top table shows each study included in our ",
            "collection, and shows the number of molecules tied to that ",
            "study. You can search the articles by title, or filter for a ",
            "specific PMID or type of omics data."
          ),

          p(
            "By clicking on a row in the top table, another table listing all ",
            "the molecules in that study will appear below. You can also ",
            "download this study-specific table via the button which appears ",
            "further down."
          ),

          hr(),

          # Input for the user to search article titles
          textAreaInput(
            inputId     = "by_study_title_input",
            label       = "Search article titles",
            placeholder = "Enter terms here...",
            height      = 41,
            resize      = "none",
          ),

          # Filter for PMID
          textAreaInput(
            inputId     = "by_study_pmid_input",
            label       = "Filter for a particular PMID",
            placeholder = "E.g. 32788292",
            height      = 41,
            resize      = "none"
          ),

          # Omic type
          selectInput(
            inputId  = "tabStudy_omic_type_input",
            label    = "Omic Type",
            choices  = unique(not_NA(full_data$`Omic Type`)),
            multiple = TRUE
          ),

          # UI for the download button
          uiOutput("clicked_study_download_button"),
          hr(),

          # Reset button for the tab (from shinyjs) - note this mostly relies on
          # normal R/Shiny code to work, since it's resetting DT stuff which
          # doesn't respond to the shinyjs reset button (i.e. we have to
          # manually reset the state of variables/DT tables).
          actionButton(
            class   = "btn-info",
            style   = "width: 170px",
            inputId = "by_study_reset",
            icon    = icon("undo"),
            label   = "Restore defaults"
          )
        ),

        mainPanel = mainPanel(
          width = 9,
          uiOutput("by_study_grouped_render"),
          uiOutput("by_study_clicked_render")
        )
      )
    ),


    # * 2.d Visualize Molecule Occurrence ---------------------------------

    tabPanel(
      value = "vis_tab",
      icon  = icon("chart-bar"),
      title = "Visualize Molecule Occurrence",

      sidebarLayout(
        sidebarPanel = sidebarPanel(
          id    = "tabViz_sidebar",
          width = 3,

          h4("Vizualize Molecule Occurrence", style = "margin-top: 0"),

          p(
            "The plot on the right displays the 50 most common molecules in ",
            "our collection. You can hover over the bars with your cursor to ",
            "see the molecule's name and how many entries it has in our ",
            "database."
          ),

          p(HTML(
            "The inputs below will filter the data and change what is ",
            "displayed in the plot. For example, you can see the top ",
            "metabolites using the <b>Molecule Type</b> input."
          )),

          p(
            "You can click on any bar in the plot to bring up a table ",
            "containing all the occurrences of that molecule, and can ",
            "download the molecule-specific table using the button below."
          ),

          hr(),

          # Just like the Table tab, we're building all of these inputs in the
          # server section so we don't have to repeat the same code many times
          uiOutput("tabViz_select_inputs"),
          hr(),

          # Dynamically render the download button, to download the table only
          # when there is something to actually download.
          uiOutput("click_table_download_button"),

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
          uiOutput("plot_panel"),
          uiOutput("table_click_panel")
        )
      )
    ),



    # * 2.e Perform Enrichment --------------------------------------------

    tabPanel(
      value = "enrich_tab",
      icon  = icon("calculator"),
      title = "Perform Enrichment Tests",

      sidebarLayout(
        sidebarPanel = sidebarPanel(
          id = "tabEnrich_sidebar",
          width = 3,

          h4("Perform Enrichment Tests", style = "margin-top: 0"),

          p(HTML(
            "Paste a list of genes into the field below (one per line) to ",
            "test for enriched pathways using ReactomePA and enrichR. Input ",
            "genes may be either Ensembl, Entrez, or HGNC identifiers. ",
            "Results are automatically filtered using the adjusted p-value ",
            "provided by each tool."
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
            placeholder = "One per line...",
            height      = 200,
            resize      = "none"
          ),

          p(HTML(
            "Once you've entered your genes above, hit the <b>Submit genes",
            "</b> button to test for enriched pathways. Note that this may ",
            "take some time to complete; please be patient."
          )),

          disabled(
            actionButton(
              inputId = "tabEnrich_submit_button",
              label   = "Submit genes",
              class   = "btn btn-primary btn-tooltip",
              title   = "Test your input genes for enriched pathways"
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
            "Please allow up to 30 seconds after hitting the <b>Submit</b> ",
            "button for results to appear."
          )),
          uiOutput("result_reactomepa_ui"),
          uiOutput("result_enrichr_ui")
        )
      )
    ),



    # * 2.f About ---------------------------------------------------------

    tabPanel(
      value = "about_tab",
      icon  = icon("info-circle"),
      title = "About",

      div(
        class = "jumbotron",
        style = "padding-bottom: 6px;",

        h1("About"),
        hr(),

        div(
          class = "logoWrapper",

          p(HTML(
            "SeptiSearch was created by Travis Blimkie, Jasmine Tam & Arjun ",
            "Baghela from the <a href='http://cmdr.ubc.ca/bobh/'>REW Hancock ",
            "Lab</a> at the University of British Columbia. All data was ",
            "manually curated from published articles by Jasmine. If you ",
            "encounter a problem or bug with the app, please submit an issue ",
            "at the <a href='https://github.com/hancockinformatics/curation'>",
            "Github page</a>."
          )),

          p(HTML(
            "Pathway enrichment is performed using ",
            "<a href='https://bioconductor.org/packages/ReactomePA'>",
            "ReactomePA</a> and <a ",
            "href='https://maayanlab.cloud/Enrichr/'>enrichR</a>. For both ",
            "methods, the results are filtered using an adjusted p-value ",
            "threshold of 0.05. The following resources are searched using ",
            "enrichR: MSigDB's Hallmark collection, and the three main GO ",
            "databases: Molecular Function, Cellular Component, & Biological ",
            "Process."
          )),

          h3(strong("SeptiSearch uses the following R packages:")),

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
              tags$dd("An R interface to the DataTables JavaScript library.")
            ),

            tags$dt(
              a(href = "https://plotly.com/r/", "Plotly"),
              tags$dd("Interactive visualizations in R.")
            ),

            tags$dt(
              a(
                href = "https://bioconductor.org/packages/ReactomePA",
                "ReactomePA"
              ),
              tags$dd("Perform pathway analysis using Reactome data.")
            ),

            tags$dt(
              a(
                href = "https://cran.r-project.org/package=enrichR",
                "enrichR"
              ),
              tags$dd("Access gene set enrichment services from R.")
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




  # 3.b Explore Data in a Table -------------------------------------------

  # Create inputs, one for each column with a couple exceptions
  output$tabTable_select_inputs <- renderUI({
    tabTable_columns <- colnames(full_data_table_tab) %>%
      str_subset(., "^Molecule$|PMID", negate = TRUE)

    tabTable_columns %>%
      map(~create_selectInput(column_name = ., tab = "tabTable"))
  })

  # Set up reactive value to store input molecules from the user
  users_molecules <- reactiveVal()
  observeEvent(input$pasted_molecules, {
    input$pasted_molecules %>%
      str_split(., pattern = " |\n") %>%
      unlist() %>%
      # The next step prevents the inclusion of an empty string, if the user
      # starts a new line but doesn't type anything
      str_subset(., pattern = "^$", negate = TRUE) %>%
      users_molecules()
  }, ignoreInit = TRUE)


  # * 3.b.1 Apply filters to the table --------------------------------------

  # All the filtering steps make use of the custom `conditional_filter()`
  # function, so we don't need step-wise filtering, while keeping the output
  # reactive.
  table_molecules <- reactive({
    full_data_table_tab %>% filter(

      # User search for specific molecules
      conditional_filter(
        !all(is.null(users_molecules()) | users_molecules() == ""),
        str_detect(Molecule, paste0(users_molecules(), collapse = "|"))
      ),

      # Filter on omic type
      conditional_filter(
        length(input$tabTable_omic_type_input != 0),
        `Omic Type` %in% input$tabTable_omic_type_input
      ),

      # Molecule Type
      conditional_filter(
        length(input$tabTable_molecule_type_input) != 0,
        `Molecule Type` %in% input$tabTable_molecule_type_input
      ),

      # Tissue
      conditional_filter(
        length(input$tabTable_tissue_input) != 0,
        Tissue %in% input$tabTable_tissue_input
      ),

      # Timepoint
      conditional_filter(
        length(input$tabTable_timepoint_input) != 0,
        Timepoint %in% input$tabTable_timepoint_input
      ),

      # Case condition
      conditional_filter(
        length(input$tabTable_case_condition_input) != 0,
        `Case Condition` %in% input$tabTable_case_condition_input
      ),

      # Control Condition
      conditional_filter(
        length(input$tabTable_control_condition_input) != 0,
        `Control Condition` %in% input$tabTable_control_condition_input
      ),

      # Infection
      conditional_filter(
        length(input$tabTable_infection_input) != 0,
        Infection %in% input$tabTable_infection_input
      ),

      # Age group
      conditional_filter(
        length(input$tabTable_age_group_input) != 0,
        `Age Group` %in% input$tabTable_age_group_input
      )
    )
  })


  # Modify the above filtered table, prior to display, to make PMIDs into links
  table_molecules_hyper <- reactive({
    table_molecules() %>%
      mutate(PMID = case_when(
        !is.na(PMID) ~ paste0(
          "<a target='_blank' href='",
          "https://pubmed.ncbi.nlm.nih.gov/", PMID, "'>", PMID, "</a>"
        ),
        TRUE ~ ""
      )) %>%
      arrange(`Molecule Type`, Molecule)
  })


  # * 3.b.2 Render table --------------------------------------------------

  # Render the DT output table, with 20 rows per page
  output$table_molecules_DT <- DT::renderDataTable(
    table_molecules_hyper(),
    rownames  = FALSE,
    escape    = FALSE,
    selection = "none",
    options   = list(
      dom        = "tip",
      scrollX    = TRUE,
      pageLength = 20
    )
  )

  # Output the table and the <br> below it. Reduce the font size of the table so
  # we can see more of the data at once.
  output$table_molecules_render <- renderUI({
    tagList(
      div(
        DT::dataTableOutput("table_molecules_DT"),
        style = "font-size:13px;"
      ),
      br()
    )
  })


  # * 3.b.3 Download the table ----------------------------------------------

  output$full_table_download_handler <- downloadHandler(
    filename = "septisearch_download_full.txt",
    content  = function(filename) {
      write_tsv(
        x    = table_molecules(),
        file = fileanme
      )
    }
  )


  # * 3.b.4 Reset button ----------------------------------------------------

  # Allow the user to "reset" the page to its original/default state
  observeEvent(input$tabTable_reset, {
    shinyjs::reset("tabTable_sidebar", asis = FALSE)
  })




  # 3.c Explore Data by Study ---------------------------------------------

  # Simple text search for article titles
  by_study_title_search <- reactiveVal()
  observeEvent(input$by_study_title_input, {
    input$by_study_title_input %>% by_study_title_search()
  }, ignoreInit = TRUE)


  # Filter the table with a specific PMID (currently only supports one PMID at a
  # time)
  by_study_pmid_search <- reactiveVal()
  observeEvent(input$by_study_pmid_input, {
    input$by_study_pmid_input %>%
      str_trim() %>%
      by_study_pmid_search()
  }, ignoreInit = TRUE)


  by_study_grouped_table <- reactive({

    by_study_grouped_static_table %>% filter(

      # Omic Type
      conditional_filter(
        length(input$tabStudy_omic_type_input) != 0,
        `Omic Type` %in% input$tabStudy_omic_type_input
      ),

      # User search for words in titles
      conditional_filter(
        !all(is.null(by_study_title_search()) | by_study_title_search() == ""),
        str_detect(Title, regex(by_study_title_search(), ignore_case = TRUE))
      ),

      # Filter on PMID
      conditional_filter(
        !all(is.null(by_study_pmid_search()) | by_study_pmid_search() == ""),
        str_detect(PMID, by_study_pmid_search())
      )
    )
  })


  # * 3.c.1 Render grouped table ------------------------------------------

  output$by_study_grouped_DT <- DT::renderDataTable(
    by_study_grouped_table(),
    rownames  = FALSE,
    escape    = FALSE,
    selection = "single",
    options   = list(
      dom     = "tip",
      scrollX = TRUE
    )
  )

  output$by_study_grouped_render <- renderUI(
    tagList(
      DT::dataTableOutput("by_study_grouped_DT"),
      hr(),
      h3(
        "Click a row in the table above to see all molecules from that study."
      )
    )
  )


  # * 3.c.2 Create clicked table ------------------------------------------

  clicked_row_title  <- reactiveVal(NULL)
  clicked_row_author <- reactiveVal(NULL)

  observeEvent(input$by_study_grouped_DT_rows_selected, {
    # The title, used to filter the main table for the specific study the user
    # selected
    by_study_grouped_table() %>%
      extract2(input$by_study_grouped_DT_rows_selected, 1) %>%
      clicked_row_title()

    # The author, used to name the downloaded study-specific table
    by_study_grouped_table() %>%
      extract2(input$by_study_grouped_DT_rows_selected, 2) %>%
      str_remove_all(., "\\.") %>%
      str_replace_all(., " ", "_") %>%
      clicked_row_author()
  })

  output$test_clicked_row_title <- renderPrint(clicked_row_title())

  by_study_clicked_table <- reactive({
    if (is.null(clicked_row_title())) {
      return(NULL)
    } else {
      full_data %>%
        filter(Title == clicked_row_title()) %>%
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


  # * 3.c.3 Render clicked table ------------------------------------------

  observeEvent(input$by_study_grouped_DT_rows_selected, {
    output$by_study_clicked_DT <- DT::renderDataTable(
      by_study_clicked_table(),
      rownames  = FALSE,
      escape    = FALSE,
      selection = "none",
      options   = list(
        dom     = "tip",
        scrollX = TRUE
      )
    )
  })

  output$by_study_clicked_render <- renderUI(
    tagList(
      br(),
      # verbatimTextOutput("test_clicked_row_title"),
      DT::dataTableOutput("by_study_clicked_DT"),
      br()
    )
  )

  # Allow the user to "reset" the page to its original/default state. All the
  # values need to be reset manually; the shinyjs reset function doesn't seem to
  # apply to DT functions/objects
  observeEvent(input$by_study_reset, {
    shinyjs::reset("by_study_tab", asis = FALSE)
    selectRows(proxy = dataTableProxy("by_study_grouped_DT"), selected = NULL)
    output$by_study_clicked_DT <- NULL
    clicked_row_title(NULL)
    clicked_row_author(NULL)
  })


  # * 3.c.4 Download clicked study data -----------------------------------

  output$clicked_study_download_handler <- downloadHandler(
    filename = paste0(
      "septisearch_download_",
      clicked_row_author(),
      ".txt"
    ),
    content = function(filename) {
      write_tsv(
        x    = by_study_clicked_table(),
        file = filename
      )
    }
  )


  # Render the UI for the download (just the button and an "br").
  output$clicked_study_download_button <- renderUI({
    if (is.null(by_study_clicked_table())) {
      return(NULL)
    } else {
      return(tagList(
        br(),
        p(strong("Download the table for the chosen study:")),
        downloadButton(
          outputId = "clicked_study_download_handler",
          label    = "Download study table",
          class    = "btn btn-success"
        )
      ))
    }
  })




  # 3.d Visualize Molecule Occurrence -------------------------------------

  output$tabViz_select_inputs <- renderUI({
    tabViz_columns <- colnames(full_data_viz_tab) %>%
      str_subset(., "^Molecule$|PMID|Author", negate = TRUE)

    tabViz_columns %>%
      map(~create_selectInput(column_name = ., tab = "tabViz"))
  })


  # * 3.d.1 Start with filters ----------------------------------------------

  # All the filtering steps make use of the custom `conditional_filter()`
  # function, so we don't need step-wise filtering, while keeping it reactive.
  filtered_table <- reactive({
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


  # This creates the table shown below the plot, created when clicking on a bar.
  # Like the table from the first tab, we want the PMIDs to be links.
  plot_molecules_hyper <- reactive({
    filtered_table() %>%
      mutate(PMID = case_when(
        !is.na(PMID) ~ paste0(
          "<a target='_blank' href='",
          "https://pubmed.ncbi.nlm.nih.gov/",
          PMID, "'>", PMID, "</a>"
        ),
        TRUE ~ "none"
      )) %>%
      arrange(Author)
  })


  # Creating a table to plot the top 100 molecules based on the number of
  # citations
  tabViz_plot_table <- reactive({
    filtered_table() %>%
      group_by(Molecule, Timepoint) %>%
      summarize(count = n(), .groups = "drop") %>%
      arrange(desc(count)) %>%
      mutate(Molecule = fct_inorder(Molecule)) %>%
      drop_na(Molecule, Timepoint) %>%
      head(50)
  })


  # * 3.d.2 Plotly --------------------------------------------------------

  # Make the plot via plotly, primarily to make use of the "hover text" feature.
  # Adding the `customdata` variable here allows us to access this information
  # when a user clicks on a bar, in addition to the x value (gene/protein name).
  output$plot_object <- renderPlotly({
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


  # Grab the molecule and time point the user clicks on in a reactive value,
  # which can then dynamically be supplied to the DT render, and the download
  # button render.
  clicked_molecule_table <- reactive({
    d <- event_data("plotly_click", priority = "event")
    if (is.null(d)) {
      return(NULL)
    } else {
      plot_molecules_hyper() %>%
        filter(Molecule == d$x, Timepoint == d$customdata)
    }
  })


  # Grab the molecule name and time point for later use in naming the the
  # download file
  clicked_molecule_info <- reactive(
    list(
      molecule = unique(clicked_molecule_table()$Molecule),
      timepoint = str_replace_all(
        unique(clicked_molecule_table()$Timepoint),
        pattern = " ",
        replacement = "_"
      )
    )
  )


  # * 3.d.3 Render table --------------------------------------------------

  # Note that we are rendering the link-enabled table, not the table that is
  # used to create the plot. Again we employ some JS to automatically trim
  # strings and provide the full text as a tooltip on hover.
  output$click <- DT::renderDataTable(
    clicked_molecule_table(),
    rownames  = FALSE,
    escape    = FALSE,
    selection = "none",
    options   = list(
      dom     = "tip",
      paging  = FALSE,
      scrollX = TRUE
    )
  )

  output$testclick <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) {
      "Click to see the values:"
    } else {
      d
    }
  })

  # Rendering the plot and surrounding UI. Uncomment the `verbatimTextOutput`
  # line to see the information from the `plotly_click` event.
  output$plot_panel <- renderUI({
    tagList(
      plotlyOutput("plot_object", inline = TRUE, height = "300px"),
      # verbatimTextOutput("testclick"),
      h4("Click a bar to see all entries for that molecule & timepoint:")
    )
  })

  output$table_click_panel <- renderUI({
    tagList(
      div(
        DT::dataTableOutput("click"),
        style = "font-size: 13px"
      ),
      br()
    )
  })


  # * 3.d.4 Download clicked table ----------------------------------------

  # Download handler for the table generated when a user clicks on one of the
  # bars in the plot. Fed into the `renderUI()` chunk below so it only appears
  # when there is data to download.
  output$clicked_table_download_handler <- downloadHandler(
    filename = paste0(
      "septisearch_download_",
      clicked_molecule_info()[["molecule"]], "_",
      clicked_molecule_info()[["timepoint"]], ".txt"
    ),
    content = function(file) {
      write_tsv(
        x    = clicked_molecule_table(),
        file = filename
      )
    }
  )

  # Render the UI for the download (just the button and an "hr").
  output$click_table_download_button <- renderUI({
    if (is.null(clicked_molecule_table())) {
      return(NULL)
    } else {
      return(tagList(
        p(strong("Download the table for the chosen molecule:")),
        downloadButton(
          outputId = "clicked_table_download_handler",
          label    = "Download plot table",
          class    = "btn btn-success"
        ),
        hr()
      ))
    }
  })


  # Allow the user to "reset" the page to its original/default state, using both
  # the default shinyjs function and our own JS, sourced from "www/functions.js"
  observeEvent(input$tabViz_reset, {
    shinyjs::reset(id = "tabViz_sidebar", asis = FALSE)
    js$resetClick()
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


  # Define some reactive values to be used later on
  tabEnrich_input_genes <- reactiveVal()
  tabEnrich_input_genes_table <- reactiveVal()
  tabEnrich_test_result <- reactiveVal()


  # * 3.e.1 Parse molecule input ------------------------------------------

  # Note that input ID's need to be coerced to character to prevent mapping
  # issues when using Entrez IDs
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


  # Enable the submission button once we have some input from the user
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

    # Create notification to say the tests are running
    showNotification(
      ui = paste0(
        "Mapping and testing ",
        nrow(tabEnrich_input_genes_table()),
        " ",
        attr(tabEnrich_mapped_genes(), "id_type"),
        " input genes, please wait..."
      ),
      type     = "message",
      duration = NULL,
      id       = "tabEnrich_please_wait"
    )

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
    output$result_reactomepa <- renderDataTable(
      tabEnrich_test_result_clean()$ReactomePA,
      rownames = FALSE,
      options  = list(
        dom = "tip"
      )
    )
    output$result_reactomepa_ui <-renderUI(
      tagList(
        h3("ReactomePA:"),
        dataTableOutput("result_reactomepa"),
        hr()
      )
    )


    ### EnrichR
    output$result_enrichr <- renderDataTable(
      tabEnrich_test_result_clean()$EnrichR,
      rownames = FALSE,
      options  = list(
        dom = "tip"
      )
    )
    output$result_enrichr_ui <- renderUI(
      tagList(
        h3("EnrichR:"),
        dataTableOutput("result_enrichr"),
        br()
      )
    )
  })

  # Once the mapping is finished, remove the notification message
  observeEvent(input$tabEnrich_submit_button, {
    if ( !is.null(tabEnrich_test_result_clean()$ReactomePA) ) {
      removeNotification("tabEnrich_please_wait")
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
        tags$label("Mapping Results"),
        make_success_message(
          mapped_data = isolate(tabEnrich_mapped_genes())
        ),
        tags$label("Enrichment Results"),
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
