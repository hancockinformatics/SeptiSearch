
# 0. To-Do ----------------------------------------------------------------

# Fix shinyjs reset button for "Explore Data by Study" tab - need to somehow get
# DT to reset the tables...?




# 1. Load packages, function and data -------------------------------------

library(shiny)
library(shinyjs)
library(DT)
library(plotly)
library(tidyverse)

import::from("functions/functions.R", .all = TRUE)

current_data <-
  list.files("data", "fulldata_[0-9]{8}\\.txt", full.names = TRUE) %>%
  dplyr::last()

full_data <- read_tsv(current_data, col_types = cols()) %>%
  mutate(PMID = as.character(PMID))

message(paste0("\nUsing data file: '", current_data, "'"))

# Create JS function that allows long strings in DT tables to be trimmed, with
# the full content displayed as a tooltip on hover
DT_ellipsis_render <- JS(
  "function(data, type, row, meta) {",
  "if ( type !== 'display' ) {",
  "return data;",
  "}",
  "if ( typeof data !== 'number' && typeof data !== 'string' ) {",
  "return data;",
  "}",
  "data = data.toString();",
  "if ( data.length < 50 ) {",
  "return data;",
  "}",
  "var shortened = data.substr(0, 49);",
  "shortened = shortened.replace(/,?\\s([^\\s]*)$/, '');",
  "return '<span class=\"ellipsis\" title=\"'+data+'\">'+",
  "shortened+'&#8230;</span>';",
  "}"
)




# 2. UI -------------------------------------------------------------------

ui <- fluidPage(

  # Select the Bootswatch3 theme "Readable": https://bootswatch.com/3/readable
  theme = "css/readablebootstrap.css",

  # Link to custom CSS tweaks, JS helper functions, and use of favicons
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
    id = "navbar",
    position = "fixed-top",

    # Determines the page title in the user's browser
    windowTitle = "SeptiSearch",

    # Custom nested divs for the title, so we can have the Github logo on the
    # right side of the navbar, linking to the Github page. See "user.css" for
    # the custom changes being applied to the image.
    title = div(

      # Actual title displayed on the left side of the navbar
      tags$b("SeptiSearch"),
      # htmltools::HTML(
      #   "<img src='septisearch.svg' height='45' alt='SeptiSearch'>"
      # ),

      # Div containing the github logo for the right side of the navbar
      tags$div(
        id = "img-id",
        htmltools::HTML(paste0(
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

      tags$div(class = "jumbotron",

        h1("Welcome"),
        tags$hr(),

        tags$div(class = "logoWrapper",

          tags$p(
            "Welcome to SeptiSearch! Here you can browse, explore, and ",
            "download curated molecular results derived from sepsis studies. ",
            "The app currently allows access to over 14,500 unique molecules ",
            "from more than 70 different publications."
          ),
          tags$p(HTML(
            "To get started, select one of the tabs above. ",
            "<span style='color:#4582ec;'><b>Explore Data in a ",
            "Table</b></span> will let you browse our entire collection, with ",
            "the ability to filter the data in various ways and search for ",
            "specific molecules. <span style='color:#4582ec;'><b>Explore Data ",
            "by Study</b></span> is the easiest way to explore our collection ",
            "based on the publications we've curated. <span style=",
            "'color:#4582ec;'><b>Visualize Molecule Occurence</b></span> ",
            "displays the most cited molecules in our dataset, and allows ",
            "easy viewing of all entries for any molecule of interest."
          )),
          tags$p(HTML(
            "If you'd like to know more about <span style='color:#4582ec;'>",
            "<b>SeptiSearch</b></span>, or find where to report bugs or ",
            "issues, click the button below to visit our ",
            "<span style='color:#4582ec;'><b>About</b></span> page."
          )),

          tags$br(),

          # Provide a direct link to the "About" page
          actionButton(
            inputId = "learn_more",
            label   = "Learn more",
            class   = "btn btn-primary btn-lg"
          )
        )
      ),

      # Separate div to include the lab logo in the bottom-left corner
      tags$div(
        style = "position:fixed; bottom:0; padding-bottom:10px",
        htmltools::HTML(paste0(
          "<a href='http://cmdr.ubc.ca/bobh/'> ",
          "<img src = 'hancock-lab-logo.svg'> </a>"
        ))
      )
    ),



    # * 2.b Explore Data in a Table ---------------------------------------

    tabPanel(
      value = "table_tab",
      icon  = icon("table"),
      title = "Explore Data in a Table",

      sidebarLayout(
        sidebarPanel = sidebarPanel(
          id    = "tab1_sidebar",

          # Making the sidebarPanel a bit narrower (default is 4) to accommodate
          # our table. Note this plus the width of the main panel must equal 12.
          width = 3,

          checkboxGroupInput(
            inputId  = "tab1_molecule_type_input",
            label    = tags$div("Refine the data by molecule type"),
            choices  = unique(full_data$`Molecule Type`)
          ),

          # Area for the user to input their own genes to filter the data
          textAreaInput(
            inputId     = "pasted_molecules",
            label       = "Search for specific molecules",
            placeholder = "One per line...",
            height      = 100
          ),

          # Input for the user to search article titles
          textAreaInput(
            inputId     = "title_search",
            label       = "Search article titles",
            placeholder = "Enter terms here...",
            height      = 41,
            resize      = "none",
          ),

          # Filter for PMID
          textAreaInput(
            inputId     = "user_pmid",
            label       = "Filter for a particular PMID",
            placeholder = "E.g. 32788292",
            height      = 41,
            resize      = "none"
          ),

          tags$hr(),

          # UI for the "full" download button
          tags$p(tags$b(
            "Download the current table (tab-delimited):"
          )),

          downloadButton(
            outputId = "full_table_download_handler",
            # style    = "width: 170px",
            label    = "Download the data",
            class    = "btn-primary"
          ),

          # UI for the "slimmed" download button
          # tags$br(),
          # tags$br(),
          # downloadButton(
          #   outputId = "slim_table_download_handler",
          #   # style    = "width: 170px",
          #   label    = "Download a slimmed table",
          #   class    = "btn-primary"
          # ),

          tags$hr(),

          # Reset button for the tab (from shinyjs)
          actionButton(
            class   = "btn-info",
            style   = "width: 170px",
            inputId = "tab1_reset",
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
          id = "by_study_tab",
          width = 3,

          checkboxGroupInput(
            inputId  = "by_study_molecule_type_input",
            label    = tags$div("Refine the data by molecule type"),
            choices  = unique(full_data$`Molecule Type`)
          ),

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

          uiOutput("clicked_study_download_button"),

          # Reset button for the tab (from shinyjs). Commented out for now
          # because it doesn't work...
          # tags$hr(),
          # actionButton(
          #   class   = "btn-info",
          #   style   = "width: 170px",
          #   inputId = "by_study_reset",
          #   icon    = icon("undo"),
          #   label   = "Restore defaults"
          # )
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
          id    = "tab2_sidebar",
          width = 3,

          # Input molecule type
          checkboxGroupInput(
            inputId  = "tab2_molecule_type_input",
            label    = tags$div(
              "Refine the data by molecule type",
              style = "font-size: 110%"
            ),
            choices  = unique(full_data$`Molecule Type`)
          ),

          tags$div(
            tags$p(tags$b("Use the fields below to filter the data:")),
            style = "font-size: 110%"
          ),

          # Input platform
          selectInput(
            inputId  = "platform",
            label    = "Platform",
            choices  = not_NA(unique(full_data$Platform)),
            multiple = TRUE
          ),

          # Input tissue type
          selectInput(
            inputId  = "tissue",
            label    = "Tissue",
            choices  = not_NA(unique(full_data$Tissue)),
            multiple = TRUE
          ),

          # Input infection source
          selectInput(
            inputId  = "infection",
            label    = "Infection source",
            choices  = not_NA(unique(full_data$Infection)),
            multiple = TRUE
          ),

          # Input case condition
          selectInput(
            inputId  = "case",
            label    = "Case condition",
            choices  = not_NA(unique(full_data$`Case Condition`)),
            multiple = TRUE
          ),

          # Input control condition
          selectInput(
            inputId  = "control",
            label    = "Control condition",
            choices  = not_NA(unique(full_data$`Control Condition`)),
            multiple = TRUE
          ),

          # Input age group
          selectInput(
            inputId  = "age",
            label    = "Age group",
            choices  = not_NA(unique(full_data$`Age Group`)),
            multiple = TRUE
          ),

          tags$br(),

          # Dynamically render the download button, to download the table (only)
          # when there is something to actually download.
          uiOutput("click_table_download_button"),

          # Reset button for the tab
          actionButton(
            class   = "btn-info",
            style   = "width: 170px",
            inputId = "tab2_reset",
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


    # * 2.e About ---------------------------------------------------------

    tabPanel(
      value = "about_tab",
      icon  = icon("info-circle"),
      title = "About",

      tags$div(
        class = "jumbotron",

        h1("About"),
        tags$hr(),

        tags$div(
          class = "logoWrapper",
          tags$p(
            "SeptiSearch was created by Travis Blimkie, Jasmine Tam & Arjun ",
            "Baghela from the ",
            tags$a(
              "REW Hancock Lab",
              href = "http://cmdr.ubc.ca/bobh/",
              .noWS = c("before", "after")
            ),
            " at the University of British Columbia. All data was manually ",
            "curated from published articles by Jasmine. If you encounter a ",
            "problem or bug with the app, please submit an issue at the ",
            tags$a(
              "Github page",
              href = "https://github.com/hancockinformatics/curation",
              .noWS = c("before", "after"),
            ),
            "."
          ),

          tags$br(),

          tags$p(tags$b("SeptiSearch uses the following R packages:")),

          tags$p(
            tags$dl(

              tags$dt(
                tags$a(href = "https://shiny.rstudio.com/", "Shiny"),
                tags$dd("Create beautiful web apps with R.")
              ),

              tags$dt(
                tags$a(href = "https://deanattali.com/shinyjs/", "ShinyJS"),
                tags$dd("Extend Shiny functionality using JavaScript.")
              ),

              tags$dt(
                tags$a(href = "https://www.tidyverse.org/", "Tidyverse"),
                tags$dd("A suite of packages for data manipulation.")
              ),

              tags$dt(
                tags$a(href = "https://rstudio.github.io/DT/", "DT"),
                tags$dd("An R interface to the DataTables JavaScript library.")
              ),

              tags$dt(
                tags$a(href = "https://plotly.com/r/", "Plotly"),
                tags$dd("Interactive visualizations in R.")
              ),
            )
          )
        )
      ),

      tags$div(
        style = "position: fixed; bottom: 0px; padding-bottom: 10px",
        htmltools::HTML(paste0(
          "<a href='http://cmdr.ubc.ca/bobh/'> ",
          "<img src = 'hancock-lab-logo.svg'> </a>"
        ))
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


  # Simple text search for article titles
  users_title_search <- reactiveVal()
  observeEvent(input$title_search, {
    input$title_search %>% users_title_search()
  }, ignoreInit = TRUE)


  # Filter the table with a specific PMID (currently only supports one PMID at a
  # time)
  user_pmid_filter <- reactiveVal()
  observeEvent(input$user_pmid, {
    input$user_pmid %>%
      str_trim() %>%
      user_pmid_filter()
  }, ignoreInit = TRUE)


  # All the filtering steps make use of the custom `conditional_filter()`
  # function, so we don't need step-wise filtering, while keeping the output
  # reactive.
  # We're using `str_detect(col, paste0(input, collapse = "|"))` for most
  # string-based filters (instead of `==`), so we can easily search for one or
  # more specified inputs without needing special cases.
  table_molecules <- reactive({
    full_data %>% filter(

      # Molecule Type
      conditional_filter(
        length(input$tab1_molecule_type_input) != 0,
        `Molecule Type` %in% input$tab1_molecule_type_input
      ),

      # User search for specific molecules
      conditional_filter(
        !all(is.null(users_molecules()) | users_molecules() == ""),
        str_detect(Molecule, paste0(users_molecules(), collapse = "|"))
      ),

      # User search for words in titles
      conditional_filter(
        !all(is.null(users_title_search()) | users_title_search() == ""),
        str_detect(Title, regex(users_title_search(), ignore_case = TRUE))
      ),

      # Filter on PMID
      conditional_filter(
        !all(is.null(user_pmid_filter()) | user_pmid_filter() == ""),
        PMID == user_pmid_filter()
      )
    )
  })


  # Modify the above filtered table, prior to display, to make PMIDs into links
  table_molecules_hyper <- reactive({
    table_molecules() %>%
      mutate(PMID = case_when(
        !is.na(PMID) ~ paste0(
          "<a target='_blank' href='",
          "https://pubmed.ncbi.nlm.nih.gov/",
          PMID, "'>", PMID, "</a>"
        ),
        TRUE ~ "none"
      )) %>%
      arrange(Author, Molecule)
  })



  # * 3.b.1 Render table --------------------------------------------------

  # Render the above table to the user, with a <br> at the end to give some
  # space. Reduce the font size of the table slightly so we can see more of the
  # data at once. "scrollY" is set to 74% of the current view, so we scroll only
  # the table, and not the page (with a 1080p resolution).
  # We also include some JS so certain columns/strings (set with `target`,
  # zero-indexed) are trimmed if they exceed a certain length. An ellipsis is
  # appended, and hovering over the cell/text shows a tooltip with the whole
  # string.
  output$table_molecules_DT <- DT::renderDataTable(
    table_molecules_hyper(),
    rownames  = FALSE,
    escape    = FALSE,
    selection = "none",
    options   = list(
      dom     = "tip",
      scrollX = TRUE,
      scrollY = "78vh",
      columnDefs = list(list(
        targets = c(1, 6, 11),
        render  = DT_ellipsis_render
        # "function(data, type, row, meta) {",
        # "return type === 'display' && data.length > 50 ?",
        # "'<span title=\"' + data + '\">' + data.substr(0, 50) + ",
        # "'...</span>' : data; }"
      ))
    )
  )

  output$table_molecules_render <- renderUI({
    tagList(
      tags$div(
        DT::dataTableOutput("table_molecules_DT"),
        style = "font-size:13px;"
      ),
      tags$br()
    )
  })


  # Allow the user to download the currently displayed table
  output$full_table_download_handler <- downloadHandler(
    filename = "septisearch_download_full.txt",
    content = function(file) {
      write_delim(table_molecules(), file, delim = "\t")
    }
  )


  # slim_table_molecules <- reactive({
  #   table_molecules() %>%
  #     select(Molecule, `Molecule Type`)
  # })

  # output$slim_table_download_handler <- downloadHandler(
  #   filename = "septisearch_download_slim.txt",
  #   content = function(file) {
  #     write_delim(slim_table_molecules(), file, delim = "\t")
  #   }
  # )


  # Allow the user to "reset" the page to its original/default state
  observeEvent(input$tab1_reset, {
    shinyjs::reset("tab1_sidebar", asis = FALSE)
  })




  # 3.c Explore Data by Study ---------------------------------------------

  by_study_grouped_static_table <- full_data %>%
    select(
      Title,
      Author,
      PMID,
      `Omic Type`,
      `Molecule Type`,
      Molecule
    ) %>%
    group_by(across(c(-Molecule))) %>%
    summarise(`No. Molecules` = n(), .groups = "keep") %>%
    mutate(PMID = case_when(
      !is.na(PMID) ~ paste0(
        "<a target='_blank' href='",
        "https://pubmed.ncbi.nlm.nih.gov/",
        PMID, "'>", PMID, "</a>"
      ),
      TRUE ~ "none"
    ))

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
      # Molecule Type
      conditional_filter(
        length(input$by_study_molecule_type_input) != 0,
        `Molecule Type` %in% input$by_study_molecule_type_input
      ),

      # User search for words in titles
      conditional_filter(
        !all(is.null(by_study_title_search()) | by_study_title_search() == ""),
        str_detect(Title, regex(by_study_title_search(), ignore_case = TRUE))
      ),

      # Filter on PMID
      conditional_filter(
        !all(is.null(by_study_pmid_search()) | by_study_pmid_search() == ""),
        # PMID == by_study_pmid_search()
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
      # scrollY = "50vh"
    )
  )

  output$by_study_grouped_render <- renderUI(
    tagList(
      DT::dataTableOutput("by_study_grouped_DT"),
      tags$hr(),
      tags$h3(
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
      magrittr::extract2(input$by_study_grouped_DT_rows_selected, 1) %>%
      clicked_row_title()

    # The author, used to name the downloaded study-specific table
    by_study_grouped_table() %>%
      magrittr::extract2(input$by_study_grouped_DT_rows_selected, 2) %>%
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
        select(-c(Title, Author, PMID, `Omic Type`))
    }
  })


  # * 3.c.3 Render clicked table ------------------------------------------

    output$by_study_clicked_DT <- DT::renderDataTable(
      by_study_clicked_table(),
      rownames  = FALSE,
      escape    = FALSE,
      selection = "none",
      options   = list(
        dom     = "tip",
        scrollX = TRUE,
        scrollY = "50vh",
        columnDefs = list(list(
          targets = c(2, 3, 7),
          render  = DT_ellipsis_render
        ))
      )
    )

  output$by_study_clicked_render <- renderUI(
    tagList(
      tags$br(),
      # verbatimTextOutput("test_clicked_row_title"),
      DT::dataTableOutput("by_study_clicked_DT"),
      tags$br()
    )
  )

  # Allow the user to "reset" the page to its original/default state
  observeEvent(input$by_study_reset, {
    # selectRows(proxy = dataTableProxy("by_study_grouped_DT"), selected = NULL)

  })


  # * 3.c.4 Download clicked study data -----------------------------------

  output$clicked_study_download_handler <- downloadHandler(
    filename = paste0(
      "septisearch_download_",
      clicked_row_author(),
      ".txt"
    ),
    content = function(file) {
      write_delim(
        by_study_clicked_table(),
        file,
        delim = "\t"
      )
    }
  )


  # Render the UI for the download (just the button and an "hr").
  output$clicked_study_download_button <- renderUI({
    if (is.null(by_study_clicked_table())) {
      return(NULL)
    } else {
      return(tagList(
        tags$br(),
        tags$p(HTML("<b>Download the table for the chosen study:</b>")),
        downloadButton(
          outputId = "clicked_study_download_handler",
          label    = "Download study table",
          class    = "btn-primary"
        )
      ))
    }
  })




  # 3.d Visualize Molecule Occurrence -------------------------------------

  # First, we need to sanitize some of our inputs. These two (tissue and case
  # condition) can contain a "+", which if we do nothing is interpreted as a
  # special character by `str_detect()`. So we need to replace the "+" with a
  # "\\+" to escape the special character. We use `observeEvent()` and  store
  # these in a reactiveVal so they can update on input change.
  input_tissue <- reactiveVal()
  observeEvent(input$tissue, {
    input$tissue %>%
      str_replace(., fixed("+"), "\\+") %>%
      input_tissue()
  }, ignoreInit = TRUE)

  input_case <- reactiveVal()
  observeEvent(input$case, {
    input$case %>%
      str_replace(., fixed("+"), "\\+") %>%
      input_case()
  }, ignoreInit = TRUE)


  # All the filtering steps make use of the custom `conditional_filter()`
  # function, so we don't need step-wise filtering, while keeping it reactive.
  # We're using `str_detect(col, paste0(input, collapse = "|"))` for
  # string-based filters, so we can easily search for one or more specified
  # inputs.
  filtered_table <- reactive({
    full_data %>% filter(

      # Molecule type
      conditional_filter(
        length(input$tab2_molecule_type_input) != 0,
        `Molecule Type` %in% input$tab2_molecule_type_input
      ),

      # Platform
      conditional_filter(
        length(input$platform) != 0,
        str_detect(Platform, paste(input$platform, collapse = "|"))
      ),

      # Tissue
      conditional_filter(
        length(input$tissue) != 0,
        str_detect(Tissue, paste(input_tissue(), collapse = "|"))
      ),

      # Infection
      conditional_filter(
        length(input$infection) != 0,
        str_detect(Infection, paste(input$infection, collapse = "|"))
      ),

      # Case Condition
      conditional_filter(
        length(input$case) != 0,
        str_detect(`Case Condition`, paste(input_case(), collapse = "|"))
      ),

      # Control Condition
      conditional_filter(
        length(input$control) != 0,
        str_detect(`Control Condition`, paste(input$control, collapse = "|"))
      ),

      # Age Group
      conditional_filter(
        length(input$age) != 0,
        str_detect(`Age Group`, paste(input$age, collapse = "|"))
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
  tab2_plot_table <- reactive({
    filtered_table() %>%
      group_by(Molecule, Timepoint) %>%
      summarize(count = n(), .groups = "drop") %>%
      arrange(desc(count)) %>%
      mutate(Molecule = fct_inorder(Molecule)) %>%
      drop_na(Molecule, Timepoint) %>%
      head(100)
  })



  # * 3.d.1 Plotly --------------------------------------------------------

  # Make the plot via plotly, primarily to make use of the "hover text" feature.
  # Adding the `customdata` variable here allows us to access this information
  # when a user clicks on a bar, in addition to the x value (gene/protein name).
  output$plot_object <- renderPlotly({
    plot_ly(
      data       = tab2_plot_table(),
      x          = ~Molecule,
      y          = ~count,
      color      = ~Timepoint,
      customdata = tab2_plot_table()$Timepoint,
      type       = "bar",
      hoverinfo  = "text",
      text       = ~paste0(
        "<b>", Molecule, ":</b> ", count, "<br>"
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



  # * 3.d.2 Render table --------------------------------------------------

  # Note that we are rendering the link-enabled table, not the table that is
  # used to create the plot. Again we employ some JS to automatically trim
  # strings and provide the full text as a tooltip on hover.
  output$click <- DT::renderDataTable(
    clicked_molecule_table(),
    rownames  = FALSE,
    escape    = FALSE,
    selection = "none",
    options   = list(
      dom     = "ti",
      paging  = FALSE,
      scrollX = TRUE,
      scrollY = "50vh",
      columnDefs = list(list(
        targets = c(1, 6, 7),
        render  = DT_ellipsis_render
      ))
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
      tags$h4("Click a bar to see all entries for that molecule & timepoint:")
    )
  })

  output$table_click_panel <- renderUI({
    tagList(
      tags$div(
        DT::dataTableOutput("click"),
        style = "font-size: 12px"
      )
    )
  })


  # Download handler for the table generated when a user clicks on one of the
  # bars in the plot. Fed into the `renderUI()` chunk below so it only appears
  # when there is data to download.
  output$clicked_table_download_handler <- downloadHandler(
    filename = paste0(
      "septisearch_download_",
      clicked_molecule_info()[["molecule"]],
      "_",
      clicked_molecule_info()[["timepoint"]],
      ".txt"
    ),
    content = function(file) {
      write_delim(
        clicked_molecule_table(),
        file,
        delim = "\t"
      )
    }
  )


  # Render the UI for the download (just the button and an "hr").
  output$click_table_download_button <- renderUI({
    if (is.null(clicked_molecule_table())) {
      return(NULL)
    } else {
      return(tagList(
        tags$p(HTML("<b>Download the table for the chosen molecule:</b>")),
        downloadButton(
          outputId = "clicked_table_download_handler",
          label    = "Download plot table",
          class    = "btn-primary"
        ),
        tags$hr()
      ))
    }
  })


  # Allow the user to "reset" the page to its original/default state, using both
  # the default shinyjs function and our own JS, sourced from "www/functions.js"
  observeEvent(input$tab2_reset, {
    shinyjs::reset(id = "tab2_sidebar", asis = FALSE)
    js$resetClick()
  })
}




# 4. Run the app ----------------------------------------------------------

shinyApp(ui, server)
