
# 0. To-Do ----------------------------------------------------------------

# New tab - user uploads genes, run enrichment on them (reactomePA and enrichR;
#   code from Arjun). Display results in a table, maybe a dot plot? Also in this
#   tab - overlap of the user's genes and the various signatures (upset plot)
# Add word cloud to Visualize tab for the top 25 Molecules




# 1. Load packages, data, and functions -----------------------------------

library(shiny)
library(shinyjs)

source("global.R", local = TRUE)

import::from("functions.R", .all = TRUE)


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
          id    = "tabTable_sidebar",

          # Making the sidebarPanel a bit narrower (default is 4) to accommodate
          # our table. Note this plus the width of the main panel must equal 12.
          width = 3,

          tags$p(
            "You can search our database for any molecules using the input ",
            "box below, entering one gene/protein/metabolite per line. ",
            "The fields below to allow you to filter the table based on the ",
            "values in any of the columns displayed."
          ),

          tags$p(HTML(
            "You can also download the currently viewed table using the ",
            "button below. The <b>Restore defaults</b> button will reset any ",
            "searches or filters that have been applied to the data."
          )),
          tags$hr(),

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
          tags$hr(),

          # UI for the download button
          tags$p(tags$b(
            "Download the current table (tab-delimited):"
          )),

          downloadButton(
            outputId = "full_table_download_handler",
            label    = "Download the data",
            class    = "btn-primary"
          ),

          tags$hr(),

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
          id = "by_study_tab",
          width = 3,

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
            inputId = "tabStudy_omic_type_input",
            label   = "Omic Type",
            choices = unique(not_NA(full_data$`Omic Type`)),
            multiple = TRUE
          ),

          uiOutput("clicked_study_download_button"),

          # Reset button for the tab (from shinyjs). Commented out for now
          # because it doesn't work...
          tags$hr(),
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

          # Omic type
          selectInput(
            inputId = "tabViz_omic_type_input",
            label   = "Omic Type",
            choices = unique(not_NA(full_data$`Omic Type`)),
            multiple = TRUE
          ),

          # Molecule type
          selectInput(
            inputId  = "tabViz_molecule_type_input",
            label    = "Molecule Type",
            choices  = unique(full_data$`Molecule Type`),
            multiple = TRUE
          ),

          # Tissue
          selectInput(
            inputId  = "tabViz_tissue_input",
            label    = "Tissue",
            choices  = unique(not_NA(full_data$Tissue)),
            multiple = TRUE
          ),

          # Timepoint
          selectInput(
            inputId  = "tabViz_timepoint_input",
            label    = "Timepoint",
            choices  = unique(not_NA(full_data$Timepoint)),
            multiple = TRUE
          ),

          # Case condition
          selectInput(
            inputId  = "tabViz_case_condition_input",
            label    = "Case Condition",
            choices  = unique(not_NA(full_data$`Case Condition`)),
            multiple = TRUE
          ),

          # Control condition
          selectInput(
            inputId  = "tabViz_control_condition_input",
            label    = "Control Condition",
            choices  = unique(not_NA(full_data$`Control Condition`)),
            multiple = TRUE
          ),

          # Infection
          selectInput(
            inputId  = "tabViz_infection_input",
            label    = "Infection",
            choices  = unique(not_NA(full_data$Infection)),
            multiple = TRUE
          ),

          # Age group
          selectInput(
            inputId  = "tabViz_age_group_input",
            label    = "Age Group",
            choices  = unique(not_NA(full_data$`Age Group`)),
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
  # We're using `str_detect(col, paste0(input, collapse = "|"))` for most
  # string-based filters (instead of `==`), so we can easily search for one or
  # more specified inputs without needing special cases.
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
          "https://pubmed.ncbi.nlm.nih.gov/",
          PMID, "'>", PMID, "</a>"
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
      dom     = "tip",
      scrollX = TRUE,
      pageLength = 20
    )
  )

  # Output the table and the <br> below it. Reduce the font size of the table
  # slightly so we can see more of the data at once
  output$table_molecules_render <- renderUI({
    tagList(
      tags$div(
        DT::dataTableOutput("table_molecules_DT"),
        style = "font-size:13px;"
      ),
      tags$br()
    )
  })



  # * 3.b.3 Download the table ----------------------------------------------

  output$full_table_download_handler <- downloadHandler(
    filename = "septisearch_download_full.txt",
    content = function(file) {
      write_delim(table_molecules(), file, delim = "\t")
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
        select(
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
      tags$br(),
      # verbatimTextOutput("test_clicked_row_title"),
      DT::dataTableOutput("by_study_clicked_DT"),
      tags$br()
    )
  )

  # Allow the user to "reset" the page to its original/default state. All the
  # values need to be reset manually; the shinyjs reset function doesn't seem to
  # apply to DT functions/objects
  observeEvent(input$by_study_reset, {
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
    content = function(file) {
      write_delim(
        by_study_clicked_table(),
        file,
        delim = "\t"
      )
    }
  )


  # Render the UI for the download (just the button and an "br").
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



  # * 3.d.1 Start with filters ----------------------------------------------

  # All the filtering steps make use of the custom `conditional_filter()`
  # function, so we don't need step-wise filtering, while keeping it reactive.
  # We're using `str_detect(col, paste0(input, collapse = "|"))` for
  # string-based filters, so we can easily search for one or more specified
  # inputs.
  filtered_table <- reactive({
    full_data %>% filter(

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
        filter(Molecule == d$x, Timepoint == d$customdata) %>%
        select(
          Molecule,
          PMID,
          `Omic Type`,
          `Molecule Type`,
          Tissue,
          Timepoint,
          `Case Condition`,
          `Control Condition`,
          Infection,
          `Age Group`
        )
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
      tags$h4("Click a bar to see all entries for that molecule & timepoint:")
    )
  })

  output$table_click_panel <- renderUI({
    tagList(
      tags$div(
        DT::dataTableOutput("click"),
        style = "font-size: 13px"
      ),
      tags$br()
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
  observeEvent(input$tabViz_reset, {
    shinyjs::reset(id = "tabViz_sidebar", asis = FALSE)
    js$resetClick()
  })
}




# 4. Run the app ----------------------------------------------------------

shinyApp(ui, server)
