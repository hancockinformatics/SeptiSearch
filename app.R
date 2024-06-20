# Load packages -----------------------------------------------------------

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
        p(
          class = "mx-auto fs-5 text-muted",
          "Welcome to SeptiSearch! Here you can browse, explore, and download ",
          "curated molecular results derived from transcriptomic sepsis ",
          "studies. The database and app currently catalogs over 20,000 ",
          "unique molecules from more than 70 publications."
        ),

        p(
          class = "mx-auto fs-5 text-muted",
          HTML(
            "To get started, select one of the tabs at the top of the page,
            described below:

            <ul class='fs-5 text-muted'>

            <li><em>Explore the Database</em> makes it easy to search all
            curated gene sets by keyword or molecule, filter with select
            criteria, and view all the molecules in a gene set</li>

            <li><em>Visualize the Database</em> plots the most common molecules,
            and provides filters to see which occur most frequently based on a
            number of attributes</li>

            <li><em>Perform Pathway Enrichment</em> allows users to upload their
            own list of genes, or use one of the curated sepsis gene sets,
            to test for enriched pathways/terms using
            <a href='https://bioconductor.org/packages/ReactomePA/'>ReactomePA
            </a> and <a href='https://maayanlab.cloud/Enrichr/'>enrichR</a></li>

            <li><em>Test for Enriched Sepsis Gene Sets</em> can be used to test
            your own expression data (e.g. counts from RNA-Seq) for
            dysregulation of the curated sepsis gene sets</li>

            </ul>"
          )
        ),

        p(
          class = "mx-auto fs-5 text-muted",
          HTML(
            "SeptiSearch was created by Travis Blimkie, Jasmine Tam & Arjun
            Baghela from the <a href='https://cmdr.ubc.ca/bobh/'>Hancock Lab</a>
            at the University of British Columbia, and is published in Frontiers
            in Immunology
            (<a href='https://doi.org/10.3389/fimmu.2023.1135859'>doi:
            10.3389/fimmu.2023.1135859</a>). If you'd like to learn more, or to
            report bugs or issues, click the button below to visit our
            <em>About</em> page."
          )
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

  nav_panel(
    id = "explore_tab",
    icon = icon("table"),
    title = "Explore the database",

    layout_sidebar(
      sidebar = sidebar()
    )
  ),

  nav_panel(
    id = "viz_tab",
    icon = icon("chart-bar"),
    title = "Visualize the database",

    layout_sidebar(
      sidebar = sidebar()
    )
  ),

  nav_panel(
    id = "enrich_tab",
    icon = icon("calculator"),
    title = "Perform pathway enrichment",

    layout_sidebar(
      sidebar = sidebar()
    )
  ),

  nav_panel(
    id = "gsva_tab",
    icon = icon("laptop-code"),
    title = "Test for enriched sepsis gene sets",

    layout_sidebar(
      sidebar = sidebar()
    )
  ),

  nav_panel(
    id = "about_tab",
    icon = icon("circle-info"),
    title = "About",

    div(h1("About"))
  )
)

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
