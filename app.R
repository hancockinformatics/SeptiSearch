
### TODO
# -------------------------------------------------------------------------
# Change filtering to be not be step-wise (?).
# Add histogram to summarize citations for all molecules...?
# Make PMIDs into links to article




# Load packages, function and data ----------------------------------------

library(shiny)
library(shinyjs)
library(DT)
library(tidyverse)
library(plotly)

full_data <- read_tsv("data/fulldata_20201103.txt", col_types = cols())

import::from("functions/conditional_filter.R", conditional_filter)




# Define shiny UI -------------------------------------------------------

ui <- fluidPage(

  # Link to CSS tweaks
  tags$head(tags$link(
    rel = "stylesheet", type = "text/css", href = "css/user.css"
  )),

  # Select the Bootswatch3 theme "Readable": https://bootswatch.com/3/readable
  theme = "css/readablebootstrap.css",

  shinyjs::useShinyjs(),


  ### Begin the navbarPage that serves as the basis for the app
  navbarPage(
    id = "navbar",
    title = tags$b("SeptiSearch"),
    windowTitle = "SeptiSearch",


    #############
    ## Welcome ##
    #############
    tabPanel(
      value = "welcome",
      title = div(HTML("Home")),

      tags$div(
        class = "jumbotron",

        h1("Welcome"),

        tags$hr(),

        tags$div(tags$p(
          "Welcome text will go here!"
        ))
      ),

      # Separate div to include the lab logo below the main section
      tags$div(
        style = "position:fixed; bottom:0px; padding-bottom: 10px",
        htmltools::HTML(
          "<a href='http://cmdr.ubc.ca/bobh/'> <img src = 'hancock-lab-logo.svg'> </a>"
        )
      )
    ),


    #############################
    ## Explore Data in a Table ##
    #############################
    tabPanel(
      value = "table",
      title = "Explore Data in a Table",

      sidebarLayout(
        sidebarPanel = sidebarPanel(
          id = "tab1_sidebar",
          width = 3,

          checkboxGroupInput(
            inputId  = "tab1_molecule_type_input",
            label    = "Refine by molecule type:",
            choices  = unique(full_data$`Molecule Type`)
          ),

          tags$hr(),

          tags$p(
            "You can also provide a list of genes or other molecules to ",
            "filter the table (one per line):"
          ),

          textAreaInput(
            inputId     = "pasted_molecules",
            label       = NULL,
            placeholder = "Your genes here...",
            height      = 200
          ),

          tags$hr(),

          actionButton(
            class   = "btn-info",
            inputId = "tab1_reset",
            label   = "Restore Defaults"
          )
        ),

        mainPanel = mainPanel(
          width = 9,
          uiOutput("table_molecules_render")
        )
      )
    ),


    ###################################
    ## Visualize Molecule Occurrence ##
    ###################################
    tabPanel(
      value = "vis",
      title = "Visualize Molecule Occurrence",

      sidebarLayout(
        sidebarPanel = sidebarPanel(
          id = "tab2_sidebar",
          width = 3,

          # Input molecule type
          checkboxGroupInput(
            inputId  = "tab2_molecule_type_input",
            label    = "Refine by molecule type:",
            choices  = unique(full_data$`Molecule Type`)
          ),

          tags$hr(),

          tags$p("You may further filter the data using the fields below:"),

          # Input platform
          selectInput(
            inputId  = "platform",
            label    = "Platform:",
            choices  = unique(full_data$Platform),
            multiple = TRUE
          ),

          # Input tissue type
          selectInput(
            inputId  = "tissue",
            label    = "Tissue type:",
            choices  = unique(full_data$Tissue),
            multiple = TRUE
          ),

          # Input infection source
          selectInput(
            inputId  = "infection",
            label    = "Infection source:",
            choices  = unique(full_data$Infection),
            multiple = TRUE
          ),

          # Input case condition
          selectInput(
            inputId  = "case",
            label    = "Case condition:",
            choices  = unique(full_data$`Case Condition`),
            multiple = TRUE
          ),

          # Input control condition
          selectInput(
            inputId  = "control",
            label    = "Control condition:",
            choices  = unique(full_data$`Control Condition`),
            multiple = TRUE
          ),

          # Input age group
          selectInput(
            inputId  = "age",
            label    = "Age group:",
            choices  = unique(full_data$`Age Group`),
            multiple = TRUE
          ),

          tags$hr(),

          actionButton(
            class   = "btn-info",
            inputId = "tab2_reset",
            label   = "Restore Defaults"
          )
        ),

        mainPanel = mainPanel(
          width = 9,
          uiOutput("plot1")

          # uiOutput("data1")
        )
      )
    ),


    ###########
    ## About ##
    ###########
    tabPanel(
      value = "about",
      title = div(HTML("About")),

      tags$div(
        class = "jumbotron",

        h1("About"),

        tags$hr(),

        tags$div(
          tags$p(
            "Place information about the app here!"
          )
        )
      ),

      tags$div(
        style = "position:fixed; bottom:0px; padding-bottom: 10px",
        htmltools::HTML(
          "<a href='http://cmdr.ubc.ca/bobh/'> <img src = 'hancock-lab-logo.svg'> </a>"
        )
      )
    )
  )
)




# Define the server -----------------------------------------------------

server <- function(input, output, session) {


  #############################
  ## Explore Data in a Table ##
  #############################

  # Set up reactive value to read in molecules from the user
  users_molecules <- reactiveVal()

  observeEvent(input$pasted_molecules, {
    input$pasted_molecules %>%
      str_split(., pattern = " |\n") %>%
      unlist() %>%
      users_molecules()
  }, ignoreNULL = TRUE, ignoreInit = TRUE)


  table_molecules <- reactive({

    # Start with no specification of Molecule Type
    if (length(input$tab1_molecule_type_input) == 0) {

      # Sub-condition for no specified molecules from the user
      if (all(is.null(users_molecules()) | users_molecules() == "")) {
        return(full_data)
        message("Default display.")

        # Sub-condition for user-specified molecules
      } else if (!is.null(users_molecules())) {
        return(full_data %>% filter(Molecule %in% users_molecules()))
        message("All types, user input molecules.")
      }

      # Now we've specified to filter on Molecule Type
    } else if (length(input$tab1_molecule_type_input) != 0) {

      # Sub-condition in which the user hasn't specified any molecules
      if (all(is.null(users_molecules()) | users_molecules() == "")) {
        return(
          full_data %>%
            filter(`Molecule Type` %in% input$tab1_molecule_type_input)
        )

        # Sub-condition for which the user is filtering on Molecule Type and
        # asking for specific molecules
      } else if (!is.null(users_molecules())) {
        return(
          full_data %>% filter(
            Molecule %in% users_molecules(),
            `Molecule Type` %in% input$tab1_molecule_type_input
          )
        )
        message("Specific types, user input molecules.")
      }
    } else {
      return(full_data)
    }
  })

  table_molecules_hyper <- reactive({
    table_molecules() %>%
      rowwise() %>%
      mutate(
        PMID = paste0(
          "<a target='_blank' href='",
          "https://pubmed.ncbi.nlm.nih.gov/",
          PMID, "'>", PMID, "</a>"
        )
      ) %>%
      ungroup()
  })


  # Render the above table to the user, with a <br> at the end to give some
  # space. Also reduce the font size of the table slightly so we can see more
  # of the data at once. "scrollY" is set to 75% of the current view, so we
  # scroll only the table, and not the page.
  output$table_molecules_render <- renderUI({
    tagList(
      tags$div(
        DT::renderDataTable(
          table_molecules_hyper(),
          rownames  = FALSE,
          escape    = FALSE,
          selection = "none",
          options   = list(scrollX = TRUE,
                           scrollY = "75vh",
                           paging  = TRUE)
        ),
        style = "font-size: 12px;"
      ),
      tags$br()
    )
  })

  # Allow the user to "reset" the page to its original/default state
  observeEvent(input$tab1_reset, {
    shinyjs::reset("tab1_sidebar")
  })




  ###################################
  ## Visualize Molecule Occurrence ##
  ###################################

  # All the filtering steps make use of custom `conditional_filter()` function,
  # so we can avoid step-wise filtering and keep it reactive.
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
        str_detect(Tissue, paste(input$tissue, collapse = "|"))
      ),

      # Infection
      conditional_filter(
        length(input$infection) != 0,
        str_detect(Infection, paste(input$infection, collapse = "|"))
      ),

      # Case Condition
      conditional_filter(
        length(input$case) != 0,
        str_detect(`Case Condition`, paste(input$case, collapse = "|"))
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


  # Plot the number of citations for each molecule
  tab2_plot_table <- reactive({
    filtered_table() %>%
      group_by(Timepoint, Molecule) %>%
      summarize(count = n(), .groups = "drop") %>%
      arrange(desc(count)) %>%
      mutate(Molecule = fct_inorder(Molecule)) %>%
      drop_na(Molecule, Timepoint) %>%
      head(100)
  })

  output$plot_object <- renderPlotly({
    plot_ly(
      data = tab2_plot_table(),
      x = ~Molecule,
      y = ~count,
      color = ~Timepoint,
      type = "bar",
      hoverinfo = "text",
      text = ~paste0(
        "<b>", Molecule, ":</b> ", count, "<br>"
      )
    ) %>%
      plotly::style(
        hoverlabel = list(bgcolor = "white", bordercolor = "black")
      ) %>%
      plotly::layout(
        title = "<b>Top 100 molecules based on citations</b>",
        margin = list(t = 50),
        showlegend = TRUE,
        legend = list(
          title = list(text = "<b>Timepoint</b>")
        ),
        xaxis = list(
          title = "",
          zeroline = TRUE,
          showline = TRUE,
          mirror = TRUE
        ),
        yaxis = list(
          title = "<b>Number of Citations</b>",
          tick = "outside",
          ticklen = 3,
          zeroline = TRUE,
          showline = TRUE,
          mirror = TRUE
        ),
        font  = list(
          size = 16,
          color = "black"
        )
      )
  })

  output$plot1 <- renderUI({
    tagList(
      tags$div(
        plotlyOutput("plot_object", height = "50vh")
      )
    )
  })


  ### Old code that rendered a table underneath the above plot. Since its a
  ### bit redundant with the first tab, we're going to replace it with
  ### something else (TBD)...
  # output$data1 <- renderUI({
  #   tagList(
  #     tags$div(
  #       DT::renderDataTable({
  #         filtered_age()
  #       },
  #       rownames = FALSE,
  #       options = list(scrollX = TRUE,
  #                      scrollY = "100vh",
  #                      paging  = TRUE)
  #       ),
  #       style = "font-size: 13px;"
  #     ),
  #
  #     tags$br()
  #   )
  # })


  # Allow the user to "reset" the page to its original/default state
  observeEvent(input$tab2_reset, {
    shinyjs::reset("tab2_sidebar")
  })
}


shinyApp(ui, server, options = list(launch.browser = TRUE))
