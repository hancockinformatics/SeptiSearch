
### TODO
# -------------------------------------------------------------------------
# Change filtering to be not be step-wise (?).
# Fix conditionals for data filtering steps.
# Major rework of most filters so they only render/update on request from user
#   (i.e. hit the button to make your changes appear).
# Currently, if you supply a molecule to filter the data, then remove the entry
#   from the input field, no data is displayed...?




# Load packages, function and data ----------------------------------------

library(shiny)
library(shinyjs)
library(DT)
library(tidyverse)

full_data <- read_tsv("data/fulldata_20201021.txt", col_types = cols())
import::from("functions/theme_main.R", theme_main)




# Start the app! ----------------------------------------------------------

shinyApp(


  # Define shiny UI -------------------------------------------------------

  ui = fluidPage(

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

            tags$p(
              "The button below will reset the page to its default state:"
            ),

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

            # Input platform
            selectInput(
              inputId  = "platform",
              label    = "Platform:",
              choices  = c("All", unique(full_data$Platform)),
              selected = "All",
              multiple = TRUE
            ),


            # Input tissue type
            selectInput(
              inputId  = "tissue",
              label    = "Tissue type:",
              choices  = c("All", unique(full_data$Tissue)),
              selected = "All",
              multiple = TRUE
            ),


            # Input infection source
            selectInput(
              inputId  = "infection",
              label    = "Infection source:",
              choices  = c("All", unique(full_data$Infection)),
              selected = "All",
              multiple = TRUE
            ),


            # Input case condition
            selectInput(
              inputId  = "case",
              label    = "Case condition:",
              choices  = c("All", unique(full_data$`Case Condition`)),
              selected = "All",
              multiple = TRUE
            ),


            # Input control condition
            selectInput(
              inputId  = "control",
              label    = "Control condition:",
              choices  = c("All", unique(full_data$`Control Condition`)),
              selected = "All",
              multiple = TRUE
            ),


            # Input age group
            selectInput(
              inputId  = "age",
              label    = "Age group:",
              choices  = c("All", unique(full_data$`Age Group`)),
              selected = "All",
              multiple = TRUE
            ),

            tags$hr(),

            tags$p(
              "The button below will reset the page to its default state:"
            ),

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
  ),


  # Define the server -----------------------------------------------------

  server = function(input, output, session) {


    #############################
    ## Explore Data in a Table ##
    #############################

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
            full_data %>% filter(`Molecule Type` %in% input$tab1_molecule_type_input)
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



    # Render the above table to the user, with a <br> at the end to give some
    # space. Also reduce the font size of the table slightly so we can see more
    # of the data at once.
    output$table_molecules_render <- renderUI({
      tagList(
        tags$div(
          DT::renderDataTable({
            table_molecules()
          },
          rownames = FALSE,
          options = list(scrollX = TRUE,
                         scrollY = "100vh",
                         paging  = TRUE)
          ),
          style = "font-size: 13px;"
        ),
        tags$br()
      )
    })

    observeEvent(input$tab1_reset, {
      shinyjs::reset("tab1_sidebar")
    })




    ###################################
    ## Visualize Molecule Occurrence ##
    ###################################

    # By conditions, by omics
    filtered_molecule_type <- reactive({
      if (length(input$tab2_molecule_type_input) == 0) {
        full_data
      } else {
        full_data %>%
          filter(`Molecule Type` %in% input$tab2_molecule_type_input)
      }
    })

    # By platform
    filtered_platform <- reactive({
      if (input$platform == "All") {
        filtered_molecule_type()
      } else {
        filtered_omics() %>%
          filter(str_detect(Platform, input$platform))
      }
    })

    # By tissue
    filtered_tissue <- reactive({
      if (input$tissue == "All") {
        filtered_platform()
      } else {
        filtered_platform() %>%
          filter(str_detect(Tissue, input$tissue))
      }
    })

    # By infection source
    filtered_infection <- reactive({
      if (input$infection == "All") {
        filtered_tissue()
      } else {
        filtered_tissue() %>%
          filter(str_detect(Infection, input$infection))
      }
    })

    # By case condition
    filtered_case <- reactive({
      if (input$case == "All") {
        filtered_infection()
      } else {
        filtered_infection() %>%
          filter(str_detect(`Case Condition`, input$case))
      }
    })

    # By control condition
    filtered_control <- reactive({
      if (input$control == "All") {
        filtered_case()
      } else {
        filtered_case() %>%
          filter(str_detect(`Control Condition`, input$control))
      }
    })

    # By age group
    filtered_age <- reactive({
      if (input$age == "All") {
        filtered_control()
      } else {
        filtered_control() %>%
          filter(str_detect(`Age Group`, input$age))
      }
    })


    # Plot the number of citations for each molecule
    tab2_plot_table <- reactive({
      filtered_age() %>%
        select(Molecule, Timepoint) %>%
        mutate(Timepoint = as.character(Timepoint)) %>%
        group_by(Timepoint, Molecule) %>%
        summarize(count = n(), .groups = "keep") %>%
        ungroup() %>%
        arrange(desc(count)) %>%
        mutate(Molecule = fct_inorder(Molecule)) %>%
        head(50)
    })

    output$plot_object <- renderPlot({
      ggplot(tab2_plot_table(), aes(x = Molecule, y = count, fill = Timepoint)) +
        geom_bar(stat = "identity") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        scale_fill_brewer(type = "qual", palette = "Dark2") +
        theme_main() +
        labs(x = "Molecule", y = "Citations")
    })


    output$plot1 <- renderUI({
      tagList(
        tags$div(
          plotOutput("plot_object"),
        )
      )
    })


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


    observeEvent(input$tab2_reset, {
      shinyjs::reset("tab2_sidebar")
    })
  }
)
