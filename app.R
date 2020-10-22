
### TODO
# -------------------------------------------------------------------------
# Change filtering to be not be step-wise (?).
# Fix conditionals for data filtering steps.
# Major rework of most filters so they only render/update on request from user
#   (i.e. hit the button to make your changes appear).




# Load packages, function and data ----------------------------------------

message("Loading accessory packages...\n")

library(shiny)
library(DT)
library(tidyverse)

message("Loading data and functions...\n")

full_data <- read_tsv("data/fulldata_20201021.txt", col_types = cols())
import::from("functions/theme_main.R", theme_main)

message("Done.\n")



# Start the app! ----------------------------------------------------------

shinyApp(


  # Define shiny UI -------------------------------------------------------

  ui = fluidPage(

    # Link to some custom CSS tweaks
    tags$head(tags$link(
      rel = "stylesheet", type = "text/css", href = "css/user.css"
    )),

    # Select the Bootswatch3 theme "Readable": https://bootswatch.com/3/readable
    theme = "css/readablebootstrap.css",


    ### Begin the navbarPage that servers as the basis for the app
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
            width = 3,

            checkboxGroupInput(
              inputId  = "molecule_type_input",
              label    = "Refine by Molecule Type:",
              choices  = c("Gene", "Metabolite", "Non-coding RNA", "HERV"),
              selected = "All"
            ),

            # selectizeInput(
            #   inputId = "molecule_selection",
            #   label = "Use Specific Molecules:",
            #   choices = NULL,
            #   multiple = TRUE
            # )
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
            width = 3,

            # Input molecule type
            radioButtons(
              inputId  = "omics",
              label    = "Molecule type:",
              choices  = c("All", unique(full_data$`Molecule Type`)),
              selected = "All"
            ),


            # Input platform
            selectInput(
              inputId = "platform",
              label   = "Platform:",
              choices = c("All", unique(full_data$Platform)),
              selected = "All",
              multiple = TRUE
            ),


            # Input tissue type
            selectInput(
              inputId = "tissue",
              label   = "Tissue type:",
              choices = c("All", unique(full_data$Tissue)),
              selected = "All",
              multiple = TRUE
            ),


            # Input infection source
            selectInput(
              inputId = "infection",
              label   = "Infection source:",
              choices = c("All", unique(full_data$Infection)),
              selected = "All",
              multiple = TRUE
            ),


            # Input case condition
            selectInput(
              inputId = "case",
              label   = "Case condition:",
              choices = c("All", unique(full_data$`Case Condition`)),
              selected = "All",
              multiple = TRUE
            ),


            # Input control condition
            selectInput(
              inputId = "control",
              label   = "Control condition:",
              choices = c("All", unique(full_data$`Control Condition`)),
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
            )
          ),

          mainPanel = mainPanel(
            width = 9,
            plotOutput("plot1"),

            uiOutput("data1")
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
        )
      )
    )
  ),


  # Define the server -----------------------------------------------------

  server = function(input, output, session) {


    #############################
    ## Explore Data in a Table ##
    #############################

    # Updating the molecule selection input server-side for better performance
    # updateSelectizeInput(
    #   session  = session,
    #   inputId  = "molecule_selection",
    #   choices  = c("All", unique(full_data$Molecule)),
    #   selected = "All",
    #   server   = TRUE
    # )


    table_molecules <- reactive({
      if (length(input$molecule_type_input) == 0) {
        full_data
      } else {
        filter(
          full_data,
          `Molecule Type` %in% input$molecule_type_input
        )
      }
    })


    # Render the above table to the user, with a <br> at the end to give some
    # space
    output$table_molecules_render <- renderUI({
      tagList(
        DT::renderDataTable({
          table_molecules()
        },
        rownames = FALSE,
        options = list(scrollX = TRUE,
                       scrollY = "100vh",
                       paging  = TRUE)
        ),

        tags$br()
      )
    })




    ###################################
    ## Visualize Molecule Occurrence ##
    ###################################

    # By conditions, by omics
    filtered_omics <- reactive({
      if (input$omics == "All") {
        full_data
      } else {
        full_data %>%
          filter(`Molecule Type` == input$omics)
      }
    })

    # By platform
    filtered_platform <- reactive({
      if (input$platform == "All") {
        filtered_omics()
      } else {
        filtered_omics() %>%
          filter(str_detect(Platform, input$platform) == TRUE)
      }
    })

    # By tissue
    filtered_tissue <- reactive({
      if (input$tissue == "All") {
        filtered_platform()
      } else {
        filtered_platform() %>%
          filter(str_detect(Tissue, input$tissue) == TRUE)
      }
    })

    # By infection source
    filtered_infection <- reactive({
      if (input$infection == "All") {
        filtered_tissue()
      } else {
        filtered_tissue() %>%
          filter(str_detect(Infection, input$infection) == TRUE)
      }
    })

    # By case condition
    filtered_case <- reactive({
      if (input$case == "All") {
        filtered_infection()
      } else {
        filtered_infection() %>%
          filter(str_detect(`Case Condition`, input$case) == TRUE)
      }
    })

    # By control condition
    filtered_control <- reactive({
      if (input$control == "All") {
        filtered_case()
      } else {
        filtered_case() %>%
          filter(str_detect(`Control Condition`, input$control) == TRUE)
      }
    })

    # By age group
    filtered_age <- reactive({
      if (input$age == "All") {
        filtered_control()
      } else {
        filtered_control() %>%
          filter(str_detect(`Age Group`, input$age) == TRUE)
      }
    })


    # Plot the number of citations for each molecule
    output$plot1 <- renderPlot({
      filtered_age() %>%
        select(Molecule, Timepoint) %>%
        mutate(Timepoint = as.character(Timepoint)) %>%
        group_by(Timepoint, Molecule) %>%
        summarize(count = n(), .groups = "keep") %>%
        ungroup() %>%
        arrange(desc(count)) %>%
        mutate(Molecule = fct_inorder(Molecule)) %>%
        head(55) %>% # Picks just the top 39 since that's what fits on the page
        ggplot(., aes(x = Molecule, y = count, fill = Timepoint)) + # Specify we want to reorder Molecule based on the value of count
        geom_bar(stat = "identity") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        theme_main() +
        theme(
          axis.text.x = element_text(angle = -45, hjust = 0)
        ) +
        labs(x = "Molecule", y = "Citations")
    })


    output$data1 <- renderUI({
      tagList(
        DT::renderDataTable({
          filtered_age()
        },
        rownames = FALSE,
        options = list(scrollX = TRUE,
                       scrollY = "100vh",
                       paging  = TRUE)
        ),

        tags$br()
      )
    })
  }
)
