
### TODO
# -------------------------------------------------------------------------
# Rewrite using navbarPage and tabs
# Change choices for data filtering to be programmatic/reactive
# Change filtering to be not be step-wise (?)
# Fix conditionals for data filtering steps


# Load packages -----------------------------------------------------------

library(shiny)
library(tidyverse)

import::from("functions/theme_main.R", theme_main)


# Load data ---------------------------------------------------------------

full_data <- read_tsv("data/fulldata_20201021.txt", col_types = cols())

unique_molecules <- unique(full_data$Molecule)


# Define shiny UI ---------------------------------------------------------

ui <- fluidPage(

  titlePanel("Biomarker Signatures of Sepsis"),

  sidebarLayout(

    sidebarPanel(

      conditionalPanel(
        condition = "input.tabselected == 1",
        helpText("Hello")
      ),

      conditionalPanel(
        condition = "input.tabselected == 2",
        helpText("Input the desired experimental parameters:"),


        # Input omics type
        radioButtons(
          inputId  = "omics",
          label    = "Molecule type:",
          choices  = c("All", "Gene", "Metabolite", "Non-coding RNA", "HERV"),
          selected = "All"
        ),


        # Input platform
        selectInput(
          inputId = "platform",
          label   = "Platform:",
          choices = c(
            "All",
            "10x Genomics Chromium Single Cell 3' V2",
            "600 MHz Bruker Ultrashield Plus NMR spectrometer",
            "Affymetrix Human Exon 1.0 ST Array",
            "Affymetrix Human Gene 1.0 ST Array",
            "Affymetrix Human Genome U133 Plus 2.0 Array",
            "Affymetrix Human Genome U133A Plus 2.0 Array",
            "Affymetrix Human Genome U219 Array",
            "Affymetrix Human Genome U95 V2 Array",
            "Agilent Human Gene Expression 4x44K V2 Microarray",
            "Agilent Human lncRNA Microarray V4.0",
            "Agilent Microarray Scanner",
            "Agilent SurePrint G3 human GE 8 x 60K V2",
            "Agilent-014850 Whole Human Genome Microarray",
            "Agilent-072363 SurePrint G3 Human GE V3 8x60K Microarray 039494",
            "Arraystar Human lncRNA Microarray V3.0",
            "Bio-Rad Bio-Plex 200 system",
            "Bio-Rad Human Cytokine Standard 27-Plex Assays panel",
            "Bruker AVANCE 600 MHz spectrometer",
            "Bruker AVANCE-II 600 MHz spectrometer",
            "Codelink 55K Human Array",
            "Custom Affymetrix HERV-V3 GeneChip microarray",
            "Custom Affymetrix Human Transcriptome Array",
            "DiaSorin Liason",
            "FACSCanto flow cytometer",
            "GF Healthcare/Amersham Biosciences CodeLink UniSet Human I Bioarray",
            "Illumina HiSeq",
            "Illumina HiSeq 2000",
            "Illumina HiSeq 2500 sequencer",
            "Illumina Human Ref-8 V2.0 expression beadchip",
            "Illumina HumanHT-12 V3.0 Expression BeadChip",
            "Illumina HumanHT-12 V4.0 expression beadchip",
            "Illumina NextSeq 500",
            "Illumina NovaSeq6000",
            "miRCURYLNA microRNA Array",
            "NanoString nCounter",
            "Nanostring nCounter Human Immunology V2",
            "NHICU Human 19K V1.0",
            "Roche Diagnostics Cobra e602",
            "Siemens Immulite 1000",
            "Tecan HS Pro 4800"
          ),
          selected = "All",
          multiple = TRUE
        ),


        # Input tissue type
        selectInput(
          inputId = "tissue",
          label   = "Tissue type:",
          choices = c(
            "All",
            "A549 cells",
            "B cells",
            "Calu-3 cells",
            "CD14+ monocytes",
            "CD16+ monocytes",
            "CD4+ T cells",
            "CD8+ T cells",
            "Dendritic cells",
            "HUVEC",
            "NHBE cells",
            "NK cells",
            "PBMC",
            "Plasma",
            "PMN",
            "Serum",
            "Whole blood"
          ),
          selected = "All",
          multiple = TRUE
        ),


        # Input infection source
        selectInput(
          inputId = "infection",
          label   = "Infection source:",
          choices = c(
            "All",
            "Bacteria",
            "Burkholderia pseudomallei",
            "CAP",
            "E. coli",
            "Fecal peritontis",
            "Fungi",
            "Gram-negative bacteria",
            "Gram-positive bacteria",
            "LPS",
            "SARS-CoV-2",
            "Staphylococcus aureus",
            "UTI",
            "Virus"
          ),
          selected = "All",
          multiple = TRUE
        ),


        # Input case condition
        selectInput(
          inputId = "case",
          label   = "Case condition:",
          choices = c(
            "All",
            "ALI",
            "ARDS",
            "LPS",
            "SARS-CoV-2",
            "Sepsis",
            "Sepsis non-survivors",
            "Septicemic melioidosis",
            "Severe infection",
            "Severe sepsis",
            "SIRS"
          ),
          selected = "All",
          multiple = TRUE
        ),


        # Input control condition
        selectInput(
          inputId = "control",
          label   = "Control condition:",
          choices = c(
            "All",
            "Before infusion",
            "Healthy",
            "Moderate infection",
            "No fever",
            "Non-septic",
            "Non-severe infection",
            "Non-severe sepsis",
            "PBS",
            "Seasonal respiratory infection",
            "Sepsis",
            "Sepsis survivors",
            "SIRS"
          ),
          selected = "All",
          multiple = TRUE
        ),


        # Input age group
        selectInput(
          inputId  = "age",
          label    = "Age group:",
          choices  = c("All", "Neonate", "Pediatric", "Adult", "Senior"),
          selected = "All",
          multiple = TRUE
        )
      ),

      conditionalPanel(
        condition = "input.tabselected == 3",
        radioButtons(
          inputId  = "omics2",
          label    = "Molecule:",
          choices  = c("All", "Gene", "Metabolite", "Non-coding RNA", "HERV"),
          selected = "All"
        ),
        selectizeInput(
          inputId  = "molecule",
          label    = "Molecule:",
          options  = list(maxOptions = 10977),
          choices  = c("All", as.character(unique_molecules)),
          selected = "All",
          multiple = TRUE
        )
      )
    ),

    mainPanel(
      tabsetPanel(
        id = "tabselected",

        tabPanel(
          title = "About",
          value = 1,
          helpText("Filler text here.")
        ),

        tabPanel(
          title = "By experimental conditions",
          value = 2,
          helpText("Plot:"),
          plotOutput("plot1"),

          helpText("Data table:"),
          dataTableOutput("data1")
        ),

        tabPanel(
          title = "By molecule",
          value = 3,
          dataTableOutput("data2")
        )
      )
    )
  )
)




# Define server logic -----------------------------------------------------

server <- function(input, output) {


  # By conditions, by omics
  filtered_omics <- reactive({
    if (input$omics == "All") {
      full_data
    } else {
      full_data %>%
        filter(Molecule_Type == input$omics)
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
        filter(str_detect(Case_Condition, input$case) == TRUE)
    }
  })

  # By control condition
  filtered_control <- reactive({
    if (input$control == "All") {
      filtered_case()
    } else {
      filtered_case() %>%
        filter(str_detect(Control_Condition, input$control) == TRUE)
    }
  })

  # By age group
  filtered_age <- reactive({
    if (input$age == "All") {
      filtered_control()
    } else {
      filtered_control() %>%
        filter(str_detect(Age_Group, input$age) == TRUE)
    }
  })



  # By molecule
  tab3 <- reactive({
    if (input$omics2 != "All" & input$molecule != "All") {
      filter(
        full_data,
        Molecule_Type == input$omics2 & Molecule == input$molecule
      )

    } else if (input$molecule != "All") {
      filter(full_data, Molecule == input$molecule)

    } else if (input$omics2 != "All" & input$molecule == "All") {
      filter(full_data, Molecule_Type == input$omics2)

    } else {
      full_data
    }
  })

  output$plot1 <- renderPlot({
    filtered_age() %>%
      select(one_of("Molecule", "Timepoint")) %>%
      #filter(Timepoint %in% c("Within 12 hrs", "Within 24 hrs", "Within 48 hrs")) %>%
      mutate(Timepoint = as.character(Timepoint)) %>%
      group_by(Timepoint, Molecule) %>%
      summarize(count = n(), .groups = "keep") %>%
      ungroup() %>%
      arrange(desc(count)) %>%
      head(39) %>% # Picks just the top 39 since that's what fits on the page
      ggplot(., aes(x = reorder(Molecule, -count), y = count, fill = Timepoint)) + # Specify we want to reorder Molecule based on the value of count
        geom_bar(stat = "identity") +
        # scale_fill_distiller(type = "qual", palette = 1) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        theme_main() +
        theme(
          axis.text.x = element_text(angle = -45, hjust = 0)
        ) +
        labs(x = "Molecule", y = "Citations")
  })

  output$data1 <- renderDataTable({
    filtered_age()
  })

  output$data2 <- renderDataTable({
    tab3()
  })

}




# Run the app -------------------------------------------------------------

shinyApp(ui, server)
