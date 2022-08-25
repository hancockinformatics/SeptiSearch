
#' ellipsis_render
#'
#' @param l Desired length of string at which truncation will occur
#'
#' @return JS function which trims strings at desired length, appends an
#'   ellipsis to the end, and gives them hover text containing the full sting.
#'
#' @export
#'
ellipsis_render <- function(l) {
  JS(paste0(
    "function(data, type, row, meta) {",
    "if ( type !== 'display' ) {",
    "return data;",
    "}",
    "if ( typeof data !== 'number' && typeof data !== 'string' ) {",
    "return data;",
    "}",
    "data = data.toString();",
    "if ( data.length < ", l, " ) {",
    "return data;",
    "}",
    "var shortened = data.substr(0, ", l, ");",
    "shortened = shortened.replace(/,?\\s([^\\s]*)$/, '');",
    "return '<span class=\"ellipsis\" title=\"'+data+'\">'+",
    "shortened+'&#8230;</span>';",
    "}"
  ))
}




#' conditional_filter
#'
#' @param condition Test condition; typically we check the length of one of the
#'   Shiny inputs
#' @param success Desired return when `condition` is satisfied; typically a
#'   filter statement based on input in `condition`
#'
#' @return Statement to be used to filter the data, to go inside a
#'   dplyr::filter() call
#'
#' @export
#'
#' @description Simple helper function that allows filtering the data on
#'   multiple parameters, without the need for multiple step-wise filters.
#'
conditional_filter <- function(condition, success) {
  if (condition) {
    return(success)
  } else {
    return(TRUE)
  }
}




#' not_NA
#'
#' @param vector Input vector to be cleaned
#'
#' @return Vector stripped of any NA values.
#'
#' @export
#'
#' @description Simple function to remove NA values from input vector, without
#'   the extra class elements included in na.omit(), which can cause errors in
#'   other functions
#'
not_NA <- function(vector) {
  vector <- vector[!is.na(vector)]
  return(vector)
}




#' create_selectInput
#'
#' @param column_name Name of the column to filter on; used to name the input
#'   and select the appropriate column
#' @param tab Name of the tab into which this UI object is inserted, used to
#'   build the ID
#'
#' @return Shiny `selectInput` object to be used in UI creation
#'
#' @export
#'
create_selectInput <- function(column_name, tab, tooltip) {
  selectInput(
    inputId = paste0(
      tab,
      "_",
      janitor::make_clean_names(column_name),
      "_input"
    ),
    label = HTML(paste0(
      "<p title='",
      tooltip,
      "';>",
      column_name,
      "</p>"
    )),
    choices  = unique(not_NA(full_data_viz_tab[[column_name]])),
    multiple = TRUE
  )
}




#' map_genes
#'
#' @param gene_list Character vector of input genes
#' @param gene_table Tibble of input genes; one column with name "input_genes"
#'
#' @return Table of genes, including the user's input and the other two ID types
#'   used in the enrichment analysis
#'
#' @export
#'
#' @description Detects input ID type, and maps using static biomaRt data.
#'   Assumes input comes from the app, and hence is a data frame of one column
#'   named "input_genes".
#'
map_genes <- function(gene_list, gene_table) {

  message("\n==INFO: Mapping genes:")
  mapped_table <- NULL

  if (str_detect(gene_list[1], "^ENSG[0-9]*$")) {
    message(
      "\tInput was detected as Ensembl (first gene is '", gene_list[1], "')..."
    )

    mapped_table <- biomart_table %>%
      filter(ensembl_gene_id %in% gene_list)
    attr(mapped_table, "id_type") <- "Ensembl"

  } else if (str_detect(gene_list[1], "^[0-9]*$")) {
    message(
      "\tInput was detected as Entrez (first gene is '", gene_list[1], "')..."
    )
    mapped_table <- biomart_table %>%
      filter(entrez_gene_id %in% gene_list)
    attr(mapped_table, "id_type") <- "Entrez"

  } else {
    message(
      "\tInput was detected as HGNC (first gene is '", gene_list[1], "')..."
    )
    mapped_table <- biomart_table %>%
      filter(hgnc_symbol %in% gene_list)
    attr(mapped_table, "id_type") <- "HGNC"
  }

  if ( nrow(mapped_table) == 0 ) {
    message("INFO: Problem with gene mapping; no matching genes were found!")
    mapped_table <- NULL
  }

  message("\tDone.")
  return(mapped_table)
}




#' test_enrichment
#'
#' @param gene_table Data frame or tibble of genes, with standard column names
#' as output by `map_genes()`
#'
#' @return List of length two, for ReactomePA and EnrichR result. Each is a data
#' frame, with the attribute `num_input_genes`.
#'
#' @export
#'
#' @description Performed pathway enrichment using ReactomePA, and enrichR (GO
#'   and MSigDB sources).
#'
test_enrichment <- function(gene_table) {

  message("\n==INFO: Running enrichment tests...")

  # Create safe versions of enrichment functions that return NULL on error
  reactomePA_safe <- possibly(ReactomePA::enrichPathway, otherwise = NULL)
  enrichR_safe    <- possibly(enrichR::enrichr, otherwise = NULL)

  # Clean inputs by removing NA's
  input_entrez <- not_NA(gene_table[["entrez_gene_id"]])
  input_hgnc   <- not_NA(gene_table[["hgnc_symbol"]])


  # ReactomePA
  message("==INFO: Running ReactomePA...")
  reactomePA_result_1 <- suppressPackageStartupMessages(
    reactomePA_safe(gene = input_entrez)
  )

  if (is.null(reactomePA_result_1)) {
    reactomePA_result_2 <- NULL
  } else {
    reactomePA_result_2 <- reactomePA_result_1@result %>%
      filter(p.adjust <= 0.05) %>%
      tibble::as_tibble() %>%
      janitor::clean_names() %>%
      mutate(
        genes_in_pathway = as.numeric(str_remove(bg_ratio, "/[0-9]{1,5}$")),
        gene_ratio = count / genes_in_pathway
      ) %>%
      dplyr::select(
        id,
        description,
        "shared_genes" = count,
        genes_in_pathway,
        gene_ratio,
        pvalue,
        p_adjust
      )

    attr(reactomePA_result_2, "num_input_genes") <- length(input_entrez)
  }

  # enrichR
  message("\n==INFO: Running enrichR...")
  enrichR_result <- enrichR_safe(
    genes = input_hgnc,
    databases = c(
      "MSigDB_Hallmark_2020",
      "GO_Molecular_Function_2021",
      "GO_Cellular_Component_2021",
      "GO_Biological_Process_2021"
    )
  ) %>%
    bind_rows(.id = "database") %>%
    janitor::clean_names() %>%
    filter(adjusted_p_value <= 0.05) %>%
    dplyr::select(database, term, p_value, adjusted_p_value)

  attr(enrichR_result, "num_input_genes") <- length(input_hgnc)

  message("\n==INFO: Done!")
  return(list(
    "ReactomePA" = reactomePA_result_2,
    "enrichR"    = enrichR_result
  ))
}



#' make_success_message
#'
#' @param mapped_data Table of mapped genes
#'
#' @return UI elements for success message
#'
#' @export
#'
#' @description Conditionally creates and returns the appropriate UI element to
#'   be inserted into the sidebar, informing the user about their input and
#'   mapped genes. Placed into a separate function to make the main app code
#'   cleaner.
#'
make_success_message <- function(mapped_data) {

  input_type <- attr(mapped_data, "id_type")

  if (input_type == "Ensembl") {
    tags$p(
      "Success! Your ",
      length(unique(mapped_data$ensembl_gene_id)),
      " unique Ensembl genes were mapped to ",
      length(unique(mapped_data$hgnc_symbol)),
      " HGNC symbols, and ",
      length(unique(mapped_data$entrez_gene_id)),
      " Entrez IDs."
    )

  } else if (input_type == "Entrez") {
    tags$p(
      "Success! Your ",
      length(unique(mapped_data$entrez_gene_id)),
      " unique Entrez genes were mapped to ",
      length(unique(mapped_data$hgnc_symbol)),
      " HGNC symbols, and ",
      length(unique(mapped_data$ensembl_gene_id)),
      " Ensembl IDs."
    )

  } else if (input_type == "HGNC") {
    tags$p(
      "Success! Your ",
      length(unique(mapped_data$hgnc_symbol)),
      " unique HGNC symbols were mapped to ",
      length(unique(mapped_data$entrez_gene_id)),
      " Entrez IDs, and ",
      length(unique(mapped_data$ensembl_gene_id)),
      " Ensembl IDs."
    )

  } else {
    tags$p(
      "It seems there was a problem with mapping your input genes. ",
      "Please check your inputs and try again."
    )
  }
}



#' perform_gsva
#'
#' @param expr Data frame of normalized/transformed expression data. First
#'   column should contain Ensembl gene IDs. Subsequent columns are treated as
#'   samples.
#' @param gene_sets Genes sets which will be tested for enrichment; lists of
#'   Ensembl gene IDs.
#' @param metadata Optional argument to include sample metadata to be added as
#'   sample (column) annotations in the GSVA heatmap.
#'
#' @return A list containing the results as a table (to be downloaded), and the
#'   heatmap (to be rendered).
#'
#' @export
#'
#' @description Takes a list of genes from the user and performs GSVA on the
#'   signatures as the gene sets.
#'
perform_gsva <- function(expr, gene_sets, metadata) {

  # Remove genes with 0 variance across all samples
  expr <- expr[apply(expr, 1, var) != 0, ]

  # Get number of genes in the `expr` matrix which overlap with each `gene_set`
  gene_set_df <- tibble::tibble(
    "Gene Set Name"     = names(gene_sets),
    "No. Genes in Set"  = gene_sets %>% map_dbl(~length(.x)) %>% as.numeric(),
    "No. Shared Genes"  = gene_sets %>%
      map_dbl(~length(intersect(.x, rownames(expr)))) %>%
      as.numeric()
  )

  # Run GSVA
  safe_gsva <- possibly(GSVA::gsva, otherwise = NULL)
  gsva_res <- safe_gsva(
    as.matrix(expr),
    gene_sets,
    method = "gsva",
    kcdf = "Gaussian",
    abs.ranking = FALSE
  )

  # Next chunk is dependent on the above not returning NULL
  if (!is.null(gsva_res)) {

    # Prepare a results matrix
    gsva_res_df <- gsva_res %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Gene Set Name") %>%
      right_join(gene_set_df, by = "Gene Set Name") %>%
      dplyr::select(one_of(colnames(gene_set_df), colnames(expr)))
    gsva_res_df[is.na(gsva_res_df)] <- 0

    # Create a heatmap of the results, hiding sample (column) names if there are
    # more than 30 for readability. Different calls to `pheatmap()` are used
    # depending on the status of the "metadata" input argument.
    heatmap_colours <- colorRampPalette(c("#4575B4", "#FFFFFF", "#D73027"))(50)

    if ( !is.null(metadata) ) {
      suppressWarnings(
        gsva_res_plt <- pheatmap::pheatmap(
          mat            = gsva_res,
          color          = heatmap_colours,
          fontsize       = 14,
          border_color   = "white",
          show_colnames  = ifelse(ncol(expr) <= 30, TRUE, FALSE),
          legend_breaks  = c(-0.5, 0, 0.5, max(gsva_res)),
          legend_labels  = c("-0.5", "0", "0.5", "Enrichment\nScore"),
          main           = "GSVA enrichment scores and annotations",
          angle_col      = 45,
          annotation_col = metadata
        )
      )
    } else {
      suppressWarnings(
        gsva_res_plt <- pheatmap::pheatmap(
          mat           = gsva_res,
          color         = heatmap_colours,
          fontsize      = 14,
          border_color  = "white",
          show_colnames = ifelse(ncol(expr) <= 30, TRUE, FALSE),
          legend_breaks = c(-0.5, 0, 0.5, max(gsva_res)),
          legend_labels = c("-0.5", "0", "0.5", "Enrichment\nScore"),
          main          = "GSVA enrichment scores",
          angle_col     = 45
        )
      )
    }

    return(list(
      "gsva_res_df"  = gsva_res_df,
      "gsva_res_plt" = gsva_res_plt
    ))

  } else {
    return(NULL)
  }
}


