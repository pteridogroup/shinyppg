#' UI for subsetting
#'
#' Internal function
#'
#' @import shiny
#' @returns UI
#' @noRd
subset_ui <- function(id) {
  tagList(
    shinyjs::useShinyjs(),
    autocomplete_ui(
      NS(id, "order"),
      col_select = "Order",
      multiple = TRUE,
      help_text = NULL
    ),
    autocomplete_ui(
      NS(id, "family"),
      col_select = "Family",
      multiple = TRUE,
      help_text = NULL
    ),
    autocomplete_ui(
      NS(id, "genus"),
      col_select = "Genus",
      multiple = TRUE,
      help_text = NULL
    ),
    actionButton(NS(id, "subset"), "Subset data"),
    actionButton(NS(id, "reset"), "Reset")
  )
}

#' Server logic to subset PPG to a set of taxonomic groups
#'
#' Internal function
#'
#' @import shiny
#' @param id Character vector of length 1; the ID for this module.
#' @param ppg Reactive dataframe (tibble) of PPG data
#' @param ppg_remaining Reactive dataframe (tibble) of PPG data; data that was
#'   *not* selected for subsetting previously.
#' @returns Server logic
#' @autoglobal
#' @noRd
subset_server <- function(
  id,
  ppg,
  ppg_remaining
) {
  # Check args
  stopifnot(is.reactive(ppg))
  stopifnot(is.reactive(ppg_remaining))

  moduleServer(id, function(input, output, session) {
    # Initial state: not subset, so disable reset button
    is_subset <- reactiveVal(FALSE)
    shinyjs::disable("reset")
    order_select <- autocomplete_server(
      id = "order",
      ppg = ppg,
      rows_selected = NULL,
      placeholder = "Select order",
      col_select = NULL,
      fill_name = FALSE,
      switch_off = is_subset,
      taxonRank == "order"
    )
    family_select <- autocomplete_server(
      id = "family",
      ppg = ppg,
      rows_selected = NULL,
      placeholder = "Select family",
      col_select = NULL,
      fill_name = FALSE,
      switch_off = is_subset,
      taxonRank == "family"
    )
    genus_select <- autocomplete_server(
      id = "genus",
      ppg = ppg,
      rows_selected = NULL,
      placeholder = "Select genus",
      col_select = NULL,
      fill_name = FALSE,
      switch_off = is_subset,
      taxonRank == "genus"
    )
    # Subset taxa
    observeEvent(input$subset, {
      taxa_selected <- unique(
        c(order_select(), family_select(), genus_select())
      )
      if (length(taxa_selected) > 0 && !is_subset()) {
        subsetted <- subset_to_taxon(ppg(), taxa_selected) |>
          dplyr::arrange(scientificName)
        other <- ppg() |>
          dplyr::anti_join(subsetted, by = "taxonID") |>
          # to account for multiple presses of the subset button
          dplyr::bind_rows(ppg_remaining()) |>
          unique()
        # reset patch list
        set_global("global_patch_list", NULL)
        ppg(subsetted)
        ppg_remaining(other)
        shinyjs::disable("subset")
        shinyjs::enable("reset")
        is_subset(TRUE)
      }
    })
    observeEvent(input$reset, {
      if (nrow(ppg_remaining()) > 0 && is_subset()) {
        full <- dplyr::bind_rows(ppg(), ppg_remaining()) |>
          unique() |>
          dplyr::arrange(scientificName)
        # reset patch list
        set_global("global_patch_list", NULL)
        ppg(full)
        shinyjs::enable("subset")
        shinyjs::disable("reset")
        is_subset(FALSE)
      }
    })
  })
}

subset_app <- function() {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        subset_ui("subset")
      ),
      mainPanel(
        display_ppg_ui("display_ppg")
      )
    )
  )
  server <- function(input, output, session) {
    # Load data
    ppg <- load_data_server("ppg")
    credentials <- reactive(list(user_auth = TRUE))
    ppg_remaining <- reactiveVal(data.frame())
    # Other server logic
    rows_selected <- display_ppg_server("display_ppg", ppg)
    subset_server(
      id = "subset",
      ppg = ppg,
      ppg_remaining = ppg_remaining,
      credentials = credentials
    )
  }
  shinyApp(ui, server)
}
