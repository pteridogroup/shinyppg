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

#' Server logic to filter PPG by taxonomic groups
#'
#' Internal function
#'
#' @import shiny
#' @param id Character vector of length 1; the ID for this module.
#' @param ppg Reactive dataframe (tibble) of PPG data
#' @returns Reactive list of filter criteria
#' @autoglobal
#' @noRd
subset_server <- function(id, ppg) {
  # Check args
  stopifnot(is.reactive(ppg))

  moduleServer(id, function(input, output, session) {
    # Reactive to store filter state
    filter_criteria <- reactiveVal(list(
      order = NULL,
      family = NULL,
      genus = NULL
    ))

    # Initial state: not filtered
    is_filtered <- reactiveVal(FALSE)
    shinyjs::disable("reset")

    order_select <- autocomplete_server(
      id = "order",
      ppg = ppg,
      rows_selected = NULL,
      placeholder = "Select order",
      col_select = NULL,
      fill_name = FALSE,
      switch_off = is_filtered,
      taxonRank == "order"
    )
    family_select <- autocomplete_server(
      id = "family",
      ppg = ppg,
      rows_selected = NULL,
      placeholder = "Select family",
      col_select = NULL,
      fill_name = FALSE,
      switch_off = is_filtered,
      taxonRank == "family"
    )
    genus_select <- autocomplete_server(
      id = "genus",
      ppg = ppg,
      rows_selected = NULL,
      placeholder = "Select genus",
      col_select = NULL,
      fill_name = FALSE,
      switch_off = is_filtered,
      taxonRank == "genus"
    )

    # Set filter criteria
    observeEvent(input$subset, {
      taxa_selected <- list(
        order = order_select(),
        family = family_select(),
        genus = genus_select()
      )
      # Check if any filter is applied
      has_filter <- any(lengths(taxa_selected) > 0)

      if (has_filter && !is_filtered()) {
        filter_criteria(taxa_selected)
        shinyjs::disable("subset")
        shinyjs::enable("reset")
        is_filtered(TRUE)
      }
    })

    # Reset filter
    observeEvent(input$reset, {
      if (is_filtered()) {
        filter_criteria(list(order = NULL, family = NULL, genus = NULL))
        shinyjs::enable("subset")
        shinyjs::disable("reset")
        is_filtered(FALSE)
      }
    })

    return(filter_criteria)
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
    # Get filter criteria from subset module
    filter_criteria <- subset_server(id = "subset", ppg = ppg)
    # Display filtered data
    rows_selected <- display_ppg_server(
      "display_ppg",
      ppg,
      filter_criteria = filter_criteria
    )
  }
  shinyApp(ui, server)
}
