#' PPG app
#'
#' Run a Shiny app to view the Pteridophyte Phylogeny Group (PPG)
#' taxonomic database.
#'
#' @import shiny
#' @returns An object that represents the app. This function is normally called
#' for its side-effect of starting a Shiny app that can be accessed in a web
#' browser window.
#' @export
#' @examples
#' if (interactive()) {
#'   ppg_app()
#' }
ppg_app <- function() {
  ui <- fluidPage(
    titlePanel("PPG Taxonomic Database Viewer"),
    shiny::uiOutput("main_content")
  )

  server <- function(input, output, session) {
    # Define validation settings
    dwctaxon::dct_options(
      check_sci_name = FALSE,
      check_mapping_accepted_status = TRUE,
      check_mapping_parent_accepted = TRUE,
      remap_parent = TRUE,
      valid_tax_status = paste(valid_tax_status, collapse = ", "),
      stamp_modified_by = TRUE,
      stamp_modified_by_id = TRUE,
      extra_cols = c("ipniURL", "modifiedBy", "modifiedByID")
    )

    # Load PPG data (using local sample data for viewer)
    ppg <- reactiveVal(load_data("local"))
    ppg_remaining <- reactiveVal(data.frame())

    # Specify UI
    output$main_content <- shiny::renderUI({
      sidebarLayout(
        sidebarPanel(
          h3("Browse Data"),
          subset_ui("subset"),
          hr(),
          h4("User Guide"),
          p("Use the filters above to browse different taxonomic groups."),
          p("Select rows in the table to view details.")
        ),
        mainPanel(
          h3("PPG Taxonomic Database"),
          display_ppg_ui("display_ppg")
        )
      )
    })

    # Other server logic

    # - initial ppg table display
    rows_selected <- display_ppg_server("display_ppg", ppg)
    # - subset data
    subset_server(
      id = "subset",
      ppg = ppg,
      ppg_remaining = ppg_remaining
    )
  }

  shiny::shinyApp(ui, server)
}
