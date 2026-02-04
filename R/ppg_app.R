#' PPG app
#'
#' Run a Shiny app to view the Pteridophyte Phylogeny Group (PPG)
#' taxonomic database.
#'
#' @param data_source Where to load data from. Options are "local" (built-in
#'   sample data, default), "remote" (download from GitHub), or "repo"
#'   (read from local repository at /home/shiny/ppg).
#' @import shiny
#' @returns An object that represents the app. This function is normally called
#' for its side-effect of starting a Shiny app that can be accessed in a web
#' browser window.
#' @export
#' @examples
#' if (interactive()) {
#'   # Use sample data (fast, good for testing)
#'   ppg_app()
#'
#'   # Use full dataset from GitHub
#'   ppg_app("remote")
#' }
ppg_app <- function(data_source = "local") {
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
      stamp_modified_by = FALSE,
      stamp_modified_by_id = FALSE
    )

    # Load PPG data
    ppg <- reactiveVal(load_data(data_source))

    # Specify UI
    output$main_content <- shiny::renderUI({
      fluidRow(
        column(
          width = 12,
          h3(paste0("PPG v", shinyppg::ppg_version)),
          p("Use the column filters below to browse by taxonomic group."),
          display_ppg_ui("display_ppg")
        )
      )
    })

    # Display ppg table
    rows_selected <- display_ppg_server("display_ppg", ppg)
  }

  shiny::shinyApp(ui, server)
}
