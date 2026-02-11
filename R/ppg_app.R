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
#'   # Use full dataset
#'   ppg_app("full")
#'   # or
#'   ppg_app()
#'
#'   # Use sample data (fast, good for testing)
#'   ppg_app("local")
#' }
ppg_app <- function(data_source = "full") {
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
      tabsetPanel(
        tabPanel(
          "Data Viewer",
          fluidRow(
            column(
              width = 12,
              h3(paste0("PPG v", shinyppg::ppg_version)),
              display_ppg_ui("display_ppg")
            )
          )
        ),
        tabPanel(
          "How to Use",
          fluidRow(
            column(
              width = 10,
              offset = 1,
              h3("How to Use the PPG Taxonomic Database Viewer"),

              h4("Filtering Data"),
              p(
                "At the top of the data table, you will see text boxes for 
                each column. You can type in these boxes to filter the data:"
              ),
              tags$ul(
                tags$li("Type text to search within that column"),
                tags$li("Multiple filters can be applied simultaneously"),
                tags$li("Filtering is case-insensitive"),
                tags$li(
                  "Use the filters to narrow down to specific taxa, 
                        ranks, or taxonomic groups"
                )
              ),

              h4("Navigation Buttons"),
              p(
                "Click on a row to select it. Then, below the data table are
                several buttons to help you 
                navigate the taxonomic hierarchy based on your selection:"
              ),
              tags$ul(
                tags$li(
                  tags$b("Select None:"),
                  " Deselects the currently 
                        selected row"
                ),
                tags$li(
                  tags$b("Jump to parent:"),
                  " Displays the parent taxon of the currently selected row"
                ),
                tags$li(
                  tags$b("Jump to accepted:"),
                  " For synonyms, jumps to the accepted name"
                ),
                tags$li(
                  tags$b("Show synonyms:"),
                  " Filters the table to 
                        show all synonyms of the selected accepted name"
                ),
                tags$li(
                  tags$b("Show children:"),
                  " Filters the table to 
                        show all direct children of the selected taxon"
                )
              ),

              h4("Other Buttons"),
              tags$ul(
                tags$li(
                  tags$b("Reset:"),
                  " Clears all filters and 
                        deselects all rows. ",
                  tags$b(
                    "Note: This button 
                        can be slow (up to ~10 seconds), so please be 
                        patient when using it."
                  )
                ),
                tags$li(
                  tags$b("Show higher taxa / Hide higher taxa:"),
                  " Toggles the visibility of higher taxonomic 
                        rank columns (Class, Subclass, Order, etc.)"
                ),
                tags$li(
                  tags$b("Download CSV:"),
                  " Downloads the currently 
                        displayed (filtered) data as a CSV file"
                )
              ),

              h4("Tips"),
              tags$ul(
                tags$li("You can only select one row at a time"),
                tags$li(
                  "Selected rows remain selected even when filtered 
                        out of view"
                ),
                tags$li(
                  "Combine filtering and navigation buttons to 
                        explore specific parts of the taxonomy"
                )
              )
            )
          )
        )
      )
    })

    # Display ppg table
    rows_selected <- display_ppg_server("display_ppg", ppg)
  }

  shiny::shinyApp(ui, server)
}
