#' Server logic to add a row
#'
#' Internal function
#'
#' @import shiny
#' @param id Character vector of length 1; the ID for
#' this module.
#' @param ppg Reactive dataframe (tibble) of PPG data
#' @param rows_selected A reactive value: currently selected rows
#' @param composed_name String; scientific name, that may be composed
#' from auto-completed fields
#' @param rows_selected Numeric vector; index of selected rows
#' @param show_advanced Logical; should advanced input fields be displayed?
#' @returns Value of `show_advanced` so this can be shared across modules
#' @autoglobal
#' @noRd
add_row_server <- function(
  id,
  ppg,
  composed_name,
  rows_selected,
  show_advanced
) {
  # Check args
  stopifnot(is.reactive(ppg))
  stopifnot(is.reactive(rows_selected))
  stopifnot(is.reactive(composed_name))
  stopifnot(is.reactive(show_advanced))

  moduleServer(id, function(input, output, session) {
    # initiate error message
    error_msg <- reactiveVal("")

    acceptedNameUsage <- autocomplete_server(
      id = "acceptedNameUsage",
      ppg = ppg,
      rows_selected = rows_selected,
      placeholder = "Select accepted name",
      col_select = "acceptedNameUsage",
      fill_name = FALSE,
      switch_off = NULL,
      TRUE
    )

    parentNameUsage <- autocomplete_server(
      id = "parentNameUsage",
      ppg = ppg,
      rows_selected = rows_selected,
      placeholder = "Select parent name",
      col_select = "parentNameUsage",
      fill_name = FALSE,
      switch_off = NULL,
      !taxonRank %in% c("form", "subspecies", "variety")
    )

    observeEvent(input$apply, {
      # Reset error message each time apply is clicked
      error_msg("")

      # Add one row, catching any errors in error_msg
      tryCatch(
        {
          updated_data <- dwctaxon::dct_add_row(
            ppg(),
            taxonID = null_if_blank(input$taxonID),
            scientificName = null_if_blank(composed_name()),
            namePublishedIn = null_if_blank(input$namePublishedIn),
            taxonRank = null_if_blank(input$taxonRank),
            taxonomicStatus = null_if_blank(input$taxonomicStatus),
            taxonRemarks = null_if_blank(input$taxonRemarks),
            acceptedNameUsageID = null_if_blank(input$acceptedNameUsageID),
            acceptedNameUsage = null_if_blank(acceptedNameUsage()),
            parentNameUsageID = null_if_blank(input$parentNameUsageID),
            parentNameUsage = null_if_blank(parentNameUsage())
          )
          # Save patch for undoing
          save_patch(data_original = ppg(), data_changed = updated_data)
          ppg(updated_data)
        },
        error = function(e) {
          error_msg(paste("Error:", e$message))
        }
      )

      output$error_msg <- renderText(error_msg())
    })

    # Toggle advanced options
    observeEvent(input$advanced_options, {
      show_advanced(!show_advanced())
    })
    observe({
      shinyjs::toggle("taxonID", condition = show_advanced())
      shinyjs::toggle("acceptedNameUsageID", condition = show_advanced())
      shinyjs::toggle("parentNameUsageID", condition = show_advanced())
      if (show_advanced()) {
        shinyjs::html("advanced_options", "Hide advanced options")
      } else {
        shinyjs::html("advanced_options", "Show advanced options")
      }
    })
    return(show_advanced)
  })
}
