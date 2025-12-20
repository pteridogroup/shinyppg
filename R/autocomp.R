#' UI to display a select ("selectize") menu of possible values, which are
#' generated from the dataframe being edited
#'
#' Internal function
#'
#' This uses server-side processing to decrease load times, since the range of
#' possible entry values is very large. See:
#' https://shiny.posit.co/r/articles/build/selectize/
#'
#' @import shiny
#' @param id Character vector of length 1; the ID for this module.
#' @param col_select Name of the column to use for auto-populating select values
#' @param help_text A string of explanation text to display beneath the select
#'   entry menu UI element
#' @returns UI
#' @noRd
autocomplete_ui <- function(id, col_select, help_text, multiple = FALSE) {
  tagList(
    shinyjs::useShinyjs(),
    selectizeInput(
      NS(id, "autocomp_col"),
      label = col_select,
      choices = NULL,
      multiple = multiple
    ),
    helpText(help_text)
  )
}

#' Server logic to fill a select menu with options created from ppg data
#'
#' Internal function
#'
#' @import shiny
#' @param id Character vector of length 1; the ID for this module.
#' @param ppg Reactive dataframe (tibble) of PPG data
#' @param rows_selected Reactive value; index of selected rows
#' @param fill_name Logical; should the selected value in the menu be
#'   auto-filled from the selected row?
#' @param ... Passed to a filter() call to filter the rows of the ppg dataframe
#'   to only the rows that should be used for providing items in the selectize
#'   menu
#' @returns Server logic
#' @autoglobal
#' @noRd
autocomplete_server <- function(
  id,
  ppg,
  rows_selected,
  placeholder,
  col_select,
  fill_name = FALSE,
  switch_off = NULL,
  ...
) {
  stopifnot(is.reactive(ppg))
  stopifnot(!is.reactive(fill_name))
  if (fill_name) {
    stopifnot(is.reactive(rows_selected))
    stopifnot(!is.reactive(col_select))
  }
  if (!is.null(switch_off)) {
    stopifnot(is.reactive(switch_off))
  }

  moduleServer(id, function(input, output, session) {
    accepted_higher_names <-
      reactive({
        ppg() |>
          dplyr::filter(taxonomicStatus == "accepted") |>
          dplyr::filter(...) |>
          dplyr::pull(scientificName) |>
          unique() |>
          sort()
      })
    observe({
      choices <- accepted_higher_names()
      if (length(choices) > 0) {
        initialize_selectize_input(
          session = session,
          choices = choices,
          placeholder = placeholder,
          selected = ""
        )
      }
    })
    if (!is.null(switch_off)) {
      observe({
        if (switch_off()) {
          shinyjs::disable("autocomp_col")
        }
        if (!switch_off()) {
          shinyjs::enable("autocomp_col")
        }
      })
    }
    if (fill_name) {
      # Fill in row editing text boxes with data from selected row
      observeEvent(rows_selected(), {
        if (length(rows_selected()) == 1 && fill_name) {
          selected_row <- ppg()[rows_selected(), ]
          initialize_selectize_input(
            session = session,
            choices = accepted_higher_names(),
            placeholder = placeholder,
            selected = selected_row[[col_select]][[1]]
          )
        }
      })
      # Reset row editing text boxes when zero or >1 rows selected
      mult_or_no_rows_selected <- check_mult_or_no_rows_selected(rows_selected)
      observeEvent(mult_or_no_rows_selected(), {
        if (mult_or_no_rows_selected() && fill_name) {
          initialize_selectize_input(
            session = session,
            choices = accepted_higher_names(),
            placeholder = placeholder,
            selected = ""
          )
        }
      })
    }
    reactive(input$autocomp_col)
  })
}

#' Simple app for testing
#'
#'  internal function
#'
#' @import shiny
#' @autoglobal
#' @noRd
autocomplete_app <- function() {
  ui <- fluidPage(
    autocomplete_ui(
      id = "parentNameUsage",
      col_select = "parentNameUsage",
      help_text = "Scientific name of parent taxon"
    ),
    textOutput("result")
  )
  server <- function(input, output, session) {
    dwctaxon::dct_options(
      user_name = "user",
      user_id = "123"
    )
    # Load data
    ppg <- load_data_server("ppg")
    # Set initial values
    rows_selected <- display_ppg_server("display_ppg", ppg)
    # Other server logic
    res <- autocomplete_server(
      id = "parentNameUsage",
      ppg = ppg,
      rows_selected = rows_selected,
      placeholder = "Select parent name",
      col_select = "parentNameUsage",
      fill_name = FALSE,
      !taxonRank %in% c("form", "subspecies", "variety")
    )
    output$result <- renderText(res())
  }
  shinyApp(ui, server)
}
