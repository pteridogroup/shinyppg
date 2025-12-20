#' UI to display data entry fields for composing a scientific name
#'
#' Internal function
#'
#' @import shiny
#' @param id Character vector of length 1; the ID for this module.
#' @returns UI
#' @noRd
compose_name_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectizeInput(
      ns("higher_name"),
      label = "Genus or higher name",
      choices = NULL,
      multiple = FALSE
    ),
    selectizeInput(
      ns("epithet"),
      label = "Specific epithet",
      choices = NULL,
      multiple = FALSE
    ),
    selectizeInput(
      ns("author"),
      label = "Author",
      choices = NULL,
      multiple = FALSE
    ),
    textInput(ns("scientificName"), "ScientificName"),
    helpText("Scientific name, including author")
  )
}

#' Server logic to compose a taxonomic name
#'
#' Internal function
#'
#' @import shiny
#' @param id Character vector of length 1; the ID for
#'   this module.
#' @param higher_names Character vector of higher-level taxonomic names
#' @param epithets Character vector of specific epithets
#' @param authors Character vector of author names
#' @param ppg Dataframe: the PPG taxonomic system (in DWC format)
#' @param rows_selected Vector of currently selected row indices
#' @param fill_sci_name Logical; should the scientific name be filled in
#'   from the currently selected row?
#' @returns Server logic
#' @noRd
compose_name_server <- function(
  id,
  higher_names,
  epithets,
  authors,
  composed_name,
  ppg,
  rows_selected,
  fill_sci_name = FALSE
) {
  # Check args
  stopifnot(is.reactive(composed_name))
  stopifnot(is.reactive(rows_selected))
  stopifnot(is.reactive(ppg))
  stopifnot(!is.reactive(higher_names))
  stopifnot(!is.reactive(epithets))
  stopifnot(!is.reactive(authors))
  stopifnot(!is.reactive(fill_sci_name))

  moduleServer(id, function(input, output, session) {
    # Update scientific name from composed input
    observeEvent(input$scientificName, {
      composed_name(input$scientificName)
    })

    update_selectize_compose_name(
      session,
      "higher_name",
      choices = higher_names,
      placeholder = "Select higher name"
    )

    update_selectize_compose_name(
      session,
      "epithet",
      choices = epithets,
      placeholder = "Select specific epithet"
    )

    update_selectize_compose_name(
      session,
      "author",
      choices = authors,
      placeholder = "Select author"
    )

    observeEvent(input$higher_name, {
      current_input <- input$higher_name
      if (current_input %in% higher_names) {
        existing_text <- input$scientificName
        new_text <- if (existing_text == "") {
          current_input
        } else {
          paste0(existing_text, current_input)
        }
        updateTextInput(session, "scientificName", value = new_text)
        updateSelectizeInput(session, "higher_name", selected = "")
        composed_name(new_text)
      }
    })

    observeEvent(input$epithet, {
      current_input <- input$epithet
      if (current_input %in% epithets) {
        existing_text <- input$scientificName
        new_text <- if (existing_text == "") {
          current_input
        } else {
          paste0(existing_text, current_input)
        }
        updateTextInput(session, "scientificName", value = new_text)
        updateSelectizeInput(session, "epithet", selected = "")
        composed_name(new_text)
      }
    })

    observeEvent(input$author, {
      current_input <- input$author
      if (current_input %in% authors) {
        existing_text <- input$scientificName
        new_text <- if (existing_text == "") {
          current_input
        } else {
          paste0(existing_text, current_input)
        }
        updateTextInput(session, "scientificName", value = new_text)
        updateSelectizeInput(session, "author", selected = "")
        composed_name(new_text)
      }
    })

    # Fill in row editing text boxes with data from selected row
    observeEvent(rows_selected(), {
      if (length(rows_selected()) == 1 && fill_sci_name) {
        selected_row <- ppg()[rows_selected(), ]
        composed_name(selected_row$scientificName[[1]])
        updateTextInput(session, "scientificName", value = composed_name())
      }
    })
    # Reset row editing text boxes when zero or >1 rows selected
    mult_or_no_rows_selected <- check_mult_or_no_rows_selected(rows_selected)
    observeEvent(mult_or_no_rows_selected(), {
      if (mult_or_no_rows_selected() && fill_sci_name) {
        updateTextInput(session, "scientificName", value = "")
      }
    })
  })
}

# Test app
compose_name_app <- function() {
  ui <- fluidPage(
    compose_name_ui("sci_name")
  )
  server <- function(input, output, session) {
    # Load data
    ppg <- load_data_server("ppg")
    higher_names <- load_pterido_higher_names()
    epithets <- load_pterido_sp_epithets()
    ipni_authors <- load_authors()

    # Set initial values
    composed_name_example <- reactiveVal("")
    rows_selected <- display_ppg_server("display_ppg", ppg)

    # Other server logic
    compose_name_server(
      "sci_name",
      higher_names = higher_names,
      epithets = epithets,
      authors = ipni_authors,
      composed_name = composed_name_example,
      ppg = ppg,
      rows_selected = rows_selected,
    )
  }
  shinyApp(ui, server)
}
