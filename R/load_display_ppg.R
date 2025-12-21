#' UI to display PPG
#'
#' Internal function
#'
#' @import shiny
#' @param id Character vector of length 1; the ID for this module.
#' @returns UI
#' @noRd
display_ppg_ui <- function(id) {
  tagList(
    DT::dataTableOutput(NS(id, "ppg_table"), width = "100%"),
    actionButton(NS(id, "select_all"), "Select All"),
    actionButton(NS(id, "select_none"), "Select None"),
    actionButton(NS(id, "toggle_columns"), "Show advanced columns"),
    textOutput(NS(id, "selected_rows_message"))
  )
}

#' Server logic to load PPG
#'
#' Internal function
#'
#' @import shiny
#' @param id Character vector of length 1; the ID for this module.
#' @returns Server logic
#' @noRd
load_data_server <- function(id, data_source = Sys.getenv("DATA_SOURCE")) {
  moduleServer(id, function(input, output, session) {
    reactiveVal(load_data(data_source))
  })
}

#' Server logic to display PPG
#'
#' Internal function
#'
#' @import shiny
#' @param id Character vector of length 1; the ID for this module.
#' @param ppg Reactive dataframe (tibble) of PPG data
#' @returns List of metadata about current table
#' @noRd
display_ppg_server <- function(id, ppg) {
  # Check args
  stopifnot(is.reactive(ppg))

  # Reactive value to store column visibility state
  column_visibility <- reactiveVal(FALSE)

  moduleServer(id, function(input, output, session) {
    # Set up PPG table
    render_table <- function() {
      DT::renderDataTable(
        {
          DT::datatable(
            data = ppg(),
            rownames = FALSE,
            filter = "top",
            selection = "multiple",
            escape = FALSE,
            options = list(
              order = list(
                list(select_sort_col(ppg(), "modified"), "desc"),
                list(select_sort_col(ppg(), "scientificName"), "asc")
              ),
              columnDefs = list(
                list(
                  targets = c(
                    select_sort_col(ppg(), "taxonID"),
                    select_sort_col(ppg(), "acceptedNameUsageID"),
                    select_sort_col(ppg(), "parentNameUsageID")
                  ),
                  visible = column_visibility()
                )
              )
            )
          )
        },
        server = TRUE
      )
    }

    # Initial render
    output$ppg_table <- render_table()

    # Set up proxy for handling row selection
    dt_proxy <- DT::dataTableProxy("ppg_table")

    # Select / deselect rows
    observeEvent(
      input$select_all,
      DT::selectRows(
        dt_proxy,
        unique(c(input$ppg_table_rows_all, input$ppg_table_rows_selected))
      )
    )
    observeEvent(
      input$select_none,
      DT::selectRows(dt_proxy, NULL)
    )

    # Show / hide ID coluns
    observeEvent(input$toggle_columns, {
      current_visibility <- column_visibility()
      column_visibility(!current_visibility)

      # Redraw the datatable with updated visibility
      output$ppg_table <- render_table()
      DT::replaceData(dt_proxy, ppg(), resetPaging = FALSE, rownames = FALSE)

      # Toggle appearance of button
      if (isTRUE(input$toggle_columns %% 2 == 1)) {
        shinyjs::html("toggle_columns", "Hide advanced columns")
      } else {
        shinyjs::html("toggle_columns", "Show advanced columns")
      }
    })

    # Display number of selected rows
    selected_rows <- reactive(
      input$ppg_table_rows_selected
    )
    output$selected_rows_message <- renderText({
      num_selected <- length(selected_rows())
      paste("Number of rows selected:", num_selected)
    })
    return(selected_rows)
  })
}

# test app
display_ppg_app <- function() {
  ui <- fluidPage(
    display_ppg_ui("ppg_table")
  )
  server <- function(input, output, session) {
    # Load data
    ppg <- load_data_server("ppg", "repo")
    display_ppg_server("ppg_table", ppg)
  }
  shinyApp(ui, server)
}
