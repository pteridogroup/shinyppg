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
    shinyjs::useShinyjs(),
    DT::dataTableOutput(NS(id, "ppg_table"), width = "100%"),
    actionButton(NS(id, "select_all"), "Select All"),
    actionButton(NS(id, "select_none"), "Select None"),
    actionButton(NS(id, "jump_to_parent"), "Jump to parent"),
    actionButton(NS(id, "toggle_columns"), "Show higher taxa"),
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
    # Disable jump to parent button initially
    shinyjs::disable("jump_to_parent")

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
                list(
                  select_sort_col(ppg(), "scientificName"),
                  "asc"
                )
              ),
              columnDefs = list(
                list(
                  targets = c(
                    select_sort_col(ppg(), "taxonID"),
                    select_sort_col(ppg(), "acceptedNameUsageID"),
                    select_sort_col(ppg(), "parentNameUsageID")
                  ),
                  visible = FALSE
                ),
                list(
                  targets = c(
                    select_sort_col(ppg(), "class"),
                    select_sort_col(ppg(), "subclass"),
                    select_sort_col(ppg(), "order"),
                    select_sort_col(ppg(), "suborder"),
                    select_sort_col(ppg(), "family"),
                    select_sort_col(ppg(), "subfamily"),
                    select_sort_col(ppg(), "genus")
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

    # Show / hide higher taxa columns
    observeEvent(input$toggle_columns, {
      current_visibility <- column_visibility()
      column_visibility(!current_visibility)

      # Redraw the datatable with updated visibility
      output$ppg_table <- render_table()
      DT::replaceData(
        dt_proxy,
        ppg(),
        resetPaging = FALSE,
        rownames = FALSE
      )

      # Toggle appearance of button
      if (isTRUE(input$toggle_columns %% 2 == 1)) {
        shinyjs::html("toggle_columns", "Hide higher taxa")
      } else {
        shinyjs::html("toggle_columns", "Show higher taxa")
      }
    })

    # Display number of selected rows and enable/disable jump to parent button
    selected_rows <- reactive(
      input$ppg_table_rows_selected
    )

    # Enable/disable jump to parent button based on selection
    observeEvent(selected_rows(), {
      if (length(selected_rows()) == 1) {
        # Check if selected taxon has a parent
        row_index <- selected_rows()
        selected_data <- ppg()[row_index, ]
        if (
          !is.na(selected_data$parentNameUsageID) &&
            selected_data$parentNameUsageID != ""
        ) {
          shinyjs::enable("jump_to_parent")
        } else {
          shinyjs::disable("jump_to_parent")
        }
      } else {
        shinyjs::disable("jump_to_parent")
      }
    })

    # Jump to parent taxon
    observeEvent(input$jump_to_parent, {
      if (length(selected_rows()) == 1) {
        row_index <- selected_rows()
        selected_data <- ppg()[row_index, ]
        parent_id <- selected_data$parentNameUsageID

        if (!is.na(parent_id) && parent_id != "") {
          # Find parent row in the data
          parent_row <- which(ppg()$taxonID == parent_id)

          if (length(parent_row) > 0) {
            # Clear current selection
            DT::selectRows(dt_proxy, NULL)

            # Search for exact taxonID in the taxonID column (column 0)
            # This ensures only one row matches
            col_index <- which(names(ppg()) == "taxonID") - 1
            search_cols <- rep("", ncol(ppg()))
            search_cols[col_index + 1] <- parent_id

            DT::updateSearch(
              dt_proxy,
              keywords = list(global = "", columns = search_cols)
            )

            # Select the parent row (should be row 1 after filtering)
            DT::selectRows(dt_proxy, 1)
          }
        }
      }
    })

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
