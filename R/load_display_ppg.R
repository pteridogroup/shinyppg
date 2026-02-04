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
    actionButton(NS(id, "select_none"), "Select None"),
    actionButton(NS(id, "jump_to_parent"), "Jump to parent"),
    actionButton(NS(id, "jump_to_accepted"), "Jump to accepted"),
    actionButton(NS(id, "show_synonyms"), "Show synonyms"),
    actionButton(NS(id, "show_children"), "Show children"),
    actionButton(NS(id, "clear_search"), "Clear search"),
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
    # Disable jump buttons initially
    shinyjs::disable("jump_to_parent")
    shinyjs::disable("jump_to_accepted")
    shinyjs::disable("show_synonyms")
    shinyjs::disable("show_children")

    # Set up PPG table
    render_table <- function() {
      DT::renderDataTable(
        {
          DT::datatable(
            data = ppg(),
            rownames = FALSE,
            filter = "top",
            selection = "single",
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

    # Hide higher taxa columns initially
    observeEvent(
      input$ppg_table_search,
      {
        if (!column_visibility()) {
          higher_taxa_cols <- c(
            which(names(ppg()) == "class") - 1,
            which(names(ppg()) == "subclass") - 1,
            which(names(ppg()) == "order") - 1,
            which(names(ppg()) == "suborder") - 1,
            which(names(ppg()) == "family") - 1,
            which(names(ppg()) == "subfamily") - 1,
            which(names(ppg()) == "genus") - 1
          )
          DT::hideCols(dt_proxy, higher_taxa_cols)
        }
      },
      once = TRUE,
      ignoreInit = FALSE
    )

    # Deselect rows
    observeEvent(
      input$select_none,
      DT::selectRows(dt_proxy, NULL)
    )

    # Show / hide higher taxa columns
    observeEvent(input$toggle_columns, {
      current_visibility <- column_visibility()
      column_visibility(!current_visibility)

      # Get column indices for higher taxa
      higher_taxa_cols <- c(
        which(names(ppg()) == "class") - 1,
        which(names(ppg()) == "subclass") - 1,
        which(names(ppg()) == "order") - 1,
        which(names(ppg()) == "suborder") - 1,
        which(names(ppg()) == "family") - 1,
        which(names(ppg()) == "subfamily") - 1,
        which(names(ppg()) == "genus") - 1
      )

      # Show or hide columns without resetting search
      if (column_visibility()) {
        DT::showCols(dt_proxy, higher_taxa_cols)
      } else {
        DT::hideCols(dt_proxy, higher_taxa_cols)
      }

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

    # Enable/disable jump buttons based on selection
    observeEvent(selected_rows(), {
      if (length(selected_rows()) == 1) {
        # With server=TRUE, selected_rows gives the index in the full dataset
        row_index <- selected_rows()
        if (row_index > 0 && row_index <= nrow(ppg())) {
          selected_data <- ppg()[row_index, ]

          # Enable jump to parent if taxon has a parent
          if (
            !is.na(selected_data$parentNameUsageID) &&
              selected_data$parentNameUsageID != ""
          ) {
            shinyjs::enable("jump_to_parent")
          } else {
            shinyjs::disable("jump_to_parent")
          }

          # Enable jump to accepted if taxon is a synonym
          if (
            !is.na(selected_data$acceptedNameUsageID) &&
              selected_data$acceptedNameUsageID != "" &&
              selected_data$taxonomicStatus != "accepted"
          ) {
            shinyjs::enable("jump_to_accepted")
          } else {
            shinyjs::disable("jump_to_accepted")
          }

          # Enable show synonyms if taxon is an accepted name AND has synonyms
          if (selected_data$taxonomicStatus == "accepted") {
            # Check if this taxon has any synonyms
            taxon_id <- selected_data$taxonID
            has_synonyms <- any(
              ppg()$acceptedNameUsageID == taxon_id,
              na.rm = TRUE
            )
            if (has_synonyms) {
              shinyjs::enable("show_synonyms")
            } else {
              shinyjs::disable("show_synonyms")
            }
          } else {
            shinyjs::disable("show_synonyms")
          }

          # Enable show children if taxon has children
          taxon_id <- selected_data$taxonID
          has_children <- any(
            ppg()$parentNameUsageID == taxon_id,
            na.rm = TRUE
          )
          if (has_children) {
            shinyjs::enable("show_children")
          } else {
            shinyjs::disable("show_children")
          }
        } else {
          shinyjs::disable("jump_to_parent")
          shinyjs::disable("jump_to_accepted")
          shinyjs::disable("show_synonyms")
          shinyjs::disable("show_children")
        }
      } else {
        shinyjs::disable("jump_to_parent")
        shinyjs::disable("jump_to_accepted")
        shinyjs::disable("show_synonyms")
        shinyjs::disable("show_children")
      }
    })

    # Jump to parent taxon
    observeEvent(input$jump_to_parent, {
      if (length(selected_rows()) == 1) {
        row_index <- selected_rows()
        if (row_index > 0 && row_index <= nrow(ppg())) {
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

              # Select the parent row
              # After filtering, need to select by the parent's actual row index
              DT::selectRows(dt_proxy, parent_row)
            }
          }
        }
      }
    })

    # Jump to accepted taxon
    observeEvent(input$jump_to_accepted, {
      if (length(selected_rows()) == 1) {
        row_index <- selected_rows()
        if (row_index > 0 && row_index <= nrow(ppg())) {
          selected_data <- ppg()[row_index, ]
          accepted_id <- selected_data$acceptedNameUsageID

          if (!is.na(accepted_id) && accepted_id != "") {
            # Find accepted name row in the data
            accepted_row <- which(ppg()$taxonID == accepted_id)

            if (length(accepted_row) > 0) {
              # Clear current selection
              DT::selectRows(dt_proxy, NULL)

              # Search for exact taxonID in the taxonID column
              col_index <- which(names(ppg()) == "taxonID") - 1
              search_cols <- rep("", ncol(ppg()))
              search_cols[col_index + 1] <- accepted_id

              DT::updateSearch(
                dt_proxy,
                keywords = list(global = "", columns = search_cols)
              )

              # Select the accepted name row
              DT::selectRows(dt_proxy, accepted_row)
            }
          }
        }
      }
    })

    # Show all synonyms of selected accepted name
    observeEvent(input$show_synonyms, {
      if (length(selected_rows()) == 1) {
        row_index <- selected_rows()
        if (row_index > 0 && row_index <= nrow(ppg())) {
          selected_data <- ppg()[row_index, ]
          taxon_id <- selected_data$taxonID

          if (!is.na(taxon_id) && taxon_id != "") {
            # Find all rows where acceptedNameUsageID matches this taxon
            synonym_rows <- which(ppg()$acceptedNameUsageID == taxon_id)

            if (length(synonym_rows) > 0) {
              # Clear current selection
              DT::selectRows(dt_proxy, NULL)

              # Search for taxonID in the acceptedNameUsageID column
              col_index <- which(names(ppg()) == "acceptedNameUsageID") - 1
              search_cols <- rep("", ncol(ppg()))
              search_cols[col_index + 1] <- taxon_id

              DT::updateSearch(
                dt_proxy,
                keywords = list(global = "", columns = search_cols)
              )
            }
          }
        }
      }
    })

    # Show all children of selected taxon
    observeEvent(input$show_children, {
      if (length(selected_rows()) == 1) {
        row_index <- selected_rows()
        if (row_index > 0 && row_index <= nrow(ppg())) {
          selected_data <- ppg()[row_index, ]
          taxon_id <- selected_data$taxonID

          if (!is.na(taxon_id) && taxon_id != "") {
            # Find all rows where parentNameUsageID matches this taxon
            child_rows <- which(ppg()$parentNameUsageID == taxon_id)

            if (length(child_rows) > 0) {
              # Clear current selection
              DT::selectRows(dt_proxy, NULL)

              # Search for taxonID in the parentNameUsageID column
              col_index <- which(names(ppg()) == "parentNameUsageID") - 1
              search_cols <- rep("", ncol(ppg()))
              search_cols[col_index + 1] <- taxon_id

              DT::updateSearch(
                dt_proxy,
                keywords = list(global = "", columns = search_cols)
              )
            }
          }
        }
      }
    })

    # Clear all search filters
    observeEvent(input$clear_search, {
      # Clear global search and all column searches
      DT::updateSearch(
        dt_proxy,
        keywords = list(
          global = "",
          columns = rep("", ncol(ppg()))
        )
      )
      # Clear selection
      DT::selectRows(dt_proxy, NULL)
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
