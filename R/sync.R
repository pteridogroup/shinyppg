#' Text input with limited width
#' see https://github.com/rstudio/shiny/issues/3305#issuecomment-1407028520
#' @noRd
#' @autoglobal
textInput2 <- function(
  inputId,
  label,
  value = "",
  width = NULL,
  placeholder = NULL,
  maxlength = NULL
) {
  tag <- shiny::textInput(
    inputId = inputId,
    label = label,
    value = value,
    width = width,
    placeholder = placeholder
  )

  if (!is.null(maxlength)) {
    htmltools::tagQuery(tag)$children("input")$addAttrs(
      maxlength = maxlength
    )$allTags()
  } else {
    tag
  }
}

#' UI for displaying current session name
#' @autoglobal
#' @noRd
display_session_ui <- function(id) {
  tagList(
    textOutput(NS(id, "current_session"))
  )
}

#' Server for displaying current session name
#' @autoglobal
#' @noRd
display_session_server <- function(id, current_branch) {
  moduleServer(id, function(input, output, session) {
    stopifnot(is.reactive(current_branch))
    output$current_session <- renderText(
      paste("Current session:", current_branch())
    )
  })
}

#' UI for syncing
#' @autoglobal
#' @noRd
sync_ui <- function(id) {
  tagList(
    h3("Start a new session"),
    actionButton(NS(id, "new"), "Start new session"),
    h3("Load an existing session"),
    DT::dataTableOutput(NS(id, "session_table"), width = "100%"),
    actionButton(NS(id, "load"), "Load session"),
    actionButton(NS(id, "refresh"), "Refresh session info"),
    h3("Save current session"),
    textInput2(NS(id, "title"), "Session title", maxlength = 50),
    textAreaInput(NS(id, "summary"), "Summary of changes"),
    actionButton(NS(id, "push"), "Save session")
  )
}

#' Server for syncing
#' @autoglobal
#' @noRd
sync_server <- function(id, ppg, dry_run = FALSE) {
  moduleServer(id, function(input, output, session) {
    stopifnot(is.reactive(ppg))

    # Setup repo
    if (!fs::dir_exists("/home/shiny/ppg")) {
      setup_repo("/home/shiny/ppg")
    }

    # Get initial branch
    current_branch <- reactiveVal(
      gert::git_branch(repo = "/home/shiny/ppg")
    )

    # Summarize branches ----
    branch_summary <- reactiveVal(tibble::tibble())

    observe({
      req(credentials())
      branch_summary(summarize_branches(user_id = credentials()$info$user))
    })

    observeEvent(input$refresh, {
      req(credentials())
      branch_summary(summarize_branches(user_id = credentials()$info$user))
    })

    output$session_table <- DT::renderDataTable({
      DT::datatable(
        rownames = FALSE,
        selection = "single",
        branch_summary()
      )
    })

    # Load an existing session ----
    # length of selected_row() is 0 when nothing selected, 1 otherwise
    selected_row <- reactive({
      input$session_table_rows_selected
    })

    observeEvent(input$load, {
      req(selected_row())
      if (length(selected_row()) == 1) {
        load_existing_session(
          session_summary = branch_summary(),
          selected_row = selected_row()
        )
        current_branch(gert::git_branch(repo = "/home/shiny/ppg"))
        # Load ppg
        ppg(load_data("repo"))
      }
    })

    # Start a new session ----
    observeEvent(input$new, {
      req(credentials())
      # Checkout new branch (any unsaved changes will be lost)
      start_new_session(
        user_id = credentials()$info$user,
        ppg_path = "/home/shiny/ppg/data/ppg.csv",
        ppg_repo = "/home/shiny/ppg"
      )
      current_branch(gert::git_branch(repo = "/home/shiny/ppg"))
      # Load ppg
      ppg(load_data("repo"))
    })

    # Save session ----
    # (does not change branch)
    observeEvent(input$push, {
      req(credentials())
      submit_changes(
        ppg = ppg(),
        user_name = credentials()$info$name,
        user_id = credentials()$info$user,
        title = input$title,
        summary = input$summary,
        dry_run = dry_run,
        ppg_path = "/home/shiny/ppg/data/ppg.csv",
        ppg_repo = "/home/shiny/ppg"
      )
    })
    return(current_branch)
  })
}

#' Demo app for syncing
#' @autoglobal
#' @noRd
sync_app <- function() {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        delete_row_ui("delete_row"),
        undo_ui("undo"),
        sync_ui("sync"),
        display_session_ui("branch")
      ),
      mainPanel(
        display_ppg_ui("display_ppg")
      )
    )
  )
  server <- function(input, output, session) {
    ppg <- load_data_server("ppg", "repo")
    rows_selected <- display_ppg_server("display_ppg", ppg)
    delete_row_server("delete_row", ppg, rows_selected)
    undo_server("undo", ppg)
    credentials <- reactive(
      list(
        info = list(
          user = "user",
          name = "Test User"
        )
      )
    )
    current_branch <- sync_server(
      "sync",
      ppg = ppg,
      credentials = credentials,
      dry_run = FALSE
    )
    display_session_server("branch", current_branch)
  }
  shinyApp(ui, server)
}
