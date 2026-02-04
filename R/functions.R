#' Convert output of reactable::getReactableState() for column sorting to
#' format that can be used to specify column sort order.
#'
#' The output of this function can be used as input to the `columns` arg
#' of reactable::reactable()
#'
#' Internal function
#'
#' @param col_list Output of reactable::getReactableState(name = "sorted"):
#' a named list of columns with values of "asc" for ascending order or "desc"
#' for descending order, or NULL if no columns are sorted
#'
#' @return list
#' @noRd
set_asc_desc <- function(col_list) {
  res <- col_list
  for (i in seq_along(col_list)) {
    res[[i]] <- reactable::colDef(defaultSortOrder = col_list[[i]])
  }
  res
}

#' Add higher taxonomy columns
#'
#' Add columns for each higher taxonomic rank to enable filtering without
#' subsetting data
#'
#' Internal function
#'
#' @param ppg PPG dataframe
#' @return PPG with additional columns for higher taxonomy
#' @noRd
#' @autoglobal
add_higher_taxonomy <- function(ppg) {
  # Define target ranks in hierarchical order
  target_ranks <- c(
    "class",
    "subclass",
    "order",
    "suborder",
    "family",
    "subfamily",
    "genus"
  )

  # Initialize columns with NA
  for (rank in target_ranks) {
    ppg[[rank]] <- NA_character_
  }

  # For each row, traverse up the hierarchy and fill in higher ranks
  for (i in seq_len(nrow(ppg))) {
    current_id <- ppg$taxonID[i]
    current_rank <- ppg$taxonRank[i]

    # Fill in current rank if it's one of the target ranks
    if (current_rank %in% target_ranks) {
      ppg[[current_rank]][i] <- ppg$scientificName[i]
    }

    # Traverse up the hierarchy
    parent_id <- ppg$parentNameUsageID[i]
    while (!is.na(parent_id) && parent_id != "") {
      parent_row <- which(ppg$taxonID == parent_id)
      if (length(parent_row) == 0) {
        break
      }

      parent_rank <- ppg$taxonRank[parent_row]
      if (parent_rank %in% target_ranks) {
        ppg[[parent_rank]][i] <- ppg$scientificName[parent_row]
      }

      parent_id <- ppg$parentNameUsageID[parent_row]
    }
  }

  return(ppg)
}

#' Load data
#'
#' Loads the most recent PPG dataset
#'
#' Internal function
#'
#' @param data_source Where to get the data. If 'local', data will be read
#' from saved data file in ./data. Otherwise, data will be downloaded.
#'
#' @return Tibble
#' @noRd
load_data <- function(data_source = Sys.getenv("DATA_SOURCE")) {
  if (data_source == "local") {
    ppg <- shinyppg::ppg_small
  } else if (data_source == "repo") {
    path <- "https://raw.githubusercontent.com/pteridogroup/ppg/refs/heads/main/data/ppg.csv"
    ppg <- readr::read_csv(
      path,
      col_types = readr::cols(.default = readr::col_character())
    ) |>
      as.data.frame()
    attributes(ppg)$spec <- NULL
    # Remove obsolete columns if they exist (for backward compatibility)
    obsolete_cols <- c("ipniURL", "modifiedBy", "modifiedByID")
    ppg <- ppg[, !names(ppg) %in% obsolete_cols, drop = FALSE]
  }

  # Generate acceptedNameUsage and parentNameUsage columns
  ppg <- ppg |>
    dwctaxon::dct_fill_col(
      fill_to = "acceptedNameUsage",
      fill_from = "scientificName",
      match_to = "taxonID",
      match_from = "acceptedNameUsageID",
      stamp_modified = FALSE
    ) |>
    dwctaxon::dct_fill_col(
      fill_to = "parentNameUsage",
      fill_from = "scientificName",
      match_to = "taxonID",
      match_from = "parentNameUsageID",
      stamp_modified = FALSE
    )

  # Add higher taxonomy columns
  ppg <- add_higher_taxonomy(ppg)

  return(ppg)
}

#' Load IPNI authors
#'
#' Internal function
#'
#' @return Character vector
#' @noRd
load_authors <- function() {
  shinyppg::ipni_authors
}

#' Load higher names of pteridophytes
#'
#' Internal function
#'
#' @return Character vector
#' @noRd
load_pterido_higher_names <- function() {
  shinyppg::pterido_higher_names
}

#' Load specific epithets of pteridophytes
#'
#' Internal function
#'
#' @return Character vector
#' @noRd
load_pterido_sp_epithets <- function() {
  shinyppg::pterido_sp_epithets
}

#' updateSelectizeInput for compose_name_server()
#'
#' Internal function
#'
#' @param session The session object passed to function given to shinyServer
#' @param inputId The id of the input object.
#' @param choices Selection choices to include in the selectize input
#' @param placeholder Text to display before selection is made.
#' @return Output of updateSelectizeInput()
#' @noRd
update_selectize_compose_name <- function(
  session,
  inputId,
  choices,
  placeholder
) {
  shiny::observe({
    updateSelectizeInput(
      session = session,
      inputId = inputId,
      choices = choices,
      selected = "",
      server = TRUE,
      options = list(
        placeholder = placeholder,
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  })
}

#' Check if multiple, or zero, rows are selected
#'
#' Internal function
#'
#' @param rows_selected A reactive value: currently selected rows
#' @return A reactive value
#' @noRd
check_mult_or_no_rows_selected <- function(rows_selected) {
  reactive({
    is.null(rows_selected()) ||
      length(rows_selected()) == 0 ||
      length(rows_selected()) > 1
  })
}

#' Convert values of `""` to NULL
#'
#' Internal function
#'
#' @param x Input; a character vector of length 1
#' @return `NULL`
#' @noRd
null_if_blank <- function(x) {
  stopifnot(length(x) == 1)
  if (x == "") {
    return(NULL)
  }
  x
}

#' Reset text in a data entry box to empty (`""`)
#'
#' Internal function
#'
#' @param session Passed to updateTextInput
#' @param item Name of text input to reset
#' @noRd
reset_data_entry <- function(session, item) {
  shiny::updateTextInput(
    session,
    item,
    value = ""
  )
}

#' Set text in a data entry box to the value from a selected row
#'
#' Internal function
#'
#' @param session Passed to updateTextInput
#' @param item Name of text input to reset
#' @param selected_row Dataframe with one row; selected row of data from PPG
#' @noRd
fill_data_entry_from_row <- function(session, item, selected_row) {
  shiny::updateTextInput(
    session,
    item,
    value = selected_row[[item]]
  )
}

#' Not used here, but needed to pass R CMD CHECK since we use
#' pkgload::load_all() in ./app.R
#'
#' @noRd
pkgload_load_all <- function(...) {
  pkgload::load_all(...)
}

#' Initialize a select menu of input items
#'
#' Internal function used in autocomplete_server()
#'
#' @param session Current session
#' @param choices Input choices
#' @param placeholder Default value to display
#' @param selected Value to select
#'
#' @noRd
initialize_selectize_input <- function(
  session,
  choices,
  placeholder,
  selected
) {
  updateSelectizeInput(
    session,
    inputId = "autocomp_col",
    choices = choices,
    selected = selected,
    server = TRUE,
    options = list(
      placeholder = placeholder,
      onInitialize = I('function() { this.setValue(""); }')
    )
  )
}

#' Check if PPG data are valid or not, and return either TRUE or FALSE
#' but no error
#'
#' @param ppg PPG dataframe
#'
#' @noRd
initial_validate <- function(ppg) {
  tryCatch(
    {
      res <- dwctaxon::dct_validate(
        ppg,
        on_success = "logical",
        on_fail = "error"
      )
      return(res)
    },
    error = function(e) {
      return(FALSE)
    }
  )
}

#' Select a column of the PPG dataframe for sorting with DT::datatable()
#'
#' Note that sorting uses zero-based counting
#'
#' @param ppg PPG dataframe
#' @param col_name Name of column to sort by
#'
#' @noRd
select_sort_col <- function(ppg, col_name) {
  if (!col_name %in% colnames(ppg)) {
    return(NULL)
  }
  which(colnames(ppg) == col_name) - 1
}

#' Runs on package load
#'
#' Internal function
#'
#' @noRd
.onLoad <- function(libname, pkgname) {
  # Load dwctaxon namespace so we can access objects like dct_terms
  requireNamespace("dwctaxon", quietly = TRUE)
}

# Undo ----

#' Set a global variable
#'
#' Internal function
#'
#' @param name Name of object
#' @param value Value to assign to object
#'
#' @return Nothing; run for its side effect
#'
#' @noRd
set_global <- function(name, value) {
  assign(name, value, envir = pkg_env)
  invisible()
}

#' Get list of patches
#'
#' The patches in the list can be used to restore the ppg dataframe (undo the
#' last change made to the dataframe)
#'
#' Internal function
#'
#' @return List of patches, if any exist
#'
#' @noRd
get_patch_list <- function() {
  if (exists("global_patch_list", envir = pkg_env)) {
    get("global_patch_list", envir = pkg_env)
  } else {
    set_global("global_patch_list", NULL)
    return(NULL)
  }
}

#' Save a patch
#'
#' The patches in the list can be used to restore the ppg dataframe (undo the
#' last change made to the dataframe)
#'
#' Internal function
#'
#' @param data_original Original dataframe
#' @param data_changed Changed dataframe
#'
#' @return Nothing; called for its side effect, which is to add a patch to
#' the global list of patches.
#'
#' @noRd
save_patch <- function(data_original, data_changed) {
  patch_list <- get_patch_list()
  new_patch <- daff::diff_data(
    data_ref = data_changed,
    data = data_original
  )
  patch_list <- c(patch_list, list(new_patch))
  set_global("global_patch_list", patch_list)
  invisible()
}

#' Undo the last change made to ppg
#'
#' Internal function
#'
#' @param data The modified ppg dataframe
#'
#' @return ppg dataframe, with the most recent change undone. Also removes
#' the most recent patch from the global patch list
#'
#' @noRd
undo_change <- function(data) {
  patch_list <- get_patch_list()
  last_patch_i <- length(patch_list)
  patch_to_apply <- patch_list[[last_patch_i]]
  res <- daff::patch_data(data, patch_to_apply)
  patch_list[last_patch_i] <- NULL
  set_global("global_patch_list", patch_list)
  return(res)
}

# Taxonomic subsetting ----

#' Fetch a single parent
#'
#' Internal function
#'
#' @param tax_dat Taxonomic data in DwC format
#' @param target_taxon_id taxonID of a single taxon for subsetting
#' @param current_level Counter to keep track of the number of times this
#'   function has been used in a loop
#'
#' @return Taxonomic data subset to the parent of the target taxon
#'
#' @noRd
#' @autoglobal
fetch_single_parent <- function(tax_dat, target_taxon_id, current_level = 1) {
  query_dat <-
    tax_dat |>
    dplyr::filter(taxonID == target_taxon_id) |>
    dplyr::select(taxonID = parentNameUsageID)

  assertthat::assert_that(
    nrow(query_dat) == 1,
    msg = "target_taxon_id does not match exactly one row in tax_dat"
  )

  n_ranks <- length(unique(tax_dat$taxonRank))

  if (is.na(query_dat$taxonID) || current_level > n_ranks) {
    return(NULL)
  }
  dplyr::left_join(
    query_dat,
    tax_dat,
    by = "taxonID",
    relationship = "one-to-one"
  ) |>
    dplyr::select(tidyselect::all_of(colnames(tax_dat)))
}

#' Recursively fetch all parents
#'
#' Internal function
#'
#' @param tax_dat Taxonomic data in DwC format
#' @param target_taxon_id taxonID of a single taxon for subsetting
#'
#' @return taxonIDs of all the recursive parents of the target taxon
#'
#' @noRd
#' @autoglobal
fetch_parents <- function(tax_dat, target_taxon_id) {
  all_parents_list <- list()
  i <- 0

  while (TRUE) {
    i <- i + 1
    this_parent <- fetch_single_parent(tax_dat, target_taxon_id, i)
    if (is.null(this_parent)) {
      break
    }
    all_parents_list[[i]] <- this_parent
    target_taxon_id <- this_parent$taxonID
  }

  if (length(all_parents_list) > 0) {
    res <- dplyr::bind_rows(all_parents_list) |>
      dplyr::pull(taxonID)
  } else {
    res <- NULL
  }
  res
}

#' Fetch all synonyms at the same level as the target taxon
#'
#' Internal function
#'
#' @param tax_dat Taxonomic data in DwC format
#' @param target_taxon_id taxonID of a single taxon for subsetting
#'
#' @return taxonIDs of all the synonyms of target taxon
#'
#' @noRd
#' @autoglobal
fetch_synonyms <- function(tax_dat, target_taxon_id) {
  dplyr::filter(tax_dat, taxonID == target_taxon_id) |>
    dplyr::select(acceptedNameUsageID = taxonID) |>
    dplyr::inner_join(
      tax_dat,
      by = "acceptedNameUsageID",
      relationship = "one-to-many"
    ) |>
    dplyr::pull(taxonID)
}

#' Fetch all children of the target taxon
#'
#' For PPG, this includes synonyms, since their parent name maps to the
#' parent of the accepted name
#'
#' Internal function
#'
#' @param tax_dat Taxonomic data in DwC format
#' @param target_taxon_id taxonID of a single taxon for subsetting
#'
#' @return taxonIDs of all the children of the target taxon, one level down
#'
#' @noRd
#' @autoglobal
#' @examples
#' fetch_children_one_level_single(ppg_small, "wfo-4000001234")
fetch_children_one_level_single <- function(
  tax_dat,
  target_taxon_id,
  current_level = 1
) {
  n_ranks <- length(unique(tax_dat$taxonRank))

  if (is.na(target_taxon_id) || current_level > n_ranks) {
    return(NULL)
  }

  dplyr::filter(tax_dat, parentNameUsageID == target_taxon_id) |>
    dplyr::select(tidyselect::all_of(colnames(tax_dat)))
}

fetch_children_one_level <- function(
  tax_dat,
  target_taxon_ids,
  current_level = 1
) {
  if (length(target_taxon_ids) < 1) {
    return(NULL)
  }
  purrr::map_df(
    target_taxon_ids,
    ~ fetch_children_one_level_single(tax_dat, ., current_level)
  )
}

#' Subset a taxonomic dataframe to one taxonomic group
#'
#' Internal function
#'
#' @param tax_dat Taxonomic data in DwC format
#' @param target_taxon_id taxonID of a single taxon for subsetting
#' @param out_type "vec" to return a vector; returns dataframe otherwise
#'
#' @return taxonIDs of the target taxon, all its parents, all its children,
#'   and all its synonyms for out_type "vec", or dataframe of these otherwise
#'
#' @noRd
#' @autoglobal
#' @examples
#' fetch_children(ppg_full, "wfo-4000001234")
fetch_children <- function(tax_dat, target_taxon_id, out_type = "vec") {
  all_children_list <- list()
  i <- 0
  target_taxon_ids <- target_taxon_id

  while (TRUE) {
    i <- i + 1
    this_child <- fetch_children_one_level(tax_dat, target_taxon_ids, i)
    if (is.null(this_child) || nrow(this_child) == 0) {
      break
    }
    all_children_list[[i]] <- this_child
    target_taxon_ids <- this_child |>
      dplyr::filter(taxonomicStatus == "accepted") |>
      dplyr::pull(taxonID)
  }

  if (length(all_children_list) > 0) {
    res <- dplyr::bind_rows(all_children_list)
  } else {
    return(NULL)
  }

  if (out_type == "vec") {
    res <- dplyr::pull(res, taxonID)
  }

  res
}

#' Subset a taxonomic dataframe to one taxonomic group
#'
#' Internal function
#'
#' @param tax_dat Taxonomic data in DwC format
#' @param target_taxon Name of a single taxon for subsetting
#'
#' @return taxonIDs of the target taxon, all its parents, all its children,
#'   and all its synonyms
#'
#' @autoglobal
#' @noRd
subset_to_taxon_single <- function(tax_dat, target_taxon) {
  query_dat <-
    tax_dat |>
    dplyr::filter(scientificName == target_taxon) |>
    dplyr::pull(taxonID)

  assertthat::assert_that(
    assertthat::is.string(query_dat),
    msg = "target_taxon does not match exactly one row in tax_dat"
  )

  c(
    query_dat,
    fetch_parents(tax_dat, query_dat),
    fetch_synonyms(tax_dat, query_dat),
    fetch_children(tax_dat, query_dat)
  )
}

#' Subset a taxonomic dataframe to one taxonomic group
#'
#' Internal function
#'
#' @param tax_dat Taxonomic data in DwC format
#' @param target_taxon Name of one or more taxa for subsetting
#'
#' @return taxonIDs of the target taxa, all their parents, all their children,
#'   and all their synonyms
#'
#' @autoglobal
#' @noRd
subset_to_taxon <- function(tax_dat, target_taxon) {
  target_taxon <- unique(target_taxon)
  target_taxon <- target_taxon[!is.na(target_taxon)]
  target_taxon <- target_taxon[target_taxon != ""]
  assertthat::assert_that(
    length(target_taxon) > 0,
    msg = "No valid values in target_taxon"
  )
  assertthat::assert_that(
    isTRUE(all(target_taxon %in% tax_dat$scientificName)),
    msg = "target_taxon not found in tax_dat"
  )
  target_ids <- purrr::map(
    target_taxon,
    ~ subset_to_taxon_single(tax_dat, .)
  ) |>
    unlist() |>
    unique()
  tax_dat[tax_dat$taxonID %in% target_ids, ]
}

#' Subset columns to autofill during row modification
#'
#' Internal function
#'
#' @param settings Output of settings_server(); a list of TRUE/FALSE
#'   settings
#' @param cols_fill Reactive value; vector of column names to autofill
#'   when modifying data
#' @param cols_default Vector of default column names to autofill
#' @returns Updated reactive value cols_fill
#' @autoglobal
#' @noRd
subset_cols_to_fill <- function(settings, cols_fill, cols_default) {
  observeEvent(
    settings$autofill_id(),
    {
      if (settings$autofill_id()) {
        cols_fill(cols_default)
      } else {
        cols_fill(
          cols_default[
            stringr::str_detect(
              cols_default,
              "acceptedNameUsageID|parentNameUsageID",
              negate = TRUE
            )
          ]
        )
      }
    }
  )
  cols_fill
}

#' Turn URLs into hyperlinks
#'
#' Internal function
#'
#' @param url Vector of URLs
#' @returns Vector with URLs as hyperlinks
#' @autoglobal
linkize_url_col <- function(url) {
  url <- glue::glue('<a href="{url}" target="_blank">{url}</a>') |>
    as.character()
  url[stringr::str_detect(url, "\\>NA\\<")] <- NA_character_
  url
}

#' Turn URLs into hyperlinks in the ppg dataframe
#'
#' Internal function
#'
#' @param ppg PPG datafame
#' @param url_cols Names of columns with URLs
#' @returns ppg with URL columns formatted as links
#' @autoglobal
linkize_urls <- function(ppg, url_cols) {
  ppg |>
    dplyr::mutate(dplyr::across(dplyr::all_of(url_cols), linkize_url_col))
}

#' Make an empty version of PPG
#'
#' Internal function
#'
#' @param default_ppg_cols **All** of the columns in the actual PPG
#'   tibble, in correct order
#' @return An empty PPG tibble, to show on startup (before a new session
#'   is started)
#' @noRd
#' @autoglobal
make_empty_ppg <- function(default_ppg_cols) {
  tibble::tibble(default_ppg_cols) |>
    dplyr::mutate(value = "a") |>
    tidyr::pivot_wider(names_from = 1, values_from = value) |>
    dplyr::filter(FALSE)
}

#' Randomly capitalize the letters of a string
#'
#' Single string
str_to_random_single <- function(string, n_cap = NULL) {
  string <- stringr::str_to_lower(string)
  n_chr_string <- nchar(string)
  if (is.null(n_cap)) {
    n_cap <- floor(n_chr_string / 2)
  }
  cap_index <- sample(1:n_chr_string, n_cap)
  str_separated <- stringr::str_split_1(string, "")
  str_separated[cap_index] <- stringr::str_to_upper(str_separated[cap_index])
  paste(str_separated, collapse = "")
}

#' Randomly capitalize the letters of a string
#'
#' Vectorized
#'
#' @param x Character vector
#' @param n_cap Number of characters to capitalize. Default will capitalize
#'   half (rounded down)
str_to_random <- function(x, n_cap = NULL) {
  purrr::map_chr(x, ~ str_to_random_single(.x, n_cap = n_cap))
}
