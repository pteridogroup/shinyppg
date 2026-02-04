## code to prepare `ppg_full` dataset goes here

# Load full PPG dataset from released version and add higher taxonomy columns
# This script should be run when updating to a new PPG release

library(dplyr)
library(readr)

# Load package functions
devtools::load_all()

# Download released version
temp_zip <- tempfile(fileext = ".zip")
temp_dir <- tempdir()

download.file(
  "https://github.com/pteridogroup/ppg/archive/refs/tags/v0.0.0.9002.zip",
  destfile = temp_zip
)

unzip(temp_zip, exdir = temp_dir)

# Read the CSV from the extracted folder
csv_path <- file.path(temp_dir, "ppg-0.0.0.9002", "data", "ppg.csv")
ppg_full <- readr::read_csv(
  csv_path,
  col_types = readr::cols(.default = readr::col_character())
) |>
  as.data.frame()

# Clean up
unlink(temp_zip)
unlink(file.path(temp_dir, "ppg-0.0.0.9002"), recursive = TRUE)

# Remove spec attribute
attributes(ppg_full)$spec <- NULL

# Remove obsolete columns if they exist (for backward compatibility)
obsolete_cols <- c("ipniURL", "modifiedBy", "modifiedByID", "created")
ppg_full <- ppg_full[, !names(ppg_full) %in% obsolete_cols, drop = FALSE]

# Generate acceptedNameUsage and parentNameUsage columns
cat("Adding acceptedNameUsage and parentNameUsage columns...\n")
ppg_full <- ppg_full |>
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

# Add higher taxonomy columns (this may take a while for large datasets)
cat("Adding higher taxonomy columns (this may take a few minutes)...\n")
ppg_full <- add_higher_taxonomy(ppg_full)

cat("Processing complete. Dataset has", nrow(ppg_full), "rows.\n")

# Save as compressed RDA file
# Using xz compression for best compression ratio
usethis::use_data(ppg_full, overwrite = TRUE, compress = "xz")

cat("Data saved to data/ppg_full.rda\n")
