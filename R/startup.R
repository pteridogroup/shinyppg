# Define vectors used in the app ----

# Valide Darwin Core (DwC) Taxon terms (columns)
dct_terms <- dwctaxon::dct_terms

# Columns to display in the main ppg dataframe
cols_select <- c(
  "scientificName",
  "scientificNameAuthorship",
  "taxonRank",
  "taxonomicStatus",
  "nomenclaturalStatus",
  "namePublishedIn",
  "acceptedNameUsage",
  "parentNameUsage",
  "taxonID",
  "acceptedNameUsageID",
  "parentNameUsageID",
  "modified"
  # TODO: Add these columns when they are available in the data source:
  # "taxonRemarks",
  # "created"
)

# Valid values to use for taxonomicStatus
valid_tax_status <- c(
  "accepted",
  "synonym",
  "ambiguous synonym",
  "variant"
)

# Valid values to use for taxonomicRank
valid_tax_rank <- c(
  "species",
  "genus",
  "tribe",
  "subfamily",
  "family",
  "order",
  "form",
  "subspecies",
  "variety"
)

# Create environment to store data across functions (used for patch list)
pkg_env <- new.env(parent = emptyenv())

# Need this to establish dep on markdown, which is required for
# htmltools::includeMarkdown()
markdown_text <- markdown::mark("hello world")
