## About `shinyppg`

`shinyppg` is a [Shiny app](https://shiny.posit.co/) for viewing the [Pteridophyte Phylogeny Group (PPG)](https://pteridogroup.github.io/) taxonomic system for ferns and lycophytes.

## Using the Viewer

The PPG taxonomic database contains over 60,000 rows of taxonomic data. The viewer provides an interface to browse and explore this data efficiently.

### Browsing Data

The main panel displays the PPG taxonomic database in a sortable, searchable table format. You can:

- **Sort columns**: Click on any column header to sort by that column
- **Search**: Use the search box above the table to find specific taxa
- **Select rows**: Click on rows to view details

### Filtering by Taxonomic Group

To narrow down the data to specific taxonomic groups, use the filters in the left sidebar:

1. **Filter by Order**: Select one or more orders from the dropdown
2. **Filter by Family**: Select one or more families
3. **Filter by Genus**: Select one or more genera

After making your selection, click **"Subset data"** to apply the filters.

Click **"Reset"** to return to viewing the complete dataset.

### Understanding the Data

The data follows the [Darwin Core Taxon (DwC Taxon) standard](https://dwc.tdwg.org/terms/#taxon). Key columns include:

- **scientificName**: The full scientific name including author
- **scientificNameAuthorship**: The authorship of the name
- **taxonRank**: The taxonomic rank (species, genus, family, etc.)
- **taxonomicStatus**: Whether the name is accepted or a synonym
- **nomenclaturalStatus**: The nomenclatural status (e.g., valid)
- **namePublishedIn**: The original publication of the name
- **acceptedNameUsage**: For synonyms, the accepted name
- **parentNameUsage**: The parent taxon in the hierarchy

### Table Controls

Below the table you'll find buttons to:

- **Select All**: Select all visible rows
- **Select None**: Clear row selection
- **Show advanced columns**: Toggle visibility of additional data columns

### Performance Tips

For better performance when working with large subsets:

- Use the filters to narrow down to specific taxonomic groups
- Limit searches to specific columns when possible
- Be patient when loading the full dataset (60,000+ rows)

You can filter the data by typing into the text entry boxes above each column, or by using the search bar.

Note that while you have filtered the data, any selected rows stay selected (even though you can't see them). By combining selecting and filtering, you can select a very specific set of rows (for example, all accepted species in each of two genera, etc.).

## About Advanced Columns

The PPG database includes several technical columns that provide additional
information:

- **taxonID**: A unique identifier code for each row
- **acceptedNameUsageID**: Links synonyms to their accepted name
- **parentNameUsageID**: Links taxa to their parent in the hierarchy
- **created**: The date when the record was created
- **modified**: The date when the record was last modified

These columns ensure accurate linking between related taxa. While
`scientificName` should theoretically be unique, historical botanical naming
means some names have been published multiple times by the same author,
necessitating unique identifiers.

The `acceptedNameUsage` and `parentNameUsage` columns are automatically
generated from the corresponding ID columns to provide human-readable names.

Click the **"Show advanced columns"** button below the table to toggle visibility of these technical columns.

## Data Source

The PPG data is maintained at <https://github.com/pteridogroup/ppg> and follows the [Darwin Core Taxon (DwC Taxon) standard](https://dwc.tdwg.org/terms/#taxon).

For more information about Darwin Core Taxon format, see the [`dwctaxon` R package documentation](https://docs.ropensci.org/dwctaxon/articles/what-is-dwc.html).

## About PPG

The Pteridophyte Phylogeny Group (PPG) is an international collaborative system for pteridophyte (fern and lycophyte) classification. For more information, visit <https://pteridogroup.github.io/>.