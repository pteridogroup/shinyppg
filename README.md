# shinyppg

A Shiny app to view the Pteridophyte Phylogeny Group (PPG) taxonomic database.

## Installation

Install from GitHub:

```r
# install.packages("remotes")
remotes::install_github("pteridogroup/shinyppg")
```

## Usage

Launch the app with:

```r
library(shinyppg)
ppg_app()
```

The app provides a viewer interface to browse and explore PPG taxonomic data. You can filter by order, family, or genus to view specific taxonomic groups.

## Deploying

The app is deployed on [shinyapps.io](https://www.shinyapps.io/).

To deploy, use `rsconnect::deployApp()` (requires
setting up an account on shinyapps.io and
[authenticating](https://docs.rstudio.com/shinyapps.io/getting-started.html#configure-rsconnect)).

## Development

### Updating the PPG Version

To update to a new PPG release version:

1. Edit `data-raw/ppg_version.R` and change the version string to the
new version number (e.g., "0.0.0.9003")
2. Run `Rscript data-raw/ppg_version.R` to save the version data object
3. Run `Rscript data-raw/ppg_full.R` to download and process the new
version
4. The app heading will automatically display the updated version
