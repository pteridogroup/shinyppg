# shinyppg

A [Shiny](https://shiny.posit.co/) app to view the [Pteridophyte Phylogeny Group (PPG)](https://pteridogroup.github.io/) taxonomic database.

The app can be accessed at: <https://pteridogroup.shinyapps.io/shinyppg/>

## Installation

### General usage

Install from GitHub:

```r
# install.packages("remotes")
remotes::install_github("pteridogroup/shinyppg")
```

### Development

Developers can install a local version from the project root:

```r
# install.packages("devtools")
devtools::install()
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

A scheduled GitHub Actions workflow
([`.github/workflows/update-ppg-and-deploy.yml`](.github/workflows/update-ppg-and-deploy.yml))
runs once a day and handles both updating the PPG data and deploying the app
automatically (see [Development](#development) below for details). It can also
be triggered manually from the
[Actions tab](https://github.com/pteridogroup/shinyppg/actions/workflows/update-ppg-and-deploy.yml).

To deploy manually instead, use `rsconnect::deployApp()` (requires
setting up an account on [shinyapps.io](https://www.shinyapps.io/) and
[authenticating](https://docs.rstudio.com/shinyapps.io/getting-started.html#configure-rsconnect)).

## Development

### Updating the PPG Version

Updating to a new PPG release version is automated: the daily GitHub Actions
workflow checks the [latest PPG release](https://github.com/pteridogroup/ppg/releases/latest),
and if it's newer than the version in `data-raw/ppg_version.R`, it updates the
version, regenerates the data, commits the changes, and redeploys the app. No
manual action is needed.

To update manually instead:

1. Edit `data-raw/ppg_version.R` and change the version string to the
new version number (e.g., "0.0.0.9003")
2. Run `Rscript data-raw/ppg_version.R` to save the version data object
3. Run `Rscript data-raw/ppg_full.R` to download and process the new
version
4. The app heading will automatically display the updated version
