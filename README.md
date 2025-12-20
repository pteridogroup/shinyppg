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

The app is deployed using <https://ploomber.io/>

More details to come...

## Development

To develop the app within the docker container, run:

(variables `GITHUB_USER` and `GITHUB_TOKEN` must be provided for git functions to work)

```
docker run --rm -it \
  -v ${PWD}:/srv/shiny-server \
  -w /srv/shiny-server \
  -e USERID=$(id -u) \
  -e GROUPID=$(id -g) \
  -e GITHUB_USER=${GITHUB_USER} \
  -e GITHUB_TOKEN=${GITHUB_TOKEN} \
  -p 3838:3838 \
  joelnitta/shinyppg:latest bash
```

Inside the container, run `/usr/bin/shiny-start.sh` to start the shiny app.

Attach a VS Code session to the running container (for some reason, if you don't do this, the env vars won't get passed correctly (T_T)​ ​ )

Navigate to <http://localhost:3838/> to access the app.

Kill the shiny-server with ctrl+c and re-run it as necessary during development to refresh with the latest code.

Another terminal window (or VS Code session) can be opened inside the container for development.

Inspect logs in the Docker container at `/var/log/shiny-server/`