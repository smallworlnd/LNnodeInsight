# Lightning Network node insight
An `R` Shiny application to interact with Lightning Network graph data.

# Dependencies
This application runs on `R` version 4.0.5 with the following package dependencies installed from CRAN:

```
tidyverse
tidygraph
jsonlite
igraph
sna
lubridate
intergraph
networkD3
shiny
shinyWidgets
shinythemes
shinydashboard
shinycssloaders
shinyjs
htmlwidgets
htmltools
bsplus
plotly
DT
```

## Running locally

- Install `git`
- Install R: https://cran.r-project.org/bin/
- Install R dependencies
```
Rscript -e install.packages(c("tidyverse", "tidygraph", "jsonlite", "igraph", "sna", "lubridate", "intergraph", "networkD3", "shiny", "shinyWidgets", "shinythemes", "shinydashboard", "shinycssloaders", "shinyjs", "htmlwidgets", "htmltools", "bsplus", "plotly", "DT"))
```
- Clone repo, run the local shiny server:
```
git clone https://github.com/smallworlnd/LNnodeInsight
cd LNnodeInsight
Rscript app.R
```
