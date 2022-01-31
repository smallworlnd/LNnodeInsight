FROM rocker/tidyverse:3.6.3 AS base
RUN R -e 'install.packages(c("tidygraph", "jsonlite", "igraph", "lubridate", "intergraph", "httr", "DBI", "RPostgreSQL"))'
COPY .Renviron /opt/

FROM base AS data-processing
COPY add-latest-data-to-db.R /opt/
CMD ["Rscript", "/opt/add-latest-data-to-db.R"]
