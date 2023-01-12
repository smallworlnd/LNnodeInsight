FROM rocker/tidyverse:4.1.2

RUN apt-get update && apt-get install -y libglpk-dev \
	&& rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('jsonlite', 'lubridate', 'httr', 'DBI', 'RPostgreSQL', 'ghql', 'pool', 'intergraph', 'tidygraph'), dependencies=TRUE)" \
	&& R -e "devtools::install_version('igraph', version='1.2.6')" \
	&& strip /usr/local/lib/R/site-library/*/libs/*.so

COPY [".", "."]
