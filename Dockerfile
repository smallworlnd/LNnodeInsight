FROM rocker/shiny-verse:3.6.3
RUN apt-get --allow-releaseinfo-change update && apt-get install -y \
	libssl-dev \
	libudunits2-dev \
	libgdal-dev \
	# clean up
	&& apt-get clean \
	&& rm -rf /var/lib/apt/lists/*

RUN R -e 'install.packages(c("dbplyr", "tidygraph", "jsonlite", "igraph", "sna", "lubridate", "intergraph", "shiny", "shinyWidgets", "shinydashboard", "shinycssloaders", "shinyjs", "htmlwidgets", "htmltools", "bsplus", "httr", "plotly", "rclipboard", "stringi", "DBI", "RPostgreSQL", "ggVennDiagram"))'
RUN R -e 'devtools::install_github(c("christophergandrud/networkD3", "hrbrmstr/qrencoder", "leonawicz/apputils"))'

RUN install2.r --error \
	-r 'http://cran.rstudio.com' \
	googleAuthR \
	## install Github packages
	## clean up
	&& rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# copy our local shiny app
COPY . /srv/shiny-server/lnnodeinsight/
# select the port
EXPOSE 3838
CMD ["/usr/bin/shiny-server.sh"]
