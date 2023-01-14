FROM rocker/shiny-verse:4.1.2
RUN apt-get --allow-releaseinfo-change update && apt-get install -y \
	libssl-dev \
	libudunits2-dev \
	libgdal-dev \
	libglpk-dev \
	python3-pip \
	# clean up
	&& apt-get clean \
	&& rm -rf /var/lib/apt/lists/*

# r shiny stack
RUN R -e "devtools::install_version('ggplot2', version='3.3.5')"
RUN R -e "install.packages(c('igraph', 'dbplyr', 'tidygraph', 'lubridate', 'intergraph', 'jsonlite', 'ggVennDiagram', 'shinyWidgets', 'shinydashboard', 'shinycssloaders', 'shinyjs', 'htmlwidgets', 'htmltools', 'bsplus', 'httr', 'plotly', 'rclipboard', 'stringi', 'DBI', 'RPostgreSQL', 'pool', 'callr', 'gamlss', 'reticulate'), dependencies=TRUE)" \
	&& strip /usr/local/lib/R/site-library/*/libs/*.so \
	&& rm -rf /tmp/downloaded_packages/ /tmp/*.rds
RUN R -e 'devtools::install_github(c("christophergandrud/networkD3", "hrbrmstr/qrencoder", "leonawicz/apputils", "MarkEdmondson1234/googleCloudRunner"))'

# python dependencies
RUN pip3 install codext

# copy our local shiny app
COPY . /srv/shiny-server/
COPY ./conf/shiny-customized.config /etc/shiny-server/shiny-server.conf
# select the port
EXPOSE 3838
USER shiny
CMD ["/usr/bin/shiny-server"]
