FROM rocker/shiny-verse:4.1.2
RUN apt-get --allow-releaseinfo-change update && apt-get install -y \
	libssl-dev \
	libudunits2-dev \
	libgdal-dev \
	# clean up
	&& apt-get clean \
	&& rm -rf /var/lib/apt/lists/*

RUN install2.r --error \ 
	-r 'http://cran.rstudio.com' \
	dbplyr \
	tidygraph \
	igraph \
	lubridate \
	intergraph \
	jsonlite \
	ggVennDiagram \
	shinyWidgets \
	shinydashboard \
	shinycssloaders \
	shinyjs \
	htmlwidgets \
	htmltools \
	bsplus \
	httr \
	plotly \
	rclipboard \
	stringi \
	DBI \
	RPostgreSQL \
	pool \
	## install Github packages
	## clean up
	&& rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN R -e 'devtools::install_github(c("christophergandrud/networkD3", "hrbrmstr/qrencoder", "leonawicz/apputils", "MarkEdmondson1234/googleCloudRunner"))'

# copy our local shiny app
COPY . /srv/shiny-server/
COPY ./conf/shiny-customized.config /etc/shiny-server/shiny-server.conf
# select the port
EXPOSE 3838
USER shiny
CMD ["/usr/bin/shiny-server"]
