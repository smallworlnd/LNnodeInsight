FROM rocker/tidyverse:4.1.2
RUN apt-get update \
	&& apt-get install -y \
	libssl-dev \
	libudunits2-dev \
	libsodium23 \
	libsodium-dev \
	libgdal-dev \
	libglpk-dev \
	&& rm -rf /var/lib/apt/lists/*
RUN R -e "install.packages(c('lubridate', 'httr', 'stringi', 'sodium', 'DBI', 'RPostgreSQL', 'pool', 'plumber'), dependencies=TRUE)" \
	&& strip /usr/local/lib/R/site-library/*/libs/*.so \
	&& rm -rf /tmp/downloaded_packages/ /tmp/*.rds

COPY ["./", "./"]

EXPOSE 8080
ENTRYPOINT ["Rscript", "server.R"]
