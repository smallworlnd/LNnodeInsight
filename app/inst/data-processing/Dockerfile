FROM r-base:4.1.2
RUN apt-get update \
	&& apt-get install -y \
	libglpk-dev \
	r-cran-tidyverse \
	r-cran-tidygraph \
	r-cran-jsonlite \
	r-cran-lubridate \
	r-cran-httr \
	r-cran-dbi \
	r-cran-rpostgresql \
	&& rm -rf /var/lib/apt/lists/*
RUN install2.r --error \ 
	-r 'http://cran.rstudio.com' \
	intergraph \
	ghql

COPY ["add-latest-data-to-db.R", "/opt"]
COPY [".Renviron", "/opt"]
