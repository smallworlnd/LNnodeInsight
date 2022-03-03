FROM r-base:4.1.2
RUN apt-get update \
	&& apt-get install -y \
	libglpk-dev \
	r-cran-tidyverse \
	r-cran-igraph \
	r-cran-tidygraph \
	r-cran-dbi \
	r-cran-rpostgresql \
	r-cran-plumber \
	&& rm -rf /var/lib/apt/lists/*
RUN install2.r --error \ 
	-r 'http://cran.rstudio.com' \
	intergraph \
	pool

COPY ["./", "./"]

EXPOSE 8000
ENTRYPOINT ["Rscript", "server.R"]
CMD ["api.R"]
