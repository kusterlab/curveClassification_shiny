FROM r-base:3.4.2

RUN apt-get update -qq \
	&& apt-get install --no-install-recommends -y \
	libcurl4-gnutls-dev libssl-dev 

MAINTAINER Tobias Schmidt "tobias.k.schmidt@tum.de"

EXPOSE 8787

WORKDIR /srv/shiny/


RUN Rscript -e "install.packages('https://cran.r-project.org/src/contrib/Archive/packrat/packrat_0.4.9-1.tar.gz')"

COPY ./packrat /srv/shiny/packrat
RUN Rscript -e "packrat::restore('/srv/shiny/')"

RUN Rscript -e "source('packrat/init.R'); install.packages('devtools'); require(devtools); devtools::install_github('kusterlab/curveClassification_package', upgrade_dependencies=FALSE, dependencies=TRUE)"




COPY shiny.sh /usr/bin/shiny.sh
COPY www /srv/shiny/www
COPY helper /srv/shiny/helper
COPY ui.R /srv/shiny/ui.R
COPY server.R /srv/shiny/server.R
COPY configuration.R /srv/shiny/configuration.R
COPY .Rprofile /srv/shiny/.Rprofile

CMD ["/usr/bin/shiny.sh"]

