FROM r-base:3.4.0

RUN apt-get update -qq \
	&& apt-get install --no-install-recommends -y \
	libcurl4-gnutls-dev libssl-dev 

MAINTAINER Tobias Schmidt "tobias.k.schmidt@tum.de"

EXPOSE 8787

WORKDIR /srv/shiny/


RUN Rscript -e "install.packages('https://cran.r-project.org/src/contrib/Archive/packrat/packrat_0.4.9-1.tar.gz')"

COPY ./packrat /srv/shiny/packrat
RUN Rscript -e "packrat::restore('/srv/shiny/')"

RUN apt-get install -y libxml2-dev

RUN Rscript -e "source('packrat/init.R'); source('https://bioconductor.org/biocLite.R'); BiocInstaller::biocLite(c('Biobase')); install.packages('devtools'); require(devtools); devtools::install_github('kusterlab/curveClassification_package', upgrade_dependencies=FALSE, dependencies=TRUE)"




COPY shiny.sh /usr/bin/shiny.sh
COPY www /srv/shiny/www
COPY helper /srv/shiny/helper
COPY ui.R /srv/shiny/ui.R
COPY server.R /srv/shiny/server.R
COPY configuration.R /srv/shiny/configuration.R
COPY .Rprofile /srv/shiny/.Rprofile

# necessary for docker on Windows
# https://github.com/docker/labs/issues/215
RUN apt-get update -qq \
	&& apt-get install --no-install-recommends -y \
	dos2unix
RUN dos2unix /usr/bin/shiny.sh

CMD ["/usr/bin/shiny.sh"]

