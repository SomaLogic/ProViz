FROM rocker/shiny:4.0.3

RUN apt-get update && apt-get install libcurl4-openssl-dev libssl-dev -y

RUN R -e "install.packages('devtools')"
RUN R -e "devtools::install_version('DT', version = '0.17', repos = 'http://cran.us.r-project.org')"
RUN R -e "devtools::install_version('dplyr', version = '1.0.5', repos = 'http://cran.us.r-project.org')"
RUN R -e "devtools::install_version('ggbeeswarm', version = '0.6.0', repos = 'http://cran.us.r-project.org')"
RUN R -e "devtools::install_version('ggplot2', version = '3.3.3', repos = 'http://cran.us.r-project.org')"
RUN R -e "devtools::install_version('magrittr', version = '2.0.1', repos = 'http://cran.us.r-project.org')"
RUN R -e "devtools::install_version('plotly', version = '4.9.3', repos = 'http://cran.us.r-project.org')"
RUN R -e "devtools::install_version('readr', version = '1.4.0', repos = 'http://cran.us.r-project.org')"
RUN R -e "devtools::install_version('shiny', version = '1.6.0', repos = 'http://cran.us.r-project.org')"
RUN R -e "devtools::install_version('shinydashboard', version = '0.7.1', repos = 'http://cran.us.r-project.org')"
RUN R -e "devtools::install_version('shinyWidgets', version = '0.6.0', repos = 'http://cran.us.r-project.org')"
RUN R -e "devtools::install_version('tidyr', version = '1.0.2', repos = 'http://cran.us.r-project.org')"
RUN R -e "devtools::install_version('usethis', version = '1.6.0', repos = 'http://cran.us.r-project.org')"

RUN R -e "devtools::install_github('Somalogic/SomaDataIO@v5.0.0')"

RUN mkdir /srv/shiny-server/ProViz
COPY global.R /srv/shiny-server/ProViz/.
COPY ui.R /srv/shiny-server/ProViz/.
COPY server.R /srv/shiny-server/ProViz/.

RUN mkdir /srv/shiny-server/ProViz/www
COPY www /srv/shiny-server/ProViz/www/.

RUN chmod -R 755 /srv/shiny-server/

EXPOSE 3838

CMD ["/usr/bin/shiny-server"] 
