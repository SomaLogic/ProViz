FROM rocker/shiny:4.0.3

RUN apt-get update && apt-get install libcurl4-openssl-dev -y

RUN R -e "install.packages(c('devtools', \
                             'DT', \
                             'dplyr', \
                             'ggbeeswarm', \
                             'ggplot2', \
                             'magrittr', \
                             'plotly', \
                             'readr', \
                             'shinydashboard', \
                             'shinyWidgets', \
                             'usethis' \
))"

COPY SomaDataIO.tar.gz /opt/.
RUN cd /opt
RUN tar -xf /opt/SomaDataIO.tar.gz
RUN R --vanilla CMD INSTALL SomaDataIO

RUN mkdir /srv/shiny-server/ProViz
COPY global.R /srv/shiny-server/ProViz/.
COPY ui.R /srv/shiny-server/ProViz/.
COPY server.R /srv/shiny-server/ProViz/.

RUN mkdir /srv/shiny-server/ProViz/www
COPY www /srv/shiny-server/ProViz/www/.

RUN chmod -R 755 /srv/shiny-server/

EXPOSE 3838

CMD ["/usr/bin/shiny-server"] 
