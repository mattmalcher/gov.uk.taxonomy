# start from the rocker tidyverse image
FROM rocker/tidyverse:3.6.2

RUN R -e 'remotes::install_cran("golem")'
RUN R -e 'remotes::install_cran("urltools")'
RUN R -e 'remotes::install_cran("data.tree")'
