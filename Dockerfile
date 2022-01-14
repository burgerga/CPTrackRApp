FROM rocker/tidyverse:4.1.2

RUN R -e 'install.packages("pak", repos = "https://r-lib.github.io/p/pak/dev/")'

RUN R -e 'pak::pkg_install("burgerga/CPTrackRapp")'

RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" >> /usr/local/lib/R/etc/Rprofile.site

EXPOSE 3838

CMD ["R", "-e", "CPTrackRApp::run_app()"]
