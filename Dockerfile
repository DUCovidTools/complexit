FROM rocker/shiny:3.6.3

# Install any dependencies via apt-get
 RUN apt-get update && apt-get install libssl-dev -y

# Make sure the bookmark folder is in place
RUN mkdir -p /var/lib/shiny-server/bookmarks/shiny

# Download and install R dependencies
RUN R -e "install.packages(c('shinydashboard', 'shinyjs', 'shinyWidgets', 'ggplot2', 'ggrepel', 'dplyr', 'tidyr', 'timeDate', 'tidyselect', 'markdown', 'lmvar', 'mvtnorm', 'Cairo'))"

# Copy the app folder to the image
COPY App /srv/shiny-server/

# make all app files readable (solves issue when dev in Windows, but building in Ubuntu)
RUN chmod -R 755 /srv/shiny-server/

# Expose port 3838 (default port for shiny server)
EXPOSE 3838

# Run shiny-server
CMD ["/usr/bin/shiny-server.sh"]
