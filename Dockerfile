FROM rocker/tidyverse:4.4.1

# Install Python
RUN apt-get update && apt-get install -y python3 python3-pip

# Make a folder to hold model files
RUN mkdir /app

# Copy your model files
COPY src /app

# Install python packages as defined in a requirements file
RUN pip install -r /app/software/requirements.txt

# Install Renv
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"

# Restore the exact package versions from the lockfile
RUN R -e 'renv::restore(lockfile = "/app/software/renv.lock")'

# Change working directory
WORKDIR /app

CMD ["/bin/sh", "./scripts/run_scripts.sh"]