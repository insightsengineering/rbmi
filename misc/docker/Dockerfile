
FROM rocker/r-ver:latest

ENV REQD_PKGS="\
    libicu-dev \
    g++ \
    gcc \
    gfortran \
    gsfonts \
    zlib1g-dev \
    make \
    libblas-dev \
    libssl-dev \
    pandoc \
    libxml2-dev \
    libcurl4-openssl-dev\
    git\
    vim\
    curl\
    libgit2-dev\
    libv8-dev\
    libsodium-dev\
    libgsl-dev\
    libpng-dev\
    libfontconfig1-dev\
    libopenblas-dev\
    liblapack-dev\
    libgmp3-dev\
    libfribidi-dev\
    libharfbuzz-dev\
    libjpeg-dev\
    libfreetype6-dev\
    libtiff5-dev \
    wget\
    gdebi-core \
    cargo\
    qpdf \
    devscripts \
    cmake \
    libpq-dev \
    "

RUN apt-get update -y --fix-missing && \
    apt-get upgrade -y && \
    apt-get install -y $REQD_PKGS

ADD misc/docker/install_packages.R /
ADD DESCRIPTION /

RUN Rscript  /install_packages.R


ENV TINYTEX_INSTALLER=TinyTeX
RUN wget -qO- "https://yihui.org/tinytex/install-bin-unix.sh" | sh && \
    ln -s ~/.TinyTeX/bin/*/makeindex  /usr/local/bin
