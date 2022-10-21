ARG IMAGE

FROM ${IMAGE}

ARG CRANURL
ARG MMRM_VERSION

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
    libgit2-dev\
    pandoc-citeproc\
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
    qpdf \
    devscripts \
    cmake \
    "

RUN apt-get update -y --fix-missing && \
    apt-get upgrade -y && \
    apt-get install -y $REQD_PKGS

ADD install_packages.R /

RUN Rscript  /install_packages.R


RUN wget -qO- "https://yihui.org/tinytex/install-unx.sh" | sh -s - --admin --no-path && \
    ~/.TinyTeX/bin/*/tlmgr path add && \
    tlmgr install makeindex && \
    ln -s ~/.TinyTeX/bin/*/makeindex  /usr/local/bin
