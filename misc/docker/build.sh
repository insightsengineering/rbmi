




docker build \
    -t gowerc/rbmi:r410 \
    --build-arg IMAGE=rocker/r-ver:4.1.0 \
    --build-arg CRANURL=https://packagemanager.rstudio.com/cran/2021-08-04 \
    . 



docker build \
    -t gowerc/rbmi:r404 \
    --build-arg IMAGE=rocker/r-ver:4.0.4 \
    --build-arg CRANURL=https://packagemanager.rstudio.com/cran/2021-04-20 \
    . 



docker build \
    -t gowerc/rbmi:r363 \
    --build-arg IMAGE=rocker/r-ver:3.6.3 \
    --build-arg CRANURL=https://packagemanager.rstudio.com/cran/2020-03-09 \
    . 


docker login

docker push gowerc/rbmi:r410

docker push gowerc/rbmi:r404

docker push gowerc/rbmi:r363

