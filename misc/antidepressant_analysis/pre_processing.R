# install.packages("httr")
# install.packages("haven")

library(httr)
library(haven)
library(dplyr)

### URL to download the zip file from
link <- "https://lshtm.sharepoint.com/sites/MissingDataPublicFiles/_layouts/15/download.aspx?SourceUrl=%2Fsites%2FMissingDataPublicFiles%2FShared%20Documents%2Ffiles%5Ffor%5Fsharepoint%2FExample%5Fdata%5Fsets%2Fantidepressant%2FMBSW2011%5Fexample%2Ezip"

### Grab the files binary contents
resp <- GET(link)
x <- content(resp, type = "raw")

### Save zip file to a temporary directory
tdir <- tempdir()
tfile <- file.path(tdir, "temp.zip")
tmp <- file(tfile, raw = TRUE, open='w+b')
writeBin(x, tmp)
close.connection(tmp)

### Extract zip file
unzip(zipfile = tfile,  overwrite= TRUE, exdir = tdir)

### Identify extracted sas dataset
sasfile <- list.files(tdir, pattern = "sas7bdat$", full.names = TRUE)

### Import sas dataset into R
data <- read_sas(sasfile)

### pre-processing: continuous variables as numeric and categorical variables as factor
### additionally, change all column names to have capital letters
data <- data %>%
    mutate_at(
        vars(c(PATIENT, VISIT, THERAPY)),
        as.factor
    ) %>%
    mutate_at(
        vars(c(basval, change)),
        as.numeric
    ) %>%
    rename(
        BASVAL = basval,
        CHANGE = change
    )

# save(data, file = "antidepressant_data.RData")
