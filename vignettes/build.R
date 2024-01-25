# In order to keep the run time of R CMD check down vignettes are pre-built
# To keep the pkdown site and the package vignettes in sync the "VignetteIndexEntry"
# Has to be specified in both the ".asis" file as well as the ".Rmd" file.
# This is because pkgdown only supports ".Rmd" https://github.com/r-lib/pkgdown/issues/781


devtools::install(dependencies = FALSE)


rmarkdown::render(
    input = "./vignettes/quickstart.Rmd",
    output_dir = "./vignettes/",
    output_file = "quickstart.html"
)

rmarkdown::render(
    input = "./vignettes/stat_specs.Rmd",
    output_dir = "./vignettes/",
    output_file = "stat_specs.html"
)

rmarkdown::render(
    input = "./vignettes/advanced.Rmd",
    output_dir = "./vignettes/",
    output_file = "advanced.html"
)

rmarkdown::render(
    input = "./vignettes/CondMean_Inference.Rmd",
    output_dir = "./vignettes/",
    output_file = "CondMean_Inference.html"
)

rmarkdown::render(
    input = "./vignettes/FAQ.Rmd",
    output_dir = "./vignettes/",
    output_file = "FAQ.html"
)
