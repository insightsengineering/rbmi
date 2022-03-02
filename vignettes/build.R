devtools::install()


rmarkdown::render(
    input = "./vignettes/_quickstart.Rmd",
    output_dir = "./vignettes/",
    output_file = "quickstart.html"
)

rmarkdown::render(
    input = "./vignettes/_stat_specs.Rmd",
    output_dir = "./vignettes/",
    output_file = "stat_specs.html"
)

rmarkdown::render(
    input = "./vignettes/_advanced.Rmd",
    output_dir = "./vignettes/",
    output_file = "advanced.html"
)

