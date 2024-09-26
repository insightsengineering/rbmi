# Contributing to `rbmi`

This file outlines how to propose and make changes to `rbmi` as well as providing details about more obscure aspects of the package’s development process.


## Setup

In order to develop or contribute to `rbmi` you will need to access to a C/C++ compiler. If you are on Windows you should install [rtools](https://cran.r-project.org/bin/windows/Rtools/rtools42/rtools.html) or if you are on macOS you should install [Xcode](https://apps.apple.com/us/app/xcode/id497799835?mt=12).  Likewise, you will also need to install all of the package's development dependencies. This can be done by launching R from within the project root and executing:

```
devtools::install_dev_deps()
```

## Code changes

If you want to make a code contribution, it's a good idea to first file an issue and make sure someone from the team agrees that it’s needed. 
If you’ve found a bug, please file an issue that illustrates the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed).

### Pull request process

* This project uses a simple GitHub flow model for development. That is, code changes should be done in their own feature branch based off of the `main` branch and merged back into the `main` branch once complete.

* Pull Requests will not be accepted unless all CI/CD checks have passed. (See the CI/CD section for more information).

* Pull Requests relating to any of the package's core R code must be accompanied by a corresponding unit test. Any pull requests containing changes to the core R code that do not contain a unit test to demonstrate that it is working as intended will not be accepted. (See the Unit Testing section for more information).

* Pull Requests should add a few lines about what has been changed to the `NEWS.md` file.


### Coding Considerations

*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.  

* Please ensure your code conforms to [lintr](https://cran.r-project.org/web/packages/lintr/readme/README.html#available-linters). You can check this by running `lintr::lint("FILE NAME")` on any files you have modified and ensuring that the findings are kept to as few as possible. We do not have any hard requirements on following `lintr`'s conventions but do encourage developers to follow its guidance as closely as possible.

* This project uses 4 space indents, contributions not following this will not be accepted. 

* This project makes use of [S3](https://adv-r.hadley.nz/s3.html) and [R6](https://adv-r.hadley.nz/r6.html) for OOP. Usage of S4 and other OOP systems should be avoided unless absolutely necessary to ensure consistency. Having said that it is recommended to stick to S3 unless modification in place or other R6 specific features are required.

* The current desire of this package is to keep the dependency tree as small as possible. To that end you are discouraged from adding any additional packages to the "Depends" / "Imports" section unless absolutely essential. If you are importing a package just to use a single function then consider just copying the source code of that function instead, though please check the licence and include proper attribution/notices. There are no such expectations for "Suggests" and you are free to use any package in the vignettes / unit tests, though again please be mindful to not be unnecessarily excessive with this.


## Unit Testing & CI/CD

This project uses `testthat` to perform unit testing in combination with GitHub Actions for CI/CD.

### Scheduled Testing

Due to the stochastic nature of this package some unit tests take a considerable amount of time to execute. To avoid issues with usability, unit tests that take more than a couple of seconds to run should be deferred to the scheduled testing. These are tests that are only run occasionally on a periodic basis (currently twice a month) and not on every pull request / push event.

To defer a test to the scheduled build simply include `skip_if_not(is_full_test())` to the top of the `test_that()` block i.e.

```r
test_that("some unit test", {
    skip_if_not(is_full_test())
    expect_equal(1,1)
})
```

The scheduled tests can also be manually activated by going to "https://github.com/insightsengineering/rbmi" -> "Actions" -> "Bi-Weekly" -> "Run Workflow". It is advisable to do this before releasing to CRAN.

### CRAN Releases

In order to release a package to CRAN it needs to be tested across multiple different OS's and versions of R. This has been implemented in this project via a GitHub Action Workflow titled "Check for CRAN" which needs to be manually activated. To do this go to "https://github.com/insightsengineering/rbmi" -> "Actions" -> "Check for CRAN" -> "Run Workflow".

If all these tests pass then the package can be safely released to CRAN (after updating the relevant `cran-comments.md` file)


### Docker Images

To support CI/CD, in terms of reducing setup time, a Docker images has been created which contains all the packages and system dependencies required for this project. The image can be found at:

- ghcr.io/insightsengineering/rbmi:latest

This image is automatically re-built once a month to contain the latest version of R and its packages. The code to create this images can be found in `misc/docker`.


### Reproducibility, Print Tests & Snaps

A particular issue with testing this package is reproducibility. For the most part this is handled well via `set.seed()` however `stan`/`rstan` does [not guarantee reproducibility](https://mc-stan.org/docs/2_29/reference-manual/reproducibility.html) even with the same seed if run on different hardware.

This issue surfaces itself when testing the print messages of the `pool` object which displays treatment estimates which are thus not identical when run on different machines. To address this issue pre-made `pool` objects have been generated and stored in `R/sysdata.rda` (which itself is generated by `data-raw/create_print_test_data.R`). The generated print messages are compared to expected values which are stored in `tests/testthat/_snaps/` (which themselves are automatically created by `testthat::expect_snapshot()`)


## Fitting MMRM's

This package currently uses the `mmrm` package to fit MMRM models. This package is still fairly
new but has so far proven to be very stable, fast and reliable. If you do spot any issues
with the MMRM package please do raise them in the corresponding GitHub Repository - [link](https://github.com/openpharma/mmrm/issues)

As the `mmrm` package uses `TMB` it is not uncommon to see warnings about either inconsistent
versions between what `TMB` and the `Matrix` package were compiled as. In order to resolve this
you may wish to re-compile these packages from source using:

```
install.packages(c("TMB", "mmrm"), type = "source")
``` 

Note that you will need to have [rtools](https://cran.r-project.org/bin/windows/Rtools/rtools42/rtools.html) installed if you are on a Windows machine or [Xcode](https://apps.apple.com/us/app/xcode/id497799835?mt=12) if you are running macOS (or somehow else have access to a C/C++ compiler).


## rstan

The Bayesian models fitted by this package are implemented via [stan/rstan](https://mc-stan.org/users/interfaces/rstan). The code for this can be found in `inst/stan/MMRM.stan`. Note that the package will automatically take care of compiling this code when you install it or run `devtools::load_all()`. Please note that the package won't recompile the code unless you have changed the source code or you delete the `src` directory.


## Vignettes

CRAN imposes a 10-minute run limit on building, compiling and testing your package. To keep to this limit the vignettes are pre-built; that is to say that simply changing the source code will not automatically update the vignettes, you will need to manually re-build them.

To do this you need to run:
```
Rscript vignettes/build.R
```

Once re-built you will then need to commit the updated `*.html` files to the git repository.

For reference this static vignette process works by using the "asis" vignette engine provided by `R.rsp`. This works by getting R to only recognise vignettes as files ending in `*.html.asis`; it then builds them by simply copying the corresponding files ending in `*.html` to the relevent `docs/` folder in the built package.

## Misc & Local Folders

The `misc/` folder in this project is used to hold useful scripts, analyses, simulations & infrastructure code that we wish to keep but isn’t essential to the build or deployment of the package. Feel free to store additional stuff in here that you feel is worth keeping.

Likewise, `local/` has been added to the `.gitignore` file meaning anything stored in this folder won't be committed to the repository. For example, you may find this useful for storing personal scripts for testing or more generally exploring the package during development.

