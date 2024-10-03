
#' Create a rbmi ready cluster
#'
#' @param ncores Number of parallel processes to use or an existing cluster to make use of
#' @param objects a named list of objects to export into the sub-processes
#' @param packages a character vector of libraries to load in the sub-processes
#'
#' This function is a wrapper around `parallel::makePSOCKcluster()` but takes
#' care of configuring rbmi to be used in the sub-processes as well as loading
#' user defined objects and libraries and setting the seed for reproducibility.
#'
#' If `ncores` is `1` this function will return `NULL`.
#'
#' If `ncores` is a cluster created via `parallel::makeCluster()` then this function
#' just takes care of inserting the relevant rbmi objects into the existing cluster.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' make_rbmi_cluster(5)
#'
#' # User objects + libraries
#' VALUE <- 5
#' myfun <- function(x) {
#'     x + day(VALUE) # From lubridate::day()
#' }
#' make_rbmi_cluster(5, list(VALUE = VALUE, myfun = myfun), c("lubridate"))
#'
#' # Using a already created cluster
#' cl <- parallel::makeCluster(5)
#' make_rbmi_cluster(cl)
#' }
#' @export
make_rbmi_cluster <- function(ncores = 1, objects = NULL, packages = NULL) {

    if (is.numeric(ncores) && ncores == 1) {
        return(NULL)
    } else if (is.numeric(ncores)) {
        cl <- parallel::makePSOCKcluster(ncores)
    } else if (is(ncores, "cluster")) {
        cl <- ncores
    } else {
        stop(sprintf(
            "`ncores` has unsupported class of: %s",
            paste(class(ncores), collapse = ", ")
        ))
    }

    # Load user defined objects into the globalname space
    if (!is.null(objects) && length(objects)) {
        export_env <- list2env(objects)
        parallel::clusterExport(cl, names(objects), export_env)
    }

    # Load user defined packages
    packages <- if (is.null(packages)) {
        # TODO - can't remember why this is needed; need to look into
        "assertthat"
    } else {
        c(packages, "assertthat")
    }
    # Remove attempts to load rbmi as this will be covered later
    packages <- grep("^rbmi$", packages, value = TRUE, invert = TRUE)
    devnull <- parallel::clusterCall(
        cl,
        function(pkgs) lapply(pkgs, function(x) library(x, character.only = TRUE)),
        as.list(packages)
    )

    # Ensure reproducibility
    parallel::clusterSetRNGStream(cl, sample.int(1))

    # If user has previously configured rbmi sub-processes then early exit
    exported_rbmi <- unlist(parallel::clusterEvalQ(cl, exists("..exported..parallel..rbmi")))
    if (all(exported_rbmi)) {
        return(cl)
    }

    # Ensure that exported and unexported objects are all directly accessible
    # from the globalenv in the sub-processes
    if (is_in_rbmi_development()) {
        devnull <- parallel::clusterEvalQ(cl, pkgload::load_all())
    } else {
        devnull <- parallel::clusterEvalQ(
            cl,
            {
                .namespace <- getNamespace("rbmi")
                for (.nsfun in ls(.namespace)) {
                    assign(.nsfun, get(.nsfun, envir = .namespace))
                }
            }
        )
    }

    # Set variable to signify rbmi has been configured
    devnull <- parallel::clusterEvalQ(cl, {
        ..exported..parallel..rbmi <- TRUE
    })

    return(cl)
}


#' Is package in development mode?
#'
#' Returns `TRUE` if the package is being developed on i.e. you have a local copy of the
#' source code which you are actively editing
#' Returns `FALSE` otherwise
#'
#' Main use of this function is in parallel processing to indicate whether the sub-processes
#' need to load the current development version of the code or whether they should load
#' the main installed package on the system
is_in_rbmi_development <- function() {
    path <- tryCatch(
        pkgload::pkg_path(),
        error = function(e) return("")
    )
    if (path == ""){
        return(FALSE)
    }
    if (pkgload::pkg_name() == "rbmi" & file.exists(file.path(path,"misc/do_not_delete.txt"))) {
        return(TRUE)
    }
    return(FALSE)
}



#' Parallelise Lapply
#'
#' Simple wrapper around `lapply` and [`parallel::clusterApplyLB`] to abstract away
#' the logic of deciding which one to use
#' @param cl Cluster created by [`parallel::makeCluster()`] or `NULL`
#' @param fun Function to be run
#' @param x object to be looped over
#' @param ... extra arguements passed to `fun`
par_lapply <- function(cl, fun, x, ...) {
    if (is.null(cl)) {
        return(lapply(x, fun, ...))
    } else {
        return(parallel::clusterApplyLB(cl, x, fun, ...))
    }
}

