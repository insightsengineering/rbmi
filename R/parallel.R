
#' Create cluster
#'
#' @param ncores Number of parallel processes to use
#'
#' If `ncores` is `1` this function will return NULL
#' This function spawns a PSOCK cluster.
#' Ensures that `rbmi` and `assert_that` have been loaded
#' on the sub-processes
#'
get_cluster <- function(ncores = 1) {
    if (ncores == 1) {
        return(NULL)
    }

    cl <- parallel::makePSOCKcluster(
        ncores
    )

    devnull <- parallel::clusterEvalQ(cl, {
        library(assertthat)
    })

    if(is_in_rbmi_development()){
        devnull <- parallel::clusterEvalQ(cl, pkgload::load_all())
    } else {
        devnull <- parallel::clusterEvalQ(cl, {attach(getNamespace("rbmi"))})
    }
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



#' Encapsulate get_mmrm_sample
#'
#' Function creates a new wrapper function around [get_mmrm_sample()]
#' so that the arguments of [get_mmrm_sample()] are enclosed within
#' the new function. This makes running parallel and single process
#' calls to the function smoother. In particular this function takes care
#' of exporting the arguments if required to parallel process in a cluster
#'
#' @seealso [get_cluster()] for more documentation on the function inputs
#'
#' @param cl Either a cluster from [get_cluster()] or `NULL`
#' @param longdata A longdata object from `longDataConstructor$new()`
#' @param method A method object
#' @param optimizer an optimizer list
encap_get_mmrm_sample <- function(cl, longdata, method) {
    fun <- function(ids) {
        get_mmrm_sample(
            ids = ids,
            longdata = longdata,
            method = method
        )
    }
    lfun <- function(ids) {
        lapply(ids, fun)
    }

    if (is.null(cl)) {
        return(lfun)
    }

    parallel::clusterExport(
        cl = cl,
        varlist = c("longdata", "method"),
        envir = environment()
    )

    lfun <- function(ids) {
        parallel::clusterApplyLB(cl, ids, fun)
    }

    return(lfun)
}


