
#' Create cluster
#'
#' @param ncores Number of parallel processes to use
#'
#' This function spawns a PSOCK cluster and exports all of the
#' rbmi namespace into the the sub processes as well as loading
#' assertthat and glmmTMB
get_cluster <- function(ncores = 1) {
    if (ncores == 1) {
        return(NULL)
    }

    cl <- parallel::makePSOCKcluster(
        ncores
    )

    pkgenv <- environment(get_cluster)

    parallel::clusterEvalQ(cl, {
        library(assertthat)
        library(glmmTMB)
    })

    for (fun in ls(pkgenv)) {
        parallel::clusterCall(
            cl = cl,
            fun = assign,
            fun,
            get(fun, envir = pkgenv),
            envir = .GlobalEnv
        )
    }
    return(cl)
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
encap_get_mmrm_sample <- function(cl, longdata, method, optimizer) {
    fun <- function(ids) {
        get_mmrm_sample(
            ids = ids,
            longdata = longdata,
            method = method,
            optimizer = optimizer
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
        varlist = c("longdata", "method", "optimizer"),
        envir = environment()
    )

    lfun <- function(ids) {
        parallel::clusterApplyLB(cl, ids, fun)
    }

    return(lfun)
}


