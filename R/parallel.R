

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
            envir = pkgenv
        )
    }
    return(cl)
}


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


