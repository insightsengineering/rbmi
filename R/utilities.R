
#' Title
#'
#' @param x TODO
#' @param cls TODO
#' @export
as_class <- function(x, cls){
    class(x) <- cls
    return(x)
}



#' Title
#'
#' @param vars TODO
as_simple_formula <- function(vars){
    variables <- c(
        vars$group,
        vars$visit,
        vars$covariates
    )
    frm <- stats::as.formula(
        paste0(
            vars$outcome,
            "~ 1 + ",
            paste0( variables, collapse = " + " )
        )
    )
    return(frm)
}


#' Title
#'
#' @param dat TODO
#' @param frm TODO
as_model_df <- function(dat, frm){
    design_mat <- stats::model.matrix(frm, dat)
    stopifnot( nrow(design_mat) == nrow(dat) )
    outcome <- as.character(attr(stats::terms(frm), "variables")[[2]])
    full_mat <- cbind(dat[[outcome]] , design_mat)
    design <- as.data.frame(full_mat)
    return(design)
}
