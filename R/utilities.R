
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

    outcome <- as.character(attr(stats::terms(frm), "variables")[[2]])
    dat[[outcome]][is.na(dat[[outcome]])] <- Inf
    design_mat <- stats::model.matrix(frm, dat)
    dat[[outcome]][is.infinite(dat[[outcome]])] <- NA

    assert_that(
        nrow(design_mat) == nrow(dat),
        msg = "Model matrix has less rows than input dataset. You may have missing values."
    )

    full_mat <- cbind(dat[[outcome]] , design_mat)
    colnames(full_mat)[1] <- outcome
    design <- as.data.frame(full_mat)
    return(design)
}
