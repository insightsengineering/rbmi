
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


#' Expand dataframe into a design matrix
#'
#' Expands out a dataframe using a formula to create a design matrix.
#' Key details are that it will always place the outcome variable into 
#' the first column of the return object.
#'
#' The outcome column may contain NA's but none of the other variables
#' listed in the formula should contain missing values
#'
#' @param dat a data.frame
#' @param frm a formula
as_model_df <- function(dat, frm){

    outcome <- as.character(attr(stats::terms(frm), "variables")[[2]])
    is_missing <- is.na(dat[[outcome]])
    dat[[outcome]][is_missing] <- 999
    design_mat <- stats::model.matrix(frm, dat)
    dat[[outcome]][is_missing] <- NA

    assert_that(
        nrow(design_mat) == nrow(dat),
        msg = "Model matrix has less rows than input dataset. You may have missing values."
    )
    full_mat <- cbind(dat[[outcome]], design_mat)
    colnames(full_mat)[[1]] <- outcome
    design <- as.data.frame(full_mat)
    class(design) <- class(dat)
    return(design)
}


#' Title - TODO
#' 
#' Converts all character variables within a dataframe to factor
#' 
#' @param data A dataframe
char2fct <- function(data) {
    for (v in colnames(data)) {
        if (is.character(data[[v]])) {
            data[[v]] <- factor(data[[v]])
        }
    }
    return(data)
}


#' Title - TODO
#'
#' A wrapper around if() else() to prevent unexpected
#' interactions between ifelse() and factor variables
#' 
#' @param x True / False
#' @param a value to return if True
#' @param b value to return if False
ife <- function(x, a, b){
    if (x) {
        return(a)
    } else {
        return(b)
    }
}