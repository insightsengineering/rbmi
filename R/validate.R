

# TODO - More meaningful error messages

validate_datalong <- function(data, vars){
    validate_datalong_varIsChar(vars)
    validate_datalong_varExists(data, vars)
    validate_datalong_types(data,vars)
    validate_datalong_notMissing(data, vars)
    validate_datalong_complete(data, vars)
    validate_datalong_unifromStrata(data,vars)
    return(invisible(TRUE))
}


validate_dataice<- function(data_ice, vars, visits){
    # TODO
    return(invisible(TRUE))
}


validate_datalong_varIsChar <- function(vars){
    covars <- extract_covariates(vars$covariates)
    stopifnot(
        is_char_one(vars$outcome),
        is_char_one(vars$group),
        is_char_one(vars$visit),
        is_char_one(vars$subjid),
        is.character(vars$strata),
        is.character(covars)
    )
    return(invisible(TRUE))
}


validate_datalong_varExists <- function(data, vars){
    covars <- extract_covariates(vars$covariates)
    stopifnot(
        vars$outcome %in% names(data),
        vars$group %in% names(data),
        vars$visit %in% names(data),
        vars$subjid %in% names(data),
        all(vars$strata %in% names(data)) | is.null(vars$strata),
        all(covars %in% names(data)) | is.null(covars)
    )
    return(invisible(TRUE))
}


validate_datalong_types <- function(data, vars){
    covars <- extract_covariates(vars$covariates)
    stopifnot(
        is_char_fact(data[[vars$subjid]]),
        is_char_fact(data[[vars$group]]),
        is.factor(data[[vars$visit]]),
        is.numeric(data[[vars$outcome]])
    )
    if(!is.null(c(covars, vars$strata))){
        for( var in covars){
            if( !is_num_char_fact(data[[var]])){
                stop("Invalid Data Type")
            }
        }
    }
    return(invisible(TRUE))
}


validate_datalong_notMissing <- function(data, vars){
    non_missing_variables <- c(
        vars$group,
        vars$visit,
        vars$subjid,
        vars$strata,
        extract_covariates(vars$covariates)
    )
    for( var in non_missing_variables){
        if( any(is.na(data[[var]]))){
            stop(paste0("Variable ", var, " contains missing data"))
        }
    }
    return(invisible(TRUE))
}


validate_datalong_complete <- function(data, vars){
    unique_subjects <- unique(data[[vars$subjid]])
    unique_visits <- levels(data[[vars$visit]])
    is_complete_list <- tapply(
        data[[vars$visit]],
        data[[vars$subjid]],
        function(x) {
            all(unique_visits %in% x) & length(unique_visits) == length(x)
        },
        simplify = FALSE
    )
    is_complete <- unlist(is_complete_list, use.names = FALSE)
    if( ! all(is_complete)){
        stop("At least one subject has incomplete data")
    }
    return(invisible(TRUE))
}


validate_datalong_unifromStrata <- function(data, vars){
    for( var in vars$strata){
        x <- tapply(
            data[[var]],
            data[[vars$subjid]],
            function(x) length(unique(x))
        )
        if(!all(x ==1)){
            stop(
                "Stratification variable '", var,
                "' is not constant within at least one subject"
            )
        }
    }
    return(invisible(TRUE))
}


extract_covariates <- function(x){
    x_split <- strsplit(x, ":|\\*")
    unique(unlist(x_split, use.names = FALSE))
}


is_char_one <- function(x){
    is.character(x) & (length(x) == 1)
}


is_char_fact <- function(x){
    is.character(x) | is.factor(x)
}


is_num_char_fact <- function(x){
    is.numeric(x) | is.character(x) | is.factor(x)
}

