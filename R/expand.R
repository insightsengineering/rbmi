




locf <- function(x) {
    res <- vector(typeof(x), length(x))
    inds <- cumsum(!is.na(x))
    x[inds > 0L] <- x[!is.na(x)][inds]
    x
}



expand <- function(data, ..., .fill_fun = locf, .fill_vars = NULL, .fill_group = NULL){

    vars <- list(...)

    assert_that(
        is.data.frame(data),
        msg = "`data` must be a data.frame"
    )

    if(length(vars) != 0 ){
        for(var in names(vars)){

            assert_that(
                var %in% names(data),
                msg = sprintf("Variable `%s` does not exist in `data`", var)
            )

            df_val <- unique(data[[var]])

            assert_that(
                is.character(df_val) | is.factor(df_val),
                msg = sprintf(
                    "Variable `%s` is neither character nor factor. Cannot expand non-categorical variables",
                    var
                )
            )

            assert_that(
                all( df_val %in% vars[[var]]),
                msg = sprintf(paste(
                    "Variable `%s` contains values/levels that were not specified.",
                    "Please remove any levels that are not required prior to using this function"
                ), var)
            )
        }

        reference <- expand.grid(vars, stringsAsFactors = FALSE)
        df2 <- merge(reference, data, by = names(vars), all.x = TRUE)

        for( var in names(vars)){
            df2[[var]] <- factor( df2[[var]], levels = vars[[var]])
        }
    } else {
        df2 <- data
    }

    if( !is.null(.fill_vars)){
        assert_that(
            is.null(.fill_vars) | is.character(.fill_vars),
            is.null(.fill_group) | is.character(.fill_group),
            msg = "`.vars` and `.group` must be either NULL or a character vector"
        )

        assert_that(
            is.function(.fill_fun),
            msg = "`.fun` must be a function"
        )

        for( var in .fill_vars){
            assert_that(
                var %in% names(df2),
                msg = sprintf("Variable `%s` does not exist in `data`", var)
            )
        }

        group_index <- as_strata(df2[.fill_group])

        for( var in c(.fill_vars,.fill_group)){
            df2[[var]] <- unlist(tapply( df2[[var]], group_index, .fill_fun), use.names = FALSE)
        }
    }

    class(df2) <- class(df)
    return(df2)
}

