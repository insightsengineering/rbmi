

subjectConstructor <- R6::R6Class(
    classname = "subject",
    public = list(

        subjid = NULL,
        indexes = NULL,
        is_missing = NULL,
        is_mar = NULL,
        strategy = NULL,
        group = NULL,
        rmData = NULL,

        initialize = function(
            subjid,
            indexes,
            is_missing,
            group,
            rmData,
            is_mar = NULL,
            strategy = NULL
        ){
            self$subjid <- subjid
            self$indexes <- indexes
            self$is_missing <- is_missing
            self$is_mar <- is_mar
            self$strategy <- strategy
            self$group <- group
            self$rmData <- rmData
        }

    )
)


rmDataConstructor <- R6::R6Class(
    classname = "rmData",

    private = list(
        get_data_list = function(imputations){
            len <- length(imputations)
            impute_values <- vector(mode = "list", length = len)
            data_index <- vector(mode = "list", length = len)
            missing_index <- vector(mode = "list", length = len)
            new_ids <- vector(mode = "list", length = len)

            ids <- vapply(imputations, function(x) x$id, FUN.VALUE = character(1))
            subjects <- self$get_subjects(ids)

            for(index in seq_len(len)){

                imputation <- imputations[[index]]
                subject <- subjects[[imputation$id]]
                missing <- subject$is_missing

                stopifnot( sum(missing) == length(imputation$values))

                impute_values[[index]] <- imputation$values
                data_index[[index]] <- subject$indexes
                missing_index[[index]] <- missing
                new_ids[[index]] <- rep(paste0("new_subjid_", index), length(missing))

            }

            missing_vec <- unlist(missing_index)
            impute_vec <- unlist(impute_values)
            data_vec <- unlist(data_index)
            new_ids_vec <- unlist(new_ids)

            dat <- self$data[data_vec,]
            dat[missing_vec, self$vars$outcome] <- impute_vec
            dat[,self$vars$subjid] <- new_ids_vec
            return(dat)
        },


        get_data_ids = function(ids) {
            self$validate_ids(ids,throw_error = TRUE)

            count <- tapply(ids, ids, length)
            subjects <- self$subjects
            indexes_list <- lapply(
                names(count),
                function(id) subjects[[id]]$indexes
            )

            indexes_list_rep <- mapply(
                function(x,y) rep(x, times = y),
                indexes_list,
                count,
                SIMPLIFY = FALSE
            )

            lens_list <- lapply(
                indexes_list,
                function(x) length(x)
            )

            lens_list_rep <- mapply(
                function(x,y) rep(x, times = y),
                lens_list,
                count,
                SIMPLIFY = FALSE
            )

            lens <- unlist(lens_list_rep)
            indexes <- unlist(indexes_list_rep)

            new_subjid <- paste0("new_subjid_", seq_along(lens))
            new_subjid_column <- rep(new_subjid, times = lens)

            new_data <- self$data[indexes,]
            new_data[[self$vars$subjid]] <- new_subjid_column

            return(new_data)
        }
    ),

    public = list(
        data = NULL,
        vars = NULL,
        visits = NULL,
        subjects = NULL,


        get_data = function(obj){
            if("list" %in% class(obj)) {
                return(private$get_data_list(obj))
            } else if( "character" %in% class(obj)){
                return(private$get_data_ids(obj))
            } else {
                stop("Invalid type")
            }
        },


        add_subject = function(id) {
            vars <- self$vars
            ids <- self$data[[vars$subjid]]
            indexes <- which(ids == id)
            data_subject <- self$data[indexes,]
            is_missing <- is.na(data_subject[[vars$outcome]])
            group <- unique(data_subject[[vars$group]])
            existing_id <- self$validate_ids(id)
            stopifnot(
                length(indexes) >= 1,
                length(group) == 1,
                length(is_missing) == length(indexes),
                !existing_id
            )
            self$subjects[[id]] <- subjectConstructor$new(
                subjid = id,
                indexes = indexes,
                is_missing = is_missing,
                group = group,
                rmData = self,
            )
        },


        validate_ids = function(ids, throw_error = FALSE){
            is_in <- ids %in% names(self$subjects)
            if( throw_error){
                if(! all(is_in)){
                    stop("subjids are not in self")
                }
            }
            return(invisible(is_in))
        },


        get_subject = function(id) {
            return(self$subjects[[id]])
        },

        get_subjects = function(ids) {
            return(self$subjects[unique(ids)])
        },


        add_strategy = function(x) {},
        get_strategy = function(x) {},


        initialize = function(data, vars){
            self$data = data
            self$vars = vars

            keyvars <- c(
                vars$outcome,
                vars$group,
                vars$visit,
                vars$subjid
            )

            stopifnot(
                all(keyvars %in% names(data)),
                is.character(data[[vars$subjid]]) | is.factor(data[[vars$subjid]])
            )

            subjects = unique(data[[vars$subjid]])

            self$subjects <- vector(mode = "list", length = length(subjects))
            for( id in subjects){
                self$add_subject(id)
            }
        }

    )
)









library(dplyr)


n <- 4
nv <- 3


covars <- tibble(
    id = 1:n,
    age = rnorm(n),
    group = factor(sample(c("A", "B"), size = n, replace = TRUE), levels = c("A", "B")),
    sex = factor(sample(c("M", "F"), size = n, replace = TRUE), levels = c("M", "F"))
)

dat <- tibble(
    id = rep.int(1:n, nv)
) %>%
    left_join(covars, by = "id") %>%
    mutate( outcome = rnorm(
        n(),
        age * 3 + (as.numeric(sex) - 1) * 3 + (as.numeric(group) - 1) * 4,
        sd = 3
    )) %>%
    arrange(id) %>%
    group_by(id) %>%
    mutate( visit = factor(paste0("Visit ", 1:n())))  %>%
    ungroup() %>%
    mutate(id = as.character(id))


dat[c(1,2,3,4,5,7), "outcome"] <- NA






rmd <- rmDataConstructor$new(
    data = dat,
    vars = list(
        outcome = "outcome",
        visit = "visit",
        subjid = "id",
        group = "group"
    )
)
implist <- list(
    list( id = "1", values = c(1,2,3)),
    list( id = "1", values = c(4,5,6)),
    list( id = "2", values = c(7,8)),
    list( id = "3", values = c(9)),
    list( id = "1", values = c(10,11,12))
)
rmd$get_data(implist)

time_it <- function(expr){
    start <- Sys.time()
    expr
    stop <- Sys.time()
    difftime(stop, start, units = "secs")
}



implist <- list()
for( i in 1:10000){
    implist[[i]] <- list(
        id = "1",
        values = c(3,4,5)
    )
}

time_it({
    for( i in 1:1000){
        rmd$get_data(implist)
    }
})
























